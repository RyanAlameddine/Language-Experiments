{-# LANGUAGE GADTs #-}
module HydroStrArr (generateOutput) where

import Control.Applicative
import Control.Category (Category)
import qualified Control.Category ((.), id)
import Control.Arrow
import Data.Typeable
import Text.Format

import qualified Data.Map as Map    

-- data Channel a b = CMap String (Channel c b)                    --CMap "|x| x * 5" (c :: String -> Int)
--                     | CPure b                                   --CPure 5
--                     | CLift String (Channel a c) (Channel a d)  --CLift "|(x, y)| x + y" (c :: String -> Int) (c :: String -> Int)
--                     | CId                                       --CId
--                     | CCompose (Channel a c) (Channel c b)      --CCompose (c :: String -> Int) (c :: Int -> Bool)
--                     | CArr String                               --CArr "|x| x + 4"
--                     | CMerge (Channel a' b') (Channel a'' b'')  --CMerge (c :: String -> Int) (c :: Bool -> Long)

--                     deriving Show

data Channel x y where
        CMap    :: (b -> c) -> Channel a b                      -> Channel a c
        CPure   :: b                                            -> Channel a b
        CLift   :: (b -> c -> d) -> Channel a b -> Channel a c  -> Channel a d
        CId     ::                                                 Channel a a
        CComp   :: Channel b c -> Channel a b                   -> Channel a c
        CArr    :: (a -> b)                                     -> Channel a b
        CMerge  :: Channel a b -> Channel a' b'                 -> Channel (a, a') (b, b')
        CTag    :: String -> Channel a b -> Channel a b --tag with Rust version of f
                    

instance Functor (Channel a) where
    --fmap f (Channel c) = Channel (fmap f c) --map over result
    fmap = CMap
    --o = i -> map(f)

instance Applicative (Channel a) where
    --pure = Channel . pure  --channel that throws away it's input and returns whatever is passed into pure
    pure = CPure
    --o = map(|x| constant)

    --liftA2 f (Channel a) (Channel b) = Channel (liftA2 f a b)  --Channel (f <$> a <*> b) --merge the result of two channels with a cross join, then apply f
    liftA2 = CLift
    --o = cross_join() -> map(|(a, b)| f(a, b))
    --a -> [0]o
    --b -> [1]o

instance Category Channel where
    --id = Channel id
    id = CId
    --o = map(|x| x)

    --(Channel a) . (Channel b) = Channel (a . b)
    (.) = CComp
    --o = a -> b
    
instance Arrow Channel where
    arr = CArr
    --o = map(f)    NOT THIS: --o = source_iter(vec![f])

    --first (Channel a) = Channel (first a) 

    --(Channel a) *** (Channel b) = Channel (a *** b)
    (***) = CMerge
    -- o = join()
    -- a -> enumerate() -> [0]o
    -- b -> enumerate() -> [1]o

    -- THIS IS JUST TO ADD A TAG, OTHERWISE COPIED FROM SOURCE
    f &&& g = arrT "|x| (x, x)" (\b -> (b,b)) >>> f *** g

--HELPERS TO ADD TAGS
arrT    t = CTag t . arr
liftA2T t f a b = CTag t (liftA2 f a b)


select2 :: Channel i Bool -> Channel i (o, o) -> Channel i o
select2 = liftA2T "|(b, (o1, o2))| if b {o1} else {o2}" selector
    where
        selector True  (o, _) = o
        selector False (_, o) = o

--splits into channel a or channel b and selects output depending on the predicate,
--diamond :: (i -> Bool) -> Channel i o -> Channel i o -> Channel i o


cDouble  = arrT "|x| x * 2" ((*2) :: Int -> Int)
cPlusOne = arrT "|x| x + 1" ((+1) :: Int -> Int)

cPlusOnePlusDouble = liftA2T "|(x, y)| x + y" (+) cPlusOne cDouble


--double if even, plusOne if odd
cDoubleOrPlus = select2 (arrT "|x| x % 2 == 0" even) c
    where c = liftA2T "|(x, y)| (x, y)" (,) cDouble cPlusOne

-- for example, 5 will output (6, 12)
cFancy = cDoubleOrPlus &&& (cDoubleOrPlus >>> cDoubleOrPlus)




instance Show (Channel a b) where
    show = s ""
        where
            s t (CMap  f c)    = "[CMap f:" ++ t ++ ", c:" ++ show c ++ "]"
            s t (CPure b)      = "[CPure]"
            s t (CLift f a b)  = "[CLift f:" ++ t ++ ", a:" ++ show a ++ ", b:" ++ show b ++ "]"
            s t (CComp b a)    = "[CComp, a:" ++ show a ++ ", b:" ++ show b ++ "]"
            s t (CArr  f)      = "[CArr, f: " ++ t ++ "]"
            s t (CMerge a b)   = "[CMerge, a:" ++ show a ++ ", b:" ++ show b ++ "]"
            s t CId            = "[CId]"
            s _ (CTag t c)     = s t c

-- -- input: channel number of input to channel, i: counter for naming purposes, tag: function in Rust, c: channel to transform to rust
-- --returns: (next i, rust string)
-- --o = i -> map(f)
-- toRust :: Int -> String -> Channel a b -> (Int, String)
-- toRust i tag (CMap  f c)   = (cI, cStr ++ format "c{0} = c{1} -> map({2});\n" [show i, show (i + 1), tag])
--     where (cI, cStr) = toRust (i + 1) "" c
-- --o = map(|x| constant)
-- toRust i tag (CPure b)     = (i + 1, format "c{0} = map(|_| CONSTANT);\n" [show i])
-- --o = cross_join() -> map(|(a, b)| f(a, b))
-- --a -> [0]o
-- --b -> [1]o
-- toRust i tag (CLift f a b) = (bI, aStr ++ bStr ++ format "c{0} = tee();\nc{1} = cross_join() -> map({4});\nc{0}[0] -> c{2} -> [0]c{1};\nc{0}[1] -> c{3} -> [1]c{1};\n" [show i, show $ i + 1, show $ i + 2, show aI, tag])
--     where
--         (aI, aStr) = toRust (i + 2) "" a
--         (bI, bStr) = toRust aI "" b
-- --o = a -> b
-- toRust i tag (CComp b a)   = (bI, aStr ++ bStr ++ format "c{0} = c{2} -> c{1};\n" [show i, show aI, show $ i + 1])
--     where
--         (aI, aStr) = toRust (i + 1) "" a
--         (bI, bStr) = toRust aI "" b
-- --o = map(f)
-- toRust i tag (CArr  f)     = (i + 1, format "c{0} = map({1});\n" [show i, tag])
-- -- o = join()
-- -- a -> enumerate() -> [0]o
-- -- b -> enumerate() -> [1]o
-- toRust i tag (CMerge a b)  = (bI, aStr ++ bStr ++ format "c{0} = join();\nc{1} -> enumerate() -> [0]c{0};\nc{2} -> enumerate() -> [1]c{0};\n" [show i, show aI, show $ i + 1])
--     where
--         (aI, aStr) = toRust (i + 1) "" a
--         (bI, bStr) = toRust aI "" b
-- --o = map(|x| x)
-- toRust i tag CId           = (i + 1, format "c{0} = map(|x| x);\n" [show i]);
-- toRust i _   (CTag t c)    = toRust i t c


-- channel -> (hydroflow, input )
rustGen :: Channel i o -> String
rustGen c = format "\n{0}\ninput -> c0;\nc{1} -> output;" [rust, show o]
    where (rust, o, _) = toRust 0 "" c

-- input #, tag, channel -> (HydroFlow string, output #, next free)
toRust :: Int -> String -> Channel i o -> (String, Int, Int)
--o = i -> map(f)
--inputC = inputA
--outputC = outputA -> map(f)
toRust inputC tag (CMap  f a) = (strA ++ strC, outputC, nextFree)
    where
        outputC = succ inputC
        inputA = succ outputC
        (strA, outputA, nextFree) = toRust inputA "" a
        strC = format "c{0} = c{1};\nc{2} = c{3} -> map({4});\n" $ map show [inputC, inputA, outputC, outputA] ++ [tag]
--o = map(|x| constant)
--inputOutputC = map(|x| constant)
toRust inOutC tag (CPure b) = (strC, inOutC, succ inOutC)
    where
        strC = format "c{0} = map(|x| {1});\n" [show inOutC, tag]
--o = cross_join() -> map(|(a, b)| f(a, b)); a -> [0]o; b -> [1]o
--inputC = tee()
--inputC[0] -> inputA
--inputC[1] -> inputB
--outputC = cross_join<'tick, 'tick>() -> map(f)
--outputA -> [0]outputC
--outputB -> [1]outputC
toRust inputC tag (CLift f a b) = (strA ++ strB ++ strC, outputC, nextFree)
    where
        outputC = succ inputC
        inputA = succ outputC
        (strA, outputA, inputB)   = toRust inputA "" a
        (strB, outputB, nextFree) = toRust inputB "" b
        strC = format "c{0} = tee();\nc{0}[0] -> c{1};\nc{0}[1] -> c{2};\nc{3} = cross_join::<'tick, 'tick>() -> map({6});\nc{4} -> [0]c{3};\nc{5} -> [1]c{3};\n" $ map show [inputC, inputA, inputB, outputC, outputA, outputB] ++ [tag]
--o = a -> b
--inputC = inputA
--outputA -> inputB
--outputC = outputB
toRust inputC tag (CComp b a) = (strA ++ strB ++ strC, outputC, nextFree)
    where
        outputC = succ inputC
        inputA = succ outputC
        (strA, outputA, inputB)   = toRust inputA "" a
        (strB, outputB, nextFree) = toRust inputB "" b
        strC = format "c{0} = c{1};\nc{4} -> {2};\n{3} = {5};\n" $ map show [inputC, inputA, inputB, outputC, outputA, outputB] ++ [tag]
--o = map(f)
--inputOutputC = map(f)
toRust inOutC tag (CArr f) = (strC, inOutC, succ inOutC)
    where
        strC = format "c{0} = map({1});\n" [show inOutC, tag]
-- o = join();a -> enumerate() -> [0]o;b -> enumerate() -> [1]o
--inputC = tee()
--inputC[0] -> map(|(x, _)| x) -> inputA
--inputC[1] -> map(|(_, x)| x) -> inputB
--outputC = join<'tick, 'tick>() -> map(|(k, v)| v);
--outputA -> enumerate() -> [0]outputC;
--outputB -> enumerate() -> [1]outputC;
toRust inputC tag (CMerge b a) = (strA ++ strB ++ strC, outputC, nextFree)
    where
        outputC = succ inputC
        inputA = succ outputC
        (strA, outputA, inputB)   = toRust inputA "" a
        (strB, outputB, nextFree) = toRust inputB "" b
        strC = format "c{0} = tee();\nc{0}[0] -> map(|(x, _)| x) -> c{1};\nc{0}[1] -> map(|(_, x)| x) -> c{2};\nc{3} = join::<'tick, 'tick>() -> map(|(k, v)| v);\nc{4} -> enumerate() -> [0]c{3};\nc{5} -> enumerate() -> [1]c{3};\n" $ map show [inputC, inputA, inputB, outputC, outputA, outputB] ++ [tag]
--o = map(|x| x)
toRust inOutC tag (CPure b) = (strC, inOutC, succ inOutC)
    where
        strC = format "c{0} = map(|x| x);\n" [show inOutC, tag]
--tag
toRust i _ (CTag t c)    = toRust i t c


-- unwrap Nothing = ""
-- unwrap (Just s) = s

--main = print $ show cDouble -- "[CArr, f: *2]"
--main = print $ show cDoubleOrPlus -- "[CLift f:selector, a:[CArr, f: even], b:[CLift f:(,), a:[CArr, f: *2], b:[CArr, f: +1]]]"

cTest =  liftA2T "|(x, y)| x + y" (+) cPlusOne cDouble `CComp` cDouble



output = cDoubleOrPlus

generateOutput = putStr $ rustGen output
