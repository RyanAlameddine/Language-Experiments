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
select2 = liftA2T "|(b, (o1, o2))| b ? o1 : o2" selector
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

-- input: channel number of input to channel, i: counter for naming purposes, tag: function in Rust, c: channel to transform to rust
--returns: (next i, rust string)
--o = i -> map(f)
toRust :: Int -> String -> Channel a b -> (Int, String)
toRust i tag (CMap  f c)   = (cI, cStr ++ format "c{0} = c{1} -> map({2});\n" [show i, show (i + 1), tag])
    where (cI, cStr) = toRust (i + 1) "" c
--o = map(|x| constant)
toRust i tag (CPure b)     = (i + 1, format "c{0} = map(|_| CONSTANT);\n" [show i])
--o = cross_join() -> map(|(a, b)| f(a, b))
--a -> [0]o
--b -> [1]o
toRust i tag (CLift f a b) = (bI, format "c{0} = cross_join() -> map({3});\nc{1} -> [0]c{0};\nc{2} -> [1]c{0};\n" [show i, show $ i + 1, show aI, tag])
    where
        (aI, aStr) = toRust (i + 1) "" a
        (bI, bStr) = toRust aI "" b
--o = a -> b
toRust i tag (CComp b a)   = (bI, format "c{0} = c{1} -> c{2};\n" [show i, show aI, show $ i + 1])
    where
        (aI, aStr) = toRust (i + 1) "" a
        (bI, bStr) = toRust aI "" b
--o = map(f)
toRust i tag (CArr  f)     = (i + 1, format "c{0} = map({1});\n" [show i, tag])
-- o = join()
-- a -> enumerate() -> [0]o
-- b -> enumerate() -> [1]o
toRust i tag (CMerge a b)  = (bI, format "c{0} = join();\nc{1} -> enumerate() -> [0]c{0};\nc{2} -> enumerate() -> [1]c{0};\n" [show i, show aI, show $ i + 1])
    where
        (aI, aStr) = toRust (i + 1) "" a
        (bI, bStr) = toRust aI "" b
--o = map(|x| x)
toRust i tag CId           = (i + 1, format "c{0} = map(|x| x);\n" [show i]);
toRust i _   (CTag t c)    = toRust i t c

-- unwrap Nothing = ""
-- unwrap (Just s) = s

--main = print $ show cDouble -- "[CArr, f: *2]"
--main = print $ show cDoubleOrPlus -- "[CLift f:selector, a:[CArr, f: even], b:[CLift f:(,), a:[CArr, f: *2], b:[CArr, f: +1]]]"

output = cDouble
generateOutput = putStr rust
    where 
        (i, raw) = toRust 0 "" output
        rust = format "\n{1}\ninput -> c0 -> output;" ["c" ++ show i, raw]
--generateOutput = print $ show cFancy
