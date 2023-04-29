{-# LANGUAGE GADTs #-}

import Control.Applicative
import Control.Category (Category)
import qualified Control.Category ((.), id)
import Control.Arrow
import Data.Typeable

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
    --o = source_iter(vec![f])

    --first (Channel a) = Channel (first a) 

    --(Channel a) *** (Channel b) = Channel (a *** b)
    (***) = CMerge
    -- o = join()
    -- a -> enumerate() -> [0]o
    -- b -> enumerate() -> [1]o


select2 :: Channel i Bool -> Channel i (o, o) -> Channel i o
select2 p c = CTag "selector" $ liftA2 selector p c
    where
        selector True  (o, _) = o
        selector False (_, o) = o

--splits into channel a or channel b and selects output depending on the predicate,
--diamond :: (i -> Bool) -> Channel i o -> Channel i o -> Channel i o


cDouble  = CTag "*2" $ arr ((*2) :: Int -> Int)
cPlusOne = CTag "+1" $ arr ((+1) :: Int -> Int)

cPlusOnePlusDouble = CTag "+" $ liftA2 (+) cPlusOne cDouble

--double if even, plusOne if odd
cDoubleOrPlus = select2 (CTag "even" $ arr even) c
    where c = CTag "(,)" $ liftA2 (,) cDouble cPlusOne

-- for example, 5 will output (6, 12)
cFancy = cDoubleOrPlus &&& (cDoubleOrPlus >>> cDoubleOrPlus)
--         CMap    :: (b -> c) -> Channel a b                      -> Channel a c
--         CPure   :: b                                            -> Channel a b
--         CLift   :: (b -> c -> d) -> Channel a b -> Channel a c  -> Channel a d
--         CId     ::                                                 Channel a a
--         CComp   :: Channel b c -> Channel a b                   -> Channel a c
--         CArr    :: (a -> b)                                     -> Channel a b
--         CMerge  :: Channel a b -> Channel a' b'                 -> Channel (a, a') (b, b')

-- m = Map.fromList [(t2, "a"), (p1, "b")]



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


-- unwrap Nothing = ""
-- unwrap (Just s) = s

--main = print $ show cDouble -- "[CArr, f: *2]"
--main = print $ show cDoubleOrPlus -- "[CLift f:selector, a:[CArr, f: even], b:[CLift f:(,), a:[CArr, f: *2], b:[CArr, f: +1]]]"

main = print $ show cFancy
