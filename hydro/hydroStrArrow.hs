{-# LANGUAGE GADTs #-}

import Control.Applicative
import Control.Category (Category)
import qualified Control.Category ((.), id)
import Control.Arrow
import Data.Typeable

import qualified Data.Map as Map    

-- data Channel a b = 
--                    CMap (a -> b) (Channel a b)                    --CMap "|x| x * 5" (c :: String -> Int)
--                     | CPure b                                   --CPure 5
--                     | CLift (a -> b) (Channel a b) (Channel a b)  --CLift "|(x, y)| x + y" (c :: String -> Int) (c :: String -> Int)
--                     | CId                                       --CId
--                     | CComp (Channel a b) (Channel a b)         --CCompose (c :: String -> Int) (c :: Int -> Bool)
--                     | CArr (a -> b)                               --CArr "|x| x + 4"
--                     | CMerge (Channel a b) (Channel a b)        --CMerge (c :: String -> Int) (c :: Bool -> Long)
                    

data Channel x y where
        CMap    :: (b -> c) -> Channel a b                      -> Channel a c
        CPure   :: b                                            -> Channel a b
        CLift   :: (b -> c -> d) -> Channel a b -> Channel a c  -> Channel a d
        CId     ::                                                 Channel a a
        CComp   :: Channel b c -> Channel a b                   -> Channel a c
        CArr    :: (a -> b)                                     -> Channel a b
        CMerge  :: Channel a b -> Channel a' b'                 -> Channel (a, a') (b, b')
                    

instance Functor (Channel String) where
    --fmap f (Channel c) = Channel (fmap f c) --map over result
    fmap = CMap
    --o = i -> map(f)

instance Applicative (Channel String) where
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

cs :: String -> String -> String
cs = const

cs2 :: String -> String -> String -> String
cs2 = const . const



--select2 :: (i -> Bool) -> Channel i (o, o) -> Channel i o
select2 p = liftA2 (cs2 "|(p, (o1, o2))| p ? o1 : o2") (arr p)

--splits into channel a or channel b and selects output depending on the predicate,
--diamond :: (i -> Bool) -> Channel i o -> Channel i o -> Channel i o



cDouble  = arr (cs "|x| x * 2") :: Channel String String
cPlusOne = arr (cs "|x| x + 1") :: Channel String String

cPlusOnePlusDouble = liftA2 (cs2 "|(x, y)| x + y") cPlusOne cDouble

--double if even, plusOne if odd
cDoubleOrPlus = select2 (cs "|x| x % 2 == 0") c
    where c = liftA2 (cs2 "|(x, y)| (x, y)") cDouble cPlusOne

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



instance Show (Channel String String) where
    show (CMap  (f :: String -> String) c)    = "[CMap f:" ++ f "" ++ ", c:" ++ show c ++ "]"
    show (CPure b)      = "[CPure]"
    show (CLift f a b)  = "[CLift f:" ++ f "" ++ ", a:" ++ show a ++ ", b:" ++ show b ++ "]"
    show (CComp b a)    = "[CComp, a:" ++ show a ++ ", b:" ++ show b ++ "]"
    show (CArr  f)      = "[CArr, f: " ++ f "" ++ "]"
    show (CMerge a b)   = "[CMerge, a:" ++ show a ++ ", b:" ++ show b ++ "]"
    show CId            = "[CId]"
    show _ = "Error"


-- unwrap Nothing = ""
-- unwrap (Just s) = s

main = print $ show cFancy-- ++ unwrap (Map.lookup (+1) m)
