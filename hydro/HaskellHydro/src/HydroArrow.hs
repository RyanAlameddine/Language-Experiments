{-# LANGUAGE GADTs #-}
module HydroArrow  where

import Control.Applicative
import Control.Category (Category)
import qualified Control.Category ((.), id)
import Control.Arrow
import Data.Typeable
import Text.Format

import qualified Data.Map as Map    

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

instance Applicative (Channel a) where
    --pure = Channel . pure  --channel that throws away it's input and returns whatever is passed into pure
    pure = CPure

    --liftA2 f (Channel a) (Channel b) = Channel (liftA2 f a b)  --Channel (f <$> a <*> b) --merge the result of two channels with a cross join, then apply f
    liftA2 = CLift

instance Category Channel where
    --id = Channel id
    id = CId
    --(Channel a) . (Channel b) = Channel (a . b)
    (.) = CComp
    
instance Arrow Channel where
    arr = CArr
    --(Channel a) *** (Channel b) = Channel (a *** b)
    (***) = CMerge
    -- THIS IS JUST TO ADD A TAG, OTHERWISE COPIED FROM SOURCE
    f &&& g = arrT "|x| (x, x)" (\b -> (b,b)) >>> f *** g

--SHOW INSTANCE FOR DEBUGGING
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


--HELPERS TO ADD TAGS
arrT    t = CTag t . arr
liftA2T t f a b = CTag t (liftA2 f a b)

--if predicate then a else b
cIf :: Channel i Bool -> Channel i o -> Channel i o -> Channel i o
cIf p a b = cSelect2 p $ a &&& b

cSelect2 :: Channel i Bool -> Channel i (o, o) -> Channel i o
cSelect2 = liftA2T "|(b, (o1, o2))| if b {o1} else {o2}" selector
    where
        selector True  (o, _) = o
        selector False (_, o) = o


