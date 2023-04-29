import Control.Applicative
import Control.Category (Category)
import qualified Control.Category ((.), id)
import Control.Arrow

-- newtype Channel a = Channel [a]

-- instance Functor Channel where
--     fmap f (Channel a) = Channel (fmap f a) -- map over each element

-- instance Applicative Channel where
--     pure a = Channel [a] -- channel with single value a for this tick
--     liftA2 f (Channel a) (Channel b) = Channel (zipWith f a b)

-- --CHANNELS IN HYDROFLOW NOT MONADIC (I think?)
-- -- instance Monad Channel where
-- --     a >>= f = Channel (concat inner)
-- --         where
-- --             (Channel inner) = fmap (extract . f) a -- output is Channel<Channel<b>>
-- --             extract (Channel b) = b



-- Channels are not containers for streams, they are more akin to functions?

newtype Channel a b = Channel (a -> b)

instance Functor (Channel a) where
    fmap f (Channel c) = Channel (fmap f c) --map over result

instance Applicative (Channel a) where
    pure = Channel . pure  --channel that throws away it's input and returns whatever is passed into pure
    -- a <*> b :: Channel a (b -> c) -> Channel a b -> Channel a c

    -- (a1 -> b -> c) -> Channel a a1 -> Channel a b -> Channel a c
    -- f <$> a :: a -> (b -> c), b :: a -> b, f <$> a <*> b :: a -> c
    liftA2 f (Channel a) (Channel b) = Channel (liftA2 f a b)  --Channel (f <$> a <*> b) --merge the result of two channels with a cross join, then apply f

instance Category Channel where
    id = Channel id
    (Channel a) . (Channel b) = Channel (a . b)
    
instance Arrow Channel where
    arr = Channel
    first (Channel a) = Channel (first a) 

select2 :: (i -> Bool) -> Channel i (o, o) -> Channel i o
select2 p = liftA2 selector (arr p)
    where
        selector True  (o, _) = o
        selector False (_, o) = o

--splits into channel a or channel b and selects output depending on the predicate,
--diamond :: (i -> Bool) -> Channel i o -> Channel i o -> Channel i o


cDouble = arr (*2)
cPlusOne = arr (+1)

cPlusOnePlusDouble = liftA2 (+) cPlusOne cDouble

--double if even, plusOne if odd
cDoubleOrPlus = select2 even c
    where c = liftA2 (,) cDouble cPlusOne

cFinal = cDoubleOrPlus &&& (cDoubleOrPlus >>> cDoubleOrPlus)

--main :: IO ()
main = interact (unlines . map process . lines)
    where
        (Channel f) = cFinal
        process = show . f . read

-- main :: IO ()
-- main = (print . show) o
--     where
--         (Channel f) = cFinal
--         --(Channel f) = liftA2 (+) cPlusOne cDouble
--         o = (f 5)

-- main :: IO ()
-- main = (print . show) o
--     where
--         (Channel f) = cPlusOne >>> cDouble
--         o = f 5





--instance MonadPlus Channel where



-- join is merge?