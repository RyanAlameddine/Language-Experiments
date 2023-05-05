{-# LANGUAGE GADTs #-}
module RustGenerator where

import HydroArrow
import Control.Applicative
import Control.Category (Category)
import qualified Control.Category ((.), id)
import Control.Arrow
import Data.Typeable
import Text.Format


-- RUST GENERATOR --
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
        strC = format "c{0} = c{1};\nc{4} -> c{2};\nc{3} = c{5};\n" $ map show [inputC, inputA, inputB, outputC, outputA, outputB] ++ [tag]
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