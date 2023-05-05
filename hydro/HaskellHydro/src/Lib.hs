module Lib
    ( generateOutput
    ) where

import HydroArrow
import RustGenerator
import Control.Arrow

--Note: Although the haskell functions are provided next to their rust equivalents
--this is simply to ensure that Haskell will type check the Channels for me.


--cEven is a channel that takes in ints, and outputs bools (if the input is even)
cEven :: Channel Int Bool
cEven = arrT "|x| x % 2 == 0" even
--Note that the this is accomplished by "lifting" the haskell even function into the Channel arrow
--and then "tagging" it with the rust code, hence arrT instead of arr.

cDouble  = arrT "|x| x * 2" (*2)
cPlusOne = arrT "|x| x + 1" (+1)

--cAdd is a channel that takes an input of type a, splits it across two
--input channels, and combines the output with a (+) operation
cAdd :: Channel a Int -> Channel a Int -> Channel a Int
cAdd  = liftA2T "|(x, y)| x + y" (+)


cPlusOnePlusDouble = cPlusOne `cAdd` cDouble


--cDoubleOrPlus doubles the input if even, adds 1 if odd
cDoubleOrPlus :: Channel Int Int
cDoubleOrPlus = cIf cEven cDouble cPlusOne

--Using arrow operators, cFancy takes an input, passes it into (cDoubleOrPlus) and (cDoubleOrPlus >>> cDoubleOrPlus).
--The right hand side computes computes cDoubleOrPlus twice in a pipeline.
--Finally, it takes the output of the two sides and joins them into a tuple.
--For example, inputting 5 will output (6, 12)
cFancy = cDoubleOrPlus &&& (cDoubleOrPlus >>> cDoubleOrPlus)

--cTest takes an input and splits it into cPlusOne and cDouble.
--It then takes those outputs and adds them together before piping that into cDouble.
cTest =  cAdd cPlusOne cDouble >>> cDouble


--If you would like to test your own channels, simply set output = your_channel and run the program with `stack run`
output = cFancy


generateOutput = putStr $ rustGen output
