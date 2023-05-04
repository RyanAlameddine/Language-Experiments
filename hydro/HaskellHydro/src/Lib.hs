module Lib
    ( someFunc
    ) where

import HydroArrow
import RustGenerator


--basic channels
cEven = arrT "|x| x % 2 == 0" even
cAdd  = arrT "|(x, y)| x + y" (+)

cDouble  = arrT "|x| x * 2" (*2)
cPlusOne = arrT "|x| x + 1" (+1)

cPlusOnePlusDouble = cPlusOne `cAdd` cDouble


--double if even, plusOne if odd
cDoubleOrPlus = cIf cEven cDouble cPlusOne

-- for example, 5 will output (6, 12)
cFancy = cDoubleOrPlus &&& (cDoubleOrPlus >>> cDoubleOrPlus)

cTest =  liftA2T "|(x, y)| x + y" (+) cPlusOne cDouble `CComp` cDouble


output :: Channel Int (Int, Int)
output = cFancy

generateOutput = putStr $ rustGen output
