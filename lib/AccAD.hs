module AccAD (diffAcc) where

import Data.Array.Accelerate      as A
import Numeric.AD                 as AD


diffAcc :: (Acc a -> Acc b) -> (Acc a -> Acc c)
--diff (A.map func) = A.map (derive func)
diffAcc = undefined







