{-# LANGUAGE GADTs                 #-}
module AccAD (reachIntoFunc, diffAcc) where

import Data.Array.Accelerate      as A hiding (map)
import Numeric.AD                 as AD
import Data.Array.Accelerate.Smart as Smart

-- import qualified Prelude
import DiffLang


--diffAcc :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> Acc a -> Acc b
--diff (A.map func) = A.map (derive func)
--diffAcc f arr = case f of 
                --A.map x -> A.map (diff f) arr
--diffAcc = undefined


-- diffAcc :: (PreAcc acc exp (Array sh e) -> PreAcc acc exp (Array sh e')) 
        -- ->  PreAcc acc exp (Array sh e) -> PreAcc acc exp (Array sh e')
-- diffAcc = Prelude.undefined
-- diffAcc :: PreAcc acc exp (Array sh e') ->  PreAcc acc exp (Array sh e')
-- diffAcc (Map g arr) = Map (diff g) arr
diffAcc :: (Elt b, Num b) => Acc (Array ix b) -> Acc (Array ix b)
diffAcc (Acc (Map f arr))    = Acc (Map (diff f) arr)
-- diffAcc (Acc (Fold f a arr)) = 
-- diffAcc (Acc (Use arr))      =
diffAcc _                    = Prelude.undefined


reachIntoFunc :: Acc (Array ix b) -> String
reachIntoFunc (Acc (Map f arr))    = "(Map: " Prelude.++ reachIntoFunc arr Prelude.++ ")"
reachIntoFunc (Acc (Fold f a arr)) = "(Fold: " Prelude.++ show a Prelude.++ reachIntoFunc arr Prelude.++ ")"
reachIntoFunc (Acc (Use arr))      = "(Use: " Prelude.++ show arr Prelude.++ ")"
reachIntoFunc _                    = Prelude.undefined









