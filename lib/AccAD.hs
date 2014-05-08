{-# LANGUAGE GADTs                 #-}
module AccAD (diffAcc) where

import qualified Data.Array.Accelerate      as A
import Numeric.AD                 as AD

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
diffAcc :: PreAcc acc exp (A.Array sh e') ->  PreAcc acc exp (A.Array sh e')
-- diffAcc (Map g arr) = Map (diff g) arr
diffAcc _ = Prelude.undefined







