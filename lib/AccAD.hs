{-# LANGUAGE GADTs                 #-}
-- {-# LANGUAGE ViewPatterns                 #-}
module AccAD (diffAcc) where

import qualified Data.Array.Accelerate      as A
import Numeric.AD                 as AD
import qualified Data.Array.Accelerate.Type      as T

-- import qualified Prelude
import DiffLang as L


--diffAcc :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> Acc a -> Acc b
--diff (A.map func) = A.map (derive func)
--diffAcc f arr = case f of 
                --A.map x -> A.map (diff f) arr
--diffAcc = undefined


-- diffAcc :: (PreAcc acc exp (Array sh e) -> PreAcc acc exp (Array sh e')) 
        -- ->  PreAcc acc exp (Array sh e) -> PreAcc acc exp (Array sh e')
-- diffAcc = Prelude.undefined
diffAcc :: (A.Elt e, T.IsNum e) => L.AccSubset (A.Array sh e) ->  L.AccSubset (A.Array sh e)
diffAcc (L.AccSubset (L.Use arr))      = L.AccSubset (L.Use arr)
--diffAcc (L.AccSubset (L.Map g arr))    = L.map (A.lift . AD.diff func) arr
                                         --where func 
      -- need somehow to get AD to function on Exp's....
--diffAcc (L.AccSubset (L.Fold g a arr)) = L.fold (diff g) 0 arr
diffAcc _ = Prelude.undefined

-- compileToAcc :: (A.Shape sh, A.Elt e) => AccSubset (A.Array sh e) -> S.Acc (A.Array sh e)
-- compileToAcc (AccSubset (Use arr))   = A.use   P.$ arr -- S.Map g $ compileToAccUnwrapped arr
-- compileToAcc (AccSubset (Map g arr)) = A.map g P.$ compileToAcc arr 
-- compileToAcc _ = P.undefined







