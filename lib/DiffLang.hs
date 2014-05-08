{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module DiffLang (AccFunc(..)) where

import Data.Array.Accelerate      as A
--import Numeric.AD                 as AD


--data AccFunc arrIn scalIn scalOut = Map  (scalIn -> scalOut) (AbstractArray scalIn) |
                                    --Fold (scalIn -> scalIn -> scalOut) (AbstractArray scalIn)

data AccFunc sh1 a sh2 b = 
  forall sh0 z. (Shape sh1, Shape sh2, Elt a, Elt b) =>
    Map  (a -> b)      (AccFunc sh0 z sh1 a) sh2 |
  forall sh0 z. (Shape sh1, Shape sh2, Elt a, Elt b) =>
    Fold (a -> a -> b) (AccFunc sh0 z sh1 a) sh2 |
  (Shape sh2, Elt b) =>
    InArray sh2 b

instance Show (AccFunc sh1 a sh2 b) where
  show (Map  f var sh)  = "(Map: "   Prelude.++ show (show sh, show var) Prelude.++ ")"
  show (Fold f var sh)  = "(Fold: "  Prelude.++ show (show sh, show var) Prelude.++ ")"
  show (InArray sh arr) = "(Array: " Prelude.++ show (show sh, show arr) Prelude.++ ")"

--data AbstractArray scalIn scalOut = NowArray scal |
                                    --EventuallyArray (AccFunc arrIn scal scal)

--type Func a b = (a -> b)

--composeAccFunc :: (Shape s1, Shape s2, Shape s3, Elt a, Elt b, Elt c) => 
  --AccFunc (s1, Exp a) (s2, Exp b) -> AccFunc (s2, Exp b) (s3, Exp c) -> AccFunc (s1, Exp a) (s3, Exp c)
--composeAccFunc = undefined

--compileToAcc :: (Shape sh2, Elt b) =>
  --AccFunc sh1 a sh2 b -> Acc (Array sh1 a) -> Acc (Array sh2 b)
--compileToAcc (Map  f var sh) arr = A.map  f     $ compileToAcc var arr
--compileToAcc (Fold f var sh) arr = A.fold f 0.0 $ compileToAcc var arr
--compileToAcc (InArray sh b)  arr = arr




