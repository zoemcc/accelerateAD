{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module DiffLang (AccFunc) where

import Data.Array.Accelerate      as A
--import Numeric.AD                 as AD


--data AccFunc arrIn scalIn scalOut = Map  (scalIn -> scalOut) (AbstractArray scalIn) |
                                    --Fold (scalIn -> scalIn -> scalOut) (AbstractArray scalIn)

data AccFunc sh1 a sh2 b = 
  forall sh0 z. (Shape sh0, Elt z) => Map  (a -> b)      (AccFunc sh0 z sh1 a) |
  forall sh0 z. (Shape sh0, Elt z) => Fold (a -> a -> b) (AccFunc sh0 z sh1 a) |
  InArray sh2 b

--instance Show => AccFunc (sh1, a) (sh2, b) where
  --show (Map  f var)     = "Map: "   ++ show var
  --show (Fold f var)     = "Fold: "  ++ show var
  --show (InArray sh arr) = "Array: " ++ show arr

--data AbstractArray scalIn scalOut = NowArray scal |
                                    --EventuallyArray (AccFunc arrIn scal scal)

--type Func a b = (a -> b)

--composeAccFunc :: (Shape s1, Shape s2, Shape s3, Elt a, Elt b, Elt c) => 
  --AccFunc (s1, Exp a) (s2, Exp b) -> AccFunc (s2, Exp b) (s3, Exp c) -> AccFunc (s1, Exp a) (s3, Exp c)
--composeAccFunc = undefined

--compileToAcc :: (Shape sh1, Shape sh2, Elt a, Elt b) =>
  --AccFunc (sh1, a) (sh2, b) -> Acc (Array sh2 b)
--compileToAcc (Map  f var)     = A.map  f     $ compileToAcc var
--compileToAcc (Fold f var)     = A.fold f 0.0 $ compileToAcc var
--compileToAcc (InArray sh arr) = Acc (Array sh array)



