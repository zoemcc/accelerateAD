{-# LANGUAGE TypeOperators #-}
--import qualified Prelude          as P
import Data.Array.Accelerate      as A
import Data.Array.Accelerate.CUDA as C
import Numeric.AD                 as AD
import Control.Monad

import AccAD
import DiffLang

type Matrix a = Array DIM2 a

--matMul :: (IsNum e, Elt e) => Acc (Matrix e) -> Acc (Matrix e) -> Acc (Vector e)
--matMul arr brr
  -- = A.fold (+) 0
  -- $ A.zipWith (*) arrRepl brrRepl
  --where
    --Z :. rowsA :. _     = unlift (shape arr)    --:: Z :. Int :. Int
    --Z :. _     :. colsB = unlift (shape brr)    --:: Z :. Int :. Int
--
    --arrRepl             = A.replicate (lift $ Z :. All   :. colsB :.  All) arr
    --brrRepl             = A.replicate (lift $ Z :. rowsA :. All   :.  All) (A.transpose brr)


--matVMul :: (IsNum e, Elt e) => Acc (Matrix e) -> Acc (Vector e) -> Acc (Vector e)
--matVMul arr brr
  -- = A.fold (+) 0
  -- $ A.zipWith (*) arr brrRepl
  --where
    --Z :. rowsA :. _     = unlift (shape arr)    --:: Z :. Int :. Int
    --brrRepl             = A.replicate (lift $ Z :. rowsA :. All) brr

--main :: IO ()
--main = print . C.run $ matVMul (lift mat) (lift vec)
  --where
    --mat = A.fromList (Z :. 3 :. 3) [1..10]
    --vec = A.fromList (Z :. 3)                   [1..3]

testFunc :: (Num a) => [a] -> a
testFunc [x, y, z] = x*x*z + y*z + y

testFunc2 :: (Num a) => [a] -> a
testFunc2 = Prelude.sum . Prelude.map (\e -> e*e) 

normSq :: Acc (Vector Float) -> Acc (A.Scalar Float)
normSq arr = fold (+) 0 (A.map square arr)

toVec ::  Int -> [Float] -> Vector Float
toVec dim = fromList (Z :. dim)

--testEltDeriv :: Acc (Vector Float) -> Acc (Vector (Float, Float))
--testEltDeriv arr = A.map cubeAndSquare3 arr

testEltDeriv :: Acc (Vector Float) -> Acc (Vector (Float, Float))
testEltDeriv = A.map $ lift . diff' cube

--testEltDerivHigh :: Acc (Vector Float) -> Acc (Vector Float)
--testEltDerivHigh = diffAcc (A.map cube)

square :: Num a => a -> a
square x = x * x

cube   :: Num a => a -> a
cube   x = x * x * x

--testAccFunc :: AccFunc (Z :. Int) (Exp Int) (Z :. Int) (Exp Int)
--testAccFunc = Map id (testAccFunc)
--
sh  = Z :. (5 :: Int)
x   = 5 :: Int
arr = InArray sh x 
--mArr = Map id arr sh
--fmArr = Fold (+) mArr sh

main :: IO ()
main = do
  dimString <- getLine
  let dim = read dimString :: Int
  --print . C.run . normSq . use . toVec dim $ [1..]
  print . C.run . testEltDeriv . use . toVec dim $ [1..]
  --print $ grad testFunc [1, 2, 3]

