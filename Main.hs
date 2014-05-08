import qualified Prelude          as P
import Data.Array.Accelerate      as A
import Data.Array.Accelerate.CUDA as C
import Numeric.AD.Mode.Reverse as AD

--type Vector a = Array DIM1 a
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

normSq :: Acc (Vector Float) -> Acc (Scalar Float)
normSq arr = fold (+) 0 (map (\el -> el * el) arr)

main :: IO ()
main = do
  print . C.run . normSq . use $ (fromList [1, 2, 3, 4] :: Vector Float)
  --print $ grad testFunc [1, 2, 3]

