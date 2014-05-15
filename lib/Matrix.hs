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
