{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
import Prelude          as P
import Data.Array.Accelerate      as A
import Data.Array.Accelerate.CUDA as C
import Numeric.AD.Mode            as AD 
import Numeric.AD.Mode.Forward    as Forward
import Control.Monad
import Data.Number.Erf
import Data.Traversable

import AccAD
import DiffLang as L

testFunc :: (Num a) => [a] -> a
testFunc [x, y, z] = x*x*z + y*z + y

-- 
testFuncDouble :: [Double] -> Double
testFuncDouble [x, y, z] = x*x*z + y*z + y

testFunc2 :: (Num a) => [a] -> a
testFunc2 = P.sum . P.map (\e -> e*e) 

normSq :: Acc (Vector Float) -> Acc (A.Scalar Float)
normSq arr = fold (+) 0 (A.map square arr)

toVec ::  Int -> [Float] -> A.Vector Float
toVec dimTest = fromList (Z :. dimTest)

--testEltDeriv :: Acc (Vector Float) -> Acc (Vector (Float, Float))
--testEltDeriv arr = A.map cubeAndSquare3 arr

testEltDeriv :: Acc (A.Vector Float) -> Acc (A.Vector (Float, Float))
testEltDeriv = A.map $ lift . diff' cube

--testEltDerivHigh :: Acc (Vector Float) -> Acc (Vector Float)
--testEltDerivHigh = diffAcc (A.map cube)

square :: Num a => a -> a
square x = x * x

cube   :: Num a => a -> a
cube   x = x * x * x

dimTest = 5 :: Int
sh = Z :. dimTest
arr1 = toVec dimTest [1..]
arr1Acc = A.use $ toVec dimTest [1..]

--gadtTestFold = Fold (+) (constant 0.0) (Use arr1)
-- gadtTestMap :: PreAcc (PreAcc  Exp (Array sh e')
gadtTestMap ::  A.Vector Float -> AccSubset (A.Vector Float)
gadtTestMap = L.map square . gadtTestUse
gadtTestUse :: A.Vector Float -> AccSubset (A.Vector Float)
gadtTestUse = L.use
--gadtTestFoldMap = L.Fold (+) (constant 0.0) $ Map square (Use arr1)

gadtTestMapDiff ::  A.Vector Float -> AccSubset (A.Vector Float)
gadtTestMapDiff = L.map (diff cube) . gadtTestUse

compileTestMapDiff :: A.Vector Float -> Acc (A.Vector Float)
compileTestMapDiff = compileToAcc . gadtTestMapDiff

--instance (IsNum a) => IsNum (a, a) where
  

compileTestMap :: A.Vector Float -> Acc (A.Vector Float)
compileTestMap = compileToAcc . gadtTestMap
-- compileTest = P.undefined

compileTestUse :: A.Vector Float -> Acc (A.Vector Float)
compileTestUse = compileToAcc . gadtTestUse

--testAccFunc :: AccFunc (Z :. Int) (Exp Int) (Z :. Int) (Exp Int)
--testAccFunc = Map id (testAccFunc)
--
-- sh  = Z :. (5 :: Int)
-- x   = 5 :: Int
-- arr = InArray sh x 
--mArr = Map id arr sh
--fmArr = Fold (+) mArr sh
--

testSquare :: (Num a) => a -> a
testSquare x = x * x

runDouble :: (Double -> b) -> Int -> b
runDouble = runF

runF :: Num a => (a -> b) -> Int -> b
runF f x = f $ P.fromIntegral x

testExp :: (IsNum a, Elt a) => Exp a -> Exp a
testExp x = x * x

testDoubleSquareDiff :: Int -> Double
testDoubleSquareDiff = runDouble $ \x -> Forward.diff testSquare x


--testDoubleSquareDiff :: (Mode s) => AD s Double -> AD s Double
--testDoubleSquareDiff x = x * x


--diffFunc :: (Num a, Num b) => (a -> b) -> (forall s. (AD s (Forward a)) -> AD s (Forward b))
--diffFunc f x = runAD x

--diff f a = tangent $ apply f a
--diff f a = Rank1.diff (runAD.f.AD)
--diff f a = tangent $ apply (runAD.f.AD) a

testDiff :: (Num a, Mode s) => AD s a -> AD s a
testDiff x = x + 3


main :: IO ()
main = do
  dimString <- getLine
  let dimAny = read dimString :: Int
  --print . C.run . normSq . use . toVec dimAny $ [1..]
  print . C.run . compileTestMapDiff . toVec dimAny $ [1..]
  --print . C.run . testEltDeriv . A.use . toVec dimAny $ [1..]
  --print $ diff testDoubleSquareDiff 1.0
  --let x = diff $ auto testExp (auto 3.0)
  print $ testDoubleSquareDiff 3
  return ()


