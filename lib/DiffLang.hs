{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE Rank2Types #-}
module DiffLang (PreAcc(..), AccSubset(..), compileToAcc, map, use) where

import qualified Prelude as P
import qualified Data.Array.Accelerate      as A
import qualified Data.Array.Accelerate.Smart as S
import qualified Data.Array.Accelerate.Type      as T
-- import qualified Data.Array.Accelerate.Language as L
--import Numeric.AD                 as AD


--data AccFunc arrIn scalIn scalOut = Map  (scalIn -> scalOut) (AbstractArray scalIn) |
                                    --Fold (scalIn -> scalIn -> scalOut) (AbstractArray scalIn)

-- data AccFunc sh1 a sh2 b = 
  -- forall sh0 z. (Shape sh1, Shape sh2, Elt a, Elt b) =>
    -- Map  (a -> b)      (AccFunc sh0 z sh1 a) sh2 |
  -- forall sh0 z. (Shape sh1, Shape sh2, Elt a, Elt b) =>
    -- Fold (a -> a -> b) (AccFunc sh0 z sh1 a) sh2 |
  -- (Shape sh2, Elt b) =>
    -- InArray sh2 b


data PreAcc acc as where
    -- Needed for conversion to de Bruijn form
  -- Pipe          :: (Arrays as, Arrays bs, Arrays cs)
                -- => (Acc as -> Acc bs)           -- see comment above on why 'Acc' and not 'acc'
                -- -> (Acc bs -> Acc cs)
                -- -> acc as
                -- -> PreAcc acc exp cs

  -- Aforeign      :: (Arrays arrs, Arrays a, Foreign f)
                -- => f arrs a
                -- -> (Acc arrs -> Acc a)
                -- -> acc arrs
                -- -> PreAcc acc exp a
-- 
  -- Acond         :: Arrays as
                -- => exp Bool
                -- -> acc as
                -- -> acc as
                -- -> PreAcc acc exp as
-- 
  -- Awhile        :: Arrays arrs
                -- => (Acc arrs -> acc (Scalar Bool))
                -- -> (Acc arrs -> acc arrs)
                -- -> acc arrs
                -- -> PreAcc acc exp arrs
-- 
  -- Atuple        :: (Arrays arrs, IsTuple arrs)
                -- => Tuple.Atuple acc (TupleRepr arrs)
                -- -> PreAcc acc exp arrs
-- 
  -- Aprj          :: (Arrays arrs, IsTuple arrs, Arrays a)
                -- => TupleIdx (TupleRepr arrs) a
                -- ->        acc     arrs
                -- -> PreAcc acc exp a
-- 
  -- Generate      :: (Shape sh, Elt e)
                -- => exp sh
                -- -> (Exp sh -> exp e)
                -- -> PreAcc acc exp (Array sh e)
-- 
  -- Reshape       :: (Shape sh, Shape sh', Elt e)
                -- => exp sh
                -- -> acc (Array sh' e)
                -- -> PreAcc acc exp (Array sh e)
-- 
  -- Replicate     :: (Slice slix, Elt e,
                    -- Typeable (SliceShape slix), Typeable (FullShape slix))
                    -- -- the Typeable constraints shouldn't be necessary as they are implied by
                    -- -- 'SliceIx slix' — unfortunately, the (old) type checker doesn't grok that
                -- => exp slix
                -- -> acc            (Array (SliceShape slix) e)
                -- -> PreAcc acc exp (Array (FullShape  slix) e)
-- 
  -- Slice         :: (Slice slix, Elt e,
                    -- Typeable (SliceShape slix), Typeable (FullShape slix))
                    -- -- the Typeable constraints shouldn't be necessary as they are implied by
                    -- -- 'SliceIx slix' — unfortunately, the (old) type checker doesn't grok that
                -- => acc            (Array (FullShape  slix) e)
                -- -> exp slix
                -- -> PreAcc acc exp (Array (SliceShape slix) e)
-- 

  Use           :: A.Arrays arrs
                => arrs
                -> PreAcc acc arrs

  Unit          :: A.Elt e
                => e
                -> PreAcc acc (A.Scalar e)

  -- Map           :: (A.Shape sh, A.Elt e, A.Elt e', T.IsNum e, T.IsNum e')
                -- => (A.Exp e -> exp e')
                -- -> acc (A.Array sh e)
                -- -> PreAcc acc exp (A.Array sh e')

  Map           :: (A.Shape sh, A.Elt e, T.IsNum e)
                => (e -> e)
                -> acc (A.Array sh e)
                -> PreAcc acc (A.Array sh e)

  ZipWith       :: (A.Shape sh, A.Elt e1, A.Elt e2, A.Elt e3)
                => (e1 -> e2 -> e3)
                -> acc (A.Array sh e1)
                -> acc (A.Array sh e2)
                -> PreAcc acc (A.Array sh e3)

  Fold          :: (A.Shape sh, A.Elt e)
                => (e -> e -> e)
                -> e
                -> acc (A.Array (sh A.:. P.Int) e)
                -> PreAcc acc (A.Array sh e)
-- 
  -- Fold1         :: (Shape sh, Elt e)
                -- => (Exp e -> Exp e -> exp e)
                -- -> acc (Array (sh:.Int) e)
                -- -> PreAcc acc exp (Array sh e)
-- 
  -- FoldSeg       :: (Shape sh, Elt e, Elt i, IsIntegral i)
                -- => (Exp e -> Exp e -> exp e)
                -- -> exp e
                -- -> acc (Array (sh:.Int) e)
                -- -> acc (Segments i)
                -- -> PreAcc acc exp (Array (sh:.Int) e)
-- 
  -- Fold1Seg      :: (Shape sh, Elt e, Elt i, IsIntegral i)
                -- => (Exp e -> Exp e -> exp e)
                -- -> acc (Array (sh:.Int) e)
                -- -> acc (Segments i)
                -- -> PreAcc acc exp (Array (sh:.Int) e)
-- 
  -- Scanl         :: Elt e
                -- => (Exp e -> Exp e -> exp e)
                -- -> exp e
                -- -> acc (Vector e)
                -- -> PreAcc acc exp (Vector e)
-- 
  -- Scanl'        :: Elt e
                -- => (Exp e -> Exp e -> exp e)
                -- -> exp e
                -- -> acc (Vector e)
                -- -> PreAcc acc exp (Vector e, Scalar e)
-- 
  -- Scanl1        :: Elt e
                -- => (Exp e -> Exp e -> exp e)
                -- -> acc (Vector e)
                -- -> PreAcc acc exp (Vector e)
-- 
  -- Scanr         :: Elt e
                -- => (Exp e -> Exp e -> exp e)
                -- -> exp e
                -- -> acc (Vector e)
                -- -> PreAcc acc exp (Vector e)
-- 
  -- Scanr'        :: Elt e
                -- => (Exp e -> Exp e -> exp e)
                -- -> exp e
                -- -> acc (Vector e)
                -- -> PreAcc acc exp (Vector e, Scalar e)
-- 
  -- Scanr1        :: Elt e
                -- => (Exp e -> Exp e -> exp e)
                -- -> acc (Vector e)
                -- -> PreAcc acc exp (Vector e)
-- 
  -- Permute       :: (Shape sh, Shape sh', Elt e)
                -- => (Exp e -> Exp e -> exp e)
                -- -> acc (Array sh' e)
                -- -> (Exp sh -> exp sh')
                -- -> acc (Array sh e)
                -- -> PreAcc acc exp (Array sh' e)
-- 
  -- Backpermute   :: (Shape sh, Shape sh', Elt e)
                -- => exp sh'
                -- -> (Exp sh' -> exp sh)
                -- -> acc (Array sh e)
                -- -> PreAcc acc exp (Array sh' e)
-- 
  -- Stencil       :: (Shape sh, Elt a, Elt b, Stencil sh a stencil)
                -- => (stencil -> exp b)
                -- -> Boundary a
                -- -> acc (Array sh a)
                -- -> PreAcc acc exp (Array sh b)
-- 
  -- Stencil2      :: (Shape sh, Elt a, Elt b, Elt c,
                   -- Stencil sh a stencil1, Stencil sh b stencil2)
                -- => (stencil1 -> stencil2 -> exp c)
                -- -> Boundary a
                -- -> acc (Array sh a)
                -- -> Boundary b
                -- -> acc (Array sh b)
                -- -> PreAcc acc exp (Array sh c)

-- instance Show (PreAcc acc exp as) where
  -- show (Map func arr) = "(Map: "   Prelude.++ show arr  Prelude.++ ")"
  -- show (Fold f var sh)  = "(Fold: "  Prelude.++ show (show sh, show var) Prelude.++ ")"
  -- show (InArray sh arr) = "(Array: " Prelude.++ show (show sh, show arr) Prelude.++ ")"

newtype AccSubset a = AccSubset (PreAcc AccSubset a)
--data PreAcc acc exp as where

map :: (A.Shape ix, A.Elt a, T.IsNum a)
    => (a -> a)
    -> AccSubset (A.Array ix a)
    -> AccSubset (A.Array ix a)
map = AccSubset $$ Map

use :: A.Arrays arrays => arrays -> AccSubset arrays
use = AccSubset P.. Use


infixr 0 $$
($$) :: (b -> a) -> (c -> d -> b) -> c -> d -> a
(f $$ g) x y = f (g x y)

-- deriving instance Smart.Typeable1 AccSubset

--acc exp as

--data AccFuncGADT sh scal where 
  ----forall sh0 z. (Shape sh1, Shape sh2, Elt a, Elt b) 
  --Map           :: (Shape sh, Elt e, Elt e')
                -- => (Exp e -> exp e')
                -- -> acc (Array sh e)
                -- -> PreAcc acc exp (Array sh e')
  --Map :: (Shape sh, Elt e, Elt e')
    -- => (Exp e -> Exp e')
    -- -> 
  --Map  (scal -> scal)      (AccFuncGADT sh scal)
  --forall sh0 z. (Shape sh1, Shape sh2, Elt a, Elt b) =>
    --Fold (a -> a -> b) (AccFunc sh0 z sh1 a) sh2 |
  --(Shape sh2, Elt b) =>
    --InArray sh2 b

-- instance Show (AccFunc sh1 a sh2 b) where
  -- show (Map  f var sh)  = "(Map: "   Prelude.++ show (show sh, show var) Prelude.++ ")"
  -- show (Fold f var sh)  = "(Fold: "  Prelude.++ show (show sh, show var) Prelude.++ ")"
  -- show (InArray sh arr) = "(Array: " Prelude.++ show (show sh, show arr) Prelude.++ ")"

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

compileToAcc :: (A.Shape sh, A.Elt e) => AccSubset (A.Array sh e) -> S.Acc (A.Array sh e)
compileToAcc (AccSubset (Use arr))   = A.use   P.$ arr -- S.Map g $ compileToAccUnwrapped arr
--compileToAcc (AccSubset (Map g arr)) = A.map g P.$ compileToAcc arr -- S.Map g $ compileToAccUnwrapped arr
compileToAcc _ = P.undefined

liftExp :: (A.Elt a, A.Elt b) => (a -> b) -> A.Exp a -> A.Exp b
liftExp f x = P.undefined

--compileToAcc :: (A.Shape sh, A.Elt e) => AccSubset (A.Array sh e) -> S.Acc (A.Array sh e)
--compileToAcc input = S.Acc (compileToAccUnwrapped input)




