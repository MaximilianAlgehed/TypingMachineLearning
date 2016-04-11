{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Prelude hiding ((!))
import Data.Matrix
import FunctionInstances

type DM a = (a, Matrix a)

instance Num a => Num (DM a) where
    (fm, m) + (gm, m') = (fm+gm, matrix (nrows m) (ncols m) (\ij -> (m!ij) + (m'!ij)))
    (fm, m) * (gm, m') = (fm*gm, matrix (nrows m) (ncols m) (\ij -> fm*(m'!ij) + gm*(m!ij)))
    (fm, m) - (gm, m') = (fm-gm, matrix (nrows m) (ncols m) (\ij -> (m!ij) - (m'!ij)))
    abs                = undefined
    signum             = undefined
    negate (fm, m)     = (negate fm, fmap negate m)
    fromInteger x      = undefined

cons :: (Num a) => Int -> Int -> a -> Matrix a -> DM a
cons i j a _ = (a, matrix i j (const 0))

idm :: (Num a) => Int -> Int -> (Int, Int) -> Matrix a -> DM a
idm i j xy a = (a!xy, matrix i j (\ij -> if ij == xy then 1 else 0)) 

apply :: DM (Matrix Double -> Double) -> Matrix Double -> DM Double
apply (f, ds) a = (f a, fmap (\g -> g a) ds)

test1 :: Matrix Double -> DM Double
test1 = (idm 1 2 (1, 1)) + ((cons 1 2 2)*((idm 1 2 (1, 2))*(idm 1 2 (1, 2))))