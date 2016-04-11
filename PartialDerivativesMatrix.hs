{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module PartialDerivativesMatrix where

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
    fromInteger x      = error ("No definition of fromInteger for partial derivatives: "++(show x))

-- A constant function f(A) = c
cons :: (Num a) => Int -> Int -> a -> Matrix a -> DM a
cons i j a = const (a, matrix i j (const 0))

-- The function f(A) = A_xy
idm :: (Num a) => Int -> Int -> (Int, Int) -> Matrix a -> DM a
idm i j xy a = (a!xy, matrix i j (\ij -> if ij == xy then 1 else 0)) 

test1 :: Matrix Double -> DM Double
test1 = x*(x + two*y*y)
    where
        x   = idm 1 2 (1, 1)
        y   = idm 1 2 (1, 2)
        two = cons 1 2 2
