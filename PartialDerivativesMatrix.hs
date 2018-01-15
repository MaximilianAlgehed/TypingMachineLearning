{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module PartialDerivativesMatrix where

import Prelude hiding ((!))
import Data.Matrix
import FunctionInstances

-- Represents the results and derivatives of f :: Matrix a -> a
type DM a = (a, Matrix a)

-- Get the result of the computation and all the partial derivatives at the same time
instance Num a => Num (DM a) where
  (fm, m) + (gm, m') = (fm+gm, matrix (nrows m) (ncols m) (\ij -> (m!ij) + (m'!ij)))
  (fm, m) * (gm, m') = (fm*gm, matrix (nrows m) (ncols m) (\ij -> fm*(m'!ij) + gm*(m!ij)))
  (fm, m) - (gm, m') = (fm-gm, matrix (nrows m) (ncols m) (\ij -> (m!ij) - (m'!ij)))
  abs                = error "No definition of abs for partial derivatives"
  signum             = error "No definition of signum for partial derivatives"
  negate (fm, m)     = (negate fm, fmap negate m)
  fromInteger x      = error $ "No definition of fromInteger for partial derivatives: " ++ show x

-- Get the result fo the computation and all the partial derivatives at the same time
instance Fractional a => Fractional (DM a) where
  (fm, m) / (gm, m') = (fm/gm, matrix (nrows m) (ncols m) (\ij -> (gm*(m!ij) - fm*(m'!ij)) / (gm*gm)))
  fromRational x     = error $ "No definition of fromRational for partial derivatives: " ++ show x

-- Get the result fo the computation and all the partial derivatives at the same time
instance Floating a => Floating (DM a) where
  pi           = undefined
  exp  (fm, m) = (exp fm, fmap (*(exp fm)) m) 
  sqrt (fm, m) = (sqrt fm, fmap (/(2*sqrt fm)) m)   
  log  (fm, m) = (log fm, fmap (/fm) m)     

-- A constant function f(A) = c
cons :: (Num a) => Int -> Int -> a -> Matrix a -> DM a
cons i j a = const (a, matrix i j (const 0))

-- The function f(A) = A_xy
idm :: (Num a) => Int -> Int -> (Int, Int) -> Matrix a -> DM a
idm i j xy a = (a!xy, matrix i j (\ij -> if ij == xy then 1 else 0)) 

-- The normal num stuff
test1 :: Matrix Double -> DM Double
test1 = x*(x + two*y*y)
    where
        x   = idm 1 2 (1, 1)
        y   = idm 1 2 (1, 2)
        two = cons 1 2 2

-- Test some floating stuff
test2 :: Matrix Double -> DM Double
test2 = x*(exp y)
    where
        x   = idm 1 2 (1, 1)
        y   = idm 1 2 (1, 2)

-- Test (/)
test3 :: Matrix Double -> DM Double
test3 = x/y
    where
        x   = idm 1 2 (1, 1)
        y   = idm 1 2 (1, 2)

-- The del operator
del :: (Num a) => (Matrix a -> DM a) -> Matrix a -> Matrix a
del f a = snd (f a)
