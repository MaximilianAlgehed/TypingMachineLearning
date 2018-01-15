{-# LANGUAGE TypeSynonymInstances #-}
import Data.Function
import Control.Monad
import FunctionInstances

type Ds a = [a]

instance Num a => Num (Ds a) where
  (+)             = zipWith (+)
  (a:as) * (b:bs) = (a*b):((a:as)*bs+as*(b:bs))
  (-)             = zipWith (-)
  abs             = undefined
  negate          = (-) 0
  signum          = undefined
  fromInteger x   = (fromInteger x):0

instance Fractional a => Fractional (Ds a) where
  (a:as) / (b:bs) = (a/b):(((b:bs)*as - (a:as)*bs)/((b:bs)*(b:bs)))
  fromRational a  = (fromRational a):0

instance Floating a => Floating (Ds a) where
  pi               = pi:0
  exp   (a:as)     = (exp a):(as * exp (a:as))
  sin   (a:as)     = (sin a):(as * cos (a:as))
  cos   (a:as)     = (cos a):(as * (0 - (sin (a:as))))
  tan   (a:as)     = (tan a):(as * (1 + (tan (a:as) * tan (a:as))))
  sqrt  (a:as)     = (sqrt a):(as / (2 * (sqrt (a:as))))
  log   (a:as)     = (log a):(as / (a:as))
  asin  (a:as)     = (asin a):(as / (sqrt (1 - ((a:as)*(a:as)))))
  acos  (a:as)     = (acos a):(0 - (as / (sqrt (1-((a:as)*(a:as))))))
  atan  (a:as)     = (atan a):(as/(1+((a:as)*(a:as))))
  cosh  (a:as)     = (cosh a):(as * sinh (a:as))
  sinh  (a:as)     = (sinh a):(as * cosh (a:as))
  tanh  (a:as)     = (tanh a):(as / cosh (a:as)) 
  asinh (a:as)     = (asinh a):(as / (sqrt ((a:as)*(a:as)+1)))
  acosh (a:as)     = (acosh a):(as / (sqrt ((a:as)*(a:as)-1)))
  atanh (a:as)     = (atanh a):(as / (sqrt (1-(a:as)*(a:as))))

-- Evaluate a function `f` at a point `x` to get
-- all derivatives at that point
eval :: Num a => (Ds a -> Ds a) -> a -> Ds a
eval f x = f (x : 1)

-- The `n`th derivative of a function `f` at the point `x`
dbase :: Num a => Int -> (Ds a -> Ds a) -> a -> a
dbase n f x = eval f x !! n

x :: Ds a -> Ds a
x = id
