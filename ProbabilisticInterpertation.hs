import Prelude hiding ((!))
import Data.Matrix
import PartialDerivativesMatrix

-- The parameter type
type Parameters a  = Matrix a

-- The input data type
type Data a        = Matrix a

-- The output data type
type Results a     = Matrix a

-- Our hypothesis type
type Hypothesis a = Data a -> Parameters a -> DM a

-- Training data
type TrainingData a = Data a

-- Training results
type TrainingResults a = Results a

-- A training set
type TrainingSet a = (TrainingData a, TrainingResults a)

-- A distribution is just p(y | x; theta)
type Distribution a = a -> Data a -> Parameters a -> DM a

-- A normal distribution centered around theta^t*x
gaussian :: (Floating a) => a -> Distribution a
gaussian sigma y x = value
    where
        value = (exp (-(diff*diff) / (two * sig*sig))) / (sqrt (two*pi_*sig))
        sig   = cons 1 n sigma
        two   = cons 1 n 2
        pi_   = cons 1 n pi
        diff  = (cons 1 n y) - foldl1 (+) [(idm 1 n (1, i))*(cons 1 n (x'!(1, i))) | i <- [1..n]]
        n     = ncols x'
        x'    = matrix 1 1 (const 1) <|> x

-- Bernoulli distribution
bernoulli :: (Eq a, Floating a) => Distribution a
bernoulli y x = if y == 0 then
                    one - v
                else
                    v
                    where
                        v     = one/(one+exp (-value))
                        one   = cons 1 n 1
                        value = foldl1 (+) [(idm 1 n (1, i))*(cons 1 n (x'!(1,i))) | i <- [1..n]]
                        n     = ncols x'
                        x'    = matrix 1 1 (const 1) <|> x

-- The likelihood function L
likelihood :: (Floating a) => Distribution a -> TrainingSet a -> Parameters a -> DM a
likelihood d (x, y) = value
    where
        value = foldl1 (*) ps
        ps    = [d (y!(i, 1)) (rowVector (getRow i x)) | i <- [1..n]] 
        n     = nrows y

-- Gradient ascent algorithm
gradientAscent :: (Floating a) => a -> Distribution a -> TrainingSet a -> Parameters a -> [Parameters a]
gradientAscent alpha d s theta_0 = iterate ascent theta_0
    where
         ascent theta = theta + (fmap (alpha*) ((del (log (likelihood d s))) theta))
