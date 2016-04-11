import Prelude hiding ((!))
import Data.Matrix
import PartialDerivativesMatrix

-- The parameter type
type Parameters a  = Matrix a

-- The input data type
type Data a        = Matrix a

-- Our hypothesis type
type Hypothesis a = Data a -> Parameters a -> DM a

-- Training data
type TrainingData a = Data a

-- Training results
type TrainingResults a = Matrix a

-- A training set
type TrainingSet a = (TrainingData a, TrainingResults a)

-- The cost function, J
type CostFunction a = TrainingSet a -> Parameters a -> a

-- The del operator
del :: (Num a) => (Matrix a -> DM a) -> Matrix a -> Matrix a
del f a = snd (f a)

-- A linear hypothesis, h(x, theta) = theta^t * [1 x]
linearHypothesis :: (Num a) => Hypothesis a
linearHypothesis xs = value 
    where
        value = foldl1 (+) [(idm 1 n (1, i)) * (cons 1 n (xs'!(1, i))) | i <- [1..n]]
        n     = ncols xs'
        xs'   = matrix 1 1 (const 1) <|> xs
