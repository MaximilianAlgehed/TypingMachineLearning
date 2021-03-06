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
type CostFunction a = Hypothesis a -> TrainingSet a -> Parameters a -> DM a

-- A linear hypothesis, h(x, theta) = theta^t * [1 x]
linearHypothesis :: (Floating a) => Hypothesis a
linearHypothesis xs = value 
    where
        value = foldl1 (+) [(idm 1 n (1, i)) * (cons 1 n (xs'!(1, i))) | i <- [1..n]]
        n     = ncols xs'
        xs'   = matrix 1 1 (const 1) <|> xs

-- A logistic hypothesis, h(x, theta) = 1/(1+exp - theta^t*[1 x])
logisticHypothesis :: (Floating a) => Hypothesis a
logisticHypothesis xs = one/(one+exp (-value))
    where
        one   = cons 1 n 1
        value = foldl1 (+) [(idm 1 n (1, i)) * (cons 1 n (xs'!(1, i))) | i <- [1..n]]
        n     = ncols xs'
        xs'   = matrix 1 1 (const 1) <|> xs

-- Square error cost function
squareError :: (Floating a) => CostFunction a
squareError h (d, r) = value
    where
        value = foldl1 (+) [let x = (h (rowVector (getRow i d))) - (cons 1 n (r!(i, 1))) in x*x | i <- [1..n]]
        n     = nrows r

-- Gradient descent algorithm
gradientDescent :: (Floating a) => a -> Hypothesis a -> CostFunction a -> TrainingSet a -> Parameters a -> [Parameters a]
gradientDescent alpha h j s theta_0 = iterate descend theta_0
    where
        descend theta = theta - (fmap (alpha*) ((del (j h s)) theta))
