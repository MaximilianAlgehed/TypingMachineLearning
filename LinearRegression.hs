import Data.Matrix
import Derivatives

-- The parameter type
type Parameters a  = Matrix (Ds a)

-- The input data type
type Data a        = Matrix (Ds a)

-- Our hypothesis type
type Hypothesis a = Parameters a -> Data a -> a

-- Training data
type TrainingData a = Data a

-- Training results
type TrainingResults a = Matrix a

-- A training set
type TrainingSet a = (TrainingData a, TrainingResults a)

-- The cost function, J
type CostFunction a = TrainingSet a -> Parameters a -> a

del :: (Matrix (Ds a) -> Ds a) -> Matrix (Ds a) -> Matrix (Ds a)
