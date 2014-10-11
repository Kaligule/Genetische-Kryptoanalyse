module TypeModule (S_n, Genome, Problem, Rand, Criterion(..), Analysation(..), WeightedCriterion, Documentation(..)) where
import Moo.GeneticAlgorithm.Binary (Genome)
import Control.Monad.Mersenne.Random (Rand)

-- list to be sorted
type Problem a = [a]
-- The Symetric Group of degree n: http://en.wikipedia.org/wiki/Symmetric_group
-- a memeber of S_n should change the order of the elements, not the elements itself 
type S_n a = [a] -> [a]

data Criterion = Monogram | Bigram | Trigram | Quadrigram | Word deriving (Show)

data Analysation = ByWeight | ByScyline | ByExpWeight deriving (Show)

type WeightedCriterion = (Criterion, Analysation, Double)

data Documentation = CLI | Plot deriving (Eq, Show)