module Reordering
	( 
	-- Types
	module Moo.GeneticAlgorithm.Types
	-- Mutations
	, combineMutationOps
	, completeShiftMutate
	, swapMutate
	, listswapMutate
	, revMutate
	, blockSwapMutate
	, shuffelMutate
	, shiftMutate
	-- Crossovers
	, edgeCrossover
	-- initialization
	, initializeEnumGenome
	-- apply
	, applyReordering
	-- others
	, pairs
	, module Moo.GeneticAlgorithm.Run
	) where
import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Run

import ReorderingMutations (combineMutationOps, completeShiftMutate, swapMutate, listswapMutate, revMutate, blockSwapMutate, shuffelMutate, shiftMutate)
import ReorderingCrossOvers (edgeCrossover)

import Data.List (sortBy, permutations)
import Data.Ord (comparing)
import Control.Monad (replicateM)


applyReordering :: (Ord a) => [a] -> [b] -> [b]
applyReordering genome = map snd . sortBy (comparing fst) . zip genome

--TODO: Not a good initialization
initializeEnumGenome :: (Enum a) => Int -> Int -> Rand [Genome a]
initializeEnumGenome populationsize genomeLength = do
	replicateM populationsize . shuffle . take genomeLength $ [toEnum 0..]

-- sometimes usefull for fittness functions
-- [1,2,3,4,5] -> [(1,2),(2,3),(3,4),(4,5)]
pairs :: [a] -> [(a,a)]
pairs list = zip list (tail list) 