import Moo.GeneticAlgorithm.Binary

import Control.Arrow (first)
import Control.Monad (when)
import Data.List (intercalate, sortBy, permutations)
import Data.Ord (comparing)
import System.ProgressBar (progressBar, percentage, msg, noLabel, exact, startProgress, incProgress)
import NaturalLanguageModule  (naturalismDefault, naturalism, defaultCriterions)
import Moo.GeneticAlgorithm.Continuous (getRandomGenomes)
import BlindtextModule (cryptotext2)
import MascModule (Direction (..), MascKey, masc, initializeMascGenome)
import TypeModule
import Reordering (combineMutationOps, completeShiftMutate, swapMutate, listswapMutate, revMutate, blockSwapMutate, shuffelMutate, shiftMutate, initializeEnumGenome, edgeCrossover)

-- DANGER this of course is cheating and can not be used in a real usecase
import BlindtextModule (blindtext1Naturalism)

{-
Sorting a list (of Characters) using a genetic algorythm.
The output should be readable for human, but for a nice
plot with gnuplot you can paste this in "plot_output"...

set terminal postscript eps enhanced color font 'Helvetica,10'
set xlabel 'Generations'
set ylabel 'sorting fittnes'
set output 'output.eps'
set key bottom box
plot 'output.txt' using 1:2 with lines lc rgb 'red' lw 4 title 'best value',\
     'output.txt' using 1:3 with lines lc rgb 'dark-grey' title 'median value',\
     'output.txt' using 1:4 with lines lc rgb 'grey' title 'worst value'

...and this in "runGensort.sh"...

#!/bin/bash
ghc --make GenSort.hs
time ./GenSort > output.txt
gnuplot -p plot_output.plt
filelength=$(wc -l < output.txt)
echo "Final Generation: $(($filelength - 6))"
tail -5 output.txt

... and run it with ./runGensort
-}

-- stopconditions (they are very high)
maxiters = 50000
minFittness = blindtext1Naturalism defaultCriterions
timeLimit = 600 -- in seconds

problem :: Problem Char
problem = cryptotext2

popsize :: Int
popsize = 7

selection :: SelectionOp a
selection = rouletteSelect 5

crossover :: (Ord a) => CrossoverOp a
crossover =
	edgeCrossover 2
	--noCrossover
	--orderCrossover 0.3
	--uniformCrossover 0.3

mutation :: MutationOp a
mutation =
	combineMutationOps [shiftMutate, swapMutate]

elitesize = 1

documentation :: Documentation
documentation = Plot

natFitnes :: Problem Char -> Genome Char -> Double
natFitnes problem genome =
	naturalism defaultCriterions (masc Decrypt genome problem)

showGenome :: Problem Char -> Genome Char -> String
showGenome problem genome = "# Genome " ++ show genome
						++ "\n# makes " ++ (begining . show) problem 
						++ "\n# to " ++ (begining . show . masc Decrypt genome) problem
						++ "\n# (natFitnes: " ++ show (natFitnes problem genome) ++ ")"
						where
							showBits :: [Bool] -> String
							showBits = concatMap (show . fromEnum) 

-- run the real algorythm
geneticAlgorithm :: Problem Char -> IO (Population Char)
geneticAlgorithm problem = do
	runIO (initializeMascGenome popsize) $ loopIO
		[DoEvery 1 (logStats documentation problem), TimeLimit timeLimit]
		(Or (Generations maxiters) (IfObjective (any (>= minFittness))))
		nextGen
		where
			nextGen :: StepGA Rand Char
			--nextGen = nextSteadyState 8 Maximizing fitness selection crossover mutation
			
			nextGen = nextGeneration Maximizing fitness selection elitesize crossover mutation

fitness :: [Genome Char] -> Population Char
fitness = map (\ genome -> (genome, natFitnes problem genome))


-- Gnuplotreadable statistics for 1 Generation
logStats :: Documentation -> Problem Char -> Int -> Population Char -> IO ()
logStats docu problem iterno pop = do
	let gs = map takeGenome . bestFirst Maximizing $ pop  -- genomes
	let best = head gs
	let median = gs !! (length gs `div` 2)
	let worst = last gs
	if (docu == Plot) then
		putStrLn $ unwords	[ show iterno
							, (show . natFitnes problem) best
							, (braces . show) best
							, (show . natFitnes problem) median
							, (braces . show) median
							, (show . natFitnes problem) worst
							, (braces . show) worst
							, (take 10 . show . masc Decrypt best) problem
							]
	else do
		progressBar (msg "# Fittness") exact 122 (round $ natFitnes problem best) (round minFittness)
		putStrLn ""
	
	where
		braces :: String -> String
		braces str = "(" ++ str ++ ")"

main :: IO()
main = do
	putStrLn $ "# Fittnes to reach: " ++ (show) minFittness 
	when (documentation == Plot) $
		putStrLn "# generation medianValue bestValue"
	finalPop <- geneticAlgorithm problem
	let winner = takeGenome . head . bestFirst Maximizing $ finalPop
	putStrLn $ showGenome problem winner
	return ()


begining :: String -> String
begining xs = take n xs -- ++ "..."
	where n = 6
