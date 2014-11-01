import Moo.GeneticAlgorithm.Binary

import Control.Arrow (first)
import Control.Monad (when)
import Data.List (intercalate, sortBy, permutations)
import Data.Ord (comparing)
import NaturalLanguageModule  (naturalismDefault)
import Moo.GeneticAlgorithm.Continuous (getRandomGenomes)
import BlindtextModule (cryptotext1)
import SnModule (perm, Direction (..), PermKey)
import TypeModule
import Reordering (combineMutationOps, completeShiftMutate, swapMutate, listswapMutate, revMutate, blockSwapMutate, shuffelMutate, shiftMutate, initializeEnumGenome, edgeCrossover)
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



period = 26
genomesize = period
-- stopconditions (they are very high)
maxiters = 1000000
minFittness = 100750
timeLimit = 1200 -- in seconds

problem :: Problem Char
problem = cryptotext1

popsize :: Int
popsize = 9

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
	combineMutationOps [shiftMutate, completeShiftMutate]
	--shiftMutate
	--listswapMutate
	--blockSwapMutate

elitesize = 1

-- sortingFittnes ls == 1 is aquivalent to ls == sort ls
natFitnes :: Problem Char -> PermKey -> Double
natFitnes problem genome =
	naturalismDefault (perm Decrypt genome problem)
			-- /P (fromIntegral . twoOutOf . length) problem
	where
		twoOutOf :: Int -> Int
		twoOutOf 1 = 0
		twoOutOf n = n - 1 + twoOutOf (n-1)

showGenome :: Problem Char -> PermKey -> String
showGenome problem genome = "#Genome " ++ show genome
						++ "\n#makes " ++ (begining . show) problem 
						++ "\n#to " ++ (begining . show . perm Decrypt genome) problem
						++ "\n#(natFitnes: " ++ show (natFitnes problem genome) ++ ")"
						where
							showBits :: [Bool] -> String
							showBits = concatMap (show . fromEnum) 


geneticAlgorithm :: Problem Char -> IO (Population Int)
geneticAlgorithm problem = do
	runIO (initializeEnumGenome popsize period) $ loopIO
		[DoEvery 1 (logStats problem), TimeLimit timeLimit]
		(Or (Generations maxiters) (IfObjective (any (>= minFittness))))
		nextGen
		where
			nextGen :: StepGA Rand Int
			nextGen = nextGeneration Maximizing fitness selection elitesize crossover mutation

fitness :: [Genome Int] -> Population Int
fitness = map (\ genome -> (genome, natFitnes problem genome))


-- Gnuplotreadable statistics for 1 Generation
logStats :: Problem Char -> Int -> Population Int -> IO ()
logStats problem iterno pop = do
	when (iterno == 0) $
		putStrLn "# generation medianValue bestValue"
	let gs = map takeGenome . bestFirst Maximizing $ pop  -- genomes
	let best = head gs
	let median = gs !! (length gs `div` 2)
	let worst = last gs
	putStrLn $ unwords	[ show iterno
						, (begining . show . natFitnes problem) best
						, (braces . show) best
						, (begining . show . natFitnes problem) median
						, (braces . show) median
						, (begining . show . natFitnes problem) worst
						, (braces . show) worst
						, (take 10 . show . perm Decrypt best) problem
						]
	where
		braces :: String -> String
		braces str = "(" ++ str ++ ")"

main :: IO()
main = do
	finalPop <- geneticAlgorithm problem
	let winner = takeGenome . head . bestFirst Maximizing $ finalPop
	putStrLn $ showGenome problem winner
	return ()


begining :: String -> String
begining xs = take n xs -- ++ "..."
	where n = 6

-- Dealing with Genomes

-- just for testing
printAllPossibleSolutions :: IO()
printAllPossibleSolutions = putStrLn . unlines . map show . sortBy (comparing snd) . zip genomes . map (natFitnes problem) $ genomes
	where
		genomes = permutations . take period $ [1..]
