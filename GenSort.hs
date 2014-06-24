import Moo.GeneticAlgorithm.Binary

import Control.Arrow (first)
import Control.Monad (when)
import Data.List (intercalate, sortBy, permutations)
import Data.Ord (comparing)
import NaturalLanguageModule  (naturalism)
import Moo.GeneticAlgorithm.Continuous (getRandomGenomes)
import BlindtextModule (cryptotext1, cryptotext2)
import SnModule (makePeriodicS_n)
import TypeModule
import ReorderingModule (swapmutate,revmutate,shuffelmutate, shiftmutate, listswapmutate, orderCrossover, edgeCrossover)

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



period = 7
genomesize = period
-- stopconditions (they are very high)
maxiters = 50000
timeLimit = 20 -- in seconds

problem :: Problem Char
problem = cryptotext2

popsize :: Int
popsize = 7

selection :: SelectionOp a
selection = rouletteSelect 5

crossover :: (Ord a) => CrossoverOp a
crossover =
	edgeCrossover
	--noCrossover
	--orderCrossover 0.3
	--uniformCrossover 0.3

mutation :: MutationOp a
mutation =
	shiftmutate
	--listswapmutate
	
elitesize = 1

-- sortingFittnes ls == 1 is aquivalent to ls == sort ls
natFitnes :: (Ord a) => Problem Char -> Genome a -> Double
natFitnes problem genome =
	naturalism (makePeriodicS_n genome problem)
			-- /P (fromIntegral . twoOutOf . length) problem
	where
		twoOutOf :: Int -> Int
		twoOutOf 1 = 0
		twoOutOf n = n - 1 + twoOutOf (n-1)

showGenome :: (Ord a, Show a) => Problem Char -> Genome a -> String
showGenome problem genome = "Genome " ++ show genome
						++ "\nmakes " ++ show problem 
						++ "\nto " ++ (show . makePeriodicS_n genome) problem
						++ "\n(natFitnes: " ++ show (natFitnes problem genome) ++ ")"
						where
							showBits :: [Bool] -> String
							showBits = concatMap (show . fromEnum) 


geneticAlgorithm :: Problem Char -> IO (Population Int)
geneticAlgorithm problem = do
	runIO (initializeIntGenome popsize period) $ loopIO
		[DoEvery 1 (logStats problem), TimeLimit timeLimit]
		(Or (Generations maxiters) (IfObjective (any (>=350))))
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
						, (take 6 . show . natFitnes problem) best
						, (braces . show) best
						, (take 6 . show . natFitnes problem) median
						, (braces . show) median
						, (take 6 . show . natFitnes problem) worst
						, (braces . show) worst
						, (take 10 . show . makePeriodicS_n best) problem
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


-- Dealing with Genomes

-- in which range are the numbers that are sorted to generate the S_n
-- the bigger d the more permutations are evaluated, so they are more equaly distributed
intRange :: Int -> (Int, Int)
intRange n = (0,d*n)
	where
		d = 10

--TODO: fixme
initializeIntGenome :: Int -> Int -> Rand [Genome Int]
initializeIntGenome populationsize myPeriod = do
	return . take populationsize . map (take myPeriod) . permutations $ [0,1..] 
	--manygenomes <- getRandomGenomes myPeriod $ [intRange myPeriod]
	--return (take populationsize manygenomes) 

-- just for testing
printAllPossibleSolutions :: IO()
printAllPossibleSolutions = putStrLn . unlines . map show . sortBy (comparing snd) . zip genomes . map (natFitnes problem) $ genomes
	where
		genomes = permutations . take period $ [1..]
