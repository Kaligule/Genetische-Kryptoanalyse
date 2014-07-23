module SnModule (makePeriodicS_n) where

import Data.List (sortBy)
import Data.Ord (comparing)
import TypeModule

makePeriodicS_n :: (Ord a) => [a] -> S_n b
makePeriodicS_n genome = (periodicS_n (length genome) . ordGenomeToS_n) genome
	where
		ordGenomeToS_n :: (Ord a) => [a] -> S_n b
		ordGenomeToS_n as = map snd . sortBy (comparing fst) . zip as
		
		periodicS_n :: Int -> S_n a -> S_n a
		periodicS_n period sn xs
			| length xs >= period 	= sn (take period xs) ++ periodicS_n period sn (drop period xs)
			| otherwise 		= sn xs
