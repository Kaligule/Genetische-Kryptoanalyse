module SnModule (perm, Direction (..), PermKey) where

import Data.List (sortBy, group, sort)
import Data.Ord (comparing)
import TypeModule
import Test.QuickCheck







perm :: Direction -> PermKey -> String -> String
perm Encrypt key = makePeriodicS_n key
perm Decrypt key = makePeriodicS_n (inverse key)

inverse :: PermKey -> PermKey
inverse = map fst . sortBy (comparing snd) . zip [1..]

makePeriodicS_n :: [Int] -> S_n a
makePeriodicS_n genome = (periodicS_n (length genome) . ordGenomeToS_n) genome
	where
		ordGenomeToS_n :: (Ord b) => [b] -> S_n a
		ordGenomeToS_n as = map snd . sortBy (comparing fst) . zip as
		
		periodicS_n :: Int -> S_n a -> S_n a
		periodicS_n period sn xs
			| length xs >= period 	= sn (take period xs) ++ periodicS_n period sn (drop period xs)
			| otherwise 		= xs -- This should/could be permutated, too. Think about it.


-- TODO: Better guesses for initialisation
-- initializeMascGenome :: Int -> Rand [Genome Char] 
-- initializeMascGenome populationSize = initializePermutatedGenome populationSize charList

testall :: IO()
testall = do
  quickCheck prop_inverseTrice
  quickCheck prop_EncryptDecrypt

prop_inverseTrice :: PermKey -> Bool
prop_inverseTrice key = inverse key == (inverse . inverse . inverse) key

prop_EncryptDecrypt :: PermKey -> String -> Property
prop_EncryptDecrypt key str =
  (not . null) key
  ==> onlyUnique key
  -- ==> all (>0) key
  ==> str == (perm Encrypt key . perm Decrypt key) str
  where
    onlyUnique :: (Ord a, Eq a) => [a] -> Bool
    onlyUnique list = length list == (length . group . sort) list
