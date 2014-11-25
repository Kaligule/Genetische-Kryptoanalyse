module MascModule
	(Direction (..)
	, MascKey
	, masc
	, initializeMascGenome
	) where

import Data.Maybe (fromMaybe)
-- the charList schould always be the cleartextversion of the alphabet
import NaturalLanguageModule(charList)
import Reordering (initializePermutatedGenome)
import TypeModule (Genome, Rand, Direction(..), MascKey)



fallbackChar :: Char 
fallbackChar = '_'

masc :: Direction -> MascKey -> String -> String
masc direction key = map (mascOne direction key)

mascOne :: Direction -> MascKey -> Char -> Char
mascOne Encrypt key = justLookup (zip charList key) fallbackChar
mascOne Decrypt key = justLookup (zip key charList) fallbackChar

justLookup :: (Eq a) => [(a,b)] -> b -> a -> b
justLookup list defaultValue key = fromMaybe defaultValue (lookup key list)

-- TODO: Better guesses for initialisation
initializeMascGenome :: Int -> Rand [Genome Char] 
initializeMascGenome populationSize = initializePermutatedGenome populationSize charList
