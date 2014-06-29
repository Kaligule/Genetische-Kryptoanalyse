{-# LANGUAGE FlexibleInstances #-}
module NaturalLanguageModule (naturalism) where

import BigrammValueModule
import WordListModule (wordlist)
import NormalizeLanguageModule (normalizeLanguage)
import Test.QuickCheck
import Data.List (tails, inits, intersect)
-- for testing
import BlindtextModule (blindtext1)

--class NaturalTextElement a where
--	getAll :: String -> [a]
--	getValue :: a -> Double

--analyzeString :: String -> Double
--analyzeString str = (sum . map getValue) (getAll str :: Word)

type Word = String
type Bigramm = (Char,Char)
--type Trigramm = (Char,Char,Char)
--type Tetragramm = (Char,Char,Char,Char)

--instance NaturalTextElement Bigramm where
--	getAll = bigramms
--	getValue = evaluateOneBigramm

--instance NaturalTextElement Word where
--	getAll = words
--	getValue = evaluateWords

naturalism :: String -> Double
naturalism = sum . zipWith ($) evaluationFunctions . repeat . normalizeLanguage
	where
		evaluationFunctions :: [String -> Double]
		evaluationFunctions = 	[ evaluateBigramms
								--, evaluateWords
								]


-- Haufigkeitsanalyse for (Char, Char)
-- sort first and score the group or just score the single Bigramms?
evaluateBigramms :: String -> Double
evaluateBigramms = sum . map evaluateOneBigramm . bigramms

bigramms :: [a] -> [(a,a)]
bigramms (x1:x2:xs) = (x1,x2): bigramms (x2:xs)
bigramms _ = []

--trigramms :: [a] -> [(a,a,a)]
--trigramms (x1:x2:x3:xs) = (x1,x2,x3): trigramms (x2:x3:xs)
--trigramms _ = []

--tetragramms :: [a] -> [(a,a,a,a)]
--tetragramms (x1:x2:x3:x4:xs) = (x1,x2,x3,x4): tetragramms (x2:x3:x4:xs)
--tetragramms _ = []

-- tests if the String contains Words of actual English
-- test if substrings are words or if Words are contained in String? Difficult
evaluateWords :: String -> Double
evaluateWords = fromIntegral . length . intersect wordlist . sublists

sublists :: [a] -> [[a]]
sublists = filter (not . null). concatMap tails . inits

-- Testing

main = do
	print . naturalism $ blindtext1
	--testall

testall :: IO()
testall = do
		quickCheck prop_bigrammCounter
		quickCheck prop_evaluateEmptyString
		quickCheck prop_someWordsAreWords
		quickCheck prop_normalize_normalize
		quickCheck prop_sublists_length
		print . naturalism $ "Die Sandra ist die Sandra ist ne Maus, eine Maus! Eine Mahahaus! Ne super Mahahaus. Maus, Maus... Applaus applaus..."
		--return ()

prop_bigrammCounter :: String -> Bool
prop_bigrammCounter str
	|null str 	= (null . bigramms) str
	|otherwise	= (length . bigramms) str == length str -1

prop_evaluateEmptyString :: Bool
prop_evaluateEmptyString = naturalism "" == 0

prop_someWordsAreWords :: Bool
prop_someWordsAreWords = (fromIntegral . length) testWords == (sum . map evaluateWords . map normalizeLanguage) testWords
	where
		testWords :: [String]
		testWords = ["Sandra", "Maus"]

prop_normalize_normalize :: String -> Bool
prop_normalize_normalize str = (normalizeLanguage . normalizeLanguage) str == normalizeLanguage str

prop_sublists_length :: [Char] -> Bool
prop_sublists_length lst = (length . sublists) lst == sum [0..length lst]