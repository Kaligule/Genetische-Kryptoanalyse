{-# LANGUAGE FlexibleInstances #-}
module NaturalLanguageModule (naturalism) where

import BigrammValueModule
import WordListModule (wordlist)
import NormalizeLanguageModule (normalizeLanguage)
import Test.QuickCheck
import Data.List (tails, inits, intersect)
-- for testing
import BlindtextModule (blindtext1, cryptotext1)

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
naturalism = sum . zipWith evaluateBy evaluables . repeat . normalizeLanguage
	where
		evaluables :: [Evaluable]
		evaluables =	[ Bigram
						, Trigram
						, Quadrigram
						]

evaluateWords :: String -> Double
evaluateWords = fromIntegral . length . intersect wordlist . sublists

sublists :: [a] -> [[a]]
sublists = filter (not . null). concatMap tails . inits

-- Testing

main = do
	print "Cleartext:"
	print . naturalism $ blindtext1
	print "Kryptotext:"
	print . naturalism $ cryptotext1

	--testall

testall :: IO()
testall = do
		quickCheck prop_evaluateEmptyString
		quickCheck prop_normalize_normalize
		quickCheck prop_sublists_length
		--return ()

prop_evaluateEmptyString :: Bool
prop_evaluateEmptyString = naturalism "" == 0

prop_normalize_normalize :: String -> Bool
prop_normalize_normalize str = (normalizeLanguage . normalizeLanguage) str == normalizeLanguage str

prop_sublists_length :: [Char] -> Bool
prop_sublists_length lst = (length . sublists) lst == sum [0..length lst]