{-# LANGUAGE FlexibleInstances #-}
module NaturalLanguageModule
	( naturalismDefault
	, naturalism
	, Criterion(..)
	, charList
	) where

--import BigrammValueModule
import NormalizeLanguageModule (normalizeLanguage)
import Test.QuickCheck
import Data.List (tails, inits, intersect)
import Data.Maybe (fromMaybe)
import Data.List (zip4)
import Data.List (nub)
import TypeModule (Criterion(..))
{-
-- for testing
import BlindtextModule (blindtext1, cryptotext1)
-}

evaluateWeightedBy :: (Criterion, Double) -> String -> Double 
evaluateWeightedBy (Monogram, weight)	str = ( (*) weight . sum . map evaluateOneNgramm) ( getNgramms str :: [(Char)] )
evaluateWeightedBy (Bigram, weight)		str = ( (*) weight . sum . map evaluateOneNgramm) ( getNgramms str :: [(Char,Char)] )
evaluateWeightedBy (Trigram, weight)	str = ( (*) weight . sum . map evaluateOneNgramm) ( getNgramms str :: [(Char,Char,Char)] )
evaluateWeightedBy (Quadrigram, weight)	str = ( (*) weight . sum . map evaluateOneNgramm) ( getNgramms str :: [(Char,Char,Char,Char)] )
evaluateWeightedBy (Word, weight) 		str = ( (*) weight . sum . map evaluateOneNgramm) ( getNgramms str :: String )

-- Class Ngramm

class (Eq n) => Ngramm n where
	getNgramms :: String -> [n]
	valueList :: [(n, Double)]

evaluateOneNgramm :: (Ngramm n) => n -> Double
evaluateOneNgramm = fromMaybe 0 . flip lookup valueList

maxWordLength :: Int
maxWordLength = maximum . map (length . fst) $ wordValueList

evaluateWords :: String -> Double
evaluateWords = fromIntegral . length . intersect wordlist . sublists

--TODO: get long sublists away
sublists :: [a] -> [[a]]
sublists = filter (not . null). concatMap tails . inits

--TODO: Find good Sources
wordlist :: [String]
wordlist = (nub . map normalizeLanguage . words)
	"and animal eat give global governmen human law legal life live person principles rights take torture universal world"
	--"Human rights are moral principles that set out certain standards of human behaviour, and are regularly protected as legal rights in national and international law.[1] They are commonly understood as inalienable fundamental rights to which a person is inherently entitled simply because she or he is a human being.[2] Human rights are thus conceived as universal (applicable everywhere) and egalitarian (the same for everyone). The doctrine of human rights has been highly influential within international law, global and regional institutions. Policies of states and in the activities of non-governmental organizations and have become a cornerstone of public policy around the world. The idea of human rights[3] suggests, if the public discourse of peacetime global society can be said to have a common moral language, it is that of human rights. The strong claims made by the doctrine of human rights continue to provoke considerable skepticism and debates about the content, nature and justifications of human rights to this day. Indeed, the question of what is meant by a right is itself controversial and the subject of continued philosophical debate.[4] Many of the basic ideas that animated the human rights movement developed in the aftermath of the Second World War and the atrocities of The Holocaust, culminating in the adoption of the Universal Declaration of Human Rights in Paris by the United Nations General Assembly in 1948. The ancient world did not possess the concept of universal human rights.[5] The true forerunner of human rights discourse was the concept of natural rights which appeared as part of the medieval Natural law tradition that became prominent during the Enlightenment with such philosophers as John Locke, Francis Hutcheson, and Jean-Jacques Burlamaqui, and featured prominently in the English Bill of Rights and the political discourse"

naturalism :: [(Criterion, Double)] -> String -> Double
naturalism criterions = sum . zipWith evaluateWeightedBy criterions . repeat . normalizeLanguage

naturalismDefault :: String -> Double
naturalismDefault = naturalism defaultweights
	where
		defaultweights :: [(Criterion, Double)]
		defaultweights =	[ (Monogram		, 10)
							, (Bigram		, 1)
							, (Trigram		, 1)
							, (Quadrigram	, 1)
							, (Word			, 0.01)
							]

charList :: [Char]
charList = map fst monogramValueList
-- (Char) is an Ngramm

instance Ngramm (Char) where
	valueList = monogramValueList
	getNgramms = id

-- (Char, Char) is an Ngramm

instance Ngramm (Char, Char) where
	valueList = bigramValueList
	getNgramms list = zip list (drop 1 list) 

-- (Char, Char, Char) is an Ngramm

instance Ngramm (Char, Char, Char) where
	valueList = trigramValueList
	getNgramms list = zip3 list (drop 1 list) (drop 2 list) 

-- (Char, Char, Char, Char) is an Ngramm

instance Ngramm (Char, Char, Char, Char) where
	valueList = quadrigramValueList
	getNgramms list = zip4 list (drop 1 list) (drop 2 list) (drop 3 list)

instance Ngramm String where
	valueList = wordValueList
	getNgramms = sublists

-- Lists found here: http://www.cryptograms.org/letter-frequencies.php

monogramValueList :: [(Char, Double)]
monogramValueList =
	[ ('E', 12.575645)
	, ('T', 9.085226)
	, ('A', 8.000395)
	, ('O', 7.591270)
	, ('I', 6.920007)
	, ('N', 6.903785)
	, ('S', 6.340880)
	, ('H', 6.236609)
	, ('R', 5.959034)
	, ('D', 4.317924)
	, ('L', 4.057231)
	, ('U', 2.841783)
	, ('C', 2.575785)
	, ('M', 2.560994)
	, ('F', 2.350463)
	, ('W', 2.224893)
	, ('G', 1.982677)
	, ('Y', 1.900888)
	, ('P', 1.795742)
	, ('B', 1.535701)
	, ('V', 0.981717)
	, ('K', 0.739906)
	, ('X', 0.179556)
	, ('J', 0.145188)
	, ('Q', 0.117571)
	, ('Z', 0.079130)
	]
bigramValueList :: [((Char, Char), Double)]
bigramValueList =
	[ (('T','H'), 3.882543)
	, (('H','E'), 3.681391)
	, (('I','N'), 2.283899)
	, (('E','R'), 2.178042)
	, (('A','N'), 2.140460)
	, (('R','E'), 1.749394)
	, (('N','D'), 1.571977)
	, (('O','N'), 1.418244)
	, (('E','N'), 1.383239)
	, (('A','T'), 1.335523)
	, (('O','U'), 1.285484)
	, (('E','D'), 1.275779)
	, (('H','A'), 1.274742)
	, (('T','O'), 1.169655)
	, (('O','R'), 1.151094)
	, (('I','T'), 1.134891)
	, (('I','S'), 1.109877)
	, (('H','I'), 1.092302)
	, (('E','S'), 1.092301)
	, (('N','G'), 1.053385)
	]
trigramValueList :: [((Char, Char, Char), Double)]
trigramValueList =
	[ (('T','H', 'E'), 3.508232)
	, (('A','N', 'D'), 1.593878)
	, (('I','N', 'G'), 1.147042)
	, (('H','E', 'R'), 0.822444)
	, (('H','A', 'T'), 0.650715)
	, (('H','I', 'S'), 0.596748)
	, (('T','H', 'A'), 0.593593)
	, (('E','R', 'E'), 0.560594)
	, (('F','O', 'R'), 0.555372)
	, (('E','N', 'T'), 0.530771)
	, (('I','O', 'N'), 0.506454)
	, (('T','E', 'R'), 0.461099)
	, (('W','A', 'S'), 0.460487)
	, (('Y','O', 'U'), 0.437213)
	, (('I','T', 'H'), 0.431250)
	, (('V','E', 'R'), 0.430732)
	, (('A','L', 'L'), 0.422758)
	, (('W','I', 'T'), 0.397290)
	, (('T','H', 'I'), 0.394796)
	, (('T','I', 'O'), 0.378058)
	]
quadrigramValueList :: [((Char, Char, Char, Char), Double)]
quadrigramValueList =
	[ (('T','H','A','T'), 0.761242)
	, (('T','H','E','R'), 0.604501)
	, (('W','I','T','H'), 0.573866)
	, (('T','I','O','N'), 0.551919)
	, (('H','E','R','E'), 0.374549)
	, (('O','U','L','D'), 0.369920)
	, (('I','G','H','T'), 0.309440)
	, (('H','A','V','E'), 0.290544)
	, (('H','I','C','H'), 0.284292)
	, (('W','H','I','C'), 0.283826)
	, (('T','H','I','S'), 0.276333)
	, (('T','H','I','N'), 0.270413)
	, (('T','H','E','Y'), 0.262421)
	, (('A','T','I','O'), 0.262386)
	, (('E','V','E','R'), 0.260695)
	, (('F','R','O','M'), 0.258580)
	, (('O','U','G','H'), 0.253447)
	, (('W','E','R','E'), 0.231089)
	, (('H','I','N','G'), 0.229944)
	, (('M','E','N','T'), 0.223347)
	]

-- Source http://www.bckelk.ukfsn.org/words/uk1000n.html
wordValueList = 
  [ ("THE",		223066.9)
  , ("AND",		156214.4)
  , ("TO",		134044.8)
  , ("OF",		125510.2)
  , ("A",		99871.2)
  , ("I",		86645.5)
  , ("IN",		73206.5)
  , ("WAS",		62339.6)
  , ("HE",		57504.9)
  , ("THAT",	55328.9)
  , ("IT",		55284.2)
  , ("HIS",		46378.8)
  , ("HER",		46365.9)
  , ("YOU",		44545.0)
  , ("AS",		42712.6)
  , ("HAD",		40343.2)
  , ("WITH",	40303.7)
  , ("FOR",		38117.0)
  , ("SHE",		35959.2)
  , ("NOT",		33885.7)
  , ("AT",		31445.2)
  , ("BUT",		30030.6)
  , ("BE",		29962.6)
  , ("MY",		28018.0)
  , ("ON",		26543.8)
  , ("HAVE",	26067.5)
  , ("HIM",		24924.5)
  , ("IS",		24423.9)
  , ("SAID",	23592.0)
  , ("ME",		23526.6)
  , ("WHICH",	21234.0)
  , ("BY",		20429.7)
  , ("SO",		20209.2)
  , ("THIS",	19618.8)
  , ("ALL",		19296.7)
  , ("FROM",	17356.9)
  , ("THEY",	16650.5)
  , ("NO",		16476.9)
  , ("WERE",	16440.8)
  , ("IF",		15640.4)
  , ("WOULD",	15582.2)
  , ("OR",		14437.0)
  , ("WHEN",	14346.8)
  , ("WHAT",	14319.9)
  , ("THERE",	13358.1)
  , ("BEEN",	13311.6)
  , ("ONE",		13166.5)
  , ("COULD",	12349.9)
  , ("VERY",	12311.1)
  , ("AN",		12009.5)
  , ("WHO",		11302.9)
  , ("THEM",	11062.1)
  , ("MR",		10995.1)
  , ("WE",		10951.1)
  , ("NOW",		10939.3)
  , ("MORE",	10900.1)
  , ("OUT",		10857.6)
  , ("DO",		10784.3)
  , ("ARE",		10396.1)
  , ("UP",		10375.7)
  , ("THEIR",	10348.6)
  , ("YOUR",	10297.8)
  , ("WILL",	10001.0)
  , ("LITTLE",	9597.5)
  , ("THAN",	9222.8)
  , ("THEN",	8905.1)
  , ("SOME",	8808.3)
  , ("INTO",	8630.4)
  , ("ANY",		8514.5)
  , ("WELL",	7883.9)
  , ("MUCH",	7797.4)
  , ("ABOUT",	7650.3)
  , ("TIME",	7536.4)
  , ("KNOW",	7531.8)
  , ("SHOULD",	7518.1)
  , ("MAN",		7494.0)
  , ("DID",		7460.8)
  , ("LIKE",	7435.2)
  , ("UPON",	7415.2)
  , ("SUCH",	7280.9)
  , ("NEVER",	7229.0)
  , ("ONLY",	6945.9)
  , ("GOOD",	6920.3)
  , ("HOW",		6787.9)
  , ("BEFORE",	6754.1)
  , ("OTHER",	6559.2)
  , ("SEE",		6505.7)
  , ("MUST",	6448.8)
  , ("AM",		6246.5)
  , ("OWN",		6233.4)
  , ("COME",	6195.0)
  , ("DOWN",	6130.4)
  , ("SAY",		6001.2)
  , ("AFTER",	5955.0)
  , ("THINK",	5944.7)
  , ("MADE",	5919.4)
  , ("MIGHT",	5906.5)
  , ("BEING",	5867.4)
  , ("MRS",		5742.3)
  , ("AGAIN",	5560.7)
  ]



{-
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

-}