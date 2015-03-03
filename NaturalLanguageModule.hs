{-# LANGUAGE FlexibleInstances #-}
module NaturalLanguageModule
	( naturalismDefault
	, naturalism
	, Criterion(..)
	, Analysation(..)
	, charList
	, defaultCriterions
	, catalogiseMonogramms -- just for plotting
	, monogramValueList -- just for plottinh
	) where

import NormalizeLanguageModule (normalizeLanguage)
import qualified Data.Map as Map
import Test.QuickCheck
import Data.List (tails, inits, intersect)
import Data.Maybe (fromMaybe)
import Data.List (zip4)
import Data.List (nub)
import Data.List (group, groupBy)
import Data.Graph.Inductive.Query.Monad (mapSnd)
import TypeModule (WeightedCriterion, Criterion(..), Analysation(..))
-- for testing
import Data.List (sort, sortBy)
import Data.Function (on)
import Data.Ord (comparing)

naturalism :: [WeightedCriterion] -> String -> Double
naturalism criterions = sum . zipWith evaluateWeightedBy criterions . repeat . normalizeLanguage
	where
		evaluateWeightedBy :: WeightedCriterion -> String -> Double 
		evaluateWeightedBy (criterion, analysation, weight) str = (balanceParameter analysation criterion) * weight * (naturalismBy analysation criterion str) 

		naturalismBy :: Analysation -> Criterion -> String -> Double
		naturalismBy analysation Monogram	= evaluateCatalog monogramValueList		analysation . catalogise (map fst monogramValueList)		. getMonograms
		naturalismBy analysation Bigram		= evaluateCatalog bigramValueList		analysation . catalogise (map fst bigramValueList)		. getBigrams
		naturalismBy analysation Trigram	= evaluateCatalog trigramValueList		analysation . catalogise (map fst trigramValueList)		. getTrigrams
                naturalismBy analysation Quadrigram	= evaluateCatalog quadrigramValueList		analysation . catalogise (map fst quadrigramValueList)		. getQuadrigrams
		naturalismBy analysation Word		= evaluateCatalog wordValueList			analysation . catalogise (map fst wordValueList)		. getWords
		
                balanceParameter :: Analysation -> Criterion -> Double
                balanceParameter ByExpWeight Monogram   = 0.00001
                balanceParameter ByExpWeight Bigram     = 0.3
                balanceParameter ByExpWeight Trigram    = 5
                balanceParameter ByExpWeight Quadrigram = 50
                balanceParameter ByExpWeight Word       = 0.0000005

                balanceParameter ByScyline   Monogram   = 1000
                
                balanceParameter _ _ = 1 -- default


                
		getMonograms		= id
		getBigrams list		= zip list (drop 1 list) 
		getTrigrams list	= zip3 list (drop 1 list) (drop 2 list) 
		getQuadrigrams list	= zip4 list (drop 1 list) (drop 2 list) (drop 3 list)
		getWords			= sublistsBounded ((maximum. map length . map fst) wordValueList)
				where
					-- sublists of length smaller or equal n
					-- its basicly a (hopfully) better way to say
					-- sublistsBounded n = filter ((>=) n . length) . sublists 
					sublistsBounded :: Int -> [a] -> [[a]]
					sublistsBounded n = concatMap (tail . inits . take n) . tails

		evaluateCatalog :: (Ord a) => [(a, Double)] -> Analysation  -> [(a, Int)] -> Double
		evaluateCatalog valuelist ByWeight = sum . map (\(x, n) -> (cleverLookup valuelist x) * (fromIntegral n))
			where
				cleverLookup :: (Ord n, Eq n) => [(n, Double)] -> n -> Double
				cleverLookup list = fromMaybe 0 . flip Map.lookup (Map.fromList list)
		evaluateCatalog valuelist ByExpWeight = evaluateCatalog (map (mapSnd exp) valuelist) ByWeight
                evaluateCatalog valuelist ByScyline = skyline valuelist 


naturalismDefault :: String -> Double
naturalismDefault = naturalism defaultCriterions

defaultCriterions :: [WeightedCriterion]
defaultCriterions = 
        -- [ (Monogram             , ByScyline,   1)
        [ (Monogram             , ByScyline,     20)
        , (Monogram		, ByExpWeight,   20)
	, (Bigram		, ByExpWeight,   20)
	, (Trigram		, ByExpWeight,   10)
        , (Quadrigram	        , ByExpWeight,   5)
	, (Word			, ByExpWeight,   30)
	]


-- place for optimisation
-- they items are put in the predefined buckets and the buckets are counted
catalogise :: (Eq a, Ord a) => [a] -> [a] -> [(a, Int)]
-- catalogise _ = map (\l -> (head l, length l)) . group . sort
catalogise buckets = Map.toList . foldl putInBucket (makeEmptyBuckets buckets)
	where
		makeEmptyBuckets :: (Ord a) => [a] -> Map.Map a Int
		makeEmptyBuckets buckets = Map.fromList (zip buckets (repeat 0))

		putInBucket ::  (Ord a) => Map.Map a Int -> a -> Map.Map a Int
		putInBucket = flip (Map.adjust (+1))

catalogiseMonogramms :: String -> [(Char, Int)]
catalogiseMonogramms = catalogise charList


--skyline

skyline :: (Eq a, Ord a) => [(a, Double)] -> [(a, Int)] -> Double
skyline catalog1 catalog2 = (-) 1  $ skylinediff (normalizeBySecond catalog1) (normalizeBySecond . map (mapSnd fromIntegral)  $ catalog2)
  where
    -- makes sure that sum . map snd $ list == 1
    normalizeBySecond :: (Fractional b) => [(a,b)] -> [(a,b)]
    normalizeBySecond list = map (\(x,y) -> (x,y/bsum)) list
      where
        bsum = sum . map snd $ list
    
    skylinediff :: (Eq a, Ord a, Num b) => [(a,b)] -> [(a,b)] -> b
    skylinediff x y = sum . map (diff . map snd) . groupBy (on (==) fst) . sortBy (comparing fst) $ x ++ y


    diff :: (Num a) => [a] -> a
    diff [] = 0
    diff [x] = x
    diff [x,y] = (^2) . abs $ (x-y)
    diff _ = undefined "Something went wrong at diff: Was called with more then two arguments."

-- Lists found here: http://www.cryptograms.org/letter-frequencies.php


-- sum . map snd $ monogramValueList == 100.00000000000001
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
charList :: [Char]
charList = map fst monogramValueList
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

wordValueList :: [(String, Double)]
wordValueList =
		zip knownWordValueList 		(repeat 20)
	++	zip assumedWordValuelist 	(repeat 5)
	++	statisticWordValueList

-- words known to be in the text
knownWordValueList :: [String]
knownWordValueList = 
	[ "LICENCE"
	, "ALLOW"
	, "FREE"
	, "COPY"
	]

-- words assumed to be in the text 
assumedWordValuelist :: [String]
assumedWordValuelist = (nub . map normalizeLanguage)
	[ "SOFTWARE"
        , "LAW"
        , "SHARE"
        , "RESTRICT"
        , "REMOVE"
        , "SPREAD"
        , "CONTRIBUTE"
        , "HUMAN"
        , "SOURCE"
        , "OPEN"
        , "LINUX"
        , "DISTRIBUT"
        , "CODE"
        , "FOUND"
        ]

-- Source http://www.bckelk.ukfsn.org/words/uk1000n.html
-- absolute value multiplied by 10^whatever
statisticWordValueList = 
	[ ("THE",		2.230669)
	, ("AND",		1.562144)
	, ("TO",		1.340448)
	, ("OF",		1.255102)
	, ("A",			0.998712)
	, ("I",			0.866455)
	, ("IN",		0.732065)
	, ("WAS",		0.623396)
	, ("HE",		0.575049)
	, ("THAT",		0.553289)
	, ("IT",		0.552842)
	, ("HIS",		0.463788)
	, ("HER",		0.463659)
	, ("YOU",		0.445450)
	, ("AS",		0.427126)
	, ("HAD",		0.403432)
	, ("WITH",		0.403037)
	, ("FOR",		0.381170)
	, ("SHE",		0.359592)
	, ("NOT",		0.338857)
	, ("AT",		0.314452)
	, ("BUT",		0.300306)
	, ("BE",		0.299626)
	, ("MY",		0.280180)
	, ("ON",		0.265438)
	, ("HAVE",		0.260675)
	, ("HIM",		0.249245)
	, ("IS",		0.244239)
	, ("SAID",		0.235920)
	, ("ME",		0.235266)
	, ("WHICH",		0.212340)
	, ("BY",		0.204297)
	, ("SO",		0.202092)
	, ("THIS",		0.196188)
	, ("ALL",		0.192967)
	, ("FROM",		0.173569)
	, ("THEY",		0.166505)
	, ("NO",		0.164769)
	, ("WERE",		0.164408)
	, ("IF",		0.156404)
	, ("WOULD",		0.155822)
	, ("OR",		0.144370)
	, ("WHEN",		0.143468)
	, ("WHAT",		0.143199)
	, ("THERE",		0.133581)
	, ("BEEN",		0.133116)
	, ("ONE",		0.131665)
	, ("COULD",		0.123499)
	, ("VERY",		0.123111)
	, ("AN",		0.120095)
	, ("WHO",		0.113029)
	, ("THEM",		0.110621)
	, ("MR",		0.109951)
	, ("WE",		0.109511)
	, ("NOW",		0.109393)
	, ("MORE",		0.109001)
	, ("OUT",		0.108576)
	, ("DO",		0.107843)
	, ("ARE",		0.103961)
	, ("UP",		0.103757)
	, ("THEIR",		0.103486)
	, ("YOUR",		0.102978)
	, ("WILL",		0.100010)
	, ("LITTLE",		0.095975)
	, ("THAN",		0.092228)
	, ("THEN",		0.089051)
	, ("SOME",		0.088083)
	, ("INTO",		0.086304)
	, ("ANY",		0.085145)
	, ("WELL",		0.078839)
	, ("MUCH",		0.077974)
	, ("ABOUT",		0.076503)
	, ("TIME",		0.075364)
	, ("KNOW",		0.075318)
	, ("SHOULD",		0.075181)
	, ("MAN",		0.074940)
	, ("DID",		0.074608)
	, ("LIKE",		0.074352)
	, ("UPON",		0.074152)
	, ("SUCH",		0.072809)
	, ("NEVER",		0.072290)
	, ("ONLY",		0.069459)
	, ("GOOD",		0.069203)
	, ("HOW",		0.067879)
	, ("BEFORE",		0.067541)
	, ("OTHER",		0.065592)
	, ("SEE",		0.065057)
	, ("MUST",		0.064488)
	, ("AM",		0.062465)
	, ("OWN",		0.062334)
	, ("COME",		0.061950)
	, ("DOWN",		0.061304)
	, ("SAY",		0.060012)
	, ("AFTER",		0.059550)
	, ("THINK",		0.059447)
	, ("MADE",		0.059194)
	, ("MIGHT",		0.059065)
	, ("BEING",		0.058674)
	, ("MRS",		0.057423)
	, ("AGAIN",		0.055607)
	]















-- Testing

main = do
	putStrLn "It compiles"
{-
	testall

testall :: IO()
testall = do
		quickCheck prop_evaluateEmptyString
		quickCheck prop_normalize_normalize
		--quickCheck prop_sublists_length
		--quickCheck prop_sublistsBounded
		--return ()

prop_evaluateEmptyString :: Bool
prop_evaluateEmptyString = naturalismDefault "" == 0

prop_normalize_normalize :: String -> Bool
prop_normalize_normalize str = (normalizeLanguage . normalizeLanguage) str == normalizeLanguage str

prop_sublists_length :: [Char] -> Bool
prop_sublists_length lst = (length . sublistsBounded maxlength) lst == sum [0..maxlength]
	where
		maxlength = length lst

prop_sublistsBounded :: Int -> [Char] -> Bool
prop_sublistsBounded n list = (sort . sublistsBounded n) list == (sort . filter ((>=) n . length) . sublists) list
	where
		-- allsublists
		sublists :: [a] -> [[a]]
		sublists = filter (not . null). concatMap tails . inits
-}
