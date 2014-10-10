
{-
-- Doku to  Criterion: http://www.serpentine.com/criterion/tutorial.html
Usage of this file:
ghc --make -O2 lookuptest.hs
./lookuptest --output lookuptest.html
firefox lookuptest.html
-}


import NormalizeLanguageModule (normalizeLanguage)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as MapLazy
import qualified Data.Map.Strict as MapStrict
import Criterion.Main

-- lookUpMap (Lazy or Strict) seems to be 25 times faster then lookUpNaive

-- benchmarks
main = defaultMain
	[ bgroup "lookUpNaive"
		[ bench ("10")		$ whnf (lookupTest lookUpNaive) 10
		, bench ("100")		$ whnf (lookupTest lookUpNaive) 100
		, bench ("1000")	$ whnf (lookupTest lookUpNaive) 1000
		, bench ("10000")	$ whnf (lookupTest lookUpNaive) 10000
		, bench ("100000")	$ whnf (lookupTest lookUpNaive) 100000
		]
	, bgroup "lookupMapLazy"
		[ bench ("10")		$ whnf (lookupTest lookupMapLazy) 10
		, bench ("100")		$ whnf (lookupTest lookupMapLazy) 100
		, bench ("1000")	$ whnf (lookupTest lookupMapLazy) 1000
		, bench ("10000")	$ whnf (lookupTest lookupMapLazy) 10000
		, bench ("100000")	$ whnf (lookupTest lookupMapLazy) 100000
		]
	, bgroup "lookupMapStrict"
		[ bench ("10")		$ whnf (lookupTest lookupMapStrict) 10
		, bench ("100")		$ whnf (lookupTest lookupMapStrict) 100
		, bench ("1000")	$ whnf (lookupTest lookupMapStrict) 1000
		, bench ("10000")	$ whnf (lookupTest lookupMapStrict) 10000
		, bench ("100000")	$ whnf (lookupTest lookupMapStrict) 100000
		]
	]


lookupTest :: (String -> Double) -> Int -> Double
lookupTest f n = sum . map f $ testwordList
	where
		testwordList :: [String]
		testwordList = take n (cycle testwords)

		testwords :: [String]
		testwords =
				[ "human"
				, "animal"
				, "face"
				, "aaojxy"
				, "yxcvxc"
				, "aaweifiaoewjfoaiwejfowajfoiewfja"
				, ""	
				, "plmdsavöoadsivj"
				, "the"
				, "tune"
				]

lookUpNaive :: String -> Double
lookUpNaive = fromMaybe 0 . flip lookup wordValueList . normalizeLanguage

lookupMapLazy :: String -> Double
lookupMapLazy = fromMaybe 0 . flip MapLazy.lookup wordSet
	where
		wordSet = MapLazy.fromList $ wordValueList

lookupMapStrict :: String -> Double
lookupMapStrict = fromMaybe 0 . flip MapStrict.lookup wordSet
	where
		wordSet = MapStrict.fromList $ wordValueList












wordValueList :: [(String, Double)]
wordValueList =
		zip knownWordValueList 		(repeat 100)
	++	zip assumedWordValuelist 	(repeat 10)
	++	statisticWordValueList

-- words known to be in the text
knownWordValueList :: [String]
knownWordValueList = 
	[ "HUMAN"
	, "RIGHTS"
	, "ALL"
	, "PEOPLE"
	]

-- words assumed to be in the text 
assumedWordValuelist :: [String]
assumedWordValuelist = (nub . map normalizeLanguage)
	[ "and"
	, "animal"
	, "eat"
	, "give"
	, "global"
	, "governmen"
	, "human"
	, "law"
	, "legal"
	, "life"
	, "live"
	, "person"
	, "principles"
	, "rights"
	, "take"
	, "torture"
	, "universal"
	, "world"
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
	, ("LITTLE",	0.095975)
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
	, ("SHOULD",	0.075181)
	, ("MAN",		0.074940)
	, ("DID",		0.074608)
	, ("LIKE",		0.074352)
	, ("UPON",		0.074152)
	, ("SUCH",		0.072809)
	, ("NEVER",		0.072290)
	, ("ONLY",		0.069459)
	, ("GOOD",		0.069203)
	, ("HOW",		0.067879)
	, ("BEFORE",	0.067541)
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
