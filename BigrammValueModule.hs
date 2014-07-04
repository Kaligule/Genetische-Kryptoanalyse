{-# LANGUAGE FlexibleInstances #-}
module BigrammValueModule
	( Evaluable(..)
	, evaluateBy
	) where

import NormalizeLanguageModule (normalizeLanguage)
import Data.Maybe (fromMaybe)
import Data.List (zip4)

data Evaluable = Monogram | Bigram | Trigram | Quadrigram

weightFactor :: Evaluable -> Double
weightFactor _ = 1

evaluateBy :: Evaluable -> String -> Double
evaluateBy Monogram		str = (sum . map evaluateOneNgramm) ( getNgramms str :: [(Char)] )
evaluateBy Bigram		str = (sum . map evaluateOneNgramm) ( getNgramms str :: [(Char,Char)] )
evaluateBy Trigram		str = (sum . map evaluateOneNgramm) ( getNgramms str :: [(Char,Char,Char)] )
evaluateBy Quadrigram	str = (sum . map evaluateOneNgramm) ( getNgramms str :: [(Char,Char,Char,Char)] )

-- Class Ngramm

class (Eq n) => Ngramm n where
	getNgramms :: String -> [n]
	valueList :: [(n, Double)]

evaluateOneNgramm :: (Ngramm n) => n -> Double
evaluateOneNgramm = fromMaybe 0 . flip lookup valueList

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
