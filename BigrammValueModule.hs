module BigrammValueModule (evaluateOneBigramm) where

import Data.List (sort, group, sortBy)
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import NormalizeLanguageModule (normalizeLanguage)

--TODO: fromMaybe geht besser
evaluateOneBigramm :: (Char, Char) -> Double
evaluateOneBigramm = fromMaybe . flip lookup valueList
	where
		fromMaybe :: Maybe Double -> Double
		fromMaybe Nothing = 0
		fromMaybe (Just x) = x

-- TODO: Find good  sources
valueList :: [((Char, Char), Double)]
valueList =	
	[ (('L','D'), 0.02)
	, (('U','R'), 0.02)
	, (('A','R'), 0.04)
	, (('R','A'), 0.04)
	, (('V','E'), 0.04)
	, (('S','I'), 0.05)
	, (('S','A'), 0.06)
	, (('L','E'), 0.08)
	, (('S','E'), 0.08)
	, (('A','L'), 0.09)
	, (('D','E'), 0.09)
	, (('O','F'), 0.16)
	, (('N','G'), 0.18)
	, (('E','T'), 0.19)
	, (('T','E'), 0.27)
	, (('A','S'), 0.33)
	, (('T','I'), 0.34)
	, (('O','R'), 0.43)
	, (('H','I'), 0.46)
	, (('I','S'), 0.46)
	, (('E','A'), 0.47)
	, (('I','T'), 0.50)
	, (('O','U'), 0.50)
	, (('T','O'), 0.52)
	, (('E','D'), 0.53)
	, (('E','N'), 0.55)
	, (('S','T'), 0.55)
	, (('E','S'), 0.56)
	, (('H','A'), 0.56)
	, (('N','T'), 0.56)
	, (('O','N'), 0.57)
	, (('A','T'), 0.59)
	, (('N','D'), 0.63)
	, (('R','E'), 0.68)
	, (('A','N'), 0.82)
	, (('E','R'), 0.94)
	, (('I','N'), 0.94)
	, (('H','E'), 1.28)
	, (('T','H'), 1.52)
	]


analyzeStringForBigramms :: String -> [((Char,Char), Int)]
analyzeStringForBigramms = sortBy ((flip . comparing) snd) . map (head &&& length) . group . sort . bigramms . normalizeLanguage
	where
		bigramms :: [a] -> [(a,a)]
		bigramms (x1:x2:xs) = (x1,x2): bigramms (x2:xs)
		bigramms _ = []