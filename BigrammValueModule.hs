module BigrammValueModule (evaluateOneBigramm) where

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
	[ (('S','A'),1)
	, (('A','N'),1)
	, (('N','D'),1)
	, (('D','R'),1)
	, (('R','A'),1)
	, (('A','I'),1)
	, (('I','S'),1)
	, (('S','T'),1)
	, (('T','N'),1)
	, (('N','E'),1)
	, (('E','M'),1)
	, (('M','A'),1)
	, (('A','U'),1)
	, (('U','S'),1)
	]

