module NormalizeLanguageModule (normalizeLanguage) where
import Data.Char (toUpper)

normalizeLanguage :: String -> String
normalizeLanguage = filter (`elem` allowedCharakters) . map toUpper . concatMap umlaut

umlaut :: Char -> String
umlaut '\196' 	= "AE"
umlaut '\214' 	= "OE"
umlaut '\220' 	= "UE"
umlaut '\223' 	= "ss"
umlaut '\228' 	= "ae"
umlaut '\246' 	= "oe"
umlaut '\252' 	= "ue"
umlaut c 		= [c]

-- Which charakters are allowed in a cryptotext?
allowedCharakters :: [Char]
allowedCharakters = ['A'..'Z']