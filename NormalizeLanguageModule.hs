module NormalizeLanguageModule where
import Data.Char (toUpper)

normalizeLanguage :: String -> String
normalizeLanguage = filter (`elem` ['A'..'Z']) . map toUpper		