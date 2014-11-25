import NaturalLanguageModule (catalogiseMonogramms, monogramValueList)
import BlindtextModule (cryptotext2)
import MascModule (Direction (..), MascKey, masc, initializeMascGenome)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Control.Monad (liftM)
main :: IO()
main = do
	numberString <- liftM head getArgs
	logfile <- readFile ("solvingLogs/output" ++ numberString ++ ".txt") 
	let key = getKeyFromLogfile logfile
	writeFile ("solvingLogs/output" ++ numberString ++ "MonogramAnalysis.txt") (analyse key)

getKeyFromLogfile :: String -> String
getKeyFromLogfile = stripQuotes . last . words . number4frombehind . lines
	where
		number4frombehind :: [a] -> a
		number4frombehind = last . init . init . init

stripQuotes :: String -> String
stripQuotes = filter (not . (==) '\"')

suggestedCleartext :: MascKey -> String
suggestedCleartext key = masc Decrypt key problem

problem :: String
problem = cryptotext2

statisticList :: [(Char, Int)]
statisticList = map (\(x,d) -> (x, round ((*) 0.01 . (*) d . intToDouble . length $ problem))) $ monogramValueList

intToDouble :: Int -> Double
intToDouble = fromIntegral

analyse :: MascKey -> String
analyse key = unlines . (header:) . map showCatalogEntry . map (analyseChar (catalogiseMonogramms . suggestedCleartext $ key)) $ statisticList
	where
		showCatalogEntry :: (Char, Int, Int) -> String
		showCatalogEntry (c, n, m) = [c] ++ " " ++ show n ++ " " ++ show m

		header :: String
		header = "Characters expected found"

analyseChar :: (Eq a) => [(Char,Int)] -> (Char, a) -> (Char, a, Int)
analyseChar list (c,a) = (c, a, justLookup list 0 c)

justLookup :: (Eq a) => [(a,b)] -> b -> a -> b
justLookup list defaultValue key = fromMaybe defaultValue (lookup key list)