import NaturalLanguageModule (catalogiseMonogramms, monogramValueList)
import BlindtextModule (blindtext1)
import NormalizeLanguageModule (normalizeLanguage)
import Data.Maybe (fromMaybe)



main :: IO()
main = do
	putStrLn (analyse blindText)

blindText :: String
blindText = normalizeLanguage blindtext1

statisticList :: [(Char, Int)]
statisticList = map (\(x,d) -> (x, round ((*) 0.01 . (*) d . intToDouble . length $ blindText))) $ monogramValueList

intToDouble :: Int -> Double
intToDouble = fromIntegral

analyse :: String -> String
analyse text = unlines . (header:) . map showCatalogEntry . map (analyseChar (catalogiseMonogramms text)) $ statisticList
	where
		showCatalogEntry :: (Char, Int, Int) -> String
		showCatalogEntry (c, n, m) = [c] ++ " " ++ show n ++ " " ++ show m

		header :: String
		header = "Characters expected found"

analyseChar :: (Eq a) => [(Char,Int)] -> (Char, a) -> (Char, a, Int)
analyseChar list (c,a) = (c, a, justLookup list 0 c)

justLookup :: (Eq a) => [(a,b)] -> b -> a -> b
justLookup list defaultValue key = fromMaybe defaultValue (lookup key list)
