import NaturalLanguageModule (defaultCriterions, naturalism) 
import TypeModule (WeightedCriterion, Criterion(..), Analysation(..))
import MascModule (Direction (..), MascKey, masc)
import BlindtextModule (cryptotext2)

main = do
	file <- readFile inputFilePath
	let analysation = unlines. analyseKeys . getKeysFromFile $ file
	writeFile ("solvingLogs/output" ++ show numberOfFileToAnalyse ++ "Elite.txt") analysation

numberOfFileToAnalyse = 1

-- TODO: Import problem from better place
problem :: String
problem = cryptotext2

inputFilePath :: String
inputFilePath = "solvingLogs/output" ++ show numberOfFileToAnalyse ++ ".txt" 


getKeysFromFile :: String -> [MascKey]
getKeysFromFile = map getValues . filter validLine . lines
	where
		getValues :: String -> MascKey
		getValues = filter (not . flip elem "\"()") . (!! 2) . words
	
		validLine :: String -> Bool
		validLine = not . ('#'==) . head

analyseKeys :: [String] -> [String]
analyseKeys = (header:) . map (filter (not . ('\"'==))) . zipWith (\i s -> show i ++ " " ++ s) (map show [1..]) . map analyseOneKey
	where
		header :: String
		header = unwords . ("Generation":) . map showWeightedCriterion $ defaultCriterions

		showWeightedCriterion :: WeightedCriterion -> String
		showWeightedCriterion (crit, ana, weight) = show crit ++ "-" ++ show ana ++ "-(" ++ show weight ++ ")"

		analyseOneKey :: String -> String
		analyseOneKey key = unwords . map show $ naturalismPoints
			where
				analysationText :: String
				analysationText = masc Decrypt key problem
		
				naturalismPoints :: [Double]
				naturalismPoints = map (flip naturalism analysationText) . map return $ defaultCriterions

