import Data.List (transpose)
import Data.Maybe (listToMaybe)

main = do
	analyses <- mapM analyseFile inputFileName
	if (validateAnalysis analyses == False)
		then error "The Files where not done with the same target value."
	else do
		writeFile "solvingLogs/outputCombination.txt" (combineAnalysis analyses)

data Analyses = Analyses 	{ name :: String
							, fittnessToReach :: Double
							, valueList :: [Double]}

inputFileName :: [String]
inputFileName = map fileName [1..4]
	where
		fileName :: Int -> String
		fileName n = "solvingLogs/output" ++ show n ++ ".txt"

analyseFile :: String -> IO (Analyses)
analyseFile filepath = do
	file <- readFile filepath
	let fileLines = lines file
	return $ Analyses {name = filepath, fittnessToReach = (getMax . head) fileLines, valueList = getValues fileLines}
	where
		getValues :: [String] -> [Double]
		getValues = map (read . (!! 1) . words) . validLines
	
		validLines :: [String] -> [String]
		validLines = filter (not . ('#' ==) . (head))
getMax :: String -> Double
getMax = read . last . words

validateAnalysis :: [Analyses] -> Bool
validateAnalysis = allEqual . map fittnessToReach
	where
		allEqual :: (Eq a) => [a] -> Bool
		allEqual (x:xs) = all ((==) x) xs

combineAnalysis :: [Analyses] -> String
combineAnalysis analyses = unlines . (maxValue:) . (header:) $ collumsToTable (generationList:(map valueList analyses))
	where
		-- its meaningless wich list we choose to take maximum from, since we alreaddy assured they are all equal
		maxValue :: String
		maxValue = "# " ++ ((show . fittnessToReach . head) $ analyses)

		header :: String
		header = unwords ("Generation":(map name analyses))

		generationList :: [Double]
		generationList = [1..]

-- gets valuelists, return gnuplotable lines of values
-- When all of the lines after the first 1 are empty, the table will stop
collumsToTable :: (Show a) => [[a]] -> [String]
collumsToTable valueLists
	| (all null . drop 1) valueLists	= [] -- the first collum is an endless one 
	| otherwise = ((unwords . map showHead) valueLists):(collumsToTable . map secureTail) valueLists
	where
		showHead :: (Show a) => [a] -> String
		showHead [] = "Nan"
		showHead (x:_) = show x

		secureTail :: [a] -> [a]
		secureTail [] = []
		secureTail (_:xs) = xs