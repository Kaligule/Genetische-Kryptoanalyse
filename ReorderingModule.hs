module ReorderingModule (swapmutate, revmutate, shuffelmutate, shiftmutate, listswapmutate, orderCrossover, edgeCrossover)
	where
import Moo.GeneticAlgorithm.Binary (CrossoverOp, Genome, MutationOp, getRandomR, Rand, shuffle)
import Moo.GeneticAlgorithm.Random (getDouble, getBool, withProbability, getNormal)
import Data.Ord (comparing)
import Data.List (sortBy, sort, group, groupBy, nub, permutations)
import Test.QuickCheck
import Data.Graph.Inductive.Query.Monad (mapFst, mapSnd)
import Control.Monad (liftM)
import ShowRandModule (showRand)

-- 2 parents have Sex, we could do groupsex if we wanted 
edgeCrossover :: (Eq a, Ord a) => CrossoverOp a
edgeCrossover []  = return ([], [])
edgeCrossover [celibate] = return ([],[celibate])
edgeCrossover (g1:g2:rest) = do
	child1 <- haveSexForEdgeCrossover [g1,g2]
	child2 <- haveSexForEdgeCrossover [g1,g2]
	return ([child1,child2], rest)


haveSexForEdgeCrossover :: (Eq a, Ord a) => [[a]] -> Rand [a]
haveSexForEdgeCrossover parents = do
	let parentalEdges = concatMap edges parents
	startpoint <- chooseRandom . filterByNumber . map head $ parents
	let possibleKnots = nub . concat $ parents
	-- this is dangerous if the parents aren't made of the exactly same knots
	childTail <- findPathFrom startpoint possibleKnots parentalEdges
	return (startpoint : childTail)

-- for haveSexForEdgeCrossover
-- can deal with (startKnot `elem` knotsToPass) and parentalEdges from or to bad knots
findPathFrom :: (Eq a, Ord a) => a -> [a] -> [(a,a)] -> Rand [a]
findPathFrom startKnot knotsToPass parentalEdges = do
	let remainingEdges = filterEdgesByKnots knotsToPass parentalEdges
	let remainingKnots = removeTakenKnot startKnot knotsToPass
	if null remainingKnots
		then return []
	else do  
		nextEdge <- chooseEdge startKnot remainingKnots remainingEdges
		let nextKnot = snd nextEdge
		restPath <- findPathFrom nextKnot remainingKnots remainingEdges
		return (nextKnot:restPath)

-- tries to find the best edge of the given list of edges
-- if there is no edge that fits, one is made up 
chooseEdge :: (Eq a, Ord a) => a -> [a] -> [(a,a)] -> Rand (a,a)
chooseEdge _ [] _ = error "No knots to choose from."
chooseEdge startKnot [onlyRemainingKnot] _ = return $ fromTo startKnot onlyRemainingKnot
chooseEdge startKnot remainingKnots remainingParentalEdges = do
	let favouritEdges = filterByStartingpoint startKnot remainingParentalEdges
	if (not . null) favouritEdges then
		chooseRandom . filterByDecicions startKnot . filterByNumber $ favouritEdges
	else do
		randomKnot <- chooseRandom remainingKnots
		return (fromTo startKnot randomKnot)
		--error $ "There was no parentalEdge from this starting point." -- ++ " Was called with chooseEdge " ++ show startKnot ++ " " ++ show remainingKnots ++ " " ++ show remainingParentalEdges 

-- only returns Edges that live inside a pool of given knots
filterEdgesByKnots :: (Eq a) => [a] -> [(a,a)] -> [(a,a)]
filterEdgesByKnots goodKnots candidateEdges = [edge | edge <- candidateEdges, fst edge `elem` goodKnots, snd edge `elem` goodKnots ] 


removeTakenKnot :: (Eq a) => a -> [a] -> [a]
removeTakenKnot x = filter (/= x)

-- removes every edge from or to x
removeUntakenEdges :: (Eq a) => a -> [(a,a)] -> [(a,a)]
removeUntakenEdges x = filter (not . hasEndpoint x) . filter (not . hasStartpoint x)

-- only return those that appear most often in the list.
filterByNumber :: (Ord a, Eq a) => [a] -> [a]
filterByNumber = concat . maxArg length . group . sort

filterByStartingpoint :: (Eq a) => a -> [(a,a)] -> [(a,a)]
filterByStartingpoint x = filter (hasStartpoint x)

-- take those edges where the knot you come out has a small number of outgoing edges
filterByDecicions :: (Eq a) => a -> [(a,a)] -> [(a,a)]
filterByDecicions x xs = map (fromTo x) . minArg (decicions xs) . poissibleEndPoints . filterByStartingpoint x $ xs

-- fromTo x y creates an edge that leads from x to y. Its basicly a type constructor
fromTo :: a -> a -> (a,a)
fromTo x y = (x,y)

chooseRandom :: [b] -> Rand b
chooseRandom = liftM head . shuffle 

minArg :: (Ord b) => (a -> b) -> [a] -> [a]
minArg f xs = [x | x <- xs, f x == minimum (map f xs)]

maxArg :: (Ord b) => (a -> b) -> [a] -> [a]
maxArg f xs = [x | x <- xs, f x == maximum (map f xs)]

hasStartpoint :: (Eq a) => a -> (a,a) -> Bool
hasStartpoint x (y,_) = x == y

hasEndpoint :: (Eq a) => a -> (a,a) -> Bool
hasEndpoint x (_,y) = x == y


-- how many unique endpoints are there
-- (not startpointspecific)
poissibleEndPoints :: (Eq a) =>[(a,a)] -> [a]
poissibleEndPoints = nub . map snd

-- if I start here, in how many unique directions can I go?
decicions :: (Eq a) => [(a,a)] -> a -> Int
decicions xs x = length . poissibleEndPoints . filterByStartingpoint x $ xs

-- given lists that are just permutations of each other, return the first one. Otherwise throw an error
-- should work without (Ord a), too

knots :: (Ord a) => [[a]] -> [a]
knots parents
	| justPermutations parents	= head parents
	| otherwise 				= error "Parents were too different"
	where
		-- abuse of edges
		justPermutations :: (Ord a) => [[a]] -> Bool
		justPermutations = all (uncurry (==)) . edges . map sort

-- edges of one parent in a list
edges :: [a] -> [(a,a)]
edges [] = []
edges [_] = []
edges (x:y:zs) = (x,y): edges (y:zs)









-- type CrossoverOp a = [Genome a] -> Rand ([Genome a], [Genome a])
-- |Somehow concat the genomes and remove multiples
-- Apply with probability @p@.
orderCrossover :: (Ord a) => Double -> CrossoverOp a
orderCrossover _ []  = return ([], [])
orderCrossover _ [celibate] = return ([],[celibate])
orderCrossover p (g1:g2:rest) = do
	splitpoint <- getRandomR (0, min (length g1) (length g2))
	(h1,h2) <- withProbability p (return . orderCrossover' splitpoint) (g1,g2)
	return ([h1,h2], rest)
	where
		orderCrossover' :: (Ord a) => Int -> (Genome a, Genome a) -> (Genome a, Genome a)
		orderCrossover' splitpoint (mother, father) = (doCrossover splitpoint (mother, father), doCrossover splitpoint (father, mother))
			where
				doCrossover :: (Ord a) => Int -> (Genome a, Genome a) -> Genome a
				doCrossover splitpoint (xs1,xs2) = (map fst . sortBy (comparing snd) . uncurry (++) . mapFst (map fstWithTrd) . mapSnd (map sndWithTrd) . splitAt splitpoint . sortBy (comparing fst') . zip3 xs1 xs2) [1..]
				
				fst' :: (a,b,c) -> a
				fst' (x,_,_) = x
				snd' :: (a,b,c) -> b
				snd' (_,y,_) = y
				trd' :: (a,b,c) -> c
				trd' (_,_,z) = z
		
				fstWithTrd :: (a,b,c) -> (a,c)
				fstWithTrd (x,_,z) = (x,z)
		
				sndWithTrd :: (a,b,c) -> (b,c)
				sndWithTrd (_,y,z) = (y,z)



swapmutate :: MutationOp a
swapmutate xs = 
	if length xs < 2 then error "Your list is too short to swap anything."
	else do
		[x1,x2,x3] <- randomSplitIn 3 xs
		if length x2 < 2 then swapmutate xs
		else
			return $ x1 ++ [last x2] ++ (init . tail) x2 ++ [head x2] ++ x3

listswapmutate :: MutationOp a
listswapmutate xs = do
	[x1,x2,x3,x4,x5] <- randomSplitIn 5 xs
	return $ x1 ++ x4 ++ x3 ++ x2 ++ x5

revmutate :: MutationOp a
revmutate xs = do
	[x1,x2,x3] <- randomSplitIn 3 xs
	return $ x1 ++ reverse x2 ++ x3

shuffelmutate :: MutationOp a
shuffelmutate xs = do 
	[x1,x2,x3] <- randomSplitIn 3 xs
	shuffledx2 <- shuffle x2
	return $ x1 ++ shuffledx2 ++ x3

shiftmutate :: MutationOp a
shiftmutate xs = do
	decide <- getBool
	let shift = if decide then rightShift else leftShift
	[x1,x2,x3] <- randomSplitIn 3 xs
	return $ x1 ++ shift x2 ++ x3

	where
		rightShift :: [a] -> [a]
		rightShift [] = []
		rightShift xs = [last xs] ++ init xs
		leftShift :: [a] -> [a]
		leftShift [] = []
		leftShift xs = tail xs ++ [head xs]

randomSplitIn :: Int -> [a] -> Rand [[a]]
randomSplitIn n xs = do
	indices <- randomIntListOfLengthAndSum id n (length xs)
	return $ splitHere indices xs

splitHere :: [Int] -> [a] -> [[a]]
splitHere [] [] = []
splitHere (0:ns) xs = []: splitHere ns xs 
splitHere _ [] 	= error "Your list was too short."
splitHere [] _	= error "Your list was too long."
splitHere (n:ns) xs
	| length xs < n	= error "Your indices are malformed"
	| otherwise 	= xs1 : splitHere ns xs2
	where
		(xs1, xs2) = splitAt n xs



randomIntListOfLengthAndSum :: (Double -> Double) -> Int -> Int -> Rand [Int]
randomIntListOfLengthAndSum distribution listlength listsum = 
	(return . scaleUp listsum . map distribution) =<< getListOfDoubles listlength
-- random List of Doubles between 0 and 1 (I hope getDouble is equally distributed)
getListOfDoubles :: Int -> Rand [Double]
getListOfDoubles 0 = return []
getListOfDoubles n = do
	d <- getDouble
	-- I am not shure about the borders of getDouble, so I make sure it is between 0 and 1
	let drem = remainer d
	ds <- getListOfDoubles (n-1)
	return (drem:ds)

-- assumed n >= 0
-- scaleUp n ds returns you a list ls of Ints, so that sum ls == n and the proportions are as konstant as possible
-- we use the method of the "greatest remainer" TODO: Source needed
scaleUp :: Int -> [Double] -> [Int]
scaleUp n ds = roundList n (scaleUpDoubles n ds)
	where		
		-- same thing as scaleUp, but with Doubles as result. This is much easier, since there is no rounding envolved
		scaleUpDoubles :: Int -> [Double] -> [Double]
		scaleUpDoubles n ds = map (\d -> (d* fromIntegral n)/ sum ds ) ds
		
		roundList :: Int -> [Double] -> [Int]
		roundList n ds = zipWith (+) fracseats fullseats
			where
				fullseats :: [Int]
				fullseats = map floor ds
		
				restseats :: Int
				restseats = n - sum fullseats
		
				fracseats :: [Int]
				fracseats = greatestRemainer restseats (map remainer ds)
		
		-- the greater your remaining percent the more
		greatestRemainer :: Int -> [Double] -> [Int]
		greatestRemainer n ds = (map fst . sortBy (comparing snd) . zip seatList . map snd . sortBy (comparing fst) . zip ds) [1..]
			where
				-- [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1]
				seatList :: [Int]
				seatList = replicate (length ds - n) 0 ++ replicate n 1
				
-- the higher the more likly to get a seat
remainer :: Double -> Double
remainer d = d - (fromIntegral.floor) d




	
main = do
	showRand (edgeCrossover [[1,2,3,4,5,6], [1,2,4,3,5,6], [1,2,6,3,5,4]])
	--print "It compiles."
	--quickCheckAll

quickCheckAll :: IO()
quickCheckAll = do
	quickCheck prop_IntRemainerIsZero
	quickCheck prop_OneParty
	quickCheck prop_NumberOfPartys
	quickCheck prop_ScaleUpRank
	-- quickCheck prop_SplitHere -- takes very long
	quickCheck prop_knots

prop_knots :: [Char] -> Int -> Property
prop_knots list n = 
			n > 0 ==>
			list == knots permutationList
			where
				permutationList = take ((+) 1 . abs . flip mod 100 $ n) . cycle . permutations $ list

prop_IntRemainerIsZero :: Int -> Bool
prop_IntRemainerIsZero = (0==) . remainer . fromIntegral

prop_OneParty :: Int -> Double -> Property
prop_OneParty n d =
			d /= 0 ==>
			scaleUp n' [d] == [n']
			where
				n' = n `mod` (10^15)

prop_NumberOfPartys :: Int -> [Double] -> Bool
prop_NumberOfPartys n ds = (length . scaleUp n) ds == length ds

prop_ScaleUpRank :: Int -> [Double] -> Bool
prop_ScaleUpRank n ds = scaledUpDoubles == sort scaledUpDoubles
	where
		scaledUpDoubles = (scaleUp (abs n) . sort . map abs) ds

prop_SplitHere :: [Int] -> Bool
prop_SplitHere ns = (and . zipWith (==) anylist . concat . splitHere ns' . take nsum) anylist
	where
		ns' :: [Int]
		ns' = map abs ns

		nsum :: Int
		nsum = sum ns'

		anylist :: [Int]
		anylist = [1..]