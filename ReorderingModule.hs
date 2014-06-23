module ReorderingModule (swapmutate, revmutate, shuffelmutate, shiftmutate, listswapmutate, orderCrossover, numMutation)
	where
import Moo.GeneticAlgorithm.Binary (CrossoverOp, Genome, MutationOp, getRandomR, Rand, shuffle)
import Moo.GeneticAlgorithm.Random (getDouble, getBool, withProbability, getNormal)
import Data.Ord (comparing)
import Data.List (sortBy, sort, group, nub)
import Test.QuickCheck
import Data.Graph.Inductive.Query.Monad (mapFst, mapSnd)
import Control.Monad (liftM)

edgeCrossover :: (Ord a, Eq a) => [[a]] -> Rand [a]
edgeCrossover parents =
	do
		-- the one that is used as a startpoint by most of the parents
		startpoint <- chooseRandom . maxArg length . group . sort . map head $ parents
		return []

analyzeParents :: (Eq a, Ord a) => [[a]] -> [((a,a), Int)]
analyzeParents = map (\list -> (head list, length list)) . group . sort . concatMap analyzeParent
	where
		analyzeParent :: [a] -> [(a,a)]
		analyzeParent [] = []
		analyzeParent [_] = []
		analyzeParent (x:y:zs) = (x,y) : analyzeParent (y:zs)

-- assumes that the Genomes are made from the same set of numbers
chooseEdge :: (Ord a) => [(a,a)] -> a -> Rand (a,a)
chooseEdge edges startpoint = chooseEdgeByNumberOfDecicons startpoint . map head . maxArg length . group . sort . filter (hasStartpoint startpoint) $  edges
-- chooses those of the edges whis leads to a point of small choise

chooseEdgeByNumberOfDecicons :: (Ord a) =>  a -> [(a,a)] -> Rand (a,a)
chooseEdgeByNumberOfDecicons  startpoint possibleedges =
		chooseRandom . map (\y -> (startpoint, y)) . minArg (length . getPossibleNext possibleedges) . getPossibleNext possibleedges $ startpoint
	where
		getPossibleNext :: (Eq a) => [(a,a)] -> a -> [a]
		getPossibleNext alledges startpoint = nub . map snd . filter (hasStartpoint startpoint) $ alledges

chooseRandom :: [b] -> Rand b
chooseRandom = liftM head . shuffle 

minArg :: (Ord b) => (a -> b) -> [a] -> [a]
minArg f xs = [x | x <- xs, f x == minimum (map f xs)]

maxArg :: (Ord b) => (a -> b) -> [a] -> [a]
maxArg f xs = [x | x <- xs, f x == maximum (map f xs)]

hasStartpoint :: (Eq a) => a -> (a,a) -> Bool
hasStartpoint x (y,_) = x == y


-- edges of all parents in a list
alledges :: [[a]] -> [(a,a)]
alledges = concatMap edges 
	where
		edges :: [a] -> [(a,a)]
		edges [] = []
		edges [_] = []
		edges (x:y:zs) = (x,y):(edges (y:zs))










-- Not tested
numMutation :: (Integral a) => MutationOp a 
numMutation xs = do
	mapM gausmutate xs
	where
		gausmutate :: (Integral a) => a -> Rand a
		gausmutate x = do
			difference <- getNormal
			return $ round (fromIntegral x + difference)

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




	
main = print "It compiles."--quickCheckAll

quickCheckAll :: IO()
quickCheckAll = do
	quickCheck prop_IntRemainerIsZero
	quickCheck prop_OneParty
	quickCheck prop_NumberOfPartys
	quickCheck prop_ScaleUpRank
	quickCheck prop_SplitHere

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