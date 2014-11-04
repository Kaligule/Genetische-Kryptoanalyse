This is going to be an illustration for how paths can be combined or be changed.

compile with
runhaskell pathManipulation.lhs -o pathManipulation.svg -h 500

look at it with
firefox pathManipulation.svg

> {-# LANGUAGE NoMonomorphismRestriction #-}
>

Imports for Diagrams

> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine

Other imports

> import Data.List

> type Tour = [Int]
> type RichTour = (Int, Tour, [(Int, Int)], [Int])

Main funktion: Try to read a List of paths and  paint the Diagram of the Mutation or the crossover

> main = do
>   input <- getLine
>   let (maxNode, parents, pregnant, children, specialNodes, gaps) = (read input) :: (Int, [Tour], [Tour], [Tour], [Int], [(Int,Int)])
>   mainWith (makeTournament (maxNode, parents, pregnant, children, specialNodes, gaps))

Mutation and Crossover get different funktions
A Mutation has 1 before and 1 after, A Crossover has many befores and 1 after

> makeTournament :: (Int, [Tour], [Tour], [Tour], [Int], [(Int, Int)]) -> Diagram B R2
> makeTournament (maxNode, [before], [between], [after], specialNodes, []) = mutation maxNode before between after specialNodes
> makeTournament (maxNode, parents, [], [child], specialNodes, []) = crossover maxNode parents child specialNodes
> makeTournament (maxNode, [constant], [], [], specialNodes, []) = tournament maxNode constant specialNodes


> mutation :: Int -> Tour -> Tour -> Tour -> [Int] ->  Diagram B R2
> mutation maxNode before between after specialNodes = tournament maxNode before specialNodes
>                         ||| strutX 1
>                         ||| tournament maxNode between specialNodes
>                         ||| strutX 1
>                         ||| tournament maxNode after specialNodes

A Crossover has many (most of the time 2) parents and 1 child

> crossover :: Int -> [Tour] -> Tour -> [Int] -> Diagram B R2
> crossover maxNode parents child specialNodes =
>    (=== child_Diagram)
>    . (=== strutY 0.3)
>    . (=== hrule (numberParents * 1.2 * (1 + circleRadius))) 
>    . (=== strutY 0.5)
>    -- . showOrigin

>    . translateX (
>      ( * (1 + circleRadius + 0.5))
>      . fromIntegral
>      . ((-) 1)
>      . length
>      $ parents)

>    . foldl1 (|||)
>    . intersperse (strutX 1)
>    . map (flip (tournament maxNode) specialNodes)
>    $ parents
>   where
>     numberParents :: Double
>     numberParents = fromIntegral . length $ parents
>     
>     child_Diagram = tournament maxNode child specialNodes

A circle wirh a number in it. The name is the number. The radius is the circleRadius

> node :: Bool ->  Int -> Diagram B R2
> node special n = text (show n)
>            # fontSize (Local 0.17)
>            # fontSizeN 0.05
>            # fc black
>          <> circle circleRadius
>            # fc white
>            # lc (if special then red else black)
>            # named n
> circleRadius :: Double
> circleRadius = 0.17

I don't want arrowheads or any decoration - fix this when I have Internet

> arrowOpts = with -- & gaps  .~ small
>                  & headLength .~ Global 0
>
> tournament :: Int -> Tour -> [Int] ->  Diagram B R2
> tournament n path specialNodes = decorateTrail myPolygon (nodeList n specialNodes)
>                   -- # applyAll [connectOutside' arrowOpts j k | (j,k) <- pathpairs]
>                   # applyAll [connectOutside' arrowOpts j k | (j,k) <- pathpairs]
>                   -- # translate (r2 (0.5, -0.5))
>                   # translate (fromDirection ((1/(fromIntegral n)) @@ rad))
>                   -- # showOrigin
>   where
>     pathpairs :: [(Int, Int)]
>     pathpairs = zip path (tail path)
> 
>     nodeList :: Int -> [Int] -> [Diagram B R2]
>     nodeList maxNode specialNodes = zipWith node (map (`elem` specialNodes) [1..maxNode]) [1..maxNode]
> 
>     --Seitenlaenge 1
>     --myPolygon = regPoly n 1
>     -- radius 1
>     myPolygon = polygon $ PolygonOpts (PolyRegular n 1) OrientH origin
>     
