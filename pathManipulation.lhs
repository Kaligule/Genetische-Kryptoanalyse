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
>   let tourLists = ((read input) :: [[RichTour]])
>   mainWith (makeTournament tourLists)

Mutation and Crossover get different funktions
A Mutation has 1 before and 1 after, A Crossover has many befores and 1 after

> makeTournament :: [[RichTour]] -> Diagram B R2
> makeTournament = (concatY) . map (concatX) . map (map tournament) 

Like ||| but better (for me) and for ists

> concatX :: [Diagram B R2] -> Diagram B R2
> concatX list =
>    translateX (
>      ( * (1 + circleRadius + 0.5))
>      . ((-) 1)
>      $ listLength
>      )

>    . foldl1 (|||)
>    . intersperse (strutX 1)
>    $ list
>   where
>     listLength :: Double
>     listLength = fromIntegral . length $ list

Like === but better (for me) and for Lists

> concatY :: [Diagram B R2] -> Diagram B R2
> concatY list =
>    foldl1 (===)
>    . intersperse
>      (
>        strutY 0.3
>        ===
>        hrule (listLength * 1.2 * (1 + circleRadius))
>        ===
>        strutY 0.5
>      )
>    $ list
>   where
>     listLength :: Double
>     listLength = fromIntegral . length $ list



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
>            
> circleRadius :: Double
> circleRadius = 0.17

I don't want arrowheads or any decoration - fix this when I have Internet

> arrowOpts = with -- & gaps  .~ small
>                  & headLength .~ Global 0
>
> tournament :: RichTour ->  Diagram B R2
> tournament (n, path, invisibles, specialNodes) =
>                   decorateTrail myPolygon (nodeList n specialNodes)
>                   -- # applyAll [connectOutside' arrowOpts j k | (j,k) <- pathpairs]
>                   # applyAll [connectOutside' arrowOpts j k | (j,k) <- pathpairs, not $ (j,k) `elem` invisibles]
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
