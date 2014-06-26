mvmodule ShowRandModule (showRand) where

import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64

-- for getting a Rand a you might want to
-- import Control.Monad.Mersenne.Random 

-- test with: showRand getDouble

showRand :: (Show a) => Rand a -> IO a
showRand randX = do
	myPureMT <- newPureMT
	return (evalRandom randX myPureMT)
