module AI (
  -- * Play strageties
  randomMove
  ) where

import System.Random
import Board

choice :: [a] -> IO a
choice xs = fmap (xs !!) (getStdRandom $ randomR (0, (length xs) - 1))

randomMove :: (Eq b) => a -> Board b -> IO (Move a)
randomMove x b = fmap (\loc -> (loc, x)) (choice emptyLocations)
  where emptyLocations = filter (\l -> getBoard l b == Nothing) $ allLocations b
