module AI (
  -- * Play strageties
  randomMove
  , minMaxMove
  ) where

import System.Random
import Board
import Game

-- | Random choice from a list
choice :: [a] -> IO a
choice xs = fmap (xs !!) (getStdRandom $ randomR (0, (length xs) - 1))

-- | Random move strategy
randomMove :: (Eq b) => a -> Board b -> IO (Move a)
randomMove x b = fmap (\loc -> (loc, x)) (choice $ emptyLocations b)

-- MinMax move strategy
type Play = Bool

score :: Double -> Play -> Board Play -> Double
score r p b
  | isEndGame b && p = (-1.0)
  | isEndGame b && not p = 1.0
  | otherwise = fold goalFn $ fmap ((*r). score r (opponent p)) ns
  where moves = zip (emptyLocations b) (repeat p)
        ns = fmap (advance b) moves
        fold f (x:xs) = foldl f x xs
        goalFn = if p then max else min

maxMove :: Board Play -> Location
maxMove b = (fst. maxBy snd) moves
  where maxBy :: (Ord b) => (a -> b) -> [a] -> a
        maxBy f (x:xs) = foldl (\a b -> if (f a) < (f b) then b else a) x xs
        moves = zip locs scores
        locs = emptyLocations b
        scores = fmap (score 0.5 False. advance b) (zip locs (repeat True))

-- | Min/Max strategy
minMaxMove :: (BinaryGamePlay a) => a -> Board a -> Move a
minMaxMove x = flip (,) x. maxMove. mapBoard toBoolean
