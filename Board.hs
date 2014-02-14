{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Board (
  -- * Data types
  GameState(..)
  , Board (..)
  , Location
  , Move

  -- * Accessors
  , size
  , row
  , column
  , diagonal
  , getBoard
  , setBoard
  , mapBoard
  , emptyBoard

  , allRows
  , allColumns
  , allDiagonals
  , allElements
  , allLocations
  , emptyLocations
  ) where

import Data.List
import Data.Monoid
import Game

-- | 2D square space
newtype Board a = Board { board :: [[Maybe a]] }
type Location = (Int, Int)
type Move a = (Location, a)

instance (Show a) => Show (Board a) where
  show = intercalate "\n".
         fmap (intercalate ", ". (fmap showItem)). board
    where showItem Nothing = "_"
          showItem (Just x) = show x

instance (Eq a) => GameState (Board a) (Move a) where
  isEndGame b = (any allEqual $ filter ((/= Nothing). head) lines) ||
                (noMoreMove b)
    where allEqual [] = True
          allEqual (x:xs) = all (== x) xs
          lines = allRows b ++ allColumns b ++ allDiagonals b
          noMoreMove = all (/= Nothing). allElements

  advance b (loc, a) = case getBoard loc b of
    Nothing -> setBoard loc b a
    (Just _) -> b

-- | Replace element at index
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs = let (ys, (_:zs)) = splitAt n xs
                   in ys ++ x:zs

-- |Board size
size :: Board a -> Int
size = length. board

-- | Get row
row :: Board a -> Int -> [Maybe a]
row (Board b) n = b !! n

-- | Get column
column :: Board a -> Int -> [Maybe a]
column (Board b) n = fmap (\r -> r !! n) b

-- | Get diagonal
diagonal :: Board a -> Bool -> [Maybe a]
diagonal b rev = fmap (flip getBoard b) indices
  where n = (size b) - 1
        indices = if rev then zip [0..n] (reverse [0..n])
                  else zip [0..n] [0..n]

allRows = board
allColumns b = (take (size b). fmap (fmap head). iterate (fmap tail)) (board b)
allDiagonals b = fmap (diagonal b) [False, True]
allElements = foldr (++) []. allRows
allLocations b = [(x, y) | let xs = [0..(size b) - 1], x <- xs, y <- xs]

-- | Unfilled location on board
emptyLocations :: (Eq a) => Board a -> [Location]
emptyLocations b = filter (\l -> getBoard l b == Nothing) $ allLocations b

-- | Get board location
getBoard :: Location -> Board a -> Maybe a
getBoard (m, n) (Board b) = b !! m !! n

-- | Set board location
setBoard :: Location -> Board a -> a -> Board a
setBoard (m, n) (Board b) elem = let
  row = replaceAt n (Just elem) (b !! m)
  in Board $ replaceAt m row b

-- | Convert Board
mapBoard :: (a -> b) -> Board a -> Board b
mapBoard f = Board. fmap (fmap (fmap f)). board

-- | Empty Board
emptyBoard :: Int -> Board a
emptyBoard n = Board. reshape n. take (n*n) $ repeat Nothing

-- | List to matrix
reshape :: Int -> [a] -> [[a]]
reshape n = take n. fmap (take n). iterate (drop n)
