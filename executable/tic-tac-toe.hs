module Main where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Board
import Game
import AI

data Play = X | O deriving (Show, Eq)

opponentPlay :: Play -> Play
opponentPlay X = O
opponentPlay O = X

getMove :: Board Play -> StateT Play IO (Move Play)
getMove b = do
  p <- get
  case p of
    X -> lift $ fmap (toLoc p. fmap read. words) getLine
    O -> lift $ randomMove p b
  where toLoc p (x:y:_) = ((x, y), p)

run :: Board Play -> StateT Play IO Play
run b = do
  rlt <- runTurn b (getMove b)
  case rlt of
    (Right x) -> get
    (Left x) -> do
      lift. putStrLn $ show x
      lift. putStrLn $ ">>>"
      p <- get
      put $ opponentPlay p
      run x

main = do
  loser <- execStateT (run (emptyBoard 3)) X
  putStrLn "Game finished!!"
  putStrLn $ "Winner is " ++ show (opponentPlay loser)
