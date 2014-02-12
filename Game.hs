{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Game (
  GameState(..)
  , runTurn
  ) where

-- | Game state
class GameState s a | s -> a where
  isEndGame :: s -> Bool
  advance :: s -> a -> s

-- | Advance turn
runTurn :: (Monad m, GameState s a) => s -> m a -> m (Either s s)
runTurn s ma = do
  if isEndGame s
    then return (Right s)
    else do a <- ma
            return. Left $ advance s a
