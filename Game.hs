{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Game (
  GameState(..)
  , BinaryGamePlay(..)
  , runTurn
  ) where

-- | Game state
class GameState s a | s -> a where
  isEndGame :: s -> Bool
  advance :: s -> a -> s

-- | Binary Play has 1-1 mapping to boolean
class BinaryGamePlay p where
  toBoolean :: p -> Bool
  fromBoolean :: Bool -> p
  opponent :: p -> p
  opponent = fromBoolean. not. toBoolean

instance BinaryGamePlay Bool where
  toBoolean = id
  fromBoolean = id

-- | Advance turn
runTurn :: (Monad m, GameState s a) => s -> m a -> m (Either s s)
runTurn s ma = do
  if isEndGame s
    then return (Right s)
    else do a <- ma
            return. Left $ advance s a
