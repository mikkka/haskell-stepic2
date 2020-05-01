{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Runz where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable

limited :: (MonadState s m, MonadError e m, Num e, Enum e) => (s -> Bool) -> [State s a] -> m [a]
limited p fs = traverse limit1 (zip [0..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs = run1 (limited p fs)

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs = run2 (limited p fs)

type RUNES s = ExceptT Int (State s) 
type RUNSE s = StateT s (Except Int) 

run1 :: RUNES s [a] -> s -> (Either Int [a], s)
run1 x = runState $ runExceptT x

run2 :: RUNSE s [a] -> s -> Either Int ([a], s)
run2 x s = runExcept $ runStateT x s
