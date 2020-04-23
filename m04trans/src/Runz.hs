{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Runz where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s

limited :: (MonadState s m, MonadError Int m) => (s -> Bool) -> [State s a] -> m [a]
limited p fs = traverse limit1 (zip [0..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

run1 :: (MonadState s m, MonadError Int m) => m [a] -> s -> (Either Int [a], s)
run1 = undefined

-- run2 = undefined