{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TryRead2 where 

import Control.Monad.Except

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead ""    = throwError EmptyInput
tryRead str   = 
    case reads str of
      ((x, "") : _)  -> return x
      _              -> throwError $ NoParse str

