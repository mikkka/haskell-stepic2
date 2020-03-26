module AskPassword where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)
import Data.Semigroup
import Data.Monoid

newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

instance Show PwdError where
  show (PwdError x) = x

instance Semigroup PwdError where
  (PwdError x) <> (PwdError y) = PwdError $ x ++ y 

instance Monoid PwdError where
  mempty = PwdError ""

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  s <- liftIO getLine
  isValid0 s `catchE` (\(PwdError e) -> 
    (liftIO $ putStrLn ("Incorrect input: " ++ e ++ "!")) *> (throwE $ PwdError e))
  return s

isValid0 :: String -> PwdErrorIOMonad ()
isValid0 s
  | length s < 8 = throwE $ PwdError "password is too short"
  | not $ any isNumber s = throwE $ PwdError "password must contain some digits"
  | not $ any isPunctuation s = throwE $ PwdError "password must contain some punctuation"
  | otherwise = return ()
