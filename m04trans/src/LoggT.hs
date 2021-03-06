module LoggT where
import Control.Applicative
import Data.Functor.Identity
import MonadTrans

data Logged a = Logged String a deriving (Eq,Show)
newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }


instance Functor m => Functor (LoggT m) where
  fmap f = LoggT . fmap updater . runLoggT
    where updater ~(Logged log v') = Logged log (f v')


instance Applicative m => Applicative (LoggT m) where
  pure x = LoggT $ pure $ Logged "" x
  f <*> v = LoggT $ liftA2 updater (runLoggT f) (runLoggT v)
    where updater ~(Logged log1 f') ~(Logged log2 v') = Logged (log1 ++ log2) (f' v')


instance Monad m => Monad (LoggT m) where
  v >>= f  = LoggT $ do 
    ~(Logged log1 v')  <- runLoggT v
    ~(Logged log2 v'') <- runLoggT $ f v'
    return $ Logged (log1 ++ log2) v''

  fail msg = LoggT $ fail msg

instance MonadTrans LoggT where
  lift m = LoggT $ Logged "" <$> m

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ pure (Logged s ())

type Logg = LoggT Identity 

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT
