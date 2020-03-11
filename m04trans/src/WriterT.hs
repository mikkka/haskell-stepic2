module WriterT where
import Control.Applicative
import Data.Tuple

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return 

execWriter :: Monad m => WriterT w m a -> m w
execWriter = fmap snd . runWriterT

instance Functor m => Functor (WriterT w m) where
  fmap f = WriterT . fmap updater . runWriterT
    where updater ~(x, log) = (f x, log)

instance (Applicative m, Monoid w) => Applicative (WriterT w m) where
  pure a = WriterT $ pure (a, mempty)
  f <*> v = WriterT $ liftA2 updater (runWriterT f) (runWriterT v)
    where updater ~(f', w1) ~(v',w2) = (f' v', w1 `mappend` w2)

instance (Monad m, Monoid w) => Monad (WriterT w m) where
  v >>= f = WriterT $ do
    ~(v', x1) <- runWriterT v
    ~(v'', x2) <- runWriterT (f v')
    return (v'', mappend x1 x2)
  fail = WriterT . fail
 