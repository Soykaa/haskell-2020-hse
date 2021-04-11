{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Functor Logged where
--fmap :: (a -> b) -> Logged a -> Logged b
  fmap f (Logged s a) = Logged s (f a)

instance Monad m => Functor (LoggT m) where
  --fmap :: (a -> b) -> LoggT m a -> LoggT m b
  fmap f x = LoggT $ (fmap (fmap f) (runLoggT x))

instance Applicative Logged where
--pure :: a -> t a
  pure a = Logged "" a
--(<*>) :: t (a -> b) -> t a -> t b
  (Logged g f) <*> (Logged s a) = Logged (g ++ s) (f a)

instance Monad m => Applicative (LoggT m) where
  --pure :: a -> LoggT m a
  pure x = LoggT $ pure (pure x)
  --(<*>) :: LoggT m (a -> b) -> LoggT m a -> LoggT m b
  LoggT mf <*> LoggT mx = LoggT $ do
    f <- mf
    x <- mx
    pure (f <*> x)

instance Monad Logged where
  --return :: a -> m a
  return x = pure x
  --(>>=) :: m a -> (a -> m b) -> m b
  m >>= k  = do
   a <- m
   k a

instance Monad m => Monad (LoggT m) where
  --return :: a -> LoggT m a
  return x = pure x
  --(>>=) :: LoggT m a -> (a -> LoggT m b) -> LoggT m b
  LoggT x >>= k = LoggT $ do
    (Logged s' x')   <- x
    (Logged s'' x'') <- runLoggT (k x')
    return (Logged (s' ++ s'') x'')
  fail msg = LoggT $ fail msg

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return (Logged s ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg m = runIdentity $ runLoggT m

instance MonadTrans LoggT where
--lift :: Monad m => m a -> LoggT m a
  lift m = LoggT $ fmap (Logged "") m

instance MonadState s m => MonadState s (LoggT m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance MonadReader r m => MonadReader r (LoggT m) where
  ask       = lift ask
  local f x = mapLoggT (local f) x
  reader    = lift . reader

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f x = LoggT $ f (runLoggT x)

class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
  w2log s = LoggT $ return (Logged s ())
  logg x  = LoggT $ return x

instance MonadLogg m => MonadLogg (StateT s m) where
  w2log = lift . w2log
  logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
  w2log = lift . w2log
  logg  = lift . logg

-- logSt'' :: LoggT (State Integer) Integer      
-- logSt'' = do 
--   x <- logg $ Logged "BEGIN " 1
--   modify (+x)
--   a <- get
--   w2log $ show $ a * 10
--   put 42
--   w2log " END"
--   return $ a * 100
