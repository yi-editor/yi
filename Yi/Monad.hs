module Yi.Monad (
                 Ref(..), 
                 gets, 
                 getsA, 
                 getsAndModify, 
                 maybeM,
                 modifiesRef,
                 modifiesThenReadsRef,
                 readsRef,
                 repeatUntilM,
                 whenM,
                 with,
                 writesRef
                ) where

import Data.Accessor
import Data.IORef
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative
import Control.Concurrent.MVar

-- | Combination of the Control.Monad.State 'modify' and 'gets'
getsAndModify :: MonadState s m => (s -> (s,a)) -> m a
getsAndModify f = do
  e <- get
  let (e',result) = f e
  put e'
  return result

class Ref ref where
    readRef :: (MonadIO m) => ref a -> m a
    writeRef :: (MonadIO m) => ref a -> a -> m ()
    modifyRef :: (MonadIO m) => ref a -> (a -> a) -> m ()
    

instance Ref IORef where
    readRef r = liftIO $ readIORef r
    writeRef r x = liftIO $ writeIORef r x
    modifyRef r f = liftIO $ modifyIORef r f

instance Ref MVar where
    readRef r = liftIO $ readMVar r
    writeRef r x = liftIO $ putMVar r x
    modifyRef r f = liftIO $ modifyMVar_ r (\x -> return (f x))

modifiesRef :: (Ref ref, MonadReader r m, MonadIO m) => (r -> ref a) -> (a -> a) -> m ()
modifiesRef f g = do
  b <- asks f
  modifyRef b g

readsRef :: (Ref ref, MonadReader r m, MonadIO m) => (r -> ref a) -> m a
readsRef f = do
  r <- asks f
  readRef r

writesRef :: (MonadReader r m, MonadIO m) => (r -> IORef a) -> a -> m ()
writesRef f x = do
  r <- asks f
  writeRef r x

modifiesThenReadsRef :: (MonadReader r m, MonadIO m) => (r -> IORef a) -> (a -> a) -> m a
modifiesThenReadsRef f g = do
  modifiesRef f g
  readsRef f

with :: (MonadReader yi m, MonadIO m) => (yi -> component) -> (component -> IO a) -> m a
with f g = do
    yi <- ask
    liftIO $ g (f yi)

whenM :: Monad m => m Bool -> m () -> m ()
whenM mtest ma = mtest >>= flip when ma  

maybeM :: Monad m => (x -> m ()) -> Maybe x -> m ()
maybeM _ Nothing = return ()
maybeM f (Just x) = f x 

-- | Rerun the monad until the boolean result is false, collecting list of results.
repeatUntilM :: Monad m => m (Bool,a) -> m [a]
repeatUntilM m = do
  (proceed,x) <- m
  case proceed of 
    False -> return [x]
    True -> do xs <- repeatUntilM m
               return (x:xs)

getsA :: MonadState s m => Accessor s p -> (p -> a) -> m a
getsA a f = gets (f . getVal a)
