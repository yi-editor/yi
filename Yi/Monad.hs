module Yi.Monad where

import Data.IORef
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans


-- | Combination of the Control.Monad.State 'modify' and 'gets'
getsAndModify :: MonadState s m => (s -> (s,a)) -> m a
getsAndModify f = do
  e <- get
  let (e',result) = f e
  put e'
  return result

readRef :: (MonadIO m) => IORef a -> m a
readRef r = liftIO $ readIORef r

writeRef :: (MonadIO m) => IORef a -> a -> m ()
writeRef r x = liftIO $ writeIORef r x

modifyRef :: (MonadIO m) => IORef a -> (a -> a) -> m ()
modifyRef r f = liftIO $ modifyIORef r f


modifiesRef :: (MonadReader r m, MonadIO m) => (r -> IORef a) -> (a -> a) -> m ()
modifiesRef f g = do
  b <- asks f
  modifyRef b g

readsRef :: (MonadReader r m, MonadIO m) => (r -> IORef a) -> m a
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

-- | Rerun the monad until the boolean result is false, collecting list of results.
repeatUntilM :: Monad m => m (Bool,a) -> m [a]
repeatUntilM m = do
  (proceed,x) <- m
  case proceed of 
    False -> return [x]
    True -> do xs <- repeatUntilM m
               return (x:xs)

