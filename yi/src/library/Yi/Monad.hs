{-# LANGUAGE FlexibleContexts #-}
module Yi.Monad (
                 Ref(..),
                 gets,
                 -- uses,
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

import Data.IORef
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Base
import Control.Concurrent.MVar

-- | Combination of the Control.Monad.State 'modify' and 'gets'
getsAndModify :: MonadState s m => (s -> (s,a)) -> m a
getsAndModify f = do
  e <- get
  let (e',result) = f e
  put e'
  return result

class Ref ref where
    readRef :: (MonadBase IO m) => ref a -> m a
    writeRef :: (MonadBase IO m) => ref a -> a -> m ()
    modifyRef :: (MonadBase IO m) => ref a -> (a -> a) -> m ()


instance Ref IORef where
    readRef r = liftBase $ readIORef r
    writeRef r x = liftBase $ writeIORef r x
    modifyRef r f = liftBase $ modifyIORef r f

instance Ref MVar where
    readRef r = liftBase $ readMVar r
    writeRef r x = liftBase $ putMVar r x
    modifyRef r f = liftBase $ modifyMVar_ r (return . f)

-- TODO: this store ref in MonadReader seems like an anti-pattern

modifiesRef :: (Ref ref, MonadReader r m, MonadBase IO m) => (r -> ref a) -> (a -> a) -> m ()
modifiesRef f g = do
  b <- asks f
  modifyRef b g

readsRef :: (Ref ref, MonadReader r m, MonadBase IO m) => (r -> ref a) -> m a
readsRef f = do
  r <- asks f
  readRef r

writesRef :: (MonadReader r m, MonadBase IO m) => (r -> IORef a) -> a -> m ()
writesRef f x = do
  r <- asks f
  writeRef r x

modifiesThenReadsRef :: (MonadReader r m, MonadBase IO m) => (r -> IORef a) -> (a -> a) -> m a
modifiesThenReadsRef f g = do
  modifiesRef f g
  readsRef f

with :: (MonadReader r m, MonadBase b m) => (r -> a) -> (a -> b c) -> m c
with f g = do
    yi <- ask
    liftBase $ g (f yi)

whenM :: Monad m => m Bool -> m () -> m ()
whenM mtest ma = mtest >>= flip when ma

maybeM :: Monad m => (x -> m ()) -> Maybe x -> m ()
maybeM _ Nothing = return ()
maybeM f (Just x) = f x

-- | Rerun the monad until the boolean result is false, collecting list of results.
repeatUntilM :: Monad m => m (Bool,a) -> m [a]
repeatUntilM m = do
  (proceed,x) <- m
  if proceed 
    then (do xs <- repeatUntilM m 
             return (x:xs))
    else return [x]

-- uses :: MonadState s m => Accessor s p -> (p -> a) -> m a
-- uses a f = gets (f . getVal a)
