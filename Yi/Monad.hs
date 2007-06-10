module Yi.Monad where

import Data.IORef
import Control.Monad.Reader

-- | Repeat indefinitely the parameter.
repeatM_ :: forall m a. Monad m => m a -> m ()
repeatM_ a = a >> repeatM_ a
{-# SPECIALIZE repeatM_ :: IO a -> IO () #-}
{-# INLINE repeatM_ #-}


modifyRef :: (b -> IORef a) -> (a -> a) -> ReaderT b IO ()
modifyRef f g = do
  b <- ask
  lift $ modifyIORef (f b) g

readRef :: (b -> IORef a) -> ReaderT b IO a
readRef f = do
  r <- asks f
  lift $ readIORef r

writeRef f x = do
  r <- asks f
  liftIO $ writeIORef r x



with :: (yi -> a) -> (a -> IO b) -> ReaderT yi IO b
with f g = do
    yi <- ask
    lift $ g (f yi)
