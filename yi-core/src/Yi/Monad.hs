module Yi.Monad (
                 gets,
                 getsAndModify,
                 maybeM,
                 repeatUntilM,
                 whenM,
                 with,
                ) where

import Control.Monad.Base   (MonadBase, liftBase)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State  (MonadState, get, gets, put, when)

-- | Combination of the Control.Monad.State 'modify' and 'gets'
getsAndModify :: MonadState s m => (s -> (s,a)) -> m a
getsAndModify f = do
  e <- get
  let (e',result) = f e
  put e'
  return result

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
