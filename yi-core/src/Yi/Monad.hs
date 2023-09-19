module Yi.Monad (
                 assign,
                 gets,
                 getsAndModify,
                 maybeM,
                 repeatUntilM,
                 uses,
                 whenM,
                 with,
                ) where

import Control.Monad (when)
import Control.Monad.Base   (MonadBase, liftBase)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State  (MonadState, get, gets, put)
import Lens.Micro.Platform (Getting, ASetter, (.=), use)

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

assign :: MonadState s m => ASetter s s a b -> b -> m ()
assign = (.=)

uses :: MonadState s m => Getting a s a -> (a -> b) -> m b
uses l f = f <$> use l
