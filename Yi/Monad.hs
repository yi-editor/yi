module Yi.Monad where


-- | Repeat indefinitely the parameter.
repeatM_ :: forall m a. Monad m => m a -> m ()
repeatM_ a = a >> repeatM_ a
{-# SPECIALIZE repeatM_ :: IO a -> IO () #-}
{-# INLINE repeatM_ #-}
