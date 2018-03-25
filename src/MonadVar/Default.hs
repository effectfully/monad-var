module MonadVar.Default
  ( defaultLockUnsafeWrite
  , defaultReadWriteSwap
  , defaultLockUnsafeSwap
  , defaultReadWriteMutate_
  , defaultReadWriteMutate
  , defaultReadWriteMutateM_
  , defaultReadWriteMutateM
  , defaultLockUnsafeMutate_
  , defaultLockUnsafeMutate
  , defaultLockUnsafeMutateM_
  , defaultLockUnsafeMutateM
  , defaultLockIOMutateM_
  , defaultLockIOMutateM
  ) where

import           MonadVar.Prelude
import           MonadVar.Classes

-- | Default exception-unsafe 'write' for 'MonadLock' entities.
defaultLockUnsafeWrite
  :: MonadLock m v => v a -> a -> m ()
defaultLockUnsafeWrite v y = tryHold v *> fill v y
{-# INLINE defaultLockUnsafeWrite #-}

-- | Default 'swap' for 'MonadRead' and 'MonadWrite' entities.
defaultReadWriteSwap
  :: (MonadRead m v, MonadWrite m v) => v a -> a -> m a
defaultReadWriteSwap v y = read v <* write v y
{-# INLINE defaultReadWriteSwap #-}

-- | Default exception-unsafe 'swap' for 'MonadLock' entities.
defaultLockUnsafeSwap
  :: MonadLock m v => v a -> a -> m a
defaultLockUnsafeSwap v y = hold v <* fill v y
{-# INLINE defaultLockUnsafeSwap #-}

-- | Default 'mutate_' for 'MonadRead' and 'MonadWrite' entities.
defaultReadWriteMutate_
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> a) -> m ()
defaultReadWriteMutate_ v f = do
  x <- read v
  let !y = f x
  write v y
{-# INLINE defaultReadWriteMutate_ #-}

-- | Default 'mutate' for 'MonadRead' and 'MonadWrite' entities.
defaultReadWriteMutate
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> (a, b)) -> m b
defaultReadWriteMutate v f = do
  x <- read v
  let !(!y, z) = f x
  write v y
  return z
{-# INLINE defaultReadWriteMutate #-}

-- | Default 'mutateM_' for 'MonadRead' and 'MonadWrite' entities.
defaultReadWriteMutateM_
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> m a) -> m ()
defaultReadWriteMutateM_ v f = do
  x <- read v
  !y <- f x
  write v y
{-# INLINE defaultReadWriteMutateM_ #-}

-- | Default 'mutateM' for 'MonadRead' and 'MonadWrite' entities.
defaultReadWriteMutateM
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> m (a, b)) -> m b
defaultReadWriteMutateM v f = do
  x <- read v
  !(!y, z) <- f x
  write v y
  return z
{-# INLINE defaultReadWriteMutateM #-}

-- | Default exception-unsafe 'mutate_' for 'MonadLock' entities.
defaultLockUnsafeMutate_
  :: MonadLock m v => v a -> (a -> a) -> m ()
defaultLockUnsafeMutate_ v f = do
  x <- hold v
  let !y = f x
  fill v y
{-# INLINE defaultLockUnsafeMutate_ #-}

-- | Default exception-unsafe 'mutate' for 'MonadLock' entities.
defaultLockUnsafeMutate
  :: MonadLock m v => v a -> (a -> (a, b)) -> m b
defaultLockUnsafeMutate v f = do
  x <- hold v
  let !(!y, z) = f x
  fill v y
  return z
{-# INLINE defaultLockUnsafeMutate #-}

-- | Default exception-unsafe 'mutateM_' for 'MonadLock' entities.
defaultLockUnsafeMutateM_
  :: MonadLock m v => v a -> (a -> m a) -> m ()
defaultLockUnsafeMutateM_ v f = do
  x <- hold v
  !y <- f x
  fill v y
{-# INLINE defaultLockUnsafeMutateM_ #-}

-- | Default exception-unsafe 'mutateM' for 'MonadLock' entities.
defaultLockUnsafeMutateM
  :: MonadLock m v => v a -> (a -> m (a, b)) -> m b
defaultLockUnsafeMutateM v f = do
  x <- hold v
  !(!y, z) <- f x
  fill v y
  return z
{-# INLINE defaultLockUnsafeMutateM #-}

-- | Default 'mutateM_' for 'MonadLock' 'IO' entities
defaultLockIOMutateM_ :: MonadLock IO v => v a -> (a -> IO a) -> IO ()
defaultLockIOMutateM_ v f = mask $ \restore -> do
  x <- hold v
  y <- restore (f x) `onException` fill v x
  fill v y
  evaluate y
  return ()
{-# INLINE defaultLockIOMutateM_ #-}

-- | Default 'mutateM' for 'MonadLock' 'IO' entities.
defaultLockIOMutateM :: MonadLock IO v => v a -> (a -> IO (a, b)) -> IO b
defaultLockIOMutateM v f = mask $ \restore -> do
  x      <- hold v
  (y, z) <- restore (f x >>= evaluate) `onException` fill v x
  fill v y    -- See "Parallel and Concurrent Programming in Haskell",
  evaluate y  -- the "MVar as a Container for Shared State" section.
  return z
{-# INLINE defaultLockIOMutateM #-}
