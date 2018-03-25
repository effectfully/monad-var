module MonadVar.Instances.MVar where

import           MonadVar.Prelude
import           MonadVar.Classes
import           MonadVar.Default
import           Control.Concurrent.MVar

instance MonadNew   IO MVar where
  new = newMVar
  {-# INLINE new #-}

instance MonadLock  IO MVar where
  hold     = takeMVar
  {-# INLINE hold #-}

  fill     = putMVar
  {-# INLINE fill #-}

  tryHold  = tryTakeMVar
  {-# INLINE tryHold #-}

  tryFill  = tryPutMVar
  {-# INLINE tryFill #-}

  tryRead  = tryReadMVar
  {-# INLINE tryRead #-}

  newEmpty = newEmptyMVar
  {-# INLINE newEmpty #-}

  isEmpty  = isEmptyMVar
  {-# INLINE isEmpty #-}

instance MonadRead  IO MVar where
  read = readMVar
  {-# INLINE read #-}

instance MonadWrite IO MVar where
  write = mask_ .* defaultLockUnsafeWrite
  {-# INLINE write #-}

instance MonadSwap  IO MVar where
  swap = mask_ .* defaultLockUnsafeSwap
  {-# INLINE swap #-}

instance MonadMutate_ IO MVar where
  mutate_ v f = mutateM_ v $ return . f
  {-# INLINE mutate_ #-}

instance MonadMutate IO MVar where
  mutate v f = mutateM v $ return . f
  {-# INLINE mutate #-}

instance IO ~ io => MonadMutateM_ io IO MVar where
  mutateM_ = defaultLockIOMutateM_
  {-# INLINE mutateM_ #-}

instance IO ~ io => MonadMutateM io IO MVar where
  mutateM = defaultLockIOMutateM
  {-# INLINE mutateM #-}

