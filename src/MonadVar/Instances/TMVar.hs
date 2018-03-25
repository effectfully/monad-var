module MonadVar.Instances.TMVar where

import           MonadVar.Prelude
import           MonadVar.Classes
import           MonadVar.Default
import           Control.Concurrent.STM

instance MonadNew   STM TMVar where
  new = newTMVar
  {-# INLINE new #-}

instance MonadLock  STM TMVar where
  hold     = takeTMVar
  {-# INLINE hold #-}

  fill     = putTMVar
  {-# INLINE fill #-}

  tryHold  = tryTakeTMVar
  {-# INLINE tryHold #-}

  tryFill  = tryPutTMVar
  {-# INLINE tryFill #-}

  tryRead  = tryReadTMVar
  {-# INLINE tryRead #-}

  newEmpty = newEmptyTMVar
  {-# INLINE newEmpty #-}

  isEmpty  = isEmptyTMVar
  {-# INLINE isEmpty #-}

instance MonadRead  STM TMVar where
  read = readTMVar
  {-# INLINE read #-}

instance MonadWrite STM TMVar where
  write = defaultLockUnsafeWrite
  {-# INLINE write #-}

instance MonadSwap  STM TMVar where
  swap = defaultLockUnsafeSwap
  {-# INLINE swap #-}

instance MonadMutate_ STM TMVar where
  mutate_ = defaultLockUnsafeMutate_
  {-# INLINE mutate_ #-}

instance MonadMutate  STM TMVar where
  mutate = defaultLockUnsafeMutate
  {-# INLINE mutate #-}

instance STM ~ stm => MonadMutateM_ stm STM TMVar where
  mutateM_ = defaultLockUnsafeMutateM_
  {-# INLINE mutateM_ #-}

instance STM ~ stm => MonadMutateM  stm STM TMVar where
  mutateM = defaultLockUnsafeMutateM
  {-# INLINE mutateM #-}

instance MonadNew   IO TMVar where
  new = newTMVarIO
  {-# INLINE new #-}

instance MonadLock  IO TMVar where
  hold     = atomically . hold
  {-# INLINE hold #-}

  fill     = atomically .* fill
  {-# INLINE fill #-}

  tryHold  = atomically . tryHold
  {-# INLINE tryHold #-}

  tryFill  = atomically .* tryFill
  {-# INLINE tryFill #-}

  tryRead  = atomically . tryRead
  {-# INLINE tryRead #-}

  newEmpty = newEmptyTMVarIO
  {-# INLINE newEmpty #-}

  isEmpty  = atomically . isEmpty
  {-# INLINE isEmpty #-}

instance MonadRead  IO TMVar where
  read = atomically . read
  {-# INLINE read #-}

instance MonadWrite IO TMVar where
  write = atomically .* write
  {-# INLINE write #-}

instance MonadSwap  IO TMVar where
  swap = atomically .* swap
  {-# INLINE swap #-}

instance MonadMutate_ IO TMVar where
  mutate_ = atomically .* mutate_
  {-# INLINE mutate_ #-}

instance MonadMutate  IO TMVar where
  mutate = atomically .* mutate
  {-# INLINE mutate #-}

instance MonadMutateM_ STM IO TMVar where
  mutateM_ = atomically .* mutateM_
  {-# INLINE mutateM_ #-}

instance MonadMutateM  STM IO TMVar where
  mutateM = atomically .* mutateM
  {-# INLINE mutateM #-}

instance MonadMutateM_ IO  IO TMVar where
  mutateM_ = defaultLockIOMutateM_
  {-# INLINE mutateM_ #-}

instance MonadMutateM  IO  IO TMVar where
  mutateM = defaultLockIOMutateM
  {-# INLINE mutateM #-}
