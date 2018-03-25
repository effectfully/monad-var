module MonadVar.Instances.TVar where

import           MonadVar.Prelude
import           MonadVar.Classes
import           MonadVar.Default
import           Control.Concurrent.STM

instance MonadNew   STM TVar where
  new = newTVar
  {-# INLINE new #-}

instance MonadRead  STM TVar where
  read = readTVar
  {-# INLINE read #-}

instance MonadWrite STM TVar where
  write = writeTVar
  {-# INLINE write #-}

instance MonadSwap  STM TVar where
  swap = defaultReadWriteSwap
  {-# INLINE swap #-}

instance MonadMutate_ STM TVar where
  mutate_ = defaultReadWriteMutate_
  {-# INLINE mutate_ #-}

instance MonadMutate  STM TVar where
  mutate = defaultReadWriteMutate
  {-# INLINE mutate #-}

instance STM ~ stm => MonadMutateM_ stm STM TVar where
  mutateM_ = defaultReadWriteMutateM_
  {-# INLINE mutateM_ #-}

instance STM ~ stm => MonadMutateM  stm STM TVar where
  mutateM = defaultReadWriteMutateM
  {-# INLINE mutateM #-}

instance MonadNew   IO TVar where
  new = newTVarIO
  {-# INLINE new #-}

instance MonadRead  IO TVar where
  read = readTVarIO
  {-# INLINE read #-}

instance MonadWrite IO TVar where
  write = atomically .* writeTVar
  {-# INLINE write #-}

instance MonadSwap  IO TVar where
  swap = atomically .* swap
  {-# INLINE swap #-}
