module MonadVar.Instances.IORef where

import           MonadVar.Prelude
import           MonadVar.Classes
import           MonadVar.Default
import           Data.IORef

instance MonadNew   IO IORef where
  new = newIORef
  {-# INLINE new #-}

instance MonadRead  IO IORef where
  read = readIORef
  {-# INLINE read #-}

instance MonadWrite IO IORef where
  write = writeIORef
  {-# INLINE write #-}

instance MonadSwap  IO IORef where
  swap = defaultReadWriteSwap
  {-# INLINE swap #-}

instance MonadMutate_ IO IORef where
  mutate_ = defaultReadWriteMutate_
  {-# INLINE mutate_ #-}

instance MonadMutate  IO IORef where
  mutate = defaultReadWriteMutate
  {-# INLINE mutate #-}

instance IO ~ io => MonadMutateM_ io IO IORef where
  mutateM_ = defaultReadWriteMutateM_
  {-# INLINE mutateM_ #-}

instance IO ~ io => MonadMutateM  io IO IORef where
  mutateM = defaultReadWriteMutateM
  {-# INLINE mutateM #-}

