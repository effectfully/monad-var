module MonadVar.Instances.IORef where

import           MonadVar.Prelude
import           MonadVar.Classes
import           MonadVar.Default
import           Data.IORef

instance MonadIO m => MonadNew   m IORef where
  new = liftIO . newIORef
  {-# INLINE new #-}

instance MonadIO m => MonadRead  m IORef where
  read = liftIO . readIORef
  {-# INLINE read #-}

instance MonadIO m => MonadWrite m IORef where
  write = liftIO .* writeIORef
  {-# INLINE write #-}

instance MonadIO m => MonadSwap  m IORef where
  swap = liftIO .* defaultReadWriteSwap
  {-# INLINE swap #-}

instance MonadIO m => MonadMutate_ m IORef where
  mutate_ = liftIO .* defaultReadWriteMutate_
  {-# INLINE mutate_ #-}

instance MonadIO m => MonadMutate  m IORef where
  mutate = liftIO .* defaultReadWriteMutate
  {-# INLINE mutate #-}

instance IO ~ io => MonadMutateM_ io IO IORef where
  mutateM_ = defaultReadWriteMutateM_
  {-# INLINE mutateM_ #-}

instance IO ~ io => MonadMutateM  io IO IORef where
  mutateM = defaultReadWriteMutateM
  {-# INLINE mutateM #-}
