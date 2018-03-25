module MonadVar.Instances.MVar where

import           MonadVar.Prelude
import           MonadVar.Classes
import           MonadVar.Default
import           Control.Concurrent.MVar

instance MonadIO m => MonadNew   m MVar where
  new = liftIO . newMVar
  {-# INLINE new #-}

instance MonadIO m => MonadLock  m MVar where
  hold     = liftIO . takeMVar
  {-# INLINE hold #-}

  fill     = liftIO .* putMVar
  {-# INLINE fill #-}

  tryHold  = liftIO . tryTakeMVar
  {-# INLINE tryHold #-}

  tryFill  = liftIO .* tryPutMVar
  {-# INLINE tryFill #-}

  tryRead  = liftIO . tryReadMVar
  {-# INLINE tryRead #-}

  newEmpty = liftIO newEmptyMVar
  {-# INLINE newEmpty #-}

  isEmpty  = liftIO . isEmptyMVar
  {-# INLINE isEmpty #-}

instance MonadIO m => MonadRead  m MVar where
  read = liftIO . readMVar
  {-# INLINE read #-}

instance MonadIO m => MonadWrite m MVar where
  write = liftIO .* mask_ .* defaultLockUnsafeWrite
  {-# INLINE write #-}

instance MonadIO m => MonadSwap  m MVar where
  swap = liftIO .* mask_ .* defaultLockUnsafeSwap
  {-# INLINE swap #-}

instance MonadIO m => MonadMutate_ m MVar where
  mutate_ v f = liftIO . mutateM_ v $ return . f
  {-# INLINE mutate_ #-}

instance MonadIO m => MonadMutate  m MVar where
  mutate v f = liftIO . mutateM v $ return . f
  {-# INLINE mutate #-}

instance IO ~ io => MonadMutateM_ io IO MVar where
  mutateM_ = defaultLockIOMutateM_
  {-# INLINE mutateM_ #-}

instance IO ~ io => MonadMutateM  io IO MVar where
  mutateM = defaultLockIOMutateM
  {-# INLINE mutateM #-}
