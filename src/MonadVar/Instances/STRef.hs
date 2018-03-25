module MonadVar.Instances.STRef where

import           MonadVar.Classes
import           MonadVar.Default
import           Control.Monad.ST
import           Data.STRef

instance MonadNew   (ST s) (STRef s) where
  new = newSTRef
  {-# INLINE new #-}

instance MonadRead  (ST s) (STRef s) where
  read = readSTRef
  {-# INLINE read #-}

instance MonadWrite (ST s) (STRef s) where
  write = writeSTRef
  {-# INLINE write #-}

instance MonadSwap  (ST s) (STRef s) where
  swap = defaultReadWriteSwap
  {-# INLINE swap #-}

instance MonadMutate_ (ST s) (STRef s) where
  mutate_ = defaultReadWriteMutate_
  {-# INLINE mutate_ #-}

instance MonadMutate  (ST s) (STRef s) where
  mutate = defaultReadWriteMutate
  {-# INLINE mutate #-}

instance ST s ~ st_s => MonadMutateM_ st_s (ST s) (STRef s) where
  mutateM_ = defaultReadWriteMutateM_
  {-# INLINE mutateM_ #-}

instance ST s ~ st_s => MonadMutateM  st_s (ST s) (STRef s) where
  mutateM = defaultReadWriteMutateM
  {-# INLINE mutateM #-}
