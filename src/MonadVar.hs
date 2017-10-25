{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, BangPatterns #-}
module MonadVar
  ( MonadNew(..)
  , MonadLock(..)
  , MonadRead(..)
  , MonadWrite(..)
  , MonadSwap(..)
  , MonadMutateM(..)
  , MonadMutateM_(..)
  , MonadMutate(..)
  , MonadMutate_(..)
  , defaultLockWriteMasked
  , defaultReadWriteSwap
  , defaultLockMaskedSwap
  , defaultReadWriteMutateM
  , defaultReadWriteMutateM_
  , defaultReadWriteMutate
  , defaultReadWriteMutate_
  , defaultLockUnsafeMutateM
  , defaultLockUnsafeMutateM_
  , defaultLockUnsafeMutate
  , defaultLockUnsafeMutate_
  , defaultLockIOMutateM
  , defaultLockIOMutateM_
  , postMutateM
  , preMutateM
  , postMutateM_
  , preMutateM_
  , postMutate
  , preMutate
  , postMutate_
  , preMutate_
  ) where

import           Prelude hiding (read)
import           Data.Function
import           Data.STRef
import           Data.IORef
import           Control.Exception (mask, mask_, evaluate, onException)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad.ST
import           Control.Monad.Trans.Class

infixr 9 .*
infixl 1 <&>

(<&>) :: Functor f => f a -> (a -> b) -> f b
x <&> f = f <$> x
{-# INLINE (<&>) #-}

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .* f = \x y -> g (f x y)
{-# INLINE (.*) #-}

class Monad m => MonadNew m v where
  new :: a -> m (v a)

class (MonadRead m v, Monad m) => MonadLock m v where
  hold     :: v a -> m a
  fill     :: v a -> a -> m ()
  tryHold  :: v a -> m (Maybe a)
  tryFill  :: v a -> a -> m Bool
  tryRead  :: v a -> m (Maybe a)
  newEmpty :: m (v a)
  isEmpty  :: v a -> m Bool

class Monad m => MonadRead m v where
  read :: v a -> m a

class Monad m => MonadWrite m v where
  write :: v a -> a -> m ()

class Monad m => MonadSwap m v where
  swap :: v a -> a -> m a

class (MonadRead m v, MonadWrite m v) => MonadMutateM m n v where
  mutateM :: v a -> (a -> m (a, b)) -> n b

class MonadWrite m v => MonadMutateM_ m n v where
  mutateM_ :: v a -> (a -> m a) -> n ()

class (MonadRead m v, MonadWrite m v) => MonadMutate m v where
  mutate :: v a -> (a -> (a, b)) -> m b

class MonadWrite m v => MonadMutate_ m v where
  mutate_ :: v a -> (a -> a) -> m ()

-- What about this and similar classes?
-- class MonadLockMutate_ m v where
--   lockMutate_ :: v a -> (Maybe a -> Maybe a) -> m ()

-- Default implementations.

defaultLockWriteMasked
  :: MonadLock m v => v a -> a -> m ()
defaultLockWriteMasked v y = tryHold v *> fill v y
{-# INLINE defaultLockWriteMasked #-}

defaultReadWriteSwap
  :: (MonadRead m v, MonadWrite m v) => v a -> a -> m a
defaultReadWriteSwap v y = read v <* write v y
{-# INLINE defaultReadWriteSwap #-}

defaultLockMaskedSwap
  :: MonadLock m v => v a -> a -> m a
defaultLockMaskedSwap v y = hold v <* fill v y
{-# INLINE defaultLockMaskedSwap #-}

defaultReadWriteMutateM
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> m (a, b)) -> m b
defaultReadWriteMutateM v f = do
  x <- read v
  !(!y, z) <- f x
  write v y
  return z
{-# INLINE defaultReadWriteMutateM #-}

defaultReadWriteMutateM_
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> m a) -> m ()
defaultReadWriteMutateM_ v f = do
  x <- read v
  !y <- f x
  write v y
{-# INLINE defaultReadWriteMutateM_ #-}

defaultReadWriteMutate
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> (a, b)) -> m b
defaultReadWriteMutate v f = do
  x <- read v
  let !(!y, z) = f x
  write v y
  return z
{-# INLINE defaultReadWriteMutate #-}

defaultReadWriteMutate_
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> a) -> m ()
defaultReadWriteMutate_ v f = do
  x <- read v
  let !y = f x
  write v y
{-# INLINE defaultReadWriteMutate_ #-}

defaultLockUnsafeMutateM
  :: MonadLock m v => v a -> (a -> m (a, b)) -> m b
defaultLockUnsafeMutateM v f = do
  x <- hold v
  !(!y, z) <- f x
  fill v y
  return z
{-# INLINE defaultLockUnsafeMutateM #-}

defaultLockUnsafeMutateM_
  :: MonadLock m v => v a -> (a -> m a) -> m ()
defaultLockUnsafeMutateM_ v f = do
  x <- hold v
  !y <- f x
  fill v y
{-# INLINE defaultLockUnsafeMutateM_ #-}

defaultLockUnsafeMutate
  :: MonadLock m v => v a -> (a -> (a, b)) -> m b
defaultLockUnsafeMutate v f = do
  x <- hold v
  let !(!y, z) = f x
  fill v y
  return z
{-# INLINE defaultLockUnsafeMutate #-}

defaultLockUnsafeMutate_
  :: MonadLock m v => v a -> (a -> a) -> m ()
defaultLockUnsafeMutate_ v f = do
  x <- hold v
  let !y = f x
  fill v y
{-# INLINE defaultLockUnsafeMutate_ #-}

-- See "Parallel and Concurrent Programming in Haskell",
-- section "MVar as a Container for Shared State".

defaultLockIOMutateM :: MonadLock IO v => v a -> (a -> IO (a, b)) -> IO b
defaultLockIOMutateM v f = mask $ \restore -> do
  x      <- hold v
  (y, z) <- restore (f x >>= evaluate) `onException` fill v x
  fill v y
  evaluate y
  return z
{-# INLINE defaultLockIOMutateM #-}

defaultLockIOMutateM_ :: MonadLock IO v => v a -> (a -> IO a) -> IO ()
defaultLockIOMutateM_ v f = mask $ \restore -> do
  x <- hold v
  y <- restore (f x) `onException` fill v x
  fill v y
  evaluate y
  return ()
{-# INLINE defaultLockIOMutateM_ #-}

-- Additional functions.

postMutateM
  :: MonadMutateM m n v => v a -> (a -> m (a, b)) -> n (a, b)
postMutateM v f = mutateM v $ \x -> f x <&> \(y, z) -> (y, (x, z))
{-# INLINE postMutateM #-}

preMutateM
  :: MonadMutateM m n v => v a -> (a -> m (a, b)) -> n (a, b)
preMutateM v f = mutateM v $ \x -> f x <&> \(y, z) -> (y, (y, z))
{-# INLINE preMutateM #-}

postMutateM_
  :: MonadMutateM m n v => v a -> (a -> m a) -> n a
postMutateM_ v f = mutateM v $ \x -> f x <&> \y -> (y, x)
{-# INLINE postMutateM_ #-}

preMutateM_
  :: MonadMutateM m n v => v a -> (a -> m a) -> n a
preMutateM_ v f = mutateM v $ \x -> f x <&> \y -> (y, y)
{-# INLINE preMutateM_ #-}

postMutate
  :: MonadMutate m v => v a -> (a -> (a, b)) -> m (a, b)
postMutate v f = mutate v $ \x -> f x & \(y, z) -> (y, (x, z))
{-# INLINE postMutate #-}

preMutate
  :: MonadMutate m v => v a -> (a -> (a, b)) -> m (a, b)
preMutate v f = mutate v $ \x -> f x & \(y, z) -> (y, (y, z))
{-# INLINE preMutate #-}

postMutate_
  :: MonadMutate m v => v a -> (a -> a) -> m a
postMutate_ v f = mutate v $ \x -> f x & \y -> (y, x)
{-# INLINE postMutate_ #-}

preMutate_
  :: MonadMutate m v => v a -> (a -> a) -> m a
preMutate_ v f = mutate v $ \x -> f x & \y -> (y, y)
{-# INLINE preMutate_ #-}

-- `MonadTrans` instances.

instance (MonadTrans t, Monad (t m), MonadRead  m v) => MonadRead  (t m) v where
  read = lift . read
  {-# INLINE read #-}

instance (MonadTrans t, Monad (t m), MonadWrite m v) => MonadWrite (t m) v where
  write = lift .* write
  {-# INLINE write #-}

instance (MonadTrans t, Monad (t m), MonadSwap  m v) => MonadSwap  (t m) v where
  swap = lift .* swap
  {-# INLINE swap #-}

instance (MonadTrans t, Monad (t m), MonadLock  m v) => MonadLock  (t m) v where
  hold     = lift . hold
  {-# INLINE hold #-}

  fill     = lift .* fill
  {-# INLINE fill #-}

  tryHold  = lift . tryHold
  {-# INLINE tryHold #-}

  tryFill  = lift .* tryFill
  {-# INLINE tryFill #-}

  tryRead  = lift . tryRead
  {-# INLINE tryRead #-}

  newEmpty = lift newEmpty
  {-# INLINE newEmpty #-}

  isEmpty  = lift . isEmpty
  {-# INLINE isEmpty #-}

instance (MonadTrans t, Monad (t m), MonadMutate  m v) => MonadMutate  (t m) v where
  mutate = lift .* mutate
  {-# INLINE mutate #-}

instance (MonadTrans t, Monad (t m), MonadMutate_ m v) => MonadMutate_ (t m) v where
  mutate_ = lift .* mutate_
  {-# INLINE mutate_ #-}

-- `IORef` instances.

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

instance IO ~ io => MonadMutateM  io IO IORef where
  mutateM = defaultReadWriteMutateM
  {-# INLINE mutateM #-}

instance IO ~ io => MonadMutateM_ io IO IORef where
  mutateM_ = defaultReadWriteMutateM_
  {-# INLINE mutateM_ #-}

instance MonadMutate  IO IORef where
  mutate = defaultReadWriteMutate
  {-# INLINE mutate #-}

instance MonadMutate_ IO IORef where
  mutate_ = defaultReadWriteMutate_
  {-# INLINE mutate_ #-}

-- `STRef s` instances.

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

instance ST s ~ st_s => MonadMutateM  st_s (ST s) (STRef s) where
  mutateM = defaultReadWriteMutateM
  {-# INLINE mutateM #-}

instance ST s ~ st_s => MonadMutateM_ st_s (ST s) (STRef s) where
  mutateM_ = defaultReadWriteMutateM_
  {-# INLINE mutateM_ #-}

instance MonadMutate  (ST s) (STRef s) where
  mutate = defaultReadWriteMutate
  {-# INLINE mutate #-}

instance MonadMutate_ (ST s) (STRef s) where
  mutate_ = defaultReadWriteMutate_
  {-# INLINE mutate_ #-}

-- `MVar` instances.

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
  write = mask_ .* defaultLockWriteMasked
  {-# INLINE write #-}

instance MonadSwap  IO MVar where
  swap = mask_ .* defaultLockMaskedSwap
  {-# INLINE swap #-}

instance IO ~ io => MonadMutateM io IO MVar where
  mutateM = defaultLockIOMutateM
  {-# INLINE mutateM #-}

instance IO ~ io => MonadMutateM_ io IO MVar where
  mutateM_ = defaultLockIOMutateM_
  {-# INLINE mutateM_ #-}

instance MonadMutate IO MVar where
  mutate v f = mutateM v $ return . f
  {-# INLINE mutate #-}

instance MonadMutate_ IO MVar where
  mutate_ v f = mutateM_ v $ return . f
  {-# INLINE mutate_ #-}

-- `TVar` instances.

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

instance STM ~ stm => MonadMutateM  stm STM TVar where
  mutateM = defaultReadWriteMutateM
  {-# INLINE mutateM #-}

instance STM ~ stm => MonadMutateM_ stm STM TVar where
  mutateM_ = defaultReadWriteMutateM_
  {-# INLINE mutateM_ #-}

instance MonadMutate  STM TVar where
  mutate = defaultReadWriteMutate
  {-# INLINE mutate #-}

instance MonadMutate_ STM TVar where
  mutate_ = defaultReadWriteMutate_
  {-# INLINE mutate_ #-}

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

-- Notice the absense of `MonadMutateM* IO IO TVar` instances.

instance STM ~ stm => MonadMutateM  stm IO TVar where
  mutateM = atomically .* mutateM
  {-# INLINE mutateM #-}

instance STM ~ stm => MonadMutateM_ stm IO TVar where
  mutateM_ = atomically .* mutateM_
  {-# INLINE mutateM_ #-}

instance MonadMutate  IO TVar where
  mutate = atomically .* mutate
  {-# INLINE mutate #-}

instance MonadMutate_ IO TVar where
  mutate_ = atomically .* mutate_
  {-# INLINE mutate_ #-}

-- `TMVar` instances.

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
  write = defaultLockWriteMasked
  {-# INLINE write #-}

instance MonadSwap  STM TMVar where
  swap = defaultLockMaskedSwap
  {-# INLINE swap #-}

instance STM ~ stm => MonadMutateM  stm STM TMVar where
  mutateM = defaultLockUnsafeMutateM
  {-# INLINE mutateM #-}

instance STM ~ stm => MonadMutateM_ stm STM TMVar where
  mutateM_ = defaultLockUnsafeMutateM_
  {-# INLINE mutateM_ #-}

instance MonadMutate  STM TMVar where
  mutate = defaultLockUnsafeMutate
  {-# INLINE mutate #-}

instance MonadMutate_ STM TMVar where
  mutate_ = defaultLockUnsafeMutate_
  {-# INLINE mutate_ #-}

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

-- Notice the presense of `MonadMutateM* IO TMVar` instances.
-- These are the direct counterparts of the corresponding functions
-- defined over `MVar`s.

instance MonadMutateM  STM IO TMVar where
  mutateM = atomically .* mutateM
  {-# INLINE mutateM #-}

instance MonadMutateM_ STM IO TMVar where
  mutateM_ = atomically .* mutateM_
  {-# INLINE mutateM_ #-}

instance MonadMutateM  IO  IO TMVar where
  mutateM = defaultLockIOMutateM
  {-# INLINE mutateM #-}

instance MonadMutateM_ IO  IO TMVar where
  mutateM_ = defaultLockIOMutateM_
  {-# INLINE mutateM_ #-}

instance MonadMutate  IO TMVar where
  mutate = atomically .* mutate
  {-# INLINE mutate #-}

instance MonadMutate_ IO TMVar where
  mutate_ = atomically .* mutate_
  {-# INLINE mutate_ #-}

