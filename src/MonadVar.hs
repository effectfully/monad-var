{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, BangPatterns #-}
module MonadVar
  ( MonadNew(..)
  , MonadLock(..)
  , MonadRead(..)
  , MonadWrite(..)
  , MonadSwap(..)
  , MonadFoldMutateM(..)
  , MonadMutateM(..)
  , MonadMutateM_(..)
  , MonadFoldMutate(..)
  , MonadMutate(..)
  , MonadMutate_(..)
  , defaultLockUnsafeWrite
  , defaultReadWriteSwap
  , defaultLockUnsafeSwap
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

-- | A type class for containers that can be created and
-- initialized with a single value.
class Monad m => MonadNew m v where
  new :: a -> m (v a)

-- | A type class for at most one element containers.
-- An attempt to get a value from an empty container or
-- to put a value into a full container results in a block.
-- I.e. this is a type class for 'MVar'-like things.
class (MonadRead m v, Monad m) => MonadLock m v where
  -- | Get a value from a container. Block on empty.
  -- A la 'takeMVar'.
  hold     :: v a -> m a
  -- | Put a value to a container. Block on full.
  -- A la 'putMVar'.
  fill     :: v a -> a -> m ()
  -- | Get a value from a container. Return 'Nothing' on empty.
  -- A la 'tryTakeMVar'.
  tryHold  :: v a -> m (Maybe a)
  -- | Put a value to a container. Return 'Nothing' on full.
  -- A la 'tryPutMVar'.
  tryFill  :: v a -> a -> m Bool
  -- | Read a value from a container. Return 'Nothing' on empty.
  -- A la 'tryReadMVar'.
  tryRead  :: v a -> m (Maybe a)
  -- | Create an empty container.
  -- A la 'newEmptyMVar'.
  newEmpty :: m (v a)
  -- | Check whether a container is empty.
  -- A la 'isEmptyMVar'.
  isEmpty  :: v a -> m Bool

-- | A type class for containers from which a single value can be read.
class Monad m => MonadRead m v where
  read :: v a -> m a

-- | A type class for containers to which a single value can be written.
class Monad m => MonadWrite m v where
  write :: v a -> a -> m ()

-- | A type class for containers for which one value can be replaced
-- with an another (not necessarily at the same position).
class Monad m => MonadSwap m v where
  -- | Replace a value from a container by a new one and
  -- return the original value.
  swap :: v a -> a -> m a

-- | A type class for mutable containers which can be monadically
-- mapped and folded over simultaneously.
class MonadFoldMutateM m n v where
  foldMutateM :: Monoid b => v a -> (a -> m (a, b)) -> n b

-- | A type class for one-element containers which can be monadically
-- mapped and folded over simultaneously. These are basically variables.
class (MonadRead m v, MonadWrite m v) => MonadMutateM m n v where
  -- | Monadically mutate a variable and return an additional value.
  mutateM :: v a -> (a -> m (a, b)) -> n b

-- | A type class for mutable containers which can be monadically mapped over.
class MonadWrite m v => MonadMutateM_ m n v where
  mutateM_ :: v a -> (a -> m a) -> n ()

-- | A type class for mutable containers which can be
-- mapped and folded over simultaneously.
class MonadFoldMutate m v where
  foldMutate :: Monoid b => v a -> (a -> (a, b)) -> m b

-- | A type class for one-element containers which can be
-- mapped and folded over simultaneously. These are basically variables.
class (MonadRead m v, MonadWrite m v) => MonadMutate m v where
  -- | Mutate a variable and return an additional value.
  mutate :: v a -> (a -> (a, b)) -> m b

-- | A type class for mutable containers which can be mapped over.
class MonadWrite m v => MonadMutate_ m v where
  mutate_ :: v a -> (a -> a) -> m ()

-- It'd be nice to also have this and similar classes.
-- class MonadLockMutate_ m v where
--   lockMutate_ :: v a -> (Maybe a -> Maybe a) -> m ()

-- Default implementations.

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

-- | Default 'mutateM' for 'MonadRead' and 'MonadWrite' entities.
defaultReadWriteMutateM
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> m (a, b)) -> m b
defaultReadWriteMutateM v f = do
  x <- read v
  !(!y, z) <- f x
  write v y
  return z
{-# INLINE defaultReadWriteMutateM #-}

-- | Default 'mutateM_' for 'MonadRead' and 'MonadWrite' entities.
defaultReadWriteMutateM_
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> m a) -> m ()
defaultReadWriteMutateM_ v f = do
  x <- read v
  !y <- f x
  write v y
{-# INLINE defaultReadWriteMutateM_ #-}

-- | Default 'mutate' for 'MonadRead' and 'MonadWrite' entities.
defaultReadWriteMutate
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> (a, b)) -> m b
defaultReadWriteMutate v f = do
  x <- read v
  let !(!y, z) = f x
  write v y
  return z
{-# INLINE defaultReadWriteMutate #-}

-- | Default 'mutate_' for 'MonadRead' and 'MonadWrite' entities.
defaultReadWriteMutate_
  :: (MonadRead m v, MonadWrite m v) => v a -> (a -> a) -> m ()
defaultReadWriteMutate_ v f = do
  x <- read v
  let !y = f x
  write v y
{-# INLINE defaultReadWriteMutate_ #-}

-- | Default exception-unsafe 'mutateM' for 'MonadLock' entities.
defaultLockUnsafeMutateM
  :: MonadLock m v => v a -> (a -> m (a, b)) -> m b
defaultLockUnsafeMutateM v f = do
  x <- hold v
  !(!y, z) <- f x
  fill v y
  return z
{-# INLINE defaultLockUnsafeMutateM #-}

-- | Default exception-unsafe 'mutateM_' for 'MonadLock' entities.
defaultLockUnsafeMutateM_
  :: MonadLock m v => v a -> (a -> m a) -> m ()
defaultLockUnsafeMutateM_ v f = do
  x <- hold v
  !y <- f x
  fill v y
{-# INLINE defaultLockUnsafeMutateM_ #-}

-- | Default exception-unsafe 'mutate' for 'MonadLock' entities.
defaultLockUnsafeMutate
  :: MonadLock m v => v a -> (a -> (a, b)) -> m b
defaultLockUnsafeMutate v f = do
  x <- hold v
  let !(!y, z) = f x
  fill v y
  return z
{-# INLINE defaultLockUnsafeMutate #-}

-- | Default exception-unsafe 'mutate_' for 'MonadLock' entities.
defaultLockUnsafeMutate_
  :: MonadLock m v => v a -> (a -> a) -> m ()
defaultLockUnsafeMutate_ v f = do
  x <- hold v
  let !y = f x
  fill v y
{-# INLINE defaultLockUnsafeMutate_ #-}

-- | Default 'mutateM' for 'MonadLock' 'IO' entities.
defaultLockIOMutateM :: MonadLock IO v => v a -> (a -> IO (a, b)) -> IO b
defaultLockIOMutateM v f = mask $ \restore -> do
  x      <- hold v
  (y, z) <- restore (f x >>= evaluate) `onException` fill v x
  fill v y    -- See "Parallel and Concurrent Programming in Haskell",
  evaluate y  -- the "MVar as a Container for Shared State" section.
  return z
{-# INLINE defaultLockIOMutateM #-}

-- | Default 'mutateM_' for 'MonadLock' 'IO' entities
defaultLockIOMutateM_ :: MonadLock IO v => v a -> (a -> IO a) -> IO ()
defaultLockIOMutateM_ v f = mask $ \restore -> do
  x <- hold v
  y <- restore (f x) `onException` fill v x
  fill v y
  evaluate y
  return ()
{-# INLINE defaultLockIOMutateM_ #-}

-- Additional functions.

-- | Monadically mutate a variable and also return its old value
-- along with an additional value.
postMutateM
  :: MonadMutateM m n v => v a -> (a -> m (a, b)) -> n (a, b)
postMutateM v f = mutateM v $ \x -> f x <&> \(y, z) -> (y, (x, z))
{-# INLINE postMutateM #-}

-- | Monadically mutate a variable and also return its new value
-- along with an additional value.
preMutateM
  :: MonadMutateM m n v => v a -> (a -> m (a, b)) -> n (a, b)
preMutateM v f = mutateM v $ \x -> f x <&> \(y, z) -> (y, (y, z))
{-# INLINE preMutateM #-}

-- | Monadically mutate a variable and also return its old value.
postMutateM_
  :: MonadMutateM m n v => v a -> (a -> m a) -> n a
postMutateM_ v f = mutateM v $ \x -> f x <&> \y -> (y, x)
{-# INLINE postMutateM_ #-}

-- | Monadically mutate a variable and also return its new value.
preMutateM_
  :: MonadMutateM m n v => v a -> (a -> m a) -> n a
preMutateM_ v f = mutateM v $ \x -> f x <&> \y -> (y, y)
{-# INLINE preMutateM_ #-}

-- | Mutate a variable and also return its old value
-- along with an additional value.
postMutate
  :: MonadMutate m v => v a -> (a -> (a, b)) -> m (a, b)
postMutate v f = mutate v $ \x -> f x & \(y, z) -> (y, (x, z))
{-# INLINE postMutate #-}

-- | Mutate a variable and also return its new value
-- along with an additional value.
preMutate
  :: MonadMutate m v => v a -> (a -> (a, b)) -> m (a, b)
preMutate v f = mutate v $ \x -> f x & \(y, z) -> (y, (y, z))
{-# INLINE preMutate #-}

-- | Mutate a variable and also return its old value.
postMutate_
  :: MonadMutate m v => v a -> (a -> a) -> m a
postMutate_ v f = mutate v $ \x -> f x & \y -> (y, x)
{-# INLINE postMutate_ #-}

-- | Mutate a variable and also return its new value.
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
  write = mask_ .* defaultLockUnsafeWrite
  {-# INLINE write #-}

instance MonadSwap  IO MVar where
  swap = mask_ .* defaultLockUnsafeSwap
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
  write = defaultLockUnsafeWrite
  {-# INLINE write #-}

instance MonadSwap  STM TMVar where
  swap = defaultLockUnsafeSwap
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
