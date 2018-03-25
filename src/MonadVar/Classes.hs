module MonadVar.Classes
  ( MonadNew(..)
  , MonadLock(..)
  , MonadRead(..)
  , MonadWrite(..)
  , MonadSwap(..)
  , MonadMutate_(..)
  , MonadMutate(..)
  , MonadFoldMutate(..)
  , MonadMutateM_(..)
  , MonadMutateM(..)
  , MonadFoldMutateM(..)
  ) where

import           MonadVar.Prelude

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

-- | A type class for mutable containers which can be mapped over.
class MonadWrite m v => MonadMutate_ m v where
  mutate_ :: v a -> (a -> a) -> m ()

-- | A type class for one-element containers which can be
-- mapped and folded over simultaneously. These are basically variables.
class (MonadRead m v, MonadMutate_ m v) => MonadMutate m v where
  -- | Mutate a variable and return an additional value.
  mutate :: v a -> (a -> (a, b)) -> m b

-- | A type class for mutable containers which can be
-- mapped and folded over simultaneously.
class MonadFoldMutate m v where
  foldMutate :: Monoid b => v a -> (a -> (a, b)) -> m b

-- | A type class for mutable containers which can be monadically mapped over.
class MonadMutate_ m v => MonadMutateM_ f m v where
  mutateM_ :: v a -> (a -> f a) -> m ()

-- | A type class for one-element containers which can be monadically
-- mapped and folded over simultaneously. These are basically variables.
class (MonadMutate m v, MonadMutateM_ f m v) => MonadMutateM f m v where
  -- | Monadically mutate a variable and return an additional value.
  mutateM :: v a -> (a -> f (a, b)) -> m b

-- | A type class for mutable containers which can be monadically
-- mapped and folded over simultaneously.
class MonadFoldMutateM m n v where
  foldMutateM :: Monoid b => v a -> (a -> m (a, b)) -> n b

-- It'd be nice to also have this and similar classes.
-- class MonadLockMutate_ m v where
--   lockMutate_ :: v a -> (Maybe a -> Maybe a) -> m ()
