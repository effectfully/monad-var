module MonadVar.Combinators
  ( postMutate_
  , preMutate_
  , postMutate
  , preMutate
  , postMutateM_
  , preMutateM_
  , postMutateM
  , preMutateM
  ) where
  
import           MonadVar.Prelude
import           MonadVar.Classes

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

-- | Monadically mutate a variable and also return its old value.
postMutateM_
  :: (MonadMutateM f m v, Functor f) => v a -> (a -> f a) -> m a
postMutateM_ v f = mutateM v $ \x -> f x <&> \y -> (y, x)
{-# INLINE postMutateM_ #-}

-- | Monadically mutate a variable and also return its new value.
preMutateM_
  :: (MonadMutateM f m v, Functor f) => v a -> (a -> f a) -> m a
preMutateM_ v f = mutateM v $ \x -> f x <&> \y -> (y, y)
{-# INLINE preMutateM_ #-}

-- | Monadically mutate a variable and also return its old value
-- along with an additional value.
postMutateM
  :: (MonadMutateM f m v, Functor f) => v a -> (a -> f (a, b)) -> m (a, b)
postMutateM v f = mutateM v $ \x -> f x <&> \(y, z) -> (y, (x, z))
{-# INLINE postMutateM #-}

-- | Monadically mutate a variable and also return its new value
-- along with an additional value.
preMutateM
  :: (MonadMutateM f m v, Functor f) => v a -> (a -> f (a, b)) -> m (a, b)
preMutateM v f = mutateM v $ \x -> f x <&> \(y, z) -> (y, (y, z))
{-# INLINE preMutateM #-}
