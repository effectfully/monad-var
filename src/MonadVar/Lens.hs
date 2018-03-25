{-# LANGUAGE RankNTypes #-}
module MonadVar.Lens
  ( (./)
  , effectful
  , (.!)
  , _VarM
  , _Var
  ) where

import           MonadVar.Prelude
import           MonadVar.Compat
import           MonadVar.Classes
import           Data.Functor.Identity
import           Data.Functor.Compose

infixl 8 ^.
infixr 8 ./
infixr 9 .!

-- We define our own lenses just to not depend on anything.
type LensLike f s t a b = (a -> f b) -> s -> f t
type Lens       s t a b = forall f. Functor f => LensLike f s t a b
type ASetter    s t a b = LensLike Identity s t a b

(^.) :: s -> LensLike (Const a) s t a b -> a
s ^. _L = getConst (_L Const s)
{-# INLINE (^.) #-}

_Of :: (s -> a) -> LensLike f s b a b
_Of f g = g . f
{-# INLINE _Of #-}

-- | Go down by the first lens into a data structure and
-- apply the second lens to the result.
-- This throws away the part of the structure skipped by the first lens, e.g.
--
-- @
-- (\'a\', (\'b\', \'c\')) & _2 ./ _2 %~ succ
-- @
--
-- results in
--
-- @
-- (\'b\',\'d\')
-- @
--
(./)
  :: LensLike (Const s) v x s y
  -> LensLike  f        s t a b
  -> LensLike  f        v t a b
_L ./ _M = _Of (^. _L) . _M
{-# INLINE (./) #-}

-- | Make a lens that runs with an effect out of a simple lens. E.g.
--
-- @
-- (\"a\", \"b\") & effectful _2 .~ getLine
-- @
--
-- asks for a string and replaces the second element of the tuple with it.
effectful :: Functor f => Lens s t a b -> Lens s (f t) a (f b)
effectful _L f = getCompose . _L (Compose . f)
{-# INLINE effectful #-}

-- | Compose a simple lens and a lens that runs with some effect.
(.!)
  :: (Functor f, Functor g)
  => Lens       v  w    s t
  -> LensLike g s (f t) a b
  -> LensLike g v (f w) a b
_L .! _M = effectful _L . _M
{-# INLINE (.!) #-}

-- | A monadic setter for a variable. E.g.
--
-- @
-- do
--   v <- newIORef \'a\'
--   v & _VarM %~ \\a -> succ a <$ putStr (show a)
--   readIORef v >>= print
-- @
--
-- prints
--
-- @
-- \'a\'\'b\'
-- @
--
_VarM :: forall m n v a. MonadMutateM_ m n v => ASetter (v a) (n ()) a (m a)
_VarM f v = Identity . mutateM_ v $ runIdentity . f
{-# INLINE _VarM #-}

-- | A setter for a variable. E.g.
--
-- @
-- do
--   v <- newIORef \'a\'
--   v & _Var %~ succ
--   readIORef v >>= print
-- @
--
-- prints
--
-- @
-- \'b\'
-- @
--
_Var :: forall m v a. MonadMutate_ m v => ASetter (v a) (m ()) a a
_Var f v = Identity . mutate_ v $ runIdentity . f
{-# INLINE _Var #-}
