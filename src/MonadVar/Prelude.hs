module MonadVar.Prelude
  ( module Export
  , module MonadVar.Prelude
  ) where

import           Prelude                as Export hiding (read)
import           Data.Function          as Export
import           Control.Exception      as Export (mask, mask_, evaluate, onException)
import           Control.Monad.IO.Class as Export

infixr 9 .*
infixl 1 <&>

(<&>) :: Functor f => f a -> (a -> b) -> f b
x <&> f = f <$> x
{-# INLINE (<&>) #-}

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .* f = \x y -> g (f x y)
{-# INLINE (.*) #-}
