{-# LANGUAGE CPP #-}
module MonadVar.Compat (module Export) where

#if MIN_VERSION_base(4,9,0)
import Data.Functor.Const        as Export
#else
import Data.Functor.Const.Compat as Export
#endif
