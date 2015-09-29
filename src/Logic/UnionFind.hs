{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Logic.UnionFind where

import Prelude hiding (id,(.))
import Control.Category
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logic
import Control.Monad.Free
import Data.Functor.Identity
import Data.Function (on)

newtype FixM m f = FixM
  { unFixM :: m (f (FixM m f))
  }

cata :: (Traversable f, Monad m) => (f a -> m a) -> FixM m f -> m a
cata f (FixM m) = m >>= traverse (cata f) >>= f

cata2 :: (Traversable f, Monad m) => (f a -> f a -> m a) -> FixM m f -> FixM m f -> m a
cata2 f a = cata $ \y -> cata (\x -> f x y) a

class Traversable t => Unifiable t where
  unifyWith :: Alternative f => (a -> b -> f c) -> t a -> t b -> f (t c)

