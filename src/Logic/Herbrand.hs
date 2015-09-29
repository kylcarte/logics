{-# LANGUAGE RankNTypes #-}

module Logic.Herbrand where

import Prelude hiding (id,(.))
import Control.Category
import Control.Applicative
import Control.Monad.Free
import Control.Monad.Logic
import Control.Monad.Fresh
import Control.Monad.State

newtype Subst f a b = Subst
  { subst :: a -> Free f b
  }

instance Functor f => Category (Subst f) where
  id    = Subst return
  g . f = Subst $ subst f >=> subst g

newtype Unify f x a = Unify
  { unUnify :: FreshT x (StateT (Subst f x x) Logic) a
  }

