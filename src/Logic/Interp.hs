{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Logic.Interp where

import Control.Applicative
import Control.Monad.Free
import Control.Monad.Logic
import Control.Monad.Fix
import Data.Function (on)

newtype Term f = Term
  { walk :: forall a. Eq a => (a -> Either a (f a)) -> Logic (Free f a)
  }

fresh :: Functor f => Term f
fresh = Term $ return . mfix . unfold

{-
unify :: Unifiable f => Term f -> Term f -> Term f
unify u v = Term $ \s -> do
  u' <- walk u s
  v' <- walk v s
  case (u',v') of
    (Pure x,_     ) -> undefined
    (_     ,Pure y) -> undefined
    (Free x,Free y) -> _
-}

class Traversable t => Unifiable t where
  unifyWith :: (a -> b -> c) -> t a -> t b -> Maybe (t c)

