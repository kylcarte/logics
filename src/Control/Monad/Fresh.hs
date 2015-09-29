{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Fresh where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Reader.Class
import Data.Monoid

-- Stream {{{

data Stream a = a :< Stream a deriving (Eq,Ord,Show)
infixr 5 :<

instance Functor Stream where
  fmap f (a :< as) = f a :< fmap f as

push :: Foldable f => f a -> Stream a -> Stream a
push = foldl (\f a -> f . (a:<)) id

pop :: Monoid m => (a -> m) -> Int -> Stream a -> (m,Stream a)
pop f = \case
  n | n <= 0 -> (,) mempty
  n          -> \(a :< as) -> first (mappend $ f a) $ pop f (n-1) as

pop_ :: Int -> Stream a -> Stream a
pop_ n = snd . pop (\_ -> ()) n

peek :: Monoid m => (a -> m) -> Int -> Stream a -> m
peek f n = fst . pop f n

-- }}}

-- Fresh{,T} {{{

data Fresh a = Fresh
  { getStale :: [a]
  , getFresh :: Stream a
  }

instance Functor Fresh where
  fmap f fr = Fresh
    { getStale = f <$> getStale fr
    , getFresh = f <$> getFresh fr
    }

nextFresh :: Fresh a -> (a,Fresh a)
nextFresh (Fresh ys (x :< xs)) = (x,Fresh (x:ys) xs)

newtype FreshT x m a = FreshT
  { unFreshT :: StateT (Fresh x) m a
  }

instance Functor m => Functor (FreshT x m) where
  fmap f = FreshT . fmap f . unFreshT

instance Monad m => Applicative (FreshT x m) where
  pure  = return
  (<*>) = ap

instance MonadPlus m => Alternative (FreshT x m) where
  empty     = FreshT mzero
  ma <|> mb = FreshT $ mplus (unFreshT ma) (unFreshT mb)

instance Monad m => Monad (FreshT x m) where
  return  = FreshT . return
  m >>= f = FreshT $ unFreshT m >>= unFreshT . f

instance MonadPlus m => MonadPlus (FreshT x m) where
  mzero       = FreshT mzero
  mplus ma mb = FreshT $ mplus (unFreshT ma) (unFreshT mb)

instance MonadLogic m => MonadLogic (FreshT x m) where
  msplit = FreshT . fmap (fmap $ second FreshT) . msplit . unFreshT

instance Monad m => MonadReader x (FreshT x m) where
  ask = FreshT $ state nextFresh
  local f = FreshT . mapStateT (fmap $ second $ fmap f) . unFreshT

-- }}}

