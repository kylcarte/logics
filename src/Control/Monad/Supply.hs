{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Supply where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.RWS.Class
import Control.Monad.Error.Class
import Control.Monad.Cont.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Monoid

-- Stream {{{

data Stream a = a :< Stream a deriving (Eq,Ord,Show)
infixr 5 :<

instance Functor Stream where
  fmap f (a :< as) = f a :< fmap f as

push :: [a] -> Stream a -> Stream a
push = \case
  a:as -> (a:<) . push as
  _    -> id
{-# INLINE push #-}

pop :: Monoid m => (a -> m) -> Int -> Stream a -> (m,Stream a)
pop f = \case
  n | n <= 0 -> (,) mempty
  n          -> \(a :< as) -> first (mappend $ f a) $ pop f (n-1) as

pop_ :: Int -> Stream a -> Stream a
pop_ n = snd . pop (\_ -> ()) n

peek :: Monoid m => (a -> m) -> Int -> Stream a -> m
peek f n = fst . pop f n

-- }}}

-- Supply {{{

data Supply a = Supply
  { getStale :: [a]
  , getFresh :: Stream a
  }

instance Functor Supply where
  fmap f fr = Supply
    { getStale = f <$> getStale fr
    , getFresh = f <$> getFresh fr
    }

supplyNext :: Supply a -> (a,Supply a)
supplyNext (Supply ys (x :< xs)) = (x,Supply (x:ys) xs)

class FreshSupply x where
  freshSupply :: Supply x

instance FreshSupply Int where
  freshSupply = Supply [] $ push [0..] $ error "Empty Supply"

-- }}}

-- SupplyT {{{

newtype SupplyT x m a = SupplyT
  { unSupplyT :: StateT (Supply x) m a
  } deriving
  ( Functor , Applicative , Monad
  , Alternative , MonadPlus
  , MonadTrans
  , MonadReader r
  , MonadWriter w
  , MonadError  e
  , MonadCont
  , MonadIO
  )

runSupplyT :: Supply x -> SupplyT x m a -> m (a,Supply x)
runSupplyT s = (`runStateT` s) . unSupplyT

instance MonadRWS r w s m => MonadRWS r w s (SupplyT x m)

instance MonadLogic m => MonadLogic (SupplyT x m) where
  msplit = SupplyT . fmap (fmap $ second SupplyT) . msplit . unSupplyT

instance MonadState s m => MonadState s (SupplyT x m) where
  get = SupplyT $ lift get
  put = SupplyT . lift . put

instance Monad m => MonadReader x (SupplyT x m) where
  ask = SupplyT $ state supplyNext
  local f = SupplyT . mapStateT (fmap $ second $ fmap f) . unSupplyT

class Monad m => MonadSupply x m | m -> x where
  withSupply :: (Supply x -> (a,Supply x)) -> m a

instance Monad m => MonadSupply x (SupplyT x m) where
  withSupply = SupplyT . state

supply :: MonadSupply x m => m x
supply = withSupply supplyNext

-- }}}

