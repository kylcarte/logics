{-# LANGUAGE LambdaCase #-}

module Data.Unifiable where

import Control.Applicative
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Product
import Data.Functor.Sum

class Traversable t => Unifiable t where
  unifyWith :: Alternative f => (a -> b -> f c) -> t a -> t b -> f (t c)

instance Unifiable Identity where
  unifyWith f (Identity a) (Identity b) = Identity <$> f a b

instance (Unifiable f, Unifiable g) => Unifiable (Compose f g) where
  unifyWith f (Compose a) (Compose b) = Compose <$> unifyWith (unifyWith f) a b

instance Eq a => Unifiable (Constant a) where
  unifyWith _ (Constant a) (Constant b)
    | a == b = pure $ Constant a
    | True   = empty

instance (Unifiable f, Unifiable g) => Unifiable (Product f g) where
  unifyWith f (Pair a b) (Pair c d) = Pair <$> unifyWith f a c <*> unifyWith f b d

instance (Unifiable f, Unifiable g) => Unifiable (Sum f g) where
  unifyWith f = \case
    InL a -> \case
      InL c -> InL <$> unifyWith f a c
      InR _ -> empty
    InR b -> \case
      InL _ -> empty
      InR d -> InR <$> unifyWith f b d

