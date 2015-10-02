{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Logic.Herbrand where

import Prelude hiding (id,(.))
import Control.Category
import Control.Applicative
import Control.Monad.Free
import Control.Monad.Logic
import Control.Monad.Supply
import Control.Monad.State
import Data.Unifiable
import Data.Set (Set)
import qualified Data.Set as S

newtype Subst f a b = Subst
  { subst :: a -> Free f (Maybe b)
  }

instance Functor f => Category (Subst f) where
  id    = Subst $ \_ -> return Nothing
  g . f = Subst $ subst f >=> maybe (return Nothing) (subst g)

newtype Unify f x a = Unify
  { unUnify :: SupplyT x (StateT (Subst f x x) Logic) a
  } deriving
  ( Functor , Applicative , Monad
  , Alternative , MonadPlus
  , MonadLogic
  , MonadState (Subst f x x)
  , MonadSupply x
  )

type U f x = Unify f x (Free f x)
type U_ f  = U f Int

freeVars :: (Foldable f, Ord x) => Free f x -> Set x
freeVars = foldMap S.singleton

runUnify :: (Functor f, FreshSupply x) => U f x -> Logic (Free f x)
runUnify m = uncurry (flip walk) <$> runStateT (fst <$> runSupplyT freshSupply (unUnify m)) id

walkVar :: Functor f => Subst f x x -> x -> Free f x
walkVar s x = subst s x >>= \case
  Just y -> walkVar s y
  _      -> return x

walk :: Functor f => Subst f x x -> Free f x -> Free f x
walk s u = u >>= walkVar s

fresh :: Functor f => U f x
fresh = Pure <$> supply

freshN :: Functor f => Int -> ([Free f x] -> Unify f x a) -> Unify f x a
freshN n f = replicateM n fresh >>= f

occursCheck :: (Functor f, Foldable f, Eq x) => x -> Free f x -> Unify f x ()
occursCheck x = mapM_ $ guard . not . (==) x

extendSubst :: (Functor f, Eq x) => x -> Free f x -> U f x
extendSubst x u = state $ \s ->
  ( u
  , Subst $ \y -> if x == y
    then Just <$> u
    else subst s y
  )

unify :: (Unifiable f, Eq x) => Free f x -> Free f x -> U f x
unify u v = do
  case (u,v) of
    (Pure x,Pure y)
           | x == y -> return u
    (Pure x,_     ) -> occursCheck x v >> extendSubst x v
    (_     ,Pure y) -> occursCheck y u >> extendSubst y u
    (Free x,Free y) -> Free <$> unifyWith unify x y

(===) :: (Unifiable f, Eq x) => Free f x -> Free f x -> U f x
a === b = do
  s <- get
  let u = walk s a
      v = walk s b
  once $ unify u v

data Cons a
  = Nil
  | Int  Int
  | Cons a a
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Num (Cons a) where
  fromInteger = Int . fromInteger
  _ + _ = undefined
  _ * _ = undefined
  _ - _ = undefined
  abs _ = undefined
  signum _ = undefined

nil :: Free Cons a
nil = Free Nil

int :: Int -> Free Cons a
int = Free . Int

cons :: Free Cons a -> Free Cons a -> Free Cons a
cons a b = Free $ Cons a b

instance Num (f (Free f a)) => Num (Free f a) where
  fromInteger = Free . fromInteger
  _ + _ = undefined
  _ * _ = undefined
  _ - _ = undefined
  abs _ = undefined
  signum _ = undefined

instance Unifiable Cons where
  unifyWith f = \case
    Cons a b -> \case
      Cons c d -> Cons <$> f a c <*> f b d
      _        -> empty
    Nil      -> \case
      Nil      -> pure Nil
      _        -> empty
    Int x    -> \case
      Int y    -> Int x <$ guard (x == y)
      _        -> empty

test0 :: U_ Cons
test0 = freshN 4 $ \[q,x,y,z] -> do
  x === cons 1 y
  y === cons 2 z
  z === cons 3 nil
  q === x

