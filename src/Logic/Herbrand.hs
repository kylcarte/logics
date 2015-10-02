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

walking :: (Functor f, Foldable f, Monad m) => Subst f x x -> (x -> m ()) -> Free f x -> m ()
walking s f = mapM_ $ \x -> do
  f x
  let u = subst s x
  walking
    ( Subst $ maybe (return Nothing) $ fmap Just . subst s )
    ( maybe (return ()) f
    ) u

fresh :: Functor f => U f x
fresh = Pure <$> supply

freshN :: Functor f => Int -> ([Free f x] -> Unify f x a) -> Unify f x a
freshN n f = replicateM n fresh >>= f

occursCheck :: (Functor f, Foldable f, Eq x) => x -> Free f x -> Unify f x ()
occursCheck x u = do
  s <- get
  walking s (guard . not . (x ==)) u

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
    (Pure x,_     ) -> extendSubst x v
    (_     ,Pure y) -> extendSubst y u
    (Free x,Free y) -> Free <$> unifyWith unify x y

(===) :: (Unifiable f, Eq x) => Free f x -> Free f x -> U f x
a === b = do
  s <- get
  let u = walk s a
      v = walk s b
  once $ unify u v

data Cons a
  = Nil
  | Cons a a
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

nil :: Free Cons a
nil = Free Nil

cons :: Free Cons a -> Free Cons a -> Free Cons a
cons a b = Free $ Cons a b

instance Unifiable Cons where
  unifyWith f = \case
    Cons a b -> \case
      Cons c d -> Cons <$> f a c <*> f b d
      _        -> empty
    Nil      -> \case
      Nil      -> pure Nil
      _        -> empty

test0 :: U_ Cons
test0 = freshN 3 $ \[q,x,y] -> do
  q === cons x y
  x === nil
  y === cons x x
  return q

