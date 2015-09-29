{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Clause where

import Control.Monad.Free
import Control.Monad.Logic
import Control.Monad.State
import GHC.TypeLits
import Type.Family.Constraint
import Type.Family.List

class Clausal m where
  (.=.) :: (MonadLogic (m env), env1 :<: env, env2 :<: env)
    => E env1 a -> E env2 a -> m env a
infix 4 .=.

data Sym :: Symbol -> * where
  Sym :: KnownSymbol x => Sym x

deriving instance Show (Sym x)

type E = Exp Ø
type (:::) = '(,)
infixr 6 :::

data Exp :: [(Symbol,*)] -> [(Symbol,*)] -> * -> * where
  Var  :: Sym x -> Exp env (x ::: a :< env) a
  (:@) :: Exp env1 env2 (a -> b) -> Exp env2 env3 a -> Exp env1 env3 b
infixl 8 :@

class SubC sub sup => (sub :: [(Symbol,*)]) :<: (sup :: [(Symbol,*)]) where
  type SubC sub sup :: Constraint
  type SubC sub sup = ØC
infixr 5 :<:

class MapC ps a bs => Maps (ps :: [(Symbol,k)]) (a :: Symbol) (bs :: [k]) | ps a -> bs where
  type MapC ps a bs :: Constraint
  type MapC ps a bs = ØC

{-
instance Ø :<: env 
-}


