{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Control.Monad.Set where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Foldable as F

data SetM :: * -> * where
  MAny :: [a] -> SetM a
  MOrd :: Ord a => Set a -> SetM a

deriving instance Eq  a => Eq  (SetM a)
deriving instance Ord a => Ord (SetM a)

setMtoList :: SetM a -> [a]
setMtoList = \case
  MAny as -> as
  MOrd s  -> S.toList s

collect :: [SetM a] -> SetM a
collect = F.foldl' mplus mzero

instance Functor SetM where
  fmap = liftM

instance Applicative SetM where
  pure  = return
  (<*>) = ap

instance Monad SetM where
  return a = MAny [a]
  m >>= f = collect . fmap f $ setMtoList m

runSetM :: Ord a => SetM a -> Set a
runSetM = \case
  MAny as -> S.fromList as
  MOrd as -> as

instance Alternative SetM where
  empty = mzero
  (<|>) = mplus

instance MonadPlus SetM where
  mzero = MAny []
  mplus = \case
    MAny as -> \case
      MAny bs -> MAny $ as ++ bs
      MOrd bs -> MOrd $ S.union (S.fromList as) bs
    MOrd as -> \case
      MAny bs -> MOrd $ S.union as (S.fromList bs)
      MOrd bs -> MOrd $ S.union as bs

instance MonadLogic SetM where
  msplit = \case
    MAny as -> return $ case as of
      h:t -> Just (h,MAny t)
      _   -> Nothing
    MOrd as -> MOrd $ S.singleton $ if S.null as
      then Nothing
      else Just (h,MOrd t)
      where
      (h,t) = S.deleteFindMin as

chooseOrd :: (Foldable f, Ord a) => f a -> SetM a
chooseOrd = F.foldl' (\m a -> reflect $ Just (a,m)) (MOrd S.empty)

pyth :: Set (Int,Int,Int)
pyth = runSetM $ do
  [x,y,z] <- replicateM 3 $ chooseOrd [1..20]
  guard $ x*x + y*y == z*z
  return (x,y,z)

