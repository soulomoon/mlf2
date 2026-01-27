{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Util.IndexedRecursion (
    IxFunctor(..),
    IxBase,
    IxRecursive(..),
    IxCorecursive(..),
    IxPair(..),
    IxFix(..),
    cataIx,
    cataIxConst,
    paraIx,
    zygoIx,
    K(..)
) where

import Data.Kind (Type)

-- | Indexed functor map.
class IxFunctor (f :: k -> (k -> Type) -> Type) where
    imap :: (forall i. a i -> b i) -> f j a -> f j b

type family IxBase (t :: k -> Type) :: k -> (k -> Type) -> Type

class IxRecursive (t :: k -> Type) where
    projectIx :: t i -> IxBase t i t

class IxCorecursive (t :: k -> Type) where
    embedIx :: IxBase t i t -> t i

-- | Indexed pair type.
newtype IxPair (f :: k -> Type) (g :: k -> Type) (i :: k) =
    IxPair { unIxPair :: (f i, g i) }

-- | Indexed fixpoint type.
newtype IxFix (f :: k -> (k -> Type) -> Type) (i :: k) =
    IxFix { unIxFix :: f i (IxFix f) }

type instance IxBase (IxFix f) = f

instance IxFunctor f => IxRecursive (IxFix f) where
    projectIx (IxFix t) = t

instance IxFunctor f => IxCorecursive (IxFix f) where
    embedIx = IxFix

-- | Indexed catamorphism over IxFix.
cataIx
    :: (IxRecursive t, IxFunctor (IxBase t))
    => forall (a :: k -> Type) i. (forall j. IxBase t j a -> a j)
    -> t i
    -> a i
cataIx alg t = alg (imap (cataIx alg) (projectIx t))

-- | Indexed catamorphism that returns a constant result across indices.
cataIxConst
    :: (IxRecursive t, IxFunctor (IxBase t))
    => forall (a :: Type) i. (forall j. IxBase t j (K a) -> a)
    -> t i
    -> a
cataIxConst alg = unK . cataIx (\node -> K (alg node))

-- | Indexed paramorphism.
paraIx
    :: (IxRecursive t, IxFunctor (IxBase t))
    => forall (a :: k -> Type) i. (forall j. IxBase t j (IxPair t a) -> a j)
    -> t i
    -> a i
paraIx alg t = alg (imap (\child -> IxPair (child, paraIx alg child)) (projectIx t))

-- | Indexed zygomorphism.
zygoIx
    :: (IxRecursive t, IxFunctor (IxBase t))
    => forall (b :: k -> Type) (a :: k -> Type) i.
        (forall j. IxBase t j b -> b j)
        -> (forall j. IxBase t j (IxPair b a) -> a j)
        -> t i
        -> a i
zygoIx algB algA =
    snd . unIxPair . cataIx (\node -> IxPair (algB (imap (fst . unIxPair) node), algA node))

-- | Constant indexed functor.
newtype K a (i :: k) = K { unK :: a }
