{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Functor.Foldable (
    Base,
    Recursive(..),
    Corecursive(..),
    cata,
    para,
    ana,
    hylo,
    ListF(..)
) where

import Data.Kind (Type)

type family Base t :: Type -> Type

class Recursive t where
    project :: t -> Base t t

class Corecursive t where
    embed :: Base t t -> t

cata :: (Recursive t, Functor (Base t)) => (Base t a -> a) -> t -> a
cata alg = go
  where
    go = alg . fmap go . project

para :: (Recursive t, Functor (Base t)) => (Base t (t, a) -> a) -> t -> a
para alg = go
  where
    go t = alg (fmap (\child -> (child, go child)) (project t))

ana :: (Corecursive t, Functor (Base t)) => (a -> Base t a) -> a -> t
ana coalg = go
  where
    go = embed . fmap go . coalg

hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = go
  where
    go = alg . fmap go . coalg

data ListF a b
    = Nil
    | Cons a b
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type instance Base [a] = ListF a

instance Recursive [a] where
    project [] = Nil
    project (x : xs) = Cons x xs

instance Corecursive [a] where
    embed Nil = []
    embed (x `Cons` xs) = x : xs
