{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module MLF.Util.RecursionSchemes (
    cataM,
    cataEither,
    cataMaybe,
    foldElabType,
    foldElabTerm,
    foldInstantiation,
    foldAnnExpr
) where

import Data.Functor.Foldable (Base, Recursive, cata)

import MLF.Elab.Types (ElabTerm, ElabTermF, ElabType, Instantiation, InstantiationF, TopVar(..))
import MLF.Types.Elab (TyIF, cataIx)
import MLF.Frontend.ConstraintGen.Types (AnnExpr, AnnExprF)

-- read https://hackage.haskell.org/package/recursion-schemes-5.2.3/docs/Data-Functor-Foldable.html

cataM
    :: (Recursive t, Traversable (Base t), Monad m)
    => (Base t a -> m a)
    -> t
    -> m a
cataM alg = cata (\layer -> traverse id layer >>= alg)

cataEither :: (Recursive t, Traversable (Base t)) => (Base t a -> Either e a) -> t -> Either e a
cataEither = cataM

cataMaybe :: (Recursive t, Traversable (Base t)) => (Base t a -> Maybe a) -> t -> Maybe a
cataMaybe = cataM

foldElabType :: (forall i. TyIF i a -> a i) -> ElabType -> a 'AllowVar
foldElabType = cataIx

foldElabTerm :: (ElabTermF a -> a) -> ElabTerm -> a
foldElabTerm = cata

foldInstantiation :: (InstantiationF a -> a) -> Instantiation -> a
foldInstantiation = cata

foldAnnExpr :: (AnnExprF a -> a) -> AnnExpr -> a
foldAnnExpr = cata
