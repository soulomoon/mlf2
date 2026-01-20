{-# LANGUAGE LambdaCase #-}
module MLF.Frontend.Desugar (
    desugarCoercions
) where

import Data.Functor.Foldable (cata)

import MLF.Frontend.Syntax (Expr (..), ExprF (..))

{- Note [κσ coercions and desugaring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.1, “From λ-terms to typing constraints”) treats:

  - λ(x : σ) b  as syntactic sugar for  λ(x) let x = κσ x in b
  - (b : σ)     as syntactic sugar for  κσ b

We keep `(b : σ)` as an explicit constructor (`EAnn`) for now, and use it as the
representation of `κσ b`. Annotated lambda parameters remain explicit in the
AST so constraint generation can preserve rank-2 argument types for explicit
`forall` annotations.
-}

desugarCoercions :: Expr -> Expr
desugarCoercions = cata alg
  where
    alg = \case
        EVarF v -> EVar v
        EVarRawF v -> EVarRaw v
        ELitF l -> ELit l
        ELamF v body -> ELam v body
        ELamAnnF v ty body -> ELamAnn v ty body
        EAppF f a -> EApp f a
        ELetF v rhs body -> ELet v rhs body
        ELetAnnF v sch rhs body -> ELetAnn v sch rhs body
        EAnnF e ty -> EAnn e ty
