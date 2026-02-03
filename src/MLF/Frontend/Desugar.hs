{-# LANGUAGE LambdaCase #-}
module MLF.Frontend.Desugar (
    desugarCoercions
) where

import Data.Functor.Foldable (cata, embed)

import MLF.Frontend.Syntax (Expr (..), ExprF (..), mkCoerce)

{- Note [κσ coercions and desugaring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.1, “From λ-terms to typing constraints”) treats:

  - (b : σ)     as syntactic sugar for  κσ b
  - λ(x : σ) b  as syntactic sugar for  λ(x) let x = κσ x in b

The thesis also phrases annotated lambdas via surface `(:)` (Chapter 12.3.2):

  λ(x : τ) a  ≜  λ(x) let x = (x : τ) in a

We keep `(b : σ)` as an explicit surface constructor (`EAnn`) and desugar it to
an internal coercion term (`ECoerce`). Annotated lambda parameters remain
surface-only sugar and are desugared to the core form above so Phase 1 sees
only `ELam`/`ELet` plus coercions.
-}

desugarCoercions :: Expr -> Expr
desugarCoercions = cata alg
  where
    alg exprF = case exprF of
        EVarF v -> EVar v
        ELitF l -> ELit l
        ELamF v body -> ELam v body
        -- Thesis sugar: λ(x : τ) a  ≜  λ(x) let x = (x : τ) in a,
        -- and (x : τ) desugars to a coercion.
        ELamAnnF v ty body ->
            ELam v (ELet v (mkCoerce ty (EVar v)) body)
        EAppF f a -> EApp f a
        ELetF v rhs body -> ELet v rhs body
        EAnnF e ty -> mkCoerce ty e
        _ -> embed exprF
