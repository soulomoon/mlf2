{-# LANGUAGE LambdaCase #-}
module MLF.Desugar (
    desugarCoercions
) where

import MLF.Syntax (Expr (..))

{- Note [κσ coercions and desugaring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`papers/xmlf.txt` (§3.1, “From λ-terms to typing constraints”) treats:

  - λ(x : σ) b  as syntactic sugar for  λ(x) let x = κσ x in b
  - (b : σ)     as syntactic sugar for  κσ b

This module implements the first rewrite explicitly for our surface AST.
We keep `(b : σ)` as an explicit constructor (`EAnn`) for now, and use it as the
representation of `κσ b`.

The key effect is that annotated lambda parameters become let-bound inside the
body, matching the paper’s presentation and keeping the core constraint
translation simpler (only unannotated `ELam`, plus `ELet` and `EAnn`).
-}

desugarCoercions :: Expr -> Expr
desugarCoercions = go
  where
    go :: Expr -> Expr
    go = \case
        EVar v -> EVar v
        EVarRaw v -> EVarRaw v
        ELit l -> ELit l
        ELam v body -> ELam v (go body)
        ELamAnn v ty body ->
            ELam v (ELet v (EAnn (EVarRaw v) ty) (go body))
        EApp f a -> EApp (go f) (go a)
        ELet v rhs body -> ELet v (go rhs) (go body)
        ELetAnn v sch rhs body -> ELetAnn v sch (go rhs) (go body)
        EAnn e ty -> EAnn (go e) ty
