{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module MLF.Frontend.Desugar (
    desugarSurface
) where

import MLF.Frontend.Syntax (Expr (..), NormCoreExpr, NormSurfaceExpr)

{- Note [κσ coercions and desugaring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.1, "From λ-terms to typing constraints") treats:

  - (b : σ)     as syntactic sugar for  κσ b
  - λ(x : σ) b  as syntactic sugar for  λ(x) let x = κσ x in b

The thesis also phrases annotated lambdas via surface `(:)` (Chapter 12.3.2):

  λ(x : τ) a  ≜  λ(x) let x = (x : τ) in a

Following the thesis (and `emlf_typeing_rules.md`), we eliminate *all* surface
annotations before constraint generation by introducing an explicit coercion
constant `cτ` (represented as `ECoerceConst τ`) and treating annotations as
ordinary application/let:

  (a : τ)        ≜  cτ a
  λ(x : τ) a     ≜  λ(x) let x = cτ x in a

Surface annotations on let-bindings are also desugared to coercion terms:

  let x : σ = e in b   ≜   let x = (e : σ) in b   ≜   let x = cσ e in b

This is a normal let-binding where the RHS is a coercion application, not a
special "declared scheme" form. The coercion constant cσ is a term-level
operator that constrains its argument to have type σ.

Note: Annotated lambdas desugar to a plain lambda with a let-binding containing
a coercion. There is no special core-level constructor for annotated lambdas—
the constraint generator sees only `ELam` and processes the let normally.
-}

desugarSurface :: NormSurfaceExpr -> NormCoreExpr
desugarSurface expr = case expr of
    EVar v -> EVar v
    ELit l -> ELit l
    ELam v body -> ELam v (desugarSurface body)
    -- Annotated lambda desugars to plain lambda with let + coercion (thesis §12.3.2)
    -- λ(x : τ) body  ≜  λ(x) let x = cτ x in body
    ELamAnn v ty body ->
        ELam v (ELet v (EApp (ECoerceConst ty) (EVar v)) (desugarSurface body))
    EApp f a -> EApp (desugarSurface f) (desugarSurface a)
    ELet v rhs body -> ELet v (desugarSurface rhs) (desugarSurface body)
    EAnn e ty -> EApp (ECoerceConst ty) (desugarSurface e)
