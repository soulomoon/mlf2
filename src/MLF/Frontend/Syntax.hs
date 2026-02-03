{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
module MLF.Frontend.Syntax (
    VarName,
    Lit (..),
    Expr (EVar, ELam, ELamAnn, EApp, ELet, EAnn, ELit),
    ExprF (EVarF, ELamF, ELamAnnF, EAppF, ELetF, EAnnF, ELitF),
    SrcType (..),
    SrcTypeF (..),
    AnnotatedExpr (..),
    BindingSite (..),
    mkCoerce,
    viewCoerce
) where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.List.NonEmpty (NonEmpty)

{- Note [Surface syntax and paper alignment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module defines the *surface language* accepted by the pipeline:

  - `Expr` is the core eMLF term language (partially annotated λ-calculus).
  - `SrcType` are the user-written type annotations.

Paper reference
--------------
In `papers/these-finale-english.txt` (see `papers/xmlf.txt` §"From λ-terms to typing constraints"), the grammar for
eMLF terms is (using the paper’s notation):

  b ::= x | λ(x) b | λ(x : σ) b | b b | let x = b in b | (b : σ)

Our `Expr` constructors correspond one-to-one to that grammar:

  - `EVar`       ↔ x
  - `ELam`       ↔ λ(x) b
  - `ELamAnn`    ↔ λ(x : σ) b
  - `EApp`       ↔ b b
  - `ELet`       ↔ let x = b in b
  - `EAnn`       ↔ (b : σ)

The paper notes that term/type annotations can be desugared using coercion
functions κσ. This repository keeps annotations explicit in the AST and
implements their meaning directly during constraint generation.

Annotated lets are represented using `ELet` with an annotated RHS:

  let x : σ = e in b   ≜   let x = (e : σ) in b

Implementation boundary
-----------------------
These types intentionally remain simple (mostly `String`s) and do not encode
scope/levels. Binding levels and polymorphism decisions live in the constraint
representation (`MLF.Constraint.Types`) produced by Phase 1.
-}

-- | Source-level term variable names.
--
-- We use plain `String`s. Uniqueness / alpha-renaming is handled by the parser
-- or assumed by construction in tests.
type VarName = String

-- | Literal subset used by the pipeline.
--
-- These map to base types in constraint generation (e.g. `Int`, `Bool`, `String`).
data Lit
    = LInt Integer
    | LBool Bool
    | LString String
    deriving (Eq, Show)

-- | Source-level type syntax for annotations.
--
-- This corresponds closely to the type language presented in
-- `papers/these-finale-english.txt` (see `papers/xmlf.txt`):
--
--   - type variables (written !α in the paper; we use names like "a")
--   - arrows (τ → σ)
--   - bounded quantification ∀(α ⩾ τ). σ
--   - bottom type ⊥ (the default bound for unbounded quantification)
--
-- We add a small convenience notationally:
--
--   - In `STForall v Nothing body`, the missing bound means “bound = ⊥”, i.e.
--     the common unbounded form ∀(v). body.
--
-- Examples:
--   - @Int → Bool@
--   - @∀α. α → α@ (standard polymorphism)
--   - @∀(α ⩾ Int → Int). α@ (α must be at least as general as Int → Int)
--   - @∀(α ⩾ ∀β. β → β). α → α@ (α must be at least as general as polymorphic identity)
data SrcType
    = STVar String                              -- ^ Type variable: α
    | STArrow SrcType SrcType                   -- ^ Arrow type: τ → σ
    | STBase String                             -- ^ Base type: Int, Bool, ...
    | STCon String (NonEmpty SrcType)           -- ^ Constructor application: C σ…
    | STForall String (Maybe SrcType) SrcType   -- ^ Bounded quantification: ∀(α ⩾ τ?). σ
    | STBottom                                  -- ^ Bottom type: ⊥
    deriving (Eq, Show)

data SrcTypeF a
    = STVarF String
    | STArrowF a a
    | STBaseF String
    | STConF String (NonEmpty a)
    | STForallF String (Maybe a) a
    | STBottomF
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base SrcType = SrcTypeF

instance Recursive SrcType where
    project ty = case ty of
        STVar v -> STVarF v
        STArrow a b -> STArrowF a b
        STBase b -> STBaseF b
        STCon c args -> STConF c args
        STForall v mb body -> STForallF v mb body
        STBottom -> STBottomF

instance Corecursive SrcType where
    embed ty = case ty of
        STVarF v -> STVar v
        STArrowF a b -> STArrow a b
        STBaseF b -> STBase b
        STConF c args -> STCon c args
        STForallF v mb body -> STForall v mb body
        STBottomF -> STBottom

-- | Core term language (eMLF) supported by constraint generation.
--
-- This is intentionally small and matches the paper’s eMLF term grammar.
--
-- Downstream meaning (high level)
-- ------------------------------
-- `Expr` does not itself encode polymorphism rules. Instead:
--
--   - lambda binders (`ELam`) behave monomorphically,
--   - let binders (`ELet`) are generalization points,
--   - applications (`EApp`) generate instantiation constraints (≤),
--   - annotations (`EAnn`) are turned into constraint graph structure
--     by Phase 1,
--   - annotated lambda parameters (`ELamAnn`) are surface sugar (thesis §12.3.2):
--       λ(x : τ) a  ≜  λ(x) let x = (x : τ) in a
--     and are desugared to `ELam`/`ELet` plus coercions (see `MLF.Frontend.Desugar`)
--     before constraint generation.
--   - term annotations (`EAnn`) desugar to explicit coercions (`ECoerce`),
--     which are internal and not part of the surface grammar.
--
-- See `MLF.Frontend.ConstraintGen` for the authoritative translation.
--
-- Unannotated forms infer types; annotated forms check against provided types.
data Expr
    = EVar VarName
    | ELam VarName Expr                         -- ^ λx. e (inferred parameter type)
    | ELamAnn VarName SrcType Expr              -- ^ λ(x : τ). e (annotated parameter)
    | EApp Expr Expr
    | ELet VarName Expr Expr                    -- ^ let x = e₁ in e₂ (inferred scheme)
    | EAnn Expr SrcType                         -- ^ (e : τ) (term annotation; desugars to coercion)
    | ECoerce SrcType Expr                      -- ^ Internal: coercion term (cκ e)
    | ELit Lit
    deriving (Eq, Show)

-- | Internal coercion constructor helper.
mkCoerce :: SrcType -> Expr -> Expr
mkCoerce = ECoerce

-- | Internal coercion view (used by desugaring/translation).
viewCoerce :: Expr -> Maybe (SrcType, Expr)
viewCoerce expr = case expr of
    ECoerce ty e -> Just (ty, e)
    _ -> Nothing

data ExprF a
    = EVarF VarName
    | ELamF VarName a
    | ELamAnnF VarName SrcType a
    | EAppF a a
    | ELetF VarName a a
    | EAnnF a SrcType
    | ECoerceF SrcType a
    | ELitF Lit
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base Expr = ExprF

instance Recursive Expr where
    project expr = case expr of
        EVar v -> EVarF v
        ELam v body -> ELamF v body
        ELamAnn v ty body -> ELamAnnF v ty body
        EApp f a -> EAppF f a
        ELet v rhs body -> ELetF v rhs body
        EAnn e ty -> EAnnF e ty
        ECoerce ty e -> ECoerceF ty e
        ELit l -> ELitF l

instance Corecursive Expr where
    embed expr = case expr of
        EVarF v -> EVar v
        ELamF v body -> ELam v body
        ELamAnnF v ty body -> ELamAnn v ty body
        EAppF f a -> EApp f a
        ELetF v rhs body -> ELet v rhs body
        EAnnF e ty -> EAnn e ty
        ECoerceF ty e -> ECoerce ty e
        ELitF l -> ELit l

-- | Optional wrapper for attaching binding-site metadata to a surface expression.
--
-- This is primarily useful for tooling/debugging (e.g. reporting “this variable
-- occurrence is a lambda parameter vs a let-binding”). The main constraint
-- generator uses its own annotation structure (`MLF.Frontend.ConstraintGen.AnnExpr`).
data AnnotatedExpr = AnnotatedExpr
    { annExpr :: Expr
    , annBinding :: Maybe BindingSite
    }
    deriving (Eq, Show)

-- | Distinguish between lambda parameters and let-bound values.
--
-- This mirrors the paper’s key distinction:
--   - lambda-bound variables are not generalized (monomorphic),
--   - let-bound variables may be generalized and later instantiated.
data BindingSite
    = LamParam VarName
    | LetBinding VarName
    deriving (Eq, Show)
