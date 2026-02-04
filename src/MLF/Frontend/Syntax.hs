{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module MLF.Frontend.Syntax (
    VarName,
    Lit (..),
    ExprStage (..),
    Expr (..),
    SurfaceExpr,
    CoreExpr,
    SrcType (..),
    SrcTypeF (..),
    AnnotatedExpr (..),
    BindingSite (..)
) where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.List.NonEmpty (NonEmpty)

{- Note [Surface syntax and paper alignment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module defines the *surface language* accepted by the pipeline and the
*core language* consumed by constraint generation:

  - `Expr 'Surface` is the surface eMLF term language (partially annotated
    λ-calculus).
  - `Expr 'Core` is the annotation-free core term language used internally
    after desugaring.
  - `SrcType` are the user-written type annotations.

Paper reference
--------------
In `papers/these-finale-english.txt` (see `papers/xmlf.txt` §"From λ-terms to typing constraints"), the grammar for
eMLF terms is (using the paper’s notation):

  b ::= x | λ(x) b | λ(x : σ) b | b b | let x = b in b | (b : σ)

Our surface `Expr 'Surface` constructors correspond one-to-one to that grammar:

  - `EVar`       ↔ x
  - `ELam`       ↔ λ(x) b
  - `ELamAnn`    ↔ λ(x : σ) b
  - `EApp`       ↔ b b
  - `ELet`       ↔ let x = b in b
  - `EAnn`       ↔ (b : σ)

The paper notes that term/type annotations can be desugared using coercion
functions κσ. This repository keeps annotations explicit in the AST and
eliminates them in `MLF.Frontend.Desugar` before constraint generation.

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

data ExprStage = Surface | Core

-- | eMLF expressions, indexed by stage.
--
-- The surface stage matches the thesis' expression grammar and includes
-- annotations (`EAnn`, `ELamAnn`). The core stage is annotation-free and
-- represents term annotations via explicit coercion constants (`ECoerceConst`)
-- plus ordinary application/let (thesis §12.3.2).
--
-- Surface annotations are desugared to coercion constants before constraint
-- generation. For example:
--
--   let x : σ = e in b   ≜   let x = (e : σ) in b   ≜   let x = cσ e in b
--
-- The resulting let-binding has a coercion term as its RHS, which is treated
-- as an ordinary let-binding (not a special "declared scheme" form).
data Expr (s :: ExprStage) where
    EVar :: VarName -> Expr s
    ELit :: Lit -> Expr s
    ELam :: VarName -> Expr s -> Expr s                         -- ^ λx. e (inferred parameter type)
    EApp :: Expr s -> Expr s -> Expr s
    ELet :: VarName -> Expr s -> Expr s -> Expr s               -- ^ let x = e₁ in e₂ (inferred scheme)

    -- Surface-only.
    ELamAnn :: VarName -> SrcType -> Expr 'Surface -> Expr 'Surface
    EAnn :: Expr 'Surface -> SrcType -> Expr 'Surface

    -- Core-only.
    ECoerceConst :: SrcType -> Expr 'Core                       -- ^ cτ (coercion constant)

deriving instance Eq (Expr s)
deriving instance Show (Expr s)

type SurfaceExpr = Expr 'Surface
type CoreExpr = Expr 'Core

-- | Optional wrapper for attaching binding-site metadata to a surface expression.
--
-- This is primarily useful for tooling/debugging (e.g. reporting “this variable
-- occurrence is a lambda parameter vs a let-binding”). The main constraint
-- generator uses its own annotation structure (`MLF.Frontend.ConstraintGen.AnnExpr`).
data AnnotatedExpr = AnnotatedExpr
    { annExpr :: SurfaceExpr
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
