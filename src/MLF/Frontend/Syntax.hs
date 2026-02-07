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
    -- * Raw expression aliases (backward-compatible)
    SurfaceExpr,
    CoreExpr,
    -- * Normalized expression aliases
    NormSurfaceExpr,
    NormCoreExpr,
    -- * Raw source types (parser output)
    SrcType (..),
    SrcTypeF (..),
    -- * Staged frontend types
    TypeStage (..),
    NormSrcType (..),
    StructBound (..),
    RawSrcType,
    -- * Metadata
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
  - `SrcType` are the user-written type annotations (raw, as parsed).
  - `NormSrcType` are normalized type annotations where alias bounds have been
    inlined (see Note [Staged frontend types]).

Paper reference
--------------
In `papers/these-finale-english.txt` (see `papers/xmlf.txt` §"From λ-terms to typing constraints"), the grammar for
eMLF terms is (using the paper's notation):

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

{- Note [Staged frontend types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Frontend types come in two stages, tracked by 'TypeStage':

  - 'Raw' ('SrcType'): Produced by the parser. Forall bounds can be any type,
    including bare variable aliases like @∀(b ⩾ a). body@.

  - 'Normalized' ('NormSrcType'): Produced by 'MLF.Frontend.Normalize'. Alias
    bounds have been inlined via capture-avoiding substitution. The bound of a
    forall is always structural (arrow, base, constructor, forall, or bottom) —
    never a bare variable. This invariant is enforced by construction:
    'NormSrcType' uses 'StructBound' for forall bounds, and 'StructBound' has
    no variable constructor.

'SrcType' is the original (unchanged) data type used throughout the codebase.
'NormSrcType' is a new type introduced for the normalized stage. The alias
@RawSrcType = SrcType@ is provided for symmetry.
-}

-- | Normalization stage for frontend types.
data TypeStage = Raw | Normalized
    deriving (Eq, Show)

-- | Source-level type syntax for annotations (raw / parser output).
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
--   - In @STForall v Nothing body@, the missing bound means "bound = ⊥", i.e.
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

-- | Backward-compatible alias: 'RawSrcType' = 'SrcType'.
type RawSrcType = SrcType

-- | Normalized source-level type syntax.
--
-- Like 'SrcType' but forall bounds use 'StructBound' instead of @Maybe SrcType@,
-- ensuring that alias bounds (∀(b ⩾ a). body) are unrepresentable by
-- construction. See Note [Staged frontend types].
data NormSrcType
    = NSTVar String                                         -- ^ Type variable: α
    | NSTArrow NormSrcType NormSrcType                      -- ^ Arrow type: τ → σ
    | NSTBase String                                        -- ^ Base type: Int, Bool, ...
    | NSTCon String (NonEmpty NormSrcType)                  -- ^ Constructor application: C σ…
    | NSTForall String (Maybe StructBound) NormSrcType      -- ^ Bounded quantification: ∀(α ⩾ τ?). σ
    | NSTBottom                                             -- ^ Bottom type: ⊥
    deriving (Eq, Show)

-- | Structural bound for normalized forall types.
--
-- This type mirrors 'NormSrcType' but intentionally omits a variable
-- constructor, ensuring that alias bounds (∀(b ⩾ a). body) are
-- unrepresentable after normalization. A forall bound in normalized form is
-- always structural: arrow, base, constructor, nested forall, or bottom.
data StructBound
    = SBArrow NormSrcType NormSrcType                       -- ^ Arrow bound: τ → σ
    | SBBase String                                         -- ^ Base type bound: Int, Bool, ...
    | SBCon String (NonEmpty NormSrcType)                   -- ^ Constructor bound: C σ…
    | SBForall String (Maybe StructBound) NormSrcType       -- ^ Nested forall bound: ∀(α ⩾ τ?). σ
    | SBBottom                                              -- ^ Bottom bound: ⊥
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

-- | eMLF expressions, indexed by stage and annotation type.
--
-- The type parameter @ty@ determines which type representation annotations
-- carry: 'SrcType' for raw (parser output) or 'NormSrcType' for normalized
-- (alias bounds inlined). See Note [Staged frontend types].
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
data Expr (s :: ExprStage) ty where
    EVar :: VarName -> Expr s ty
    ELit :: Lit -> Expr s ty
    ELam :: VarName -> Expr s ty -> Expr s ty                         -- ^ λx. e (inferred parameter type)
    EApp :: Expr s ty -> Expr s ty -> Expr s ty
    ELet :: VarName -> Expr s ty -> Expr s ty -> Expr s ty            -- ^ let x = e₁ in e₂ (inferred scheme)

    -- Surface-only.
    ELamAnn :: VarName -> ty -> Expr 'Surface ty -> Expr 'Surface ty
    EAnn :: Expr 'Surface ty -> ty -> Expr 'Surface ty

    -- Core-only.
    ECoerceConst :: ty -> Expr 'Core ty                               -- ^ cτ (coercion constant)

deriving instance Eq ty => Eq (Expr s ty)
deriving instance Show ty => Show (Expr s ty)

-- | Raw surface expression (backward-compatible alias).
type SurfaceExpr = Expr 'Surface SrcType
-- | Raw core expression (backward-compatible alias).
type CoreExpr = Expr 'Core SrcType
-- | Normalized surface expression (alias bounds inlined).
type NormSurfaceExpr = Expr 'Surface NormSrcType
-- | Normalized core expression (alias bounds inlined).
type NormCoreExpr = Expr 'Core NormSrcType

-- | Optional wrapper for attaching binding-site metadata to a surface expression.
--
-- This is primarily useful for tooling/debugging (e.g. reporting "this variable
-- occurrence is a lambda parameter vs a let-binding"). The main constraint
-- generator uses its own annotation structure (`MLF.Frontend.ConstraintGen.AnnExpr`).
data AnnotatedExpr = AnnotatedExpr
    { annExpr :: SurfaceExpr
    , annBinding :: Maybe BindingSite
    }
    deriving (Eq, Show)

-- | Distinguish between lambda parameters and let-bound values.
--
-- This mirrors the paper's key distinction:
--   - lambda-bound variables are not generalized (monomorphic),
--   - let-bound variables may be generalized and later instantiated.
data BindingSite
    = LamParam VarName
    | LetBinding VarName
    deriving (Eq, Show)