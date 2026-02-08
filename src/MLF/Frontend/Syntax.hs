{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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
    SrcTy (..),
    SrcType,
    SrcTypeF (..),
    -- * Staged frontend types
    SrcNorm (..),
    SrcTopVar (..),
    SrcBound (..),
    BoundTopVar,
    mkSrcBound,
    mkNormBound,
    unNormBound,
    NormSrcType,
    StructBound,
    RawSrcType,
    -- * Backward-compatible normalized patterns
    pattern NSTVar,
    pattern NSTArrow,
    pattern NSTBase,
    pattern NSTCon,
    pattern NSTForall,
    pattern NSTBottom,
    pattern SBArrow,
    pattern SBBase,
    pattern SBCon,
    pattern SBForall,
    pattern SBBottom,
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
Frontend types use one indexed AST ('SrcTy'), tracked by two indices:

  - stage ('SrcNorm'):
      * 'RawN' ('SrcType'): Produced by the parser. Forall bounds can be any type,
    including bare variable aliases like @∀(b ⩾ a). body@.
      * 'NormN' ('NormSrcType'): Produced by 'MLF.Frontend.Normalize'. Alias
        bounds have been inlined via capture-avoiding substitution.
  - top-level bound root policy ('SrcTopVar'):
      * 'TopVarAllowed': type root may be a variable.
      * 'TopVarDisallowed': type root must be structural.

Forall bounds are wrapped in 'SrcBound' so stage-specific root policy is
captured in one place via 'BoundTopVar':
  - raw bounds: 'BoundTopVar' 'RawN' ~ 'TopVarAllowed'
  - normalized bounds: 'BoundTopVar' 'NormN' ~ 'TopVarDisallowed'

This enforces the normalized invariant by construction: a normalized forall
bound root cannot be a bare variable.
-}

-- | Normalization stage for source types.
data SrcNorm = RawN | NormN
    deriving (Eq, Show)

-- | Whether a source type root may be a variable.
data SrcTopVar = TopVarAllowed | TopVarDisallowed
    deriving (Eq, Show)

type family BoundTopVar (n :: SrcNorm) :: SrcTopVar where
    BoundTopVar 'RawN = 'TopVarAllowed
    BoundTopVar 'NormN = 'TopVarDisallowed

-- | Wrapper for forall bounds, indexed by stage.
newtype SrcBound (n :: SrcNorm) = SrcBound
    { unSrcBound :: SrcTy n (BoundTopVar n)
    }
    deriving (Eq, Show)

mkSrcBound :: SrcTy n (BoundTopVar n) -> SrcBound n
mkSrcBound = SrcBound

-- | Source-level type syntax for annotations, indexed by stage and root policy.
data SrcTy (n :: SrcNorm) (v :: SrcTopVar) where
    STVar :: String -> SrcTy n 'TopVarAllowed
    STArrow :: SrcTy n 'TopVarAllowed -> SrcTy n 'TopVarAllowed -> SrcTy n v
    STBase :: String -> SrcTy n v
    STCon :: String -> NonEmpty (SrcTy n 'TopVarAllowed) -> SrcTy n v
    STForall :: String -> Maybe (SrcBound n) -> SrcTy n 'TopVarAllowed -> SrcTy n v
    STBottom :: SrcTy n v

deriving instance Eq (SrcTy n v)
deriving instance Show (SrcTy n v)

type SrcType = SrcTy 'RawN 'TopVarAllowed
type NormSrcType = SrcTy 'NormN 'TopVarAllowed
type StructBound = SrcTy 'NormN 'TopVarDisallowed

-- | Backward-compatible alias: 'RawSrcType' = 'SrcType'.
type RawSrcType = SrcType

mkNormBound :: StructBound -> SrcBound 'NormN
mkNormBound = SrcBound

unNormBound :: SrcBound 'NormN -> StructBound
unNormBound (SrcBound b) = b

pattern NSTVar :: String -> NormSrcType
pattern NSTVar v = STVar v

pattern NSTArrow :: NormSrcType -> NormSrcType -> NormSrcType
pattern NSTArrow a b = STArrow a b

pattern NSTBase :: String -> NormSrcType
pattern NSTBase b = STBase b

pattern NSTCon :: String -> NonEmpty NormSrcType -> NormSrcType
pattern NSTCon c args = STCon c args

pattern NSTForall :: String -> Maybe StructBound -> NormSrcType -> NormSrcType
pattern NSTForall v mb body <- STForall v (fmap unNormBound -> mb) body
  where
    NSTForall v mb body = STForall v (fmap mkNormBound mb) body

pattern NSTBottom :: NormSrcType
pattern NSTBottom = STBottom

pattern SBArrow :: NormSrcType -> NormSrcType -> StructBound
pattern SBArrow a b = STArrow a b

pattern SBBase :: String -> StructBound
pattern SBBase b = STBase b

pattern SBCon :: String -> NonEmpty NormSrcType -> StructBound
pattern SBCon c args = STCon c args

pattern SBForall :: String -> Maybe StructBound -> NormSrcType -> StructBound
pattern SBForall v mb body <- STForall v (fmap unNormBound -> mb) body
  where
    SBForall v mb body = STForall v (fmap mkNormBound mb) body

pattern SBBottom :: StructBound
pattern SBBottom = STBottom

{-# COMPLETE NSTVar, NSTArrow, NSTBase, NSTCon, NSTForall, NSTBottom #-}
{-# COMPLETE SBArrow, SBBase, SBCon, SBForall, SBBottom #-}

data SrcTypeF a
    = STVarF String
    | STArrowF a a
    | STBaseF String
    | STConF String (NonEmpty a)
    | STForallF String (Maybe a) a
    | STBottomF
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base (SrcTy 'RawN 'TopVarAllowed) = SrcTypeF

instance Recursive (SrcTy 'RawN 'TopVarAllowed) where
    project ty = case ty of
        STVar v -> STVarF v
        STArrow a b -> STArrowF a b
        STBase b -> STBaseF b
        STCon c args -> STConF c args
        STForall v mb body -> STForallF v (fmap unSrcBound mb) body
        STBottom -> STBottomF

instance Corecursive (SrcTy 'RawN 'TopVarAllowed) where
    embed ty = case ty of
        STVarF v -> STVar v
        STArrowF a b -> STArrow a b
        STBaseF b -> STBase b
        STConF c args -> STCon c args
        STForallF v mb body -> STForall v (fmap mkSrcBound mb) body
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
