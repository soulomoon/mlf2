{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MLF.Frontend.Syntax
  ( VarName,
    Lit (..),
    SrcKind (..),
    TypeParam (..),
    typeParamName,
    typeParamKind,
    typeParamRef,
    firstOrderTypeParam,
    typeParamNames,
    typeParamIsFirstOrder,
    ExprStage (..),
    TermReferencePhase (..),
    TermReference (..),
    termReferenceName,
    resolvedTermReferenceDetails,
    Expr
      ( EVarNode,
        ELit,
        ELamNode,
        EApp,
        ELetNode,
        ELamAnnNode,
        EAnn,
        EExactAnn,
        EExactLamNode,
        ECoerceConst,
        EExactCoerceConst,
        EVar,
        EResolvedVar,
        ELam,
        EResolvedLam,
        ELet,
        EResolvedLet,
        ELamAnn,
        EResolvedLamAnn
      ),
    SurfaceExprF (..),

    -- * Staged expression synonyms
    SurfaceExprOf,
    NormSurfaceExprOf,
    NormCoreExprOf,
    SurfaceExpr,
    ResolvedSurfaceExpr,
    NormSurfaceExpr,
    ResolvedNormSurfaceExpr,
    NormCoreExpr,
    ResolvedNormCoreExpr,

    -- * Raw source types (parser output)
    SrcTy (..),
    ResolvedTypeBinderRef,
    ResolvedSrcTy (..),
    SrcType,
    ResolvedSrcType,
    SrcTypeF (..),

    -- * Staged frontend types
    SrcNorm (..),
    SrcTopVar (..),
    SrcBound (..),
    ResolvedSrcBound (..),
    BoundTopVar,
    mkSrcBound,
    mkResolvedSrcBound,
    resolvedSrcTypeBinderName,
    resolvedTypeBinderIdentity,
    resolvedTypeBinderName,
    resolvedTypeBinderRefFromIdentity,
    resolvedTypeBinderTypeIdentity,
    resolvedSrcTypeToSrcType,
    resolvedSrcTypeIdentityType,
    mkNormBound,
    unNormBound,
    NormSrcType,
    StructBound,

    -- * Metadata
    AnnotatedExpr (..),
    BindingSite (..),
  )
where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.List.NonEmpty (NonEmpty)
import MLF.Frontend.Symbol
  ( ResolvedSymbol,
    resolvedSymbolIdentity,
    SymbolNamespace (..),
    resolvedSymbolSpelling,
    symbolDisplayName,
    symbolIdentityStableName,
    symbolNamespace,
  )
import MLF.Types.Identity (IdDetails, TypeBinderIdentity, typeBinderIdentityStableName)

{- Note [Surface syntax and paper alignment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module defines the *surface language* accepted by the pipeline and the
\*core language* consumed by constraint generation:

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
Source spellings remain `String`s, but every variable or binder node owns a
`TermReference`: parser input is explicitly phase-indexed as raw, while resolved
program lowering carries `IdDetails`. Binding levels and polymorphism decisions still
live in the constraint representation (`MLF.Constraint.Types`) produced by
Phase 1.
-}

-- | Source-level term variable names.
--
-- These are display/source spellings. Resolved uniqueness lives in the
-- enclosing 'TermReference'.
type VarName = String

-- | Literal subset used by the pipeline.
--
-- These map to base types in constraint generation (e.g. `Int`, `Bool`, `String`, `Char`).
data Lit
  = LInt Integer
  | LBool Bool
  | LString String
  | LChar Char
  deriving (Eq, Show)

-- | Source-level kind syntax for `.mlfp` declaration parameters.
data SrcKind
  = KType
  | KArrow SrcKind SrcKind
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
  deriving (Eq, Ord, Show)

mkSrcBound :: SrcTy n (BoundTopVar n) -> SrcBound n
mkSrcBound = SrcBound

data ResolvedTypeBinderRef = ResolvedTypeBinderRef
  { resolvedTypeBinderIdentity :: TypeBinderIdentity,
    resolvedTypeBinderName :: String
  }
  deriving (Show)

resolvedTypeBinderRefFromIdentity :: TypeBinderIdentity -> String -> ResolvedTypeBinderRef
resolvedTypeBinderRefFromIdentity =
  ResolvedTypeBinderRef

instance Eq ResolvedTypeBinderRef where
  left == right =
    resolvedTypeBinderIdentity left == resolvedTypeBinderIdentity right

instance Ord ResolvedTypeBinderRef where
  compare left right =
    compare (resolvedTypeBinderIdentity left) (resolvedTypeBinderIdentity right)

-- | A source-level type parameter with its declared kind.
data TypeParam
  = TypeParam String SrcKind
  | ResolvedTypeParam ResolvedTypeBinderRef SrcKind
  deriving (Eq, Show)

typeParamName :: TypeParam -> String
typeParamName param =
  case param of
    TypeParam name _ -> name
    ResolvedTypeParam ref _ -> resolvedTypeBinderName ref

typeParamKind :: TypeParam -> SrcKind
typeParamKind param =
  case param of
    TypeParam _ kind0 -> kind0
    ResolvedTypeParam _ kind0 -> kind0

typeParamRef :: TypeParam -> Maybe ResolvedTypeBinderRef
typeParamRef param =
  case param of
    TypeParam {} -> Nothing
    ResolvedTypeParam ref _ -> Just ref

firstOrderTypeParam :: String -> TypeParam
firstOrderTypeParam name = TypeParam name KType

typeParamNames :: [TypeParam] -> [String]
typeParamNames = map typeParamName

typeParamIsFirstOrder :: TypeParam -> Bool
typeParamIsFirstOrder param = typeParamKind param == KType

-- | Source-level type syntax for annotations, indexed by stage and root policy.
data SrcTy (n :: SrcNorm) (v :: SrcTopVar) where
  STVar :: String -> SrcTy n 'TopVarAllowed
  STArrow :: SrcTy n 'TopVarAllowed -> SrcTy n 'TopVarAllowed -> SrcTy n v
  STBase :: String -> SrcTy n v
  STCon :: String -> NonEmpty (SrcTy n 'TopVarAllowed) -> SrcTy n v
  STVarApp :: String -> NonEmpty (SrcTy n 'TopVarAllowed) -> SrcTy n v
  STTyLam :: String -> SrcTy n 'TopVarAllowed -> SrcTy n v
  STTyApp :: SrcTy n 'TopVarAllowed -> SrcTy n 'TopVarAllowed -> SrcTy n v
  STForall :: String -> Maybe (SrcBound n) -> SrcTy n 'TopVarAllowed -> SrcTy n v
  STMu :: String -> SrcTy n 'TopVarAllowed -> SrcTy n v
  STBottom :: SrcTy n v

deriving instance Eq (SrcTy n v)

deriving instance Ord (SrcTy n v)

deriving instance Show (SrcTy n v)

type SrcType = SrcTy 'RawN 'TopVarAllowed

-- | Wrapper for forall bounds after `.mlfp` symbol resolution.
newtype ResolvedSrcBound (n :: SrcNorm) = ResolvedSrcBound
  { unResolvedSrcBound :: ResolvedSrcTy n (BoundTopVar n)
  }
  deriving (Eq, Show)

mkResolvedSrcBound :: ResolvedSrcTy n (BoundTopVar n) -> ResolvedSrcBound n
mkResolvedSrcBound = ResolvedSrcBound

-- | Source-level type syntax after `.mlfp` symbol resolution.
data ResolvedSrcTy (n :: SrcNorm) (v :: SrcTopVar) where
  RSTVar :: ResolvedTypeBinderRef -> ResolvedSrcTy n 'TopVarAllowed
  RSTArrow :: ResolvedSrcTy n 'TopVarAllowed -> ResolvedSrcTy n 'TopVarAllowed -> ResolvedSrcTy n v
  RSTBase :: ResolvedSymbol -> ResolvedSrcTy n v
  RSTCon :: ResolvedSymbol -> NonEmpty (ResolvedSrcTy n 'TopVarAllowed) -> ResolvedSrcTy n v
  RSTVarApp :: ResolvedTypeBinderRef -> NonEmpty (ResolvedSrcTy n 'TopVarAllowed) -> ResolvedSrcTy n v
  RSTTyLam :: ResolvedTypeBinderRef -> ResolvedSrcTy n 'TopVarAllowed -> ResolvedSrcTy n v
  RSTTyApp :: ResolvedSrcTy n 'TopVarAllowed -> ResolvedSrcTy n 'TopVarAllowed -> ResolvedSrcTy n v
  RSTForall :: ResolvedTypeBinderRef -> Maybe (ResolvedSrcBound n) -> ResolvedSrcTy n 'TopVarAllowed -> ResolvedSrcTy n v
  RSTMu :: ResolvedTypeBinderRef -> ResolvedSrcTy n 'TopVarAllowed -> ResolvedSrcTy n v
  RSTBottom :: ResolvedSrcTy n v

deriving instance Eq (ResolvedSrcTy n v)

deriving instance Show (ResolvedSrcTy n v)

type ResolvedSrcType = ResolvedSrcTy 'RawN 'TopVarAllowed

type NormSrcType = SrcTy 'NormN 'TopVarAllowed

type StructBound = SrcTy 'NormN 'TopVarDisallowed

resolvedSrcTypeBinderName :: ResolvedTypeBinderRef -> String
resolvedSrcTypeBinderName =
  resolvedTypeBinderName

resolvedTypeBinderTypeIdentity :: ResolvedTypeBinderRef -> TypeBinderIdentity
resolvedTypeBinderTypeIdentity =
  resolvedTypeBinderIdentity

resolvedSrcTypeBinderIdentityName :: ResolvedTypeBinderRef -> String
resolvedSrcTypeBinderIdentityName ref =
  typeBinderIdentityStableName (resolvedTypeBinderTypeIdentity ref)

resolvedSrcTypeToSrcType :: ResolvedSrcTy n v -> SrcTy n v
resolvedSrcTypeToSrcType ty =
  case ty of
    RSTVar ref -> STVar (resolvedSrcTypeBinderName ref)
    RSTArrow dom cod -> STArrow (resolvedSrcTypeToSrcType dom) (resolvedSrcTypeToSrcType cod)
    RSTBase symbol -> STBase (resolvedTypeHeadDisplay symbol)
    RSTCon symbol args -> STCon (resolvedTypeHeadDisplay symbol) (fmap resolvedSrcTypeToSrcType args)
    RSTVarApp ref args -> STVarApp (resolvedSrcTypeBinderName ref) (fmap resolvedSrcTypeToSrcType args)
    RSTTyLam ref body -> STTyLam (resolvedSrcTypeBinderName ref) (resolvedSrcTypeToSrcType body)
    RSTTyApp fun arg -> STTyApp (resolvedSrcTypeToSrcType fun) (resolvedSrcTypeToSrcType arg)
    RSTForall ref mb body ->
      STForall
        (resolvedSrcTypeBinderName ref)
        (fmap (SrcBound . resolvedSrcTypeToSrcType . unResolvedSrcBound) mb)
        (resolvedSrcTypeToSrcType body)
    RSTMu ref body -> STMu (resolvedSrcTypeBinderName ref) (resolvedSrcTypeToSrcType body)
    RSTBottom -> STBottom

resolvedSrcTypeIdentityType :: ResolvedSrcTy n v -> SrcTy n v
resolvedSrcTypeIdentityType ty =
  case ty of
    RSTVar ref -> STVar (resolvedSrcTypeBinderIdentityName ref)
    RSTArrow dom cod -> STArrow (resolvedSrcTypeIdentityType dom) (resolvedSrcTypeIdentityType cod)
    RSTBase symbol -> STBase (resolvedTypeHeadIdentityName symbol)
    RSTCon symbol args -> STCon (resolvedTypeHeadIdentityName symbol) (fmap resolvedSrcTypeIdentityType args)
    RSTVarApp ref args -> STVarApp (resolvedSrcTypeBinderIdentityName ref) (fmap resolvedSrcTypeIdentityType args)
    RSTTyLam ref body -> STTyLam (resolvedSrcTypeBinderIdentityName ref) (resolvedSrcTypeIdentityType body)
    RSTTyApp fun arg -> STTyApp (resolvedSrcTypeIdentityType fun) (resolvedSrcTypeIdentityType arg)
    RSTForall ref mb body ->
      STForall
        (resolvedSrcTypeBinderIdentityName ref)
        (fmap (SrcBound . resolvedSrcTypeIdentityType . unResolvedSrcBound) mb)
        (resolvedSrcTypeIdentityType body)
    RSTMu ref body -> STMu (resolvedSrcTypeBinderIdentityName ref) (resolvedSrcTypeIdentityType body)
    RSTBottom -> STBottom

resolvedTypeHeadDisplay :: ResolvedSymbol -> String
resolvedTypeHeadDisplay =
  symbolDisplayName . resolvedSymbolSpelling

resolvedTypeHeadIdentityName :: ResolvedSymbol -> String
resolvedTypeHeadIdentityName symbol =
  let identity = resolvedSymbolIdentity symbol
   in case symbolNamespace identity of
        SymbolType -> symbolIdentityStableName identity
        _ -> symbolDisplayName (resolvedSymbolSpelling symbol)

mkNormBound :: StructBound -> SrcBound 'NormN
mkNormBound = SrcBound

unNormBound :: SrcBound 'NormN -> StructBound
unNormBound (SrcBound b) = b

data SrcTypeF a
  = STVarF String
  | STArrowF a a
  | STBaseF String
  | STConF String (NonEmpty a)
  | STVarAppF String (NonEmpty a)
  | STTyLamF String a
  | STTyAppF a a
  | STForallF String (Maybe a) a
  | STMuF String a
  | STBottomF
  deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base (SrcTy 'RawN 'TopVarAllowed) = SrcTypeF

instance Recursive (SrcTy 'RawN 'TopVarAllowed) where
  project ty = case ty of
    STVar v -> STVarF v
    STArrow a b -> STArrowF a b
    STBase b -> STBaseF b
    STCon c args -> STConF c args
    STVarApp v args -> STVarAppF v args
    STTyLam v body -> STTyLamF v body
    STTyApp fun arg -> STTyAppF fun arg
    STForall v mb body -> STForallF v (fmap unSrcBound mb) body
    STMu v body -> STMuF v body
    STBottom -> STBottomF

instance Corecursive (SrcTy 'RawN 'TopVarAllowed) where
  embed ty = case ty of
    STVarF v -> STVar v
    STArrowF a b -> STArrow a b
    STBaseF b -> STBase b
    STConF c args -> STCon c args
    STVarAppF v args -> STVarApp v args
    STTyLamF v body -> STTyLam v body
    STTyAppF fun arg -> STTyApp fun arg
    STForallF v mb body -> STForall v (fmap mkSrcBound mb) body
    STMuF v body -> STMu v body
    STBottomF -> STBottom

data ExprStage = Surface | Core

data TermReferencePhase
  = RawTermReferences
  | ResolvedTermReferences

data TermReference (r :: TermReferencePhase) where
  RawTermReference :: VarName -> TermReference 'RawTermReferences
  ResolvedTermReference :: IdDetails -> VarName -> TermReference 'ResolvedTermReferences

deriving instance Eq (TermReference r)

deriving instance Show (TermReference r)

termReferenceName :: TermReference r -> VarName
termReferenceName reference =
  case reference of
    RawTermReference name -> name
    ResolvedTermReference _ name -> name

resolvedTermReferenceDetails :: TermReference 'ResolvedTermReferences -> IdDetails
resolvedTermReferenceDetails (ResolvedTermReference details _) = details

-- | eMLF expressions, indexed by stage and annotation type.
--
-- The type parameter @ty@ determines which type representation annotations
-- carry: 'SrcType' for raw (parser output) or 'NormSrcType' for normalized
-- (alias bounds inlined). See Note [Staged frontend types].
--
-- The surface stage matches the thesis' expression grammar and includes
-- annotations (`EAnn`, `ELamAnn`). The core stage represents source term
-- annotations via explicit coercion constants (`ECoerceConst`) plus ordinary
-- application/let (thesis §12.3.2). Compiler-owned annotations use the exact
-- `EExactAnn`/`EExactCoerceConst` path instead: their producer type is already
-- authoritative and Phase 6 must not infer construction from κσ's flexible
-- codomain. Compiler-owned evidence parameters use `EExactLamNode` for the
-- same reason.
--
-- Surface annotations are desugared to coercion constants before constraint
-- generation. For example:
--
--   let x : σ = e in b   ≜   let x = (e : σ) in b   ≜   let x = cσ e in b
--
-- The resulting let-binding has a coercion term as its RHS, which is treated
-- as an ordinary let-binding (not a special "declared scheme" form).
data Expr (r :: TermReferencePhase) (s :: ExprStage) ty where
  EVarNode :: TermReference r -> Expr r s ty
  ELit :: Lit -> Expr r s ty
  ELamNode ::
    TermReference r ->
    Expr r s ty ->
    -- | λx. e (inferred parameter type)
    Expr r s ty
  EApp :: Expr r s ty -> Expr r s ty -> Expr r s ty
  ELetNode ::
    TermReference r ->
    Expr r s ty ->
    Expr r s ty ->
    -- | let x = e₁ in e₂ (inferred scheme)
    Expr r s ty
  -- Surface-only.
  ELamAnnNode :: TermReference r -> ty -> Expr r 'Surface ty -> Expr r 'Surface ty
  EAnn :: Expr r 'Surface ty -> ty -> Expr r 'Surface ty
  EExactAnn ::
    Expr r 'Surface ty ->
    ty ->
    ResolvedSrcType ->
    -- | Compiler-owned exact annotation. Source syntax never constructs this
    -- node: `EAnn` retains the thesis' κσ semantics.  The resolved producer
    -- type carries binder and head identities as construction authority.
    Expr r 'Surface ty
  -- Compiler-only, preserved from surface construction into core.
  EExactLamNode ::
    TermReference r ->
    ty ->
    Expr r s ty ->
    -- | Compiler-owned lambda parameter with an authoritative exact type.
    -- Source annotations never construct this node: they elaborate through
    -- 'ECoerceConst' according to the thesis' κσ translation.
    Expr r s ty
  ECoerceConst ::
    ty ->
    -- | cτ (coercion constant)
    Expr r 'Core ty
  EExactCoerceConst ::
    ty ->
    ResolvedSrcType ->
    -- | Compiler-owned exact type authority after surface desugaring.
    Expr r 'Core ty

pattern EVar :: VarName -> Expr 'RawTermReferences s ty
pattern EVar name = EVarNode (RawTermReference name)

pattern EResolvedVar :: IdDetails -> VarName -> Expr 'ResolvedTermReferences s ty
pattern EResolvedVar details name = EVarNode (ResolvedTermReference details name)

pattern ELam :: VarName -> Expr 'RawTermReferences s ty -> Expr 'RawTermReferences s ty
pattern ELam name body = ELamNode (RawTermReference name) body

pattern EResolvedLam :: IdDetails -> VarName -> Expr 'ResolvedTermReferences s ty -> Expr 'ResolvedTermReferences s ty
pattern EResolvedLam details name body = ELamNode (ResolvedTermReference details name) body

pattern ELet :: VarName -> Expr 'RawTermReferences s ty -> Expr 'RawTermReferences s ty -> Expr 'RawTermReferences s ty
pattern ELet name rhs body = ELetNode (RawTermReference name) rhs body

pattern EResolvedLet :: IdDetails -> VarName -> Expr 'ResolvedTermReferences s ty -> Expr 'ResolvedTermReferences s ty -> Expr 'ResolvedTermReferences s ty
pattern EResolvedLet details name rhs body = ELetNode (ResolvedTermReference details name) rhs body

pattern ELamAnn :: VarName -> ty -> Expr 'RawTermReferences 'Surface ty -> Expr 'RawTermReferences 'Surface ty
pattern ELamAnn name ty body = ELamAnnNode (RawTermReference name) ty body

pattern EResolvedLamAnn :: IdDetails -> VarName -> ty -> Expr 'ResolvedTermReferences 'Surface ty -> Expr 'ResolvedTermReferences 'Surface ty
pattern EResolvedLamAnn details name ty body = ELamAnnNode (ResolvedTermReference details name) ty body

{-# COMPLETE EVar, ELit, ELam, EApp, ELet, EExactLamNode, ECoerceConst, EExactCoerceConst #-}
{-# COMPLETE EResolvedVar, ELit, EResolvedLam, EApp, EResolvedLet, EExactLamNode, ECoerceConst, EExactCoerceConst #-}
{-# COMPLETE EVar, ELit, ELam, EApp, ELet, ELamAnn, EExactAnn, EExactLamNode, EAnn #-}
{-# COMPLETE EResolvedVar, ELit, EResolvedLam, EApp, EResolvedLet, EResolvedLamAnn, EExactAnn, EExactLamNode, EAnn #-}

deriving instance (Eq ty) => Eq (Expr r s ty)

deriving instance (Show ty) => Show (Expr r s ty)

data SurfaceExprF r ty a
  = EVarSurfaceF (TermReference r)
  | ELitSurfaceF Lit
  | ELamSurfaceF (TermReference r) a
  | EAppSurfaceF a a
  | ELetSurfaceF (TermReference r) a a
  | ELamAnnSurfaceF (TermReference r) ty a
  | EExactLamSurfaceF (TermReference r) ty a
  | EAnnSurfaceF a ty
  | EExactAnnSurfaceF a ty ResolvedSrcType
  deriving (Functor, Foldable, Traversable)

type instance Base (Expr r 'Surface ty) = SurfaceExprF r ty

instance Recursive (Expr r 'Surface ty) where
  project expr = case expr of
    EVarNode ref -> EVarSurfaceF ref
    ELit l -> ELitSurfaceF l
    ELamNode ref body -> ELamSurfaceF ref body
    EApp fun arg -> EAppSurfaceF fun arg
    ELetNode ref rhs body -> ELetSurfaceF ref rhs body
    ELamAnnNode ref ty body -> ELamAnnSurfaceF ref ty body
    EExactLamNode ref ty body -> EExactLamSurfaceF ref ty body
    EAnn expr0 ty -> EAnnSurfaceF expr0 ty
    EExactAnn expr0 ty exactTy -> EExactAnnSurfaceF expr0 ty exactTy

instance Corecursive (Expr r 'Surface ty) where
  embed expr = case expr of
    EVarSurfaceF ref -> EVarNode ref
    ELitSurfaceF l -> ELit l
    ELamSurfaceF ref body -> ELamNode ref body
    EAppSurfaceF fun arg -> EApp fun arg
    ELetSurfaceF ref rhs body -> ELetNode ref rhs body
    ELamAnnSurfaceF ref ty body -> ELamAnnNode ref ty body
    EExactLamSurfaceF ref ty body -> EExactLamNode ref ty body
    EAnnSurfaceF expr0 ty -> EAnn expr0 ty
    EExactAnnSurfaceF expr0 ty exactTy -> EExactAnn expr0 ty exactTy

type SurfaceExprOf r = Expr r 'Surface SrcType

type NormSurfaceExprOf r = Expr r 'Surface NormSrcType

type NormCoreExprOf r = Expr r 'Core NormSrcType

-- | Raw surface expression accepted by the parser.
type SurfaceExpr = SurfaceExprOf 'RawTermReferences

type ResolvedSurfaceExpr = SurfaceExprOf 'ResolvedTermReferences

-- | Normalized surface expression (alias bounds inlined).
type NormSurfaceExpr = NormSurfaceExprOf 'RawTermReferences

type ResolvedNormSurfaceExpr = NormSurfaceExprOf 'ResolvedTermReferences

-- | Normalized core expression (alias bounds inlined).
type NormCoreExpr = NormCoreExprOf 'RawTermReferences

type ResolvedNormCoreExpr = NormCoreExprOf 'ResolvedTermReferences

-- | Optional wrapper for attaching binding-site metadata to a surface expression.
--
-- This is primarily useful for tooling/debugging (e.g. reporting "this variable
-- occurrence is a lambda parameter vs a let-binding"). The main constraint
-- generator uses its own annotation structure (`MLF.Frontend.ConstraintGen.AnnExpr`).
data AnnotatedExpr = AnnotatedExpr
  { annExpr :: SurfaceExpr,
    annBinding :: Maybe BindingSite
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
