{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module MLF.Frontend.ConstraintGen.Types
  ( ConstraintError (..),
    ConstraintResult (..),
    ModuleRootId (..),
    RootOwnershipIndex (..),
    ModuleConstraintRoot (..),
    ModuleConstraintResult (..),
    AnnExpr (..),
    AnnExprF (..),
    Binding (..),
    BindingKey (..),
    bindingKeyForTermReference,
    Env,
    ExternalEnv,
    ExternalBindingMode (..),
    ExternalBinding (..),
    ExternalBindingIdentity,
    externalBindingIdentityFromDetails,
    externalBindingIdentityFromResolvedVar,
    externalBindingIdentityFromDeferredRef,
    externalBindingRuntimeName,
    externalBindingDetails,
    ExternalBindings,
    replaceScopeRoot,
  )
where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..), cata)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import MLF.Constraint.RootOwnership (ModuleRootId (..), RootOwnershipIndex (..))
import MLF.Constraint.Types.Graph
import MLF.Frontend.Symbol (SymbolIdentity)
import MLF.Frontend.Syntax (Lit, NormSrcType, TermReference (..), VarName)
import MLF.Types.Elab (ResolvedVar (..))
import MLF.Types.Identity (DeferredRef, IdDetails (..), ResolvedTermIdentityKey, TypeBinderIdentity, idDetailsIdentityKey, idDetailsRuntimeName)

-- | Errors that can surface during constraint generation.
data ConstraintError
  = UnknownVariable VarName
  | -- | Unexpected internal state
    InternalConstraintError String
  | UnexpectedBareCoercionConst
  | -- | Constructor, expected arity, actual arity
    TypeConstructorArityMismatch BaseTy Int Int
  | -- | Binder name that appears in its own bound
    ForallBoundMentionsBinder String
  | RecursiveAnnotationNotSupported NormSrcType
  deriving (Eq, Show)

-- | Successful constraint generation returns the full constraint graph and the
-- root 'NodeId' that represents the program's type.
data ConstraintResult p = ConstraintResult { crConstraint :: Constraint p,
    crRoot :: NodeId,
    crAnnotated :: AnnExpr,
    -- | Original source annotation types keyed by AAnn codomain NodeId.
    -- Preserves the exact lowered 'NormSrcType' from 'buildCoerce' so that
    -- elaboration can recover annotation types that presolution strips.
    crAnnSourceTypes :: IntMap.IntMap NormSrcType,
    -- | Initial bindings created for external environment variables.
    -- Each entry maps an explicit metadata-light or resolved identity key to
    -- its 'Binding' (node + gen + optional carried identity).
    -- The pipeline uses this to seed the elaboration and type-check
    -- environments for free variables that were not wrapped in ELamAnn.
    crInitialEnv :: Env
  }
  deriving (Eq, Show)

data ModuleConstraintRoot = ModuleConstraintRoot
  { mcrRootId :: ModuleRootId,
    mcrRoot :: NodeId,
    mcrAnnotated :: AnnExpr
  }
  deriving (Eq, Show)

data ModuleConstraintResult key p = ModuleConstraintResult
  { mcrConstraint :: Constraint p,
    mcrRoots :: Map key ModuleConstraintRoot,
    mcrAnnSourceTypes :: IntMap.IntMap NormSrcType,
    mcrInitialEnv :: Env,
    mcrRootOwnership :: RootOwnershipIndex
  }
  deriving (Eq, Show)

-- | Expression annotated with the NodeIds allocated during constraint generation.
-- The NodeIds are stable and match the ones found in the constraint graph, so
-- later phases (e.g., elaboration) can recover binder types.
data AnnExpr
  = AVar VarName NodeId
  | -- | Exact resolved identity, display/runtime spelling, occurrence node.
    AResolvedVar IdDetails VarName NodeId
  | ALit Lit NodeId
  | -- | param name, optional resolved identity, param node, scope root (gen), body, result node
    ALam VarName (Maybe IdDetails) NodeId GenNodeId AnnExpr NodeId
  | -- | fun, arg, fun inst edge id, arg inst edge id, result node
    AApp AnnExpr AnnExpr EdgeId EdgeId NodeId
  | -- | binder name, optional resolved identity, scheme gen node, scheme root, expansion var, RHS scope gen, rhs, body, result node
    ALet VarName (Maybe IdDetails) GenNodeId NodeId ExpVarId GenNodeId AnnExpr AnnExpr NodeId
  | -- | expression, annotation node
    AAnn AnnExpr NodeId EdgeId
  | -- | expression, unfolded-type node, inst edge from expr to unfolded type
    AUnfold AnnExpr NodeId EdgeId
  deriving (Eq, Show)

data AnnExprF a
  = AVarF VarName NodeId
  | AResolvedVarF IdDetails VarName NodeId
  | ALitF Lit NodeId
  | -- | param name, optional resolved identity, param node, scope root (gen), body, result node
    ALamF VarName (Maybe IdDetails) NodeId GenNodeId a NodeId
  | -- | fun, arg, fun inst edge id, arg inst edge id, result node
    AAppF a a EdgeId EdgeId NodeId
  | -- | binder name, optional resolved identity, scheme gen node, scheme root, expansion var, RHS scope gen, rhs, body, result node
    ALetF VarName (Maybe IdDetails) GenNodeId NodeId ExpVarId GenNodeId a a NodeId
  | -- | expression, annotation node
    AAnnF a NodeId EdgeId
  | -- | expression, unfolded-type node, inst edge from expr to unfolded type
    AUnfoldF a NodeId EdgeId
  deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base AnnExpr = AnnExprF

instance Recursive AnnExpr where
  project expr = case expr of
    AVar v nid -> AVarF v nid
    AResolvedVar details v nid -> AResolvedVarF details v nid
    ALit l nid -> ALitF l nid
    ALam v details param scopeRoot body nid -> ALamF v details param scopeRoot body nid
    AApp fun arg funEid argEid nid -> AAppF fun arg funEid argEid nid
    ALet v details schemeGenId schemeRootId expVar scopeRoot rhs body nid ->
      ALetF v details schemeGenId schemeRootId expVar scopeRoot rhs body nid
    AAnn inner annNode eid -> AAnnF inner annNode eid
    AUnfold inner unfoldNode eid -> AUnfoldF inner unfoldNode eid

instance Corecursive AnnExpr where
  embed expr = case expr of
    AVarF v nid -> AVar v nid
    AResolvedVarF details v nid -> AResolvedVar details v nid
    ALitF l nid -> ALit l nid
    ALamF v details param scopeRoot body nid -> ALam v details param scopeRoot body nid
    AAppF fun arg funEid argEid nid -> AApp fun arg funEid argEid nid
    ALetF v details schemeGenId schemeRootId expVar scopeRoot rhs body nid ->
      ALet v details schemeGenId schemeRootId expVar scopeRoot rhs body nid
    AAnnF inner annNode eid -> AAnn inner annNode eid
    AUnfoldF inner unfoldNode eid -> AUnfold inner unfoldNode eid

data Binding = Binding
  { bindingNode :: NodeId,
    bindingGen :: Maybe GenNodeId,
    bindingIdentity :: Maybe IdDetails
  }
  | LazyExternalBinding
      { bindingExternalRoot :: GenNodeId,
        bindingExternal :: ExternalBinding
      }
  deriving (Eq, Show)

-- | Constraint-generation binding lookup is either explicitly metadata-light
-- (raw surface syntax) or keyed by the carried post-resolution identity.
-- Resolved lookups never fall back to the display spelling.
data BindingKey
  = MetadataLightBindingKey VarName
  | ResolvedBindingKey ResolvedTermIdentityKey
  deriving (Eq, Ord, Show)

bindingKeyForTermReference :: TermReference -> BindingKey
bindingKeyForTermReference reference =
  case reference of
    MetadataLightTermReference name -> MetadataLightBindingKey name
    ResolvedTermReference details _ -> ResolvedBindingKey (idDetailsIdentityKey details)

type Env = Map BindingKey Binding

data ExternalBindingMode
  = ExternalBindingScheme
  | ExternalBindingMonomorphic
  deriving (Eq, Show)

data ExternalBinding = ExternalBinding
  { externalBindingType :: NormSrcType,
    externalBindingMode :: ExternalBindingMode,
    externalBindingIdentity :: Maybe ExternalBindingIdentity,
    externalBindingTypeHeadIdentities :: Map String SymbolIdentity,
    externalBindingTypeBinderIdentities :: Map String TypeBinderIdentity
  }
  deriving (Eq, Show)

data ExternalBindingIdentity = ExternalBindingIdentity
  { externalBindingDetails :: IdDetails
  }
  deriving (Show)

externalBindingRuntimeName :: ExternalBindingIdentity -> String
externalBindingRuntimeName =
  idDetailsRuntimeName . externalBindingDetails

externalBindingIdentityFromDetails :: IdDetails -> ExternalBindingIdentity
externalBindingIdentityFromDetails details =
  ExternalBindingIdentity
    { externalBindingDetails = details
    }

externalBindingIdentityFromResolvedVar :: ResolvedVar -> ExternalBindingIdentity
externalBindingIdentityFromResolvedVar resolved =
  externalBindingIdentityFromDetails (resolvedVarDetails resolved)

externalBindingIdentityFromDeferredRef :: DeferredRef -> ExternalBindingIdentity
externalBindingIdentityFromDeferredRef ref =
  externalBindingIdentityFromDetails (DeferredId ref)

instance Eq ExternalBindingIdentity where
  left == right =
    externalBindingDetails left == externalBindingDetails right

-- | External environment: maps free variable names to their normalized
-- source types.  Used by 'generateConstraintsWithEnv' to inject
-- pre-existing type assumptions (e.g. from .mlfp program scope) into
-- constraint generation without wrapping the expression in ELamAnn.
type ExternalEnv = Map VarName NormSrcType

type ExternalBindings = Map VarName ExternalBinding

replaceScopeRoot :: GenNodeId -> GenNodeId -> AnnExpr -> AnnExpr
replaceScopeRoot from to = cata alg
  where
    repGen gid = if gid == from then to else gid
    alg ann = case ann of
      AVarF v nid -> AVar v nid
      AResolvedVarF details v nid -> AResolvedVar details v nid
      ALitF lit nid -> ALit lit nid
      ALamF v details param scopeRoot body nid ->
        ALam v details param (repGen scopeRoot) body nid
      AAppF fun arg funEid argEid nid ->
        AApp fun arg funEid argEid nid
      ALetF v details schemeNode schemeRootId expVar scopeRoot rhs body nid ->
        ALet v details (repGen schemeNode) schemeRootId expVar (repGen scopeRoot) rhs body nid
      AAnnF expr annNode eid ->
        AAnn expr annNode eid
      AUnfoldF expr unfoldNode eid ->
        AUnfold expr unfoldNode eid
