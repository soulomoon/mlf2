{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module MLF.Frontend.ConstraintGen.Types
  ( ConstraintError (..),
    ConstraintResult (..),
    ModuleRootId (..),
    RootOwnershipIndex (..),
    ModuleConstraintRoot (..),
    ModuleConstraintResult (..),
    InstantiationTargetTopology (..),
    InstantiationSite (..),
    mkInstantiationSite,
    mkArrowInstantiationSite,
    mapInstantiationSiteNodes,
    AnnExpr
      ( AResolvedVar,
        ALit,
        ALam,
        AApp,
        ALet,
        AAnn,
        AExactAnn,
        ALetScope,
        AUnfold
      ),
    AnnExprF
      ( AResolvedVarF,
        ALitF,
        ALamF,
        AAppF,
        ALetF,
        AAnnF,
        AExactAnnF,
        ALetScopeF,
        AUnfoldF
      ),
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
import MLF.Frontend.Syntax (Lit, NormSrcType, ResolvedSrcType, TermReference (..), TermReferencePhase (..), VarName)
import MLF.Types.Elab (ResolvedVar (..))
import MLF.Types.Identity (DeferredRef, IdDetails (..), IdentityGenerator, ResolvedTermIdentityKey, TypeBinderIdentity, idDetailsIdentityKey, idDetailsRuntimeName)

-- | Errors that can surface during constraint generation.
data ConstraintError
  = UnknownVariable VarName
  | UnknownTypeHead String
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
    -- | Program-level identity supply after term resolution and desugaring.
    -- Later synthetic identities must continue from this supply rather than
    -- reconstructing a local generator from a partial type view.
    crIdentityGenerator :: IdentityGenerator,
    -- | Authoritative source types keyed by AAnn codomain or compiler-owned
    -- exact-lambda parameter node. Preserves the lowered 'NormSrcType' so
    -- elaboration retains source binder identities and stripped type structure.
    crAnnSourceTypes :: IntMap.IntMap NormSrcType,
    -- | Authoritative producer types for compiler exact annotations, keyed by
    -- their construction-time edge.  Retaining the resolved source type makes
    -- exact-owner construction independent of later graph reconstruction.
    crExactProducerTypes :: IntMap.IntMap ResolvedSrcType,
    -- | Semantic identities for graph nodes that represent free source-type
    -- binders.  This is provenance for generalization, not part of the paper
    -- constraint itself.
    crSourceTypeBinderIdentities :: IntMap.IntMap TypeBinderIdentity,
    -- | Source aliases resolved while constructing this root.  Lower-level
    -- pipeline entrypoints can introduce a free annotation binder without a
    -- preceding program-resolution phase; retain its graph identity here so
    -- annotation elaboration consumes the same identity by construction.
    crSourceTypeBinderAliases :: Map String TypeBinderIdentity,
    -- | Initial bindings created for external environment variables.
    -- Each entry maps a resolved identity key to its identity-bearing
    -- 'Binding' (node + gen + carried identity).
    -- The pipeline uses this to seed the elaboration and type-check
    -- environments for free variables that were not wrapped in ELamAnn.
    crInitialEnv :: Env
  }
  deriving (Eq, Show)

data ModuleConstraintRoot = ModuleConstraintRoot
  { mcrRootId :: ModuleRootId,
    mcrRoot :: NodeId,
    mcrAnnotated :: AnnExpr,
    -- | Source aliases resolved while constructing this definition root.
    -- Keeping this provenance root-local prevents equal spellings in sibling
    -- definitions from being conflated during annotation elaboration.
    mcrSourceTypeBinderAliases :: Map String TypeBinderIdentity
  }
  deriving (Eq, Show)

data ModuleConstraintResult key p = ModuleConstraintResult
  { mcrConstraint :: Constraint p,
    mcrRoots :: Map key ModuleConstraintRoot,
    -- | Module-level identity supply after resolving/desugaring every root.
    mcrIdentityGenerator :: IdentityGenerator,
    mcrAnnSourceTypes :: IntMap.IntMap NormSrcType,
    mcrExactProducerTypes :: IntMap.IntMap ResolvedSrcType,
    mcrSourceTypeBinderIdentities :: IntMap.IntMap TypeBinderIdentity,
    mcrInitialEnv :: Env,
    mcrRootOwnership :: RootOwnershipIndex
  }
  deriving (Eq, Show)

-- | Construction-time authority for one term-owned instantiation edge.
--
-- Presolution may discharge an edge as identity (including normalization
-- grafting), at which point the edge no longer appears in @cInstEdges@ and no
-- replay witness is produced.  Keep the source and destination allocated by
-- constraint generation next to the term that owns the edge so elaboration
-- can still validate the exact paper topology instead of reconstructing it
-- from a solved type or from type-check failure.
data InstantiationTargetTopology
  = AtomicInstantiationTarget
  | ArrowInstantiationTarget
      { instantiationArrowAllocatedDomain :: NodeId,
        instantiationArrowAllocatedCodomain :: NodeId,
        instantiationArrowDomain :: NodeId,
        instantiationArrowCodomain :: NodeId
      }
  deriving (Eq, Show)

data InstantiationSite = InstantiationSite
  { instantiationSiteEdgeId :: EdgeId,
    -- | Stable allocation-domain endpoints.  These retain the structural
    -- topology owned by constraint generation even when redirects/UF make
    -- the prepared endpoints equal.
    instantiationSiteAllocatedSource :: NodeId,
    instantiationSiteAllocatedTarget :: NodeId,
    -- | Redirected/canonical endpoints used to validate prepared replay
    -- artifacts.
    instantiationSiteSource :: NodeId,
    instantiationSiteTarget :: NodeId,
    instantiationSiteTargetTopology :: InstantiationTargetTopology
  }
  deriving (Eq, Show)

mkInstantiationSite :: EdgeId -> NodeId -> NodeId -> InstantiationSite
mkInstantiationSite edgeId source target =
  InstantiationSite
    { instantiationSiteEdgeId = edgeId,
      instantiationSiteAllocatedSource = source,
      instantiationSiteAllocatedTarget = target,
      instantiationSiteSource = source,
      instantiationSiteTarget = target,
      instantiationSiteTargetTopology = AtomicInstantiationTarget
    }

mkArrowInstantiationSite :: EdgeId -> NodeId -> NodeId -> NodeId -> NodeId -> InstantiationSite
mkArrowInstantiationSite edgeId source target domain codomain =
  (mkInstantiationSite edgeId source target)
    { instantiationSiteTargetTopology =
        ArrowInstantiationTarget
          { instantiationArrowAllocatedDomain = domain,
            instantiationArrowAllocatedCodomain = codomain,
            instantiationArrowDomain = domain,
            instantiationArrowCodomain = codomain
          }
    }

mapInstantiationSiteNodes :: (NodeId -> NodeId) -> InstantiationSite -> InstantiationSite
mapInstantiationSiteNodes f site =
  site
    { instantiationSiteSource = f (instantiationSiteSource site),
      instantiationSiteTarget = f (instantiationSiteTarget site),
      instantiationSiteTargetTopology =
        case instantiationSiteTargetTopology site of
          AtomicInstantiationTarget -> AtomicInstantiationTarget
          topology@ArrowInstantiationTarget {} ->
            topology
              { instantiationArrowDomain = f (instantiationArrowDomain topology),
                instantiationArrowCodomain = f (instantiationArrowCodomain topology)
              }
    }

-- | Expression annotated with the NodeIds allocated during constraint generation.
-- The NodeIds are stable and match the ones found in the constraint graph, so
-- later phases (e.g., elaboration) can recover binder types.
data AnnExpr
  = -- | Exact resolved identity, display/runtime spelling, occurrence node.
    AResolvedVar IdDetails VarName NodeId
  | ALit Lit NodeId
  | -- | param name, resolved identity, param node, scope root (gen), body, body inst edge id, result node
    ALam VarName IdDetails NodeId GenNodeId AnnExpr EdgeId NodeId
  | -- | fun, arg, function edge site, argument edge site, result node
    AApp AnnExpr AnnExpr InstantiationSite InstantiationSite NodeId
  | -- | binder name, resolved identity, scheme gen node, scheme root, expansion var, RHS scope gen, rhs, body, result node
    ALet VarName IdDetails GenNodeId NodeId ExpVarId GenNodeId AnnExpr AnnExpr NodeId
  | -- | expression, annotation authority, annotation node
    AAnnNode AnnotationAuthority AnnExpr NodeId EdgeId
  | -- | let body, trivial result node, constraint-only identity edge
    ALetScope AnnExpr NodeId EdgeId
  | -- | expression, unfolded-type node, inst edge from expr to unfolded type
    AUnfold AnnExpr NodeId EdgeId
  deriving (Eq, Show)

data AnnExprF a
  = AResolvedVarF IdDetails VarName NodeId
  | ALitF Lit NodeId
  | -- | param name, resolved identity, param node, scope root (gen), body, body inst edge id, result node
    ALamF VarName IdDetails NodeId GenNodeId a EdgeId NodeId
  | -- | fun, arg, function edge site, argument edge site, result node
    AAppF a a InstantiationSite InstantiationSite NodeId
  | -- | binder name, resolved identity, scheme gen node, scheme root, expansion var, RHS scope gen, rhs, body, result node
    ALetF VarName IdDetails GenNodeId NodeId ExpVarId GenNodeId a a NodeId
  | -- | expression, annotation authority, annotation node
    AAnnNodeF AnnotationAuthority a NodeId EdgeId
  | -- | let body, trivial result node, constraint-only identity edge
    ALetScopeF a NodeId EdgeId
  | -- | expression, unfolded-type node, inst edge from expr to unfolded type
    AUnfoldF a NodeId EdgeId
  deriving (Eq, Show, Functor, Foldable, Traversable)

data AnnotationAuthority
  = SourceAnnotationAuthority
  | CompilerExactAnnotationAuthority ResolvedSrcType
  deriving (Eq, Show)

-- | Source annotation. Matching is intentionally authority-polymorphic so
-- existing structural traversals handle compiler exact annotations exactly as
-- annotations; construction always creates the source kappa form.
pattern AAnn :: AnnExpr -> NodeId -> EdgeId -> AnnExpr
pattern AAnn inner annNode eid <- AAnnNode _ inner annNode eid
  where
    AAnn inner annNode eid = AAnnNode SourceAnnotationAuthority inner annNode eid

pattern AExactAnn :: AnnExpr -> ResolvedSrcType -> NodeId -> EdgeId -> AnnExpr
pattern AExactAnn inner exactTy annNode eid =
  AAnnNode (CompilerExactAnnotationAuthority exactTy) inner annNode eid

pattern AAnnF :: a -> NodeId -> EdgeId -> AnnExprF a
pattern AAnnF inner annNode eid <- AAnnNodeF _ inner annNode eid
  where
    AAnnF inner annNode eid = AAnnNodeF SourceAnnotationAuthority inner annNode eid

pattern AExactAnnF :: a -> ResolvedSrcType -> NodeId -> EdgeId -> AnnExprF a
pattern AExactAnnF inner exactTy annNode eid =
  AAnnNodeF (CompilerExactAnnotationAuthority exactTy) inner annNode eid

{-# COMPLETE AResolvedVar, ALit, ALam, AApp, ALet, AAnn, ALetScope, AUnfold #-}
{-# COMPLETE AResolvedVarF, ALitF, ALamF, AAppF, ALetF, AAnnF, ALetScopeF, AUnfoldF #-}

type instance Base AnnExpr = AnnExprF

instance Recursive AnnExpr where
  project expr = case expr of
    AResolvedVar details v nid -> AResolvedVarF details v nid
    ALit l nid -> ALitF l nid
    ALam v details param scopeRoot body bodyEid nid -> ALamF v details param scopeRoot body bodyEid nid
    AApp fun arg funEid argEid nid -> AAppF fun arg funEid argEid nid
    ALet v details schemeGenId schemeRootId expVar scopeRoot rhs body nid ->
      ALetF v details schemeGenId schemeRootId expVar scopeRoot rhs body nid
    AAnnNode authority inner annNode eid -> AAnnNodeF authority inner annNode eid
    ALetScope inner resultNode eid -> ALetScopeF inner resultNode eid
    AUnfold inner unfoldNode eid -> AUnfoldF inner unfoldNode eid

instance Corecursive AnnExpr where
  embed expr = case expr of
    AResolvedVarF details v nid -> AResolvedVar details v nid
    ALitF l nid -> ALit l nid
    ALamF v details param scopeRoot body bodyEid nid -> ALam v details param scopeRoot body bodyEid nid
    AAppF fun arg funEid argEid nid -> AApp fun arg funEid argEid nid
    ALetF v details schemeGenId schemeRootId expVar scopeRoot rhs body nid ->
      ALet v details schemeGenId schemeRootId expVar scopeRoot rhs body nid
    AAnnNodeF authority inner annNode eid -> AAnnNode authority inner annNode eid
    ALetScopeF inner resultNode eid -> ALetScope inner resultNode eid
    AUnfoldF inner unfoldNode eid -> AUnfold inner unfoldNode eid

data Binding = Binding
  { bindingNode :: NodeId,
    bindingGen :: Maybe GenNodeId,
    bindingIdentity :: IdDetails
  }
  | LazyExternalBinding
      { bindingExternalRoot :: GenNodeId,
        bindingExternal :: ExternalBinding
      }
  deriving (Eq, Show)

-- | Constraint-generation binding lookup is keyed only by the carried
-- post-resolution identity. It never falls back to display spelling.
newtype BindingKey
  = ResolvedBindingKey ResolvedTermIdentityKey
  deriving (Eq, Ord, Show)

bindingKeyForTermReference :: TermReference 'ResolvedTermReferences -> BindingKey
bindingKeyForTermReference (ResolvedTermReference details _) =
  ResolvedBindingKey (idDetailsIdentityKey details)

type Env = Map BindingKey Binding

data ExternalBindingMode
  = ExternalBindingScheme
  | ExternalBindingMonomorphic
  deriving (Eq, Show)

data ExternalBinding = ExternalBinding
  { externalBindingType :: NormSrcType,
    externalBindingMode :: ExternalBindingMode,
    externalBindingIdentity :: ExternalBindingIdentity,
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
      AResolvedVarF details v nid -> AResolvedVar details v nid
      ALitF lit nid -> ALit lit nid
      ALamF v details param scopeRoot body bodyEid nid ->
        ALam v details param (repGen scopeRoot) body bodyEid nid
      AAppF fun arg funEid argEid nid ->
        AApp fun arg funEid argEid nid
      ALetF v details schemeNode schemeRootId expVar scopeRoot rhs body nid ->
        ALet v details (repGen schemeNode) schemeRootId expVar (repGen scopeRoot) rhs body nid
      AExactAnnF expr exactTy annNode eid ->
        AExactAnn expr exactTy annNode eid
      AAnnF expr annNode eid ->
        AAnn expr annNode eid
      ALetScopeF expr resultNode eid ->
        ALetScope expr resultNode eid
      AUnfoldF expr unfoldNode eid ->
        AUnfold expr unfoldNode eid
