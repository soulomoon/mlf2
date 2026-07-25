module MLF.Frontend.ConstraintGen.State
  ( ScopeFrame (..),
    BuildState (..),
    ConstraintM,
    runConstraintM,
    mkInitialState,
    mkInitialStateWithPolySyms,
    resolveTypeHeadIdentity,
    buildConstraint,
    withModuleRootOwner,
    recordNodeRootOwner,
    recordGenRootOwner,
    recordExpVarRootOwner,
    recordEdgeRootOwner,
  )
where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State.Strict (StateT, gets, modify', runStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.RootOwnership
import MLF.Constraint.Types.Graph
import MLF.Frontend.ConstraintGen.Types (Binding, BindingKey, ConstraintError (UnknownTypeHead))
import MLF.Frontend.Symbol (SymbolIdentity)
import MLF.Frontend.Syntax (NormSrcType, ResolvedSrcType)
import MLF.Types.Identity (TypeBinderIdentity)

data ScopeFrame = ScopeFrame
  { sfNodes :: !IntSet.IntSet
  }

data BuildState = BuildState
  { -- | Next available NodeId
    bsNextNode :: !Int,
    -- | Next available GenNodeId
    bsNextGen :: !Int,
    -- | Next available ExpVarId
    bsNextExpVar :: !Int,
    -- | Next available EdgeId
    bsNextEdge :: !Int,
    -- | Resolved type heads available while translating annotations.
    bsTypeHeadIdentities :: !(Map.Map String SymbolIdentity),
    -- | Resolved source type binders available while translating annotations.
    bsTypeBinderIdentities :: !(Map.Map String TypeBinderIdentity),
    -- | One graph node per source binder and definition root.
    bsTypeBinderNodes :: !(Map.Map (GenNodeId, TypeBinderIdentity) NodeId),
    -- | Source identity carried by each graph node allocated for, or proven by
    -- an annotation boundary to denote, a source-type binder, and by the
    -- forall/mu node that owns a bound binder. Constraint solving may rename or
    -- copy any of those representations; this sidecar lets root generalization
    -- restore the semantic binder identity instead of manufacturing a
    -- NodeId-derived replacement.
    bsTypeBinderNodeIdentities :: !(IntMap.IntMap TypeBinderIdentity),
    -- | Definition root that owns source-level free type binders.
    bsTypeBinderRoot :: !(Maybe GenNodeId),
    -- | Map of all allocated type nodes
    bsNodes :: !(IntMap.IntMap TyNode),
    -- | Instantiation edges (accumulated in reverse)
    bsInstEdges :: ![InstEdge],
    -- | Unification edges (accumulated in reverse)
    bsUnifyEdges :: ![UnifyEdge],
    -- | Binding edges: child -> (parent, flag)
    bsBindParents :: !BindParents,
    -- | Gen nodes (paper G constructors)
    bsGenNodes :: !(GenNodeMap GenNode),
    -- | Polymorphic type constructors (paper Poly)
    bsPolySyms :: !PolySyms,
    -- | Stack of scopes tracking newly created nodes
    bsScopes :: ![ScopeFrame],
    -- | Let-scope instantiation edges (body → trivial root)
    bsLetEdges :: !IntSet.IntSet,
    -- | Authoritative producer types for compiler exact annotations, keyed by
    -- their construction-time instantiation edge.  This is provenance rather
    -- than a paper constraint flag: later root partitioning must retain the
    -- exact producer even when the wrapper is outside the retained term root.
    bsExactProducerTypes :: !(IntMap.IntMap ResolvedSrcType),
    -- | Arity of each type constructor (paper Σ arity function)
    bsTyConArity :: !(Map.Map BaseTy Int),
    -- | Authoritative source types keyed by their graph node. Source κσ
    -- annotations use the AAnn codomain; compiler-owned exact lambdas use the
    -- parameter node. Preserving the lowered 'NormSrcType' lets elaboration
    -- retain binder identities and types that presolution may strip (e.g.
    -- TForall inside a μ body).
    bsAnnSourceTypes :: !(IntMap.IntMap NormSrcType),
    -- | External assumptions materialized selectively for referenced free
    -- variables.  The initial environment keeps compact lazy entries; once a
    -- variable is needed, its graph binding is cached here so later
    -- occurrences share the same scheme graph within this definition.
    bsExternalBindingCache :: !(Map.Map BindingKey Binding),
    -- | Current module root being generated.  This is only populated by the
    -- diagnostic multi-root .mlfp checker path.
    bsCurrentRootOwner :: !(Maybe ModuleRootId),
    -- | Side-channel ownership index for module-local batching.  It is kept
    -- outside 'Constraint' so ordinary graph fixtures and golden Show output stay
    -- unchanged.
    bsRootOwnership :: !RootOwnershipIndex
  }

type ConstraintM = StateT BuildState (Except ConstraintError)

runConstraintM :: ConstraintM a -> BuildState -> Either ConstraintError (a, BuildState)
runConstraintM action st = runExcept (runStateT action st)

mkInitialState :: BuildState
mkInitialState = mkInitialStateWithPolySyms Set.empty

mkInitialStateWithPolySyms :: PolySyms -> BuildState
mkInitialStateWithPolySyms polySyms =
  BuildState
    { bsNextNode = 0,
      bsNextGen = 0,
      bsNextExpVar = 0,
      bsNextEdge = 0,
      bsTypeHeadIdentities = Map.empty,
      bsTypeBinderIdentities = Map.empty,
      bsTypeBinderNodes = Map.empty,
      bsTypeBinderNodeIdentities = IntMap.empty,
      bsTypeBinderRoot = Nothing,
      bsNodes = IntMap.empty,
      bsInstEdges = [],
      bsUnifyEdges = [],
      bsBindParents = IntMap.empty,
      bsGenNodes = fromListGen [],
      bsPolySyms = polySyms,
      bsScopes = [ScopeFrame IntSet.empty],
      bsLetEdges = IntSet.empty,
      bsExactProducerTypes = IntMap.empty,
      bsTyConArity = Map.empty,
      bsAnnSourceTypes = IntMap.empty,
      bsExternalBindingCache = Map.empty,
      bsCurrentRootOwner = Nothing,
      bsRootOwnership = emptyRootOwnershipIndex
    }

resolveTypeHeadIdentity :: String -> ConstraintM SymbolIdentity
resolveTypeHeadIdentity name = do
  identities <- gets bsTypeHeadIdentities
  case Map.lookup name identities of
    Just identity -> pure identity
    Nothing -> throwError (UnknownTypeHead name)

buildConstraint :: BuildState -> Constraint p
buildConstraint st =
  Constraint
    { cNodes = NodeMap (bsNodes st),
      cInstEdges = reverse (bsInstEdges st),
      cUnifyEdges = reverse (bsUnifyEdges st),
      cBindParents = bsBindParents st,
      cPolySyms = bsPolySyms st,
      cEliminatedVars = IntSet.empty,
      cWeakenedVars = IntSet.empty,
      cAnnEdges = IntSet.empty,
      cLetEdges = bsLetEdges st,
      cGraftedEdges = IntSet.empty,
      cGraftResultConstructions = IntMap.empty,
      cGenNodes = bsGenNodes st
    }

withModuleRootOwner :: ModuleRootId -> ConstraintM a -> ConstraintM a
withModuleRootOwner owner action = do
  oldOwner <- gets bsCurrentRootOwner
  modify' $ \st ->
    st
      { bsCurrentRootOwner = Just owner,
        bsRootOwnership = ensureRootOwner owner (bsRootOwnership st)
      }
  out <- action
  modify' $ \st -> st {bsCurrentRootOwner = oldOwner}
  pure out

recordNodeRootOwner :: NodeId -> ConstraintM ()
recordNodeRootOwner nid =
  recordCurrentRootOwner $ \owner -> insertNodeOwner owner (getNodeId nid)

recordGenRootOwner :: GenNodeId -> ConstraintM ()
recordGenRootOwner gid =
  recordCurrentRootOwner $ \owner -> insertGenOwner owner (getGenNodeId gid)

recordExpVarRootOwner :: ExpVarId -> ConstraintM ()
recordExpVarRootOwner expVar =
  recordCurrentRootOwner $ \owner -> insertExpVarOwner owner (getExpVarId expVar)

recordEdgeRootOwner :: EdgeId -> ConstraintM ()
recordEdgeRootOwner edgeId =
  recordCurrentRootOwner $ \owner -> insertEdgeOwner owner (getEdgeId edgeId)

recordCurrentRootOwner :: (ModuleRootId -> RootOwnershipIndex -> RootOwnershipIndex) -> ConstraintM ()
recordCurrentRootOwner insertOwner = do
  mbOwner <- gets bsCurrentRootOwner
  case mbOwner of
    Nothing -> pure ()
    Just owner ->
      modify' $ \st ->
        st
          { bsRootOwnership =
              insertOwner owner (bsRootOwnership st)
          }
