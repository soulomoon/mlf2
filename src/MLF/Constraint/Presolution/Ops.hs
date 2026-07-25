{- |
Module      : MLF.Constraint.Presolution.Ops
Description : Low-level stateful operations for presolution

This module is a small “foundation” layer for presolution submodules. It
contains stateful operations on `PresolutionState` / `Constraint p` shared by
the public presolution entrypoint and its owner modules.

Keeping these operations separate makes it easier to keep those modules
cohesive without introducing import cycles.
-}
module MLF.Constraint.Presolution.Ops (
    createFreshNodeId,
    registerNode,
    setBindParentM,
    rebindWithBoundRepairTrace,
    getNode,
    getCanonicalNode,
    findRoot,
    lookupVarBound,
    createFreshVar,
    setVarBound,
    setVarBoundWithRaiseTrace,
    setCanonicalVarBound,
    setCanonicalVarBoundWithRaiseTrace,
    setCanonicalVarBoundForEdgeWithRaiseTrace,
    raiseFreeBoundFrontierToVariableScope,
    repairAllVarBoundScopes,
    validateLowerBoundGraph,
    dropVarBind
) where

import Control.Monad (foldM, void)
import Control.Monad.State (get, gets, modify', put)
import Control.Monad.Except (throwError)
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.UnionFind as UnionFind
import MLF.Constraint.Types.Graph
import MLF.Constraint.Presolution.BoundScope
    ( changedBindParentRefs
    , raiseFreeBoundFrontierToVariableScope
    , raiseFreeBoundFrontierToVariableScopeForEdge
    , repairAllVarBoundScopes
    )
import MLF.Constraint.Presolution.Base
    ( PresolutionM
    , PresolutionError(..)
    , PresolutionState(..)
    , compressUnionFindState
    , modifyConstraintDirtyTypesState
    , setConstraintDirtyBindRefsState
    )

-- | Allocate a fresh NodeId from the presolution state counter.
createFreshNodeId :: PresolutionM p NodeId
createFreshNodeId = do
    st <- get
    let nid = NodeId (psNextNodeId st)
    put $ st { psNextNodeId = psNextNodeId st + 1 }
    pure nid

-- | Register a node in the constraint’s node map.
registerNode :: NodeId -> TyNode -> PresolutionM p ()
registerNode nid node =
    modify' $ \st ->
        let c0 = psConstraint st
            nodes' = insertNode nid node (cNodes c0)
            dirty =
                IntSet.fromList $
                    getNodeId nid : map getNodeId (structuralChildrenWithBounds node)
        in modifyConstraintDirtyTypesState dirty (\c -> c { cNodes = nodes' }) st

-- | Set a binding parent for a node in the constraint.
--
-- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1):
-- this mutates the explicit binding tree relation `cBindParents`.
setBindParentM :: NodeRef -> (NodeRef, BindFlag) -> PresolutionM p ()
setBindParentM child parentInfo =
    void (rebindWithBoundRepairTrace child parentInfo)

-- | Atomically rebind a component and repair every bounded variable whose
-- scope is affected.  Unlike bound installation, a genuine scope move may
-- Raise a free lower-bound frontier to the LCA of the old and new scopes.
rebindWithBoundRepairTrace
    :: NodeRef
    -> (NodeRef, BindFlag)
    -> PresolutionM p [NodeId]
rebindWithBoundRepairTrace child (parent, flag) = do
    st <- get
    let c0 = psConstraint st
        canonical = UnionFind.frWith (psUnionFind st)
        childC = Canonicalize.canonicalRef canonical child
        parentC = Canonicalize.canonicalRef canonical parent
        cCandidate = Binding.setBindParent childC (parentC, flag) c0
    case Binding.bindingPathToRoot cCandidate childC of
        Left err -> throwError (BindingTreeError err)
        Right _ -> pure ()
    (cScoped, raiseTrace) <-
        case repairAllVarBoundScopes canonical cCandidate of
            Left err -> throwError (BindingTreeError err)
            Right result -> pure result
    let dirtyBindRefs = changedBindParentRefs c0 cScoped
    if IntSet.null dirtyBindRefs
        then pure raiseTrace
        else do
            put (setConstraintDirtyBindRefsState dirtyBindRefs cScoped st)
            pure raiseTrace

-- | Lookup a node in the term-DAG or fail.
{-# INLINE getNode #-}
getNode :: NodeId -> PresolutionM p TyNode
getNode nid = do
    nodes <- gets (cNodes . psConstraint)
    case lookupNode nid nodes of
        Just n -> pure n
        Nothing -> throwError $ NodeLookupFailed nid

-- | Find the canonical representative of a node (with path compression).
{-# INLINE findRoot #-}
findRoot :: NodeId -> PresolutionM p NodeId
findRoot nid = do
    uf <- gets psUnionFind
    let (root, uf') = UnionFind.findRootWithCompression uf nid
    modify' (compressUnionFindState uf')
    pure root

-- | Lookup a node at its current canonical representative.
{-# INLINE getCanonicalNode #-}
getCanonicalNode :: NodeId -> PresolutionM p TyNode
getCanonicalNode nid = do
    rootId <- findRoot nid
    nodes <- gets (cNodes . psConstraint)
    case lookupNode rootId nodes of
        Just node -> pure node
        Nothing -> throwError $ NodeLookupFailed rootId

-- | Lookup the instance bound of a variable (⊥ represented as 'Nothing').
--
-- Uses canonical representatives so aliasing through UF stays coherent.
{-# INLINE lookupVarBound #-}
lookupVarBound :: NodeId -> PresolutionM p (Maybe NodeId)
lookupVarBound bv = do
    root <- findRoot bv
    c <- gets psConstraint
    pure (VarStore.lookupVarBound c root)

-- | Helper to create a fresh variable node.
createFreshVar :: PresolutionM p NodeId
createFreshVar = do
    nid <- createFreshNodeId
    let node = TyVar { tnId = nid, tnBound = Nothing }
    registerNode nid node
    pure nid

-- | Update the instance bound of a type variable, intentionally discarding
-- the Raise trace.  Callers that construct edge-local Ω must use
-- 'setVarBoundWithRaiseTrace' and record the returned operations.
--
-- Missing keys are treated as ⊥; updates use UF roots so bounds follow
-- canonical representatives.
setVarBound :: NodeId -> Maybe NodeId -> PresolutionM p ()
setVarBound vid mb = void (setVarBoundWithRaiseTrace vid mb)

-- | Canonicalize a variable-bound update and install it only after every free
-- node reachable through the lower bound has been raised to the variable's
-- scope.  This makes @v >= τ@ well-scoped at construction time in both
-- operation orders: setting a bound after Raise and raising an already bounded
-- variable.
setVarBoundWithRaiseTrace :: NodeId -> Maybe NodeId -> PresolutionM p [NodeId]
setVarBoundWithRaiseTrace vid mb = do
    root <- findRoot vid
    mbRoot <- mapM findRoot mb
    setCanonicalVarBoundWithRaiseTrace root mbRoot

-- | Update a canonical variable bound, intentionally discarding the Raise
-- trace.  This wrapper is for presolution construction outside edge-local Ω.
setCanonicalVarBound :: NodeId -> Maybe NodeId -> PresolutionM p ()
setCanonicalVarBound root mbRoot =
    void (setCanonicalVarBoundWithRaiseTrace root mbRoot)

-- | Atomic, traced variant of 'setCanonicalVarBound'.  Binding-tree changes
-- are computed before the bound is written, so a scope error cannot leave an
-- invalid @TyVar.tnBound@ behind.
setCanonicalVarBoundWithRaiseTrace
    :: NodeId
    -> Maybe NodeId
    -> PresolutionM p [NodeId]
setCanonicalVarBoundWithRaiseTrace =
    setCanonicalVarBoundWithScopeRepair raiseFreeBoundFrontierToVariableScope

-- | Install an edge-local bound and return every cross-scope Raise needed to
-- make it well scoped.  The edge-unification owner must record the returned
-- nodes in the propagation witness; ordinary callers use the conservative
-- 'setCanonicalVarBoundWithRaiseTrace' seam above.
setCanonicalVarBoundForEdgeWithRaiseTrace
    :: NodeId
    -> Maybe NodeId
    -> PresolutionM p [NodeId]
setCanonicalVarBoundForEdgeWithRaiseTrace =
    setCanonicalVarBoundWithScopeRepair
        raiseFreeBoundFrontierToVariableScopeForEdge

setCanonicalVarBoundWithScopeRepair
    :: ( (NodeId -> NodeId)
         -> NodeId
         -> NodeId
         -> Constraint p
         -> Either BindingError (Constraint p, [NodeId])
       )
    -> NodeId
    -> Maybe NodeId
    -> PresolutionM p [NodeId]
setCanonicalVarBoundWithScopeRepair repairScope root mbRoot = do
    st <- get
    let c0 = psConstraint st
        canonical = UnionFind.frWith (psUnionFind st)
        rootC = canonical root
        mbRootC = fmap canonical mbRoot
        nodes = cNodes c0
    case lookupNode rootC nodes of
        Nothing -> throwError (NodeLookupFailed rootC)
        Just TyVar{} -> do
            case mbRootC of
                Nothing -> pure ()
                Just boundRoot ->
                    case validateLowerBoundGraph canonical nodes rootC boundRoot of
                        Left err -> throwError err
                        Right () -> pure ()
            (cScoped, raiseTrace) <-
                case mbRootC of
                    Nothing -> pure (c0, [])
                    Just boundRoot ->
                        case repairScope canonical rootC boundRoot c0 of
                            Left err -> throwError (BindingTreeError err)
                            Right result -> pure result
            let cBound = VarStore.setVarBound rootC mbRootC cScoped
                dirtyTypes =
                    IntSet.fromList $
                        getNodeId rootC : maybe [] ((: []) . getNodeId) mbRootC
                stTypes =
                    modifyConstraintDirtyTypesState dirtyTypes (const cBound) st
                dirtyBindRefs = changedBindParentRefs c0 cScoped
                st' =
                    if IntSet.null dirtyBindRefs
                        then stTypes
                        else setConstraintDirtyBindRefsState dirtyBindRefs cBound stTypes
            put st'
            pure raiseTrace
        Just node -> throwError (BoundTargetNotTyVar rootC node)

-- | Check the complete lower-bound graph before publishing a bound.  Bound
-- edges participate in the occurs check: accepting @a >= b, b >= a@ would
-- create a cycle even though neither variable occurs in the other's ordinary
-- structural children.
validateLowerBoundGraph
    :: (NodeId -> NodeId)
    -> NodeMap TyNode
    -> NodeId
    -> NodeId
    -> Either PresolutionError ()
validateLowerBoundGraph canonical nodes variable0 boundRoot0 =
    void (go IntSet.empty boundRoot0)
  where
    variable = canonical variable0

    go visited nid0 =
        let nid = canonical nid0
            key = getNodeId nid
        in if nid == variable
            then Left (OccursCheckPresolution variable (canonical boundRoot0))
            else if IntSet.member key visited
                then Right visited
                else case lookupNode nid nodes of
                    Nothing -> Left (NodeLookupFailed nid)
                    Just node ->
                        foldM
                            go
                            (IntSet.insert key visited)
                            (structuralChildrenWithBounds node)

-- | Mark a type variable as eliminated so elaboration will not re-quantify it.
dropVarBind :: NodeId -> PresolutionM p ()
dropVarBind vid = do
    node <- getNode vid
    case node of
        TyVar{} ->
            modify' $ \st ->
                let c0 = psConstraint st
                    c1 = VarStore.markEliminatedVar vid c0
                    dirty = IntSet.singleton (getNodeId vid)
                in modifyConstraintDirtyTypesState dirty (const c1) st
        _ -> pure ()
