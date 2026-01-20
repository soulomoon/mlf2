module MLF.Frontend.ConstraintGen.Emit (
    allocForall,
    allocVar,
    allocBase,
    allocArrow,
    allocGenNode,
    setGenNodeSchemes,
    allocExpNode,
    setVarBound,
    addInstEdge,
    recordLetEdge,
    setBindParentIfMissing,
    setBindParentOverride,
    intFromNode
) where

import Control.Monad (when)
import Control.Monad.State.Strict (gets, modify')
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import MLF.Frontend.ConstraintGen.State (BuildState(..), ConstraintM)
import qualified MLF.Frontend.ConstraintGen.Scope as Scope

-- | Allocate a TyForall node.
allocForall :: NodeId -> ConstraintM NodeId
allocForall bodyNode = do
    nid <- freshNodeId
    insertNode TyForall
        { tnId = nid
        , tnBody = bodyNode
        }
    -- Set default binding edge for the body to this forall node,
    -- but only if it doesn't already have a binding parent.
    -- This ensures all non-root nodes have binding parents while preserving
    -- existing binding structure.
    setBindParentIfMissing (typeRef bodyNode) (typeRef nid) BindFlex
    pure nid

allocVar :: ConstraintM NodeId
allocVar = do
    nid <- freshNodeId
    let node = TyVar
            { tnId = nid
            , tnBound = Nothing
            }
    insertNode node
    -- Binding parent will be set by the caller
    pure nid

allocBase :: BaseTy -> ConstraintM NodeId
allocBase base = do
    nid <- freshNodeId
    insertNode TyBase
        { tnId = nid
        , tnBase = base
        }
    -- Binding parent will be set by the caller (or this is a root)
    pure nid

allocArrow :: NodeId -> NodeId -> ConstraintM NodeId
allocArrow domNode codNode = do
    nid <- freshNodeId
    insertNode TyArrow
        { tnId = nid
        , tnDom = domNode
        , tnCod = codNode
        }
    -- Set default binding edges for children (dom/cod) to this arrow node,
    -- but only if they don't already have binding parents.
    -- This ensures all non-root nodes have binding parents while preserving
    -- existing binding structure.
    setBindParentIfMissing (typeRef domNode) (typeRef nid) BindFlex
    setBindParentIfMissing (typeRef codNode) (typeRef nid) BindFlex
    pure nid

allocGenNode :: [NodeId] -> ConstraintM GenNodeId
allocGenNode schemes = do
    gid <- freshGenNodeId
    let genNode = GenNode { gnId = gid, gnSchemes = schemes }
    modify' $ \st ->
        let gens = IntMap.insert (genNodeKey gid) genNode (bsGenNodes st)
        in st { bsGenNodes = gens }
    Scope.registerScopeNode (genRef gid)
    pure gid

setGenNodeSchemes :: GenNodeId -> [NodeId] -> ConstraintM ()
setGenNodeSchemes gid schemes =
    modify' $ \st ->
        let gens0 = bsGenNodes st
            gens' = IntMap.adjust (\g -> g { gnSchemes = schemes }) (genNodeKey gid) gens0
        in st { bsGenNodes = gens' }

allocExpNode :: NodeId -> ConstraintM (NodeId, ExpVarId)
allocExpNode bodyNode = do
    expVar <- freshExpVarId
    nid <- freshNodeId
    insertNode TyExp
        { tnId = nid
        , tnExpVar = expVar
        , tnBody = bodyNode
        }
    -- Keep TyExp on the same binding-parent chain as its body so ga′ is stable.
    bindParents <- gets bsBindParents
    case IntMap.lookup (nodeRefKey (typeRef bodyNode)) bindParents of
        Just (parent, flag) -> setBindParentIfMissing (typeRef nid) parent flag
        Nothing -> pure ()
    pure (nid, expVar)

setVarBound :: NodeId -> Maybe NodeId -> ConstraintM ()
setVarBound varNode bound = do
    modify' $ \st ->
        let nodes0 = bsNodes st
            key = intFromNode varNode
        in case IntMap.lookup key nodes0 of
            Just tv@TyVar{} ->
                st { bsNodes = IntMap.insert key tv{ tnBound = bound } nodes0 }
            _ -> st
    case bound of
        Just bnd -> do
            setBindParentIfMissing (typeRef bnd) (typeRef varNode) BindFlex
        Nothing -> pure ()

addInstEdge :: NodeId -> NodeId -> ConstraintM EdgeId
addInstEdge left right = do
    eid <- freshEdgeId
    let edgeId = EdgeId eid
        edge = InstEdge edgeId left right
    modify' $ \st -> st { bsInstEdges = edge : bsInstEdges st }
    pure edgeId

-- | Record a let-scope instantiation edge so we can drop its witness later.
recordLetEdge :: EdgeId -> ConstraintM ()
recordLetEdge (EdgeId eid) =
    modify' $ \st ->
        st { bsLetEdges = IntSet.insert eid (bsLetEdges st) }

freshNodeId :: ConstraintM NodeId
freshNodeId = do
    next <- gets bsNextNode
    modify' $ \st -> st { bsNextNode = next + 1 }
    pure (NodeId next)

freshGenNodeId :: ConstraintM GenNodeId
freshGenNodeId = do
    next <- gets bsNextGen
    modify' $ \st -> st { bsNextGen = next + 1 }
    pure (GenNodeId next)

freshExpVarId :: ConstraintM ExpVarId
freshExpVarId = do
    next <- gets bsNextExpVar
    modify' $ \st -> st { bsNextExpVar = next + 1 }
    pure (ExpVarId next)

freshEdgeId :: ConstraintM Int
freshEdgeId = do
    next <- gets bsNextEdge
    modify' $ \st -> st { bsNextEdge = next + 1 }
    pure next

insertNode :: TyNode -> ConstraintM ()
insertNode node = do
    modify' $ \st ->
        let nodes' = IntMap.insert (intFromNode (tnId node)) node (bsNodes st)
        in st { bsNodes = nodes' }
    Scope.registerScopeNode (typeRef (tnId node))

-- | Set the binding parent for a node only if it doesn't already have one.
-- This is useful for structure allocators that want to set a default binding
-- parent without overwriting an existing one.
setBindParentIfMissing :: NodeRef -> NodeRef -> BindFlag -> ConstraintM ()
setBindParentIfMissing child parent flag =
    when (child /= parent) $
        modify' $ \st ->
            let bp = bsBindParents st
            in if IntMap.member (nodeRefKey child) bp
               then st  -- Already has a parent, don't overwrite
               else st { bsBindParents = IntMap.insert (nodeRefKey child) (parent, flag) bp }

setBindParentOverride :: NodeRef -> NodeRef -> BindFlag -> ConstraintM ()
setBindParentOverride child parent flag =
    when (child /= parent) $
        modify' $ \st ->
            let bp = bsBindParents st
            in st { bsBindParents = IntMap.insert (nodeRefKey child) (parent, flag) bp }

intFromNode :: NodeId -> Int
intFromNode (NodeId x) = x
