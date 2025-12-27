module MLF.Frontend.ConstraintGen.Emit (
    allocForall,
    allocVar,
    allocBase,
    allocArrow,
    allocExpNode,
    setVarBound,
    addInstEdge,
    setBindParentIfMissing,
    intFromNode
) where

import Control.Monad (when)
import Control.Monad.State.Strict (gets, modify')
import qualified Data.IntMap.Strict as IntMap

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
    setBindParentIfMissing bodyNode nid BindFlex
    pure nid

allocVar :: ConstraintM NodeId
allocVar = do
    nid <- freshNodeId
    let node = TyVar
            { tnId = nid
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
    setBindParentIfMissing domNode nid BindFlex
    setBindParentIfMissing codNode nid BindFlex
    pure nid

allocExpNode :: NodeId -> ConstraintM (NodeId, ExpVarId)
allocExpNode bodyNode = do
    expVar <- freshExpVarId
    nid <- freshNodeId
    insertNode TyExp
        { tnId = nid
        , tnExpVar = expVar
        , tnBody = bodyNode
        }
    -- Set default binding edge for the body to this TyExp node, but only if
    -- the body doesn't already have a binding parent. This is important because
    -- TyExp nodes wrap existing nodes that may already have binding structure.
    setBindParentIfMissing bodyNode nid BindFlex
    pure (nid, expVar)

setVarBound :: NodeId -> Maybe NodeId -> ConstraintM ()
setVarBound varNode bound = modify' $ \st ->
    st { bsVarBounds = IntMap.insert (intFromNode varNode) bound (bsVarBounds st) }

addInstEdge :: NodeId -> NodeId -> ConstraintM EdgeId
addInstEdge left right = do
    eid <- freshEdgeId
    let edgeId = EdgeId eid
        edge = InstEdge edgeId left right
    modify' $ \st -> st { bsInstEdges = edge : bsInstEdges st }
    pure edgeId

freshNodeId :: ConstraintM NodeId
freshNodeId = do
    next <- gets bsNextNode
    modify' $ \st -> st { bsNextNode = next + 1 }
    pure (NodeId next)

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
        st { bsNodes = IntMap.insert (intFromNode (tnId node)) node (bsNodes st) }
    Scope.registerScopeNode (tnId node)

-- | Set the binding parent for a node only if it doesn't already have one.
-- This is useful for structure allocators that want to set a default binding
-- parent without overwriting an existing one.
setBindParentIfMissing :: NodeId -> NodeId -> BindFlag -> ConstraintM ()
setBindParentIfMissing child parent flag =
    when (child /= parent) $
        modify' $ \st ->
            let bp = bsBindParents st
            in if IntMap.member (intFromNode child) bp
               then st  -- Already has a parent, don't overwrite
               else st { bsBindParents = IntMap.insert (intFromNode child) (parent, flag) bp }

intFromNode :: NodeId -> Int
intFromNode (NodeId x) = x

