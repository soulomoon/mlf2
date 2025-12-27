module MLF.Frontend.ConstraintGen.Scope (
    pushScope,
    popScope,
    peekScope,
    registerScopeNode,
    rebindScopeNodes
) where

import Control.Monad.State.Strict (get, gets, modify', put)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import qualified MLF.Constraint.Traversal as Traversal
import MLF.Frontend.ConstraintGen.State (BuildState(..), ConstraintM, ScopeFrame(..))

pushScope :: ConstraintM ()
pushScope =
    modify' $ \st -> st { bsScopes = ScopeFrame IntSet.empty : bsScopes st }

popScope :: ConstraintM ScopeFrame
popScope = do
    st <- get
    case bsScopes st of
        [] -> error "popScope: empty scope stack"
        (frame:rest) -> do
            put st { bsScopes = rest }
            pure frame

peekScope :: ConstraintM ScopeFrame
peekScope = do
    st <- get
    case bsScopes st of
        [] -> error "peekScope: empty scope stack"
        (frame:_) -> pure frame

registerScopeNode :: NodeId -> ConstraintM ()
registerScopeNode nid =
    modify' $ \st ->
        case bsScopes st of
            [] -> st
            (frame:rest) ->
                let frame' = frame { sfNodes = IntSet.insert (getNodeId nid) (sfNodes frame) }
                in st { bsScopes = frame' : rest }

rebindScopeNodes :: NodeId -> NodeId -> ScopeFrame -> ConstraintM ()
rebindScopeNodes binder root frame = do
    nodes <- gets bsNodes
    let reachable =
            Traversal.reachableFromUnderLenient
                id
                (\nid -> IntMap.lookup (getNodeId nid) nodes)
                root
        scopeNodes = IntSet.intersection reachable (sfNodes frame)
        binderId = getNodeId binder
    modify' $ \st ->
        st
            { bsBindParents =
                IntSet.foldl'
                    (\bp nid ->
                        if nid == binderId
                            then bp
                            else IntMap.insert nid (binder, BindFlex) bp
                    )
                    (bsBindParents st)
                    scopeNodes
            }

