{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Solve
Description : Shared solve/unify helpers for edge processing
Copyright   : (c) 2024
License     : BSD-3-Clause

Core solve and unify operations used by both the edge processing orchestrator
and the interpreter. Extracted to break the module cycle between EdgeProcessing
and Interpreter.
-}
module MLF.Constraint.Presolution.EdgeProcessing.Solve (
    unifyStructure,
    solveNonExpInstantiation,
    recordEdgeWitness,
    recordEdgeTrace,
    canonicalizeEdgeTraceInteriorsM,
) where

import Control.Monad.State
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad (forM_, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Util.Trace (traceBindingM)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess (
    getCanonical,
    getConstraintAndCanonical
    )
import MLF.Constraint.Presolution.Ops (
    findRoot,
    getCanonicalNode,
    setVarBound,
    )
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Expansion (
    applyExpansionEdgeTraced,
    bindExpansionRootLikeTarget,
    decideMinimalExpansion,
    getExpansion,
    mergeExpansions,
    setExpansion
    )
import MLF.Constraint.Presolution.Witness (binderArgsFromExpansion)
import MLF.Constraint.Presolution.EdgeProcessing.Unify (
    setBindParentIfUpper
    )
import MLF.Constraint.Presolution.Unify (unifyAcyclic)
import qualified MLF.Constraint.Unify.Decompose as UnifyDecompose

-- | Record a witness for an instantiation edge.
recordEdgeWitness :: EdgeId -> EdgeWitness -> PresolutionM ()
recordEdgeWitness (EdgeId eid) w =
    modify $ \st -> st { psEdgeWitnesses = IntMap.insert eid w (psEdgeWitnesses st) }

recordEdgeTrace :: EdgeId -> EdgeTrace -> PresolutionM ()
recordEdgeTrace (EdgeId eid) tr =
    modify $ \st -> st { psEdgeTraces = IntMap.insert eid tr (psEdgeTraces st) }

canonicalizeEdgeTraceInteriorsM :: PresolutionM ()
canonicalizeEdgeTraceInteriorsM = do
    canonical <- getCanonical
    let canonInterior tr =
            let interior' =
                    fromListInterior
                        [ canonical nid
                        | nid <- toListInterior (etInterior tr)
                        ]
            in tr { etInterior = interior' }
    modify' $ \st -> st { psEdgeTraces = IntMap.map canonInterior (psEdgeTraces st) }

unifyStructure :: NodeId -> NodeId -> PresolutionM ()
unifyStructure n1 n2 = do
    root1 <- findRoot n1
    root2 <- findRoot n2
    debugBindParents
        ( "unifyStructure: n1="
            ++ show n1
            ++ " root1="
            ++ show root1
            ++ " n2="
            ++ show n2
            ++ " root2="
            ++ show root2
        )
    if root1 == root2 then return ()
    else do
        node1 <- getCanonicalNode n1
        node2 <- getCanonicalNode n2
        case (node1, node2) of
            (TyExp { tnBody = b1 }, TyExp { tnBody = b2 }) ->
                unifyStructure b1 b2
            (TyExp{}, _) ->
                unifyExpansionNode node1 (tnId node2)
            (_, TyExp{}) ->
                unifyExpansionNode node2 (tnId node1)
            _ ->
                unifyStructureNonExp node1 node2
  where
    unifyExpansionNode :: TyNode -> NodeId -> PresolutionM ()
    unifyExpansionNode expNode targetId = do
        targetNode <- getCanonicalNode targetId
        currentExp <- getExpansion (tnExpVar expNode)
        debugBindParents
            ( "unifyExpansionNode: expNode="
                ++ show (tnId expNode)
                ++ " body="
                ++ show (tnBody expNode)
                ++ " target="
                ++ show targetId
                ++ " targetNode="
                ++ nodeTag targetNode
            )
        (reqExp, unifications) <- decideMinimalExpansion True expNode targetNode
        debugBindParents
            ( "unifyExpansionNode: expNode="
                ++ show (tnId expNode)
                ++ " reqExp="
                ++ show reqExp
                ++ " unifications="
                ++ show unifications
            )
        finalExp <- mergeExpansions (tnExpVar expNode) currentExp reqExp
        debugBindParents
            ( "unifyExpansionNode: expNode="
                ++ show (tnId expNode)
                ++ " finalExp="
                ++ show finalExp
            )
        setExpansion (tnExpVar expNode) finalExp
        mapM_ (uncurry unifyStructure) unifications
        case finalExp of
            ExpIdentity ->
                unifyStructure (tnBody expNode) targetId
            _ -> do
                (resNodeId, (copyMap, _interior, frontier)) <- applyExpansionEdgeTraced finalExp expNode
                _targetBinder <- bindExpansionRootLikeTarget resNodeId targetId
                canonical <- getCanonical
                let copyMapCanon =
                        IntMap.fromListWith
                            const
                            [ (getNodeId (canonical (NodeId orig)), copy)
                            | (orig, copy) <- IntMap.toList (getCopyMapping copyMap)
                            ]
                forM_ (IntSet.toList frontier) $ \nidInt -> do
                    case IntMap.lookup nidInt copyMapCanon of
                        Nothing -> pure ()
                        Just copy -> setBindParentIfUpper copy _targetBinder
                bas <- binderArgsFromExpansion expNode finalExp
                bindExpansionArgs resNodeId bas
                forM_ (IntSet.toList frontier) $ \nidInt -> do
                    case IntMap.lookup nidInt copyMapCanon of
                        Nothing -> pure ()
                        Just copy -> unifyStructure copy (NodeId nidInt)
                unifyStructure resNodeId targetId
                unifyAcyclic (tnId expNode) resNodeId
    unifyStructureNonExp :: TyNode -> TyNode -> PresolutionM ()
    unifyStructureNonExp node1 node2 = do
        let isVarNode node = case node of
                TyVar{} -> True
                _ -> False
            trySetBound target bnd = do
                (c0, canonical) <- getConstraintAndCanonical
                let targetC = canonical target
                    bndC = canonical bnd
                occurs <- case Traversal.occursInUnder canonical (NodeAccess.lookupNode c0) targetC bndC of
                    Left _ -> pure True
                    Right ok -> pure ok
                if occurs
                    then throwError (OccursCheckPresolution targetC bndC)
                    else
                        when (bndC /= targetC) $
                            setVarBound targetC (Just bndC)
        case (node1, node2) of
            (TyVar { tnBound = mb1 }, _) | not (isVarNode node2) ->
                case mb1 of
                    Just b1 -> unifyStructure b1 (tnId node2)
                    Nothing -> trySetBound (tnId node1) (tnId node2)
            (_, TyVar { tnBound = mb2 }) | not (isVarNode node1) ->
                case mb2 of
                    Just b2 -> unifyStructure (tnId node1) b2
                    Nothing -> trySetBound (tnId node2) (tnId node1)
            _ -> do
                unifyAcyclic n1 n2

                case (node1, node2) of
                    (TyVar { tnBound = mb1 }, TyVar { tnBound = mb2 }) ->
                        case (mb1, mb2) of
                            (Just b1, Just b2) ->
                                if b1 /= b2
                                    then do
                                        b1Node <- getCanonicalNode b1
                                        b2Node <- getCanonicalNode b2
                                        case (isVarNode b1Node, isVarNode b2Node) of
                                            (True, False) -> trySetBound b1 b2
                                            (False, True) -> trySetBound b2 b1
                                            _ -> unifyStructure b1 b2
                                    else pure ()
                            _ -> pure ()
                    _ ->
                        unifyStructureChildren node1 node2

    unifyStructureChildren :: TyNode -> TyNode -> PresolutionM ()
    unifyStructureChildren node1 node2 =
        case UnifyDecompose.decomposeUnifyChildren node1 node2 of
            Right edges -> mapM_ (\edge -> unifyStructure (uniLeft edge) (uniRight edge)) edges
            Left _ -> pure ()

isSchemeRootNode :: (NodeId -> NodeId) -> Constraint -> NodeId -> PresolutionM Bool
isSchemeRootNode canonical c0 nid =
    case Binding.lookupBindParentUnder canonical c0 (typeRef nid) of
        Left _ -> pure False
        Right (Just (GenRef gid, _)) ->
            case NodeAccess.lookupGenNode c0 gid of
                Nothing -> pure False
                Just gen -> pure (nid `elem` map canonical (gnSchemes gen))
        _ -> pure False

getBindingPermission :: (NodeId -> NodeId) -> Constraint -> NodeId -> PresolutionM (Bool, Maybe GenNodeId)
getBindingPermission canonical c0 nid =
    case Binding.lookupBindParentUnder canonical c0 (typeRef nid) of
        Left err -> throwError (BindingTreeError err)
        Right (Just (GenRef gid, BindFlex)) -> pure (True, Just gid)
        Right (Just (GenRef gid, BindRigid)) -> pure (False, Just gid)
        _ -> pure (False, Nothing)

solveNonExpInstantiation :: NodeId -> NodeId -> PresolutionM ()
solveNonExpInstantiation lhs rhs = do
    lhsNode <- getCanonicalNode lhs
    rhsNode <- getCanonicalNode rhs
    case (lhsNode, rhsNode) of
        (TyVar{}, TyVar{}) -> unifyStructure lhs rhs
        (_, TyVar{ tnBound = Nothing }) ->
            solveUnboundVarInstantiation lhs rhs
        (_, TyVar{ tnBound = Just bnd }) ->
            solveBoundVarInstantiation lhs rhs bnd
        _ -> unifyStructure lhs rhs

solveUnboundVarInstantiation :: NodeId -> NodeId -> PresolutionM ()
solveUnboundVarInstantiation lhs rhs = do
    (c0, canonical) <- getConstraintAndCanonical
    let lhsC = canonical lhs
        rhsC = canonical rhs
    (allowBound, _parentGen) <- getBindingPermission canonical c0 rhsC
    isSchemeRoot <- isSchemeRootNode canonical c0 rhsC
    occurs <- checkOccurs canonical c0 rhsC lhsC
    if (allowBound || isSchemeRoot) && not occurs
        then setVarBound rhsC (Just lhsC)
        else unifyStructure lhs rhs

solveBoundVarInstantiation :: NodeId -> NodeId -> NodeId -> PresolutionM ()
solveBoundVarInstantiation lhs rhs bnd = do
    (c0, canonical) <- getConstraintAndCanonical
    let lhsC = canonical lhs
        bndC = canonical bnd
    bndNode <- getCanonicalNode bndC
    case bndNode of
        TyVar{} -> do
            occurs <- checkOccurs canonical c0 bndC lhsC
            if not occurs && bndC /= lhsC
                then setVarBound bndC (Just lhsC)
                else unifyStructure lhs rhs
        _ -> unifyStructure lhs rhs

checkOccurs :: (NodeId -> NodeId) -> Constraint -> NodeId -> NodeId -> PresolutionM Bool
checkOccurs canonical c0 rhsC lhsC =
    case Traversal.occursInUnder canonical (NodeAccess.lookupNode c0) rhsC lhsC of
        Left _ -> pure True
        Right ok -> pure ok

debugBindParents :: String -> PresolutionM ()
debugBindParents msg = do
    cfg <- ask
    traceBindingM cfg msg

nodeTag :: TyNode -> String
nodeTag = \case
    TyVar{} -> "TyVar"
    TyBottom{} -> "TyBottom"
    TyArrow{} -> "TyArrow"
    TyBase{} -> "TyBase"
    TyCon{} -> "TyCon"
    TyForall{} -> "TyForall"
    TyExp{} -> "TyExp"
