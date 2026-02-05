{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing
Description : Edge processing loop for presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the edge processing loop for MLF presolution.
It processes instantiation edges in topological order to decide minimal
expansions for expansion variables.
-}
module MLF.Constraint.Presolution.EdgeProcessing (
    runPresolutionLoop,
    processInstEdge
) where

import Control.Monad.State
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad (forM_, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Util.Trace (TraceConfig, traceBindingM)

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
    getNode,
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
    recordEdgeExpansion,
    setExpansion
    )
import MLF.Constraint.Presolution.Witness (binderArgsFromExpansion)
import MLF.Constraint.Presolution.EdgeProcessing.Unify (
    EdgeExpansionResult(..),
    runExpansionUnify,
    setBindParentIfUpper
    )
import MLF.Constraint.Presolution.EdgeProcessing.Witness (
    EdgeWitnessPlan(..),
    buildEdgeTrace,
    buildEdgeWitness,
    edgeWitnessPlan
    )
import MLF.Constraint.Presolution.Unify (unifyAcyclic)
import qualified MLF.Constraint.Unify.Decompose as UnifyDecompose

-- | The main loop processing sorted instantiation edges.
runPresolutionLoop :: [InstEdge] -> PresolutionM ()
runPresolutionLoop edges = forM_ edges processInstEdge

data EdgeCtx = EdgeCtx
    { ecEdgeId :: EdgeId
    , ecCanonical :: NodeId -> NodeId
    , ecConstraint :: Constraint
    , ecSuppressWeaken :: Bool
    , ecTraceConfig :: TraceConfig
    }

mkEdgeCtx :: InstEdge -> PresolutionM EdgeCtx
mkEdgeCtx edge = do
    (constraint0, canonical) <- getConstraintAndCanonical
    cfg <- ask
    let edgeId = instEdgeId edge
        suppressWeaken = IntSet.member (getEdgeId edgeId) (cAnnEdges constraint0)
    pure EdgeCtx
        { ecEdgeId = edgeId
        , ecCanonical = canonical
        , ecConstraint = constraint0
        , ecSuppressWeaken = suppressWeaken
        , ecTraceConfig = cfg
        }

edgeAllowsTrivialTarget :: EdgeCtx -> Bool
edgeAllowsTrivialTarget ctx =
    IntSet.member (getEdgeId (ecEdgeId ctx)) (cLetEdges (ecConstraint ctx))

edgeDebug :: EdgeCtx -> String -> PresolutionM ()
edgeDebug ctx msg = traceBindingM (ecTraceConfig ctx) msg

-- | Process a single instantiation edge.
processInstEdge :: InstEdge -> PresolutionM ()
processInstEdge edge = do
    requireValidBindingTree
    ctx <- mkEdgeCtx edge
    let n1Id = instLeft edge
        n2Id = instRight edge
        edgeId = ecEdgeId ctx

    -- Resolve nodes.
    (n1Raw, n2) <- resolveEdgeNodes ctx n1Id n2Id
    case n1Raw of
        TyExp { tnExpVar = s } -> do
            currentExp <- getExpansion s

            (reqExp, unifications) <- decideMinimalExpansion (edgeAllowsTrivialTarget ctx) n1Raw n2

            finalExp <- mergeExpansions s currentExp reqExp

            setExpansion s finalExp
            recordEdgeExpansion edgeId finalExp

            mapM_ (uncurry unifyStructure) unifications

            witnessPlan <- edgeWitnessPlan (ecSuppressWeaken ctx) n1Id n1Raw finalExp
            expansionResult <-
                if finalExp == ExpIdentity
                    then pure EdgeExpansionResult { eerTrace = emptyTrace, eerExtraOps = [] }
                    else runExpansionUnify edgeId n1Raw n2 finalExp (ewpBaseOps witnessPlan)

            let expTrace = eerTrace expansionResult
                extraOps = eerExtraOps expansionResult
            tr <- buildEdgeTrace edgeId n1Id n1Raw finalExp expTrace
            recordEdgeTrace edgeId tr
            w <- buildEdgeWitness edgeId n1Id n2Id n1Raw (ewpBaseSteps witnessPlan) extraOps
            recordEdgeWitness edgeId w
            canonicalizeEdgeTraceInteriorsM

        _ -> do
            n1 <- getCanonicalNode n1Id
            recordEdgeExpansion edgeId ExpIdentity
            witnessPlan <- edgeWitnessPlan (ecSuppressWeaken ctx) n1Id n1Raw ExpIdentity
            w <- buildEdgeWitness edgeId n1Id n2Id n1Raw (ewpBaseSteps witnessPlan) []
            recordEdgeWitness edgeId w
            solveNonExpInstantiation (tnId n1) (tnId n2)
            canonicalizeEdgeTraceInteriorsM

resolveEdgeNodes :: EdgeCtx -> NodeId -> NodeId -> PresolutionM (TyNode, TyNode)
resolveEdgeNodes ctx n1Id n2Id = do
    n1Raw <- getNode n1Id
    n2 <- getCanonicalNode n2Id
    edgeDebug ctx
        ( "processInstEdge: edge="
            ++ show (ecEdgeId ctx)
            ++ " left="
            ++ show n1Id
            ++ " ("
            ++ nodeTag n1Raw
            ++ ") right="
            ++ show n2Id
            ++ " ("
            ++ nodeTag n2
            ++ ") letEdge="
            ++ show (edgeAllowsTrivialTarget ctx)
        )
    case (n1Raw, n2) of
        (TyExp{}, TyArrow{ tnDom = dom, tnCod = cod }) -> do
            domR <- findRoot dom
            codR <- findRoot cod
            edgeDebug ctx
                ( "processInstEdge: edge="
                    ++ show (ecEdgeId ctx)
                    ++ " target arrow dom="
                    ++ show dom
                    ++ " domRoot="
                    ++ show domR
                    ++ " cod="
                    ++ show cod
                    ++ " codRoot="
                    ++ show codR
                )
        _ -> pure ()
    pure (n1Raw, n2)

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
                targetBinder <- bindExpansionRootLikeTarget resNodeId targetId
                pure ()
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
                        Just copy -> setBindParentIfUpper copy targetBinder
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

-- | Check if a node is a scheme root (directly under a GenNode).
isSchemeRootNode :: (NodeId -> NodeId) -> Constraint -> NodeId -> PresolutionM Bool
isSchemeRootNode canonical c0 nid =
    case Binding.lookupBindParentUnder canonical c0 (typeRef nid) of
        Left _ -> pure False
        Right (Just (GenRef gid, _)) ->
            case NodeAccess.lookupGenNode c0 gid of
                Nothing -> pure False
                Just gen -> pure (nid `elem` map canonical (gnSchemes gen))
        _ -> pure False

-- | Check binding permission for a node.
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

-- | Handle instantiation where RHS is an unbound variable.
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

-- | Handle instantiation where RHS is a bound variable.
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

-- | Check if lhs occurs in rhs.
checkOccurs :: (NodeId -> NodeId) -> Constraint -> NodeId -> NodeId -> PresolutionM Bool
checkOccurs canonical c0 rhsC lhsC =
    case Traversal.occursInUnder canonical (NodeAccess.lookupNode c0) rhsC lhsC of
        Left _ -> pure True
        Right ok -> pure ok

-- | Debug binding operations (uses explicit trace config).
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
