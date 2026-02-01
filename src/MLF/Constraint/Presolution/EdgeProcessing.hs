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
import Control.Monad.Except (throwError)
import Control.Monad (forM, forM_, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Util.Trace (debugBinding)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Witness.OmegaExec as OmegaExec
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess (
    liftBindingError
    )
import MLF.Constraint.Presolution.Ops (
    findRoot,
    setBindParentM,
    setVarBound,
    )
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Presolution.Expansion (
    applyExpansionEdgeTraced,
    bindExpansionRootLikeTarget,
    decideMinimalExpansion,
    getExpansion,
    mergeExpansions,
    recordEdgeExpansion,
    setExpansion
    )
import MLF.Constraint.Presolution.Witness (
    binderArgsFromExpansion,
    integratePhase2Steps,
    witnessFromExpansion
    )
import MLF.Constraint.Presolution.EdgeUnify (
    EdgeUnifyState(eusOps),
    initEdgeUnifyState,
    mkOmegaExecEnv,
    unifyAcyclicEdge,
    unifyStructureEdge
    )
import MLF.Constraint.Presolution.Unify (unifyAcyclic)

-- | The main loop processing sorted instantiation edges.
runPresolutionLoop :: [InstEdge] -> PresolutionM ()
runPresolutionLoop edges = forM_ edges processInstEdge

-- | Process a single instantiation edge.
processInstEdge :: InstEdge -> PresolutionM ()
processInstEdge edge = do
    requireValidBindingTree
    let n1Id = instLeft edge
    let n2Id = instRight edge
    let edgeId = instEdgeId edge
    constraint0 <- gets psConstraint
    let suppressWeaken = IntSet.member (getEdgeId edgeId) (cAnnEdges constraint0)

    -- Resolve nodes.
    n1Raw <- getNode n1Id
    n2 <- getCanonicalNode n2Id
    case debugBindParents
        ( "processInstEdge: edge="
            ++ show edgeId
            ++ " left="
            ++ show n1Id
            ++ " ("
            ++ nodeTag n1Raw
            ++ ") right="
            ++ show n2Id
            ++ " ("
            ++ nodeTag n2
            ++ ") letEdge="
            ++ show (IntSet.member (getEdgeId edgeId) (cLetEdges constraint0))
        )
        ()
        of
            () -> pure ()
    case (n1Raw, n2) of
        (TyExp{}, TyArrow{ tnDom = dom, tnCod = cod }) -> do
            domR <- findRoot dom
            codR <- findRoot cod
            case debugBindParents
                ( "processInstEdge: edge="
                    ++ show edgeId
                    ++ " target arrow dom="
                    ++ show dom
                    ++ " domRoot="
                    ++ show domR
                    ++ " cod="
                    ++ show cod
                    ++ " codRoot="
                    ++ show codR
                )
                ()
                of
                    () -> pure ()
        _ -> pure ()
    let edgeRoot0 = case n1Raw of
            TyExp{ tnBody = b } -> b
            _ -> tnId n1Raw
    case n1Raw of
        TyExp { tnExpVar = s, tnBody = _bodyId } -> do
            let n1 = n1Raw
            currentExp <- getExpansion s

            let allowTrivialTarget = IntSet.member (getEdgeId edgeId) (cLetEdges constraint0)
            (reqExp, unifications) <- decideMinimalExpansion allowTrivialTarget n1 n2

            finalExp <- mergeExpansions s currentExp reqExp

            setExpansion s finalExp
            recordEdgeExpansion edgeId finalExp

            mapM_ (uncurry unifyStructure) unifications

            (edgeRoot, expTrace, extraOps) <- if finalExp == ExpIdentity
                then do
                    let root0 = case n1 of
                            TyExp{ tnBody = b } -> b
                            _ -> tnId n1
                    pure (root0, emptyTrace, [])
                else do
                    let root = case n1 of
                            TyExp{ tnBody = b } -> b
                            _ -> tnId n1
                    baseSteps <- witnessFromExpansion root n1 finalExp
                    let baseSteps' = if suppressWeaken then dropWeakenSteps baseSteps else baseSteps
                        baseOps = [op | StepOmega op <- baseSteps']
                    (resNodeId, (copyMap0, interior0, frontier0)) <- applyExpansionEdgeTraced finalExp n1
                    case debugBindParents
                        ( "processInstEdge: expansion result resNodeId="
                            ++ show resNodeId
                            ++ " copyMap0="
                            ++ show copyMap0
                            ++ " frontier0="
                            ++ show frontier0
                        )
                        ()
                        of
                            () -> pure ()

                    cBeforeBind <- gets psConstraint
                    let targetNodeId = tnId n2
                        targetParent =
                            Binding.lookupBindParent cBeforeBind (typeRef targetNodeId)
                    case debugBindParents
                        ( "processInstEdge: expansion root bind target="
                            ++ show targetNodeId
                            ++ " parent="
                            ++ show targetParent
                        )
                        ()
                        of
                            () -> pure ()
                    targetBinder <- bindExpansionRootLikeTarget resNodeId targetNodeId

                    pure ()
                    canonical <- getCanonical
                    let copyMapCanon =
                            IntMap.fromListWith
                                const
                                [ (getNodeId (canonical (NodeId orig)), copy)
                                | (orig, copy) <- IntMap.toList copyMap0
                                ]
                    forM_ (IntSet.toList frontier0) $ \nidInt -> do
                        case IntMap.lookup nidInt copyMapCanon of
                            Nothing -> pure ()
                            Just copy -> setBindParentIfUpper copy targetBinder

                    bas <- binderArgsFromExpansion n1 finalExp
                    binderMetas <- forM bas $ \(bv, _arg) ->
                        case IntMap.lookup (getNodeId bv) copyMap0 of
                            Just meta -> pure (bv, meta)
                            Nothing ->
                                throwError (InternalError ("processInstEdge: missing binder-meta copy for " ++ show bv))
                    canonInterior <- getCanonical
                    let canonInteriorSet =
                            IntSet.fromList
                                [ getNodeId (canonInterior (NodeId i))
                                | i <- IntSet.toList interior0
                                ]
                    interiorExact <- edgeInteriorExact resNodeId
                    let interior = IntSet.union canonInteriorSet interiorExact
                    cForBounds <- gets psConstraint
                    let binderBounds =
                            IntMap.fromList
                                [ (getNodeId b, bnd)
                                | (b, _arg) <- bas
                                , Just bnd <- [VarStore.lookupVarBound cForBounds b]
                                ]
                    eu0 <- initEdgeUnifyState binderMetas binderBounds interior resNodeId
                    let omegaEnv = mkOmegaExecEnv copyMap0
                    (_a, eu1) <- runStateT
                        (do
                            OmegaExec.executeOmegaBaseOpsPre omegaEnv baseOps
                            bindExpansionArgs resNodeId bas
                            forM_ (IntSet.toList frontier0) $ \nidInt -> do
                                case IntMap.lookup nidInt copyMapCanon of
                                    Nothing -> pure ()
                                    Just copy -> unifyStructureEdge copy (NodeId nidInt)
                            unifyStructureEdge resNodeId (tnId n2)
                            unifyAcyclicEdge (tnId n1) resNodeId
                            OmegaExec.executeOmegaBaseOpsPost omegaEnv baseOps
                        )
                        eu0
                    resRoot <- findRoot resNodeId
                    setBindParentIfUpper resRoot targetBinder
                    setBindParentIfUpper (tnId n1) targetBinder
                    cAfterBind <- gets psConstraint
                    let resParent = Binding.lookupBindParent cAfterBind (typeRef resRoot)
                    case debugBindParents
                        ( "processInstEdge: expansion root bound resRoot="
                            ++ show resRoot
                            ++ " parent="
                            ++ show resParent
                            ++ " targetBinder="
                            ++ show targetBinder
                        )
                        ()
                        of
                            () -> pure ()
                    c1 <- gets psConstraint
                    case Binding.lookupBindParent c1 (typeRef resRoot) of
                        Nothing -> setBindParentIfUpper resRoot targetBinder
                        Just _ -> pure ()
                    pure (resNodeId, (copyMap0, interior, frontier0), eusOps eu1)

            tr <- buildEdgeTrace edgeId n1Id n1 finalExp expTrace
            recordEdgeTrace edgeId tr
            w <- buildEdgeWitness edgeId n1Id n2Id n1 finalExp extraOps edgeRoot
            recordEdgeWitness edgeId w
            canonicalizeEdgeTraceInteriorsM

        _ -> do
            n1 <- getCanonicalNode n1Id
            recordEdgeExpansion edgeId ExpIdentity
            w <- buildEdgeWitness edgeId n1Id n2Id n1Raw ExpIdentity [] edgeRoot0
            recordEdgeWitness edgeId w
            solveNonExpInstantiation (tnId n1) (tnId n2)
            canonicalizeEdgeTraceInteriorsM

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
                    IntSet.fromList
                        [ getNodeId (canonical (NodeId nid))
                        | nid <- IntSet.toList (etInterior tr)
                        ]
            in tr { etInterior = interior' }
    modify' $ \st -> st { psEdgeTraces = IntMap.map canonInterior (psEdgeTraces st) }

-- | Build an edge witness from the chosen expansion recipe.
buildEdgeWitness :: EdgeId -> NodeId -> NodeId -> TyNode -> Expansion -> [InstanceOp] -> NodeId -> PresolutionM EdgeWitness
buildEdgeWitness eid left right leftRaw expn extraOps _edgeRoot = do
    root <- case leftRaw of
        TyExp{ tnBody = b } -> pure b
        _ -> pure left
    constraint0 <- gets psConstraint
    let suppressWeaken = IntSet.member (getEdgeId eid) (cAnnEdges constraint0)
    baseSteps <- witnessFromExpansion root leftRaw expn
    let baseSteps' = if suppressWeaken then dropWeakenSteps baseSteps else baseSteps
        steps0 = integratePhase2Steps baseSteps' extraOps
        ops = [op | StepOmega op <- steps0]
        iw = InstanceWitness ops
    pure EdgeWitness
        { ewEdgeId = eid
        , ewLeft = left
        , ewRight = right
        , ewRoot = root
        , ewSteps = steps0
        , ewWitness = iw
        }

dropWeakenSteps :: [InstanceStep] -> [InstanceStep]
dropWeakenSteps = filter (not . isWeakenStep)
  where
    isWeakenStep step = case step of
        StepOmega OpWeaken{} -> True
        _ -> False

-- | Build an edge trace.
buildEdgeTrace :: EdgeId -> NodeId -> TyNode -> Expansion -> (CopyMap, InteriorSet, FrontierSet) -> PresolutionM EdgeTrace
buildEdgeTrace _eid left leftRaw expn (copyMap0, _interior0, _frontier0) = do
    bas <- binderArgsFromExpansion leftRaw expn
    root <- findRoot left
    (c0, canonical) <- getConstraintAndCanonical
    let canonicalizeInterior s =
            IntSet.fromList
                [ getNodeId (canonical (NodeId nid))
                | nid <- IntSet.toList s
                ]
    interiorRaw <- do
        s <- liftBindingError $ Binding.interiorOfUnder canonical c0 (typeRef root)
        pure $
            IntSet.fromList
                [ getNodeId nid
                | key <- IntSet.toList s
                , TypeRef nid <- [nodeRefFromKey key]
                ]
    let interior = canonicalizeInterior interiorRaw
    pure EdgeTrace { etRoot = root, etBinderArgs = bas, etInterior = interior, etCopyMap = copyMap0 }

unifyStructure :: NodeId -> NodeId -> PresolutionM ()
unifyStructure n1 n2 = do
    root1 <- findRoot n1
    root2 <- findRoot n2
    case debugBindParents
        ( "unifyStructure: n1="
            ++ show n1
            ++ " root1="
            ++ show root1
            ++ " n2="
            ++ show n2
            ++ " root2="
            ++ show root2
        )
        ()
        of
            () -> pure ()
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
        case debugBindParents
            ( "unifyExpansionNode: expNode="
                ++ show (tnId expNode)
                ++ " body="
                ++ show (tnBody expNode)
                ++ " target="
                ++ show targetId
                ++ " targetNode="
                ++ nodeTag targetNode
            )
            ()
            of
                () -> pure ()
        (reqExp, unifications) <- decideMinimalExpansion True expNode targetNode
        case debugBindParents
            ( "unifyExpansionNode: expNode="
                ++ show (tnId expNode)
                ++ " reqExp="
                ++ show reqExp
                ++ " unifications="
                ++ show unifications
            )
            ()
            of
                () -> pure ()
        finalExp <- mergeExpansions (tnExpVar expNode) currentExp reqExp
        case debugBindParents
            ( "unifyExpansionNode: expNode="
                ++ show (tnId expNode)
                ++ " finalExp="
                ++ show finalExp
            )
            ()
            of
                () -> pure ()
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
                            | (orig, copy) <- IntMap.toList copyMap
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
                    (TyArrow { tnDom = d1, tnCod = c1 }, TyArrow { tnDom = d2, tnCod = c2 }) -> do
                        unifyStructure d1 d2
                        unifyStructure c1 c2
                    (TyForall { tnBody = b1 }, TyForall { tnBody = b2 }) -> do
                        unifyStructure b1 b2
                    _ -> return ()

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

-- | Set a binding parent if the parent is upper than the child in the binding tree.
setBindParentIfUpper :: NodeId -> NodeRef -> PresolutionM ()
setBindParentIfUpper child parent = do
    cBind <- gets psConstraint
    when (Binding.isUpper cBind parent (TypeRef child)) $
        setBindParentM (TypeRef child) (parent, BindFlex)

-- | Debug binding operations (uses global trace config).
debugBindParents :: String -> a -> a
debugBindParents = debugBinding

nodeTag :: TyNode -> String
nodeTag = \case
    TyVar{} -> "TyVar"
    TyBottom{} -> "TyBottom"
    TyArrow{} -> "TyArrow"
    TyBase{} -> "TyBase"
    TyForall{} -> "TyForall"
    TyExp{} -> "TyExp"
