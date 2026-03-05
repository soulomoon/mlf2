{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.Driver
Description : Presolution driver (Phase 4)
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the principal presolution phase of MLF type inference.
It processes instantiation edges in topological order to decide minimal
expansions for expansion variables.

= Architecture

The presolution phase consists of:
1. Validation and rigidification (see "MLF.Constraint.Presolution.Validation")
2. Edge processing loop (processes instantiation edges)
3. Expansion materialization and constraint rewriting
4. Witness normalization

= Note on Module Structure

This module contains several large, complex functions (materializeExpansions,
rewriteConstraint, normalizeEdgeWitnessesM) that are tightly coupled to the
PresolutionM monad state. Further splitting would require significant refactoring
to extract shared state into explicit parameter passing or reader patterns.

= Paper References

* Rémy & Yakobowski, "Graphic Type Constraints" (ICFP 2008) - §5 "Presolution"
-}
module MLF.Constraint.Presolution.Driver (
    computePresolution,
    processInstEdge,
    validateReplayMapTraceContract
) where

import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad (foldM, forM, forM_, when)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Util.Trace (TraceConfig, traceBindingM)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Canonicalizer (canonicalizerFrom, chaseRedirectsStable)
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.Plan (buildGeneralizePlans)
import MLF.Constraint.Presolution.Rewrite (
    canonicalizeExpansion,
    canonicalizeTrace,
    canonicalizeWitness,
    rewriteGenNodes,
    rewriteNode,
    rewriteVarSet,
    )
import MLF.Constraint.Presolution.Validation (
    validateTranslatablePresolution,
    rigidifyTranslatablePresolutionM,
    bindingToPresM
    )
import MLF.Constraint.Presolution.WitnessNorm (normalizeEdgeWitnessesM)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Expansion (getExpansion)
import MLF.Constraint.Presolution.Materialization (
    materializeExpansions
    )
import MLF.Constraint.Presolution.EdgeProcessing (
    runPresolutionLoop,
    processInstEdge
    )
import MLF.Constraint.Presolution.EdgeUnify (pendingWeakenOwners)
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)
import MLF.Constraint.Acyclicity (AcyclicityResult(..))

-- | Debug binding operations (uses explicit trace config).
debugBindParents :: String -> a -> PresolutionM a
debugBindParents msg value = do
    cfg <- ask
    traceBindingM cfg msg
    pure value

-- | Main entry point: compute principal presolution.
computePresolution
    :: TraceConfig
    -> AcyclicityResult
    -> Constraint
    -> Either PresolutionError PresolutionResult
computePresolution traceCfg acyclicityResult constraint = do
    -- Initialize state
    let initialState = PresolutionState
            { psConstraint = constraint
            , psPresolution = Presolution IntMap.empty
            , psUnionFind = IntMap.empty -- Should initialize from constraint if needed
            , psNextNodeId = maxNodeIdKeyOr0 constraint + 1
            , psPendingWeakens = IntSet.empty
            , psPendingWeakenOwners = IntMap.empty
            , psBinderCache = IntMap.empty
            , psEdgeExpansions = IntMap.empty
            , psEdgeWitnesses = IntMap.empty
            , psEdgeTraces = IntMap.empty
            }

    -- Run the presolution loop.
    (_, presState) <- runPresolutionM traceCfg
        initialState
        (runPresolutionLoop traceCfg (arSortedEdges acyclicityResult))

    -- Finalization stage (thesis-aligned post-loop artifact construction):
    --  1) materialize expansions
    --  2) rewrite/canonicalize to remove TyExp
    --  3) rigidify for translatability construction
    --  4) normalize witnesses
    (redirects, finalState) <- runPresolutionM traceCfg presState $ do
        runFinalizationStage

    let finalConstraint = psConstraint finalState
    when (not (null (cUnifyEdges finalConstraint))) $
        Left (ResidualUnifyEdges (cUnifyEdges finalConstraint))
    when (not (null (cInstEdges finalConstraint))) $
        Left (ResidualInstEdges (cInstEdges finalConstraint))
    let residualTyExpNodes =
            [ tnId node
            | node@TyExp{} <- NodeAccess.allNodes finalConstraint
            ]
    when (not (null residualTyExpNodes)) $
        Left (ResidualTyExpNodes residualTyExpNodes)
    validateTranslatablePresolution finalConstraint

    let (edgeWitnesses, edgeTraces, edgeExpansions) =
            dropTrivialSchemeEdges
                finalConstraint
                (psEdgeWitnesses finalState)
                (psEdgeTraces finalState)
                (psEdgeExpansions finalState)
        nonTrivialEdgeKeys =
            IntSet.fromList
                [ getEdgeId (instEdgeId edge)
                | edge <- cInstEdges constraint
                , IntSet.notMember (getEdgeId (instEdgeId edge)) (cLetEdges finalConstraint)
                ]
        witnessKeys = IntSet.fromList (IntMap.keys edgeWitnesses)
        traceKeys = IntSet.fromList (IntMap.keys edgeTraces)
        missingWitnesses =
            map EdgeId (IntSet.toList (IntSet.difference nonTrivialEdgeKeys witnessKeys))
        missingTraces =
            map EdgeId (IntSet.toList (IntSet.difference nonTrivialEdgeKeys traceKeys))
        canonical = chaseRedirectsStable redirects
    when (not (null missingWitnesses)) $
        Left (MissingEdgeWitnesses missingWitnesses)
    when (not (null missingTraces)) $
        Left (MissingEdgeTraces missingTraces)
    forM_ (IntMap.toList edgeTraces) $ \(eid, tr) ->
        when (IntSet.member eid nonTrivialEdgeKeys) $ do
            validateReplayMapTraceContract canonical constraint finalConstraint eid tr

    return PresolutionResult
        { prConstraint = finalConstraint
        , prEdgeExpansions = edgeExpansions
        , prEdgeWitnesses = edgeWitnesses
        , prEdgeTraces = edgeTraces
        , prRedirects = redirects
        , prUnionFind = PresolutionUf (psUnionFind finalState)
        , prPlanBuilder = PresolutionPlanBuilder (buildGeneralizePlans traceCfg)
        }

runFinalizationStage :: PresolutionM (IntMap NodeId)
runFinalizationStage = do
    assertFinalizationBoundary "pre-materialization"
    mapping <- materializeExpansions
    assertMaterializationCoverage mapping
    assertFinalizationBoundary "post-materialization"
    redirects <- rewriteConstraint mapping
    assertNoResidualTyExp "post-rewrite"
    assertFinalizationBoundary "post-rewrite"
    rigidifyTranslatablePresolutionM
    cRigid <- getConstraint
    case validateTranslatablePresolution cRigid of
        Left err -> throwError err
        Right () -> pure ()
    normalizeEdgeWitnessesM
    assertWitnessTraceDomain "post-witness-normalization"
    assertFinalizationBoundary "post-witness-normalization"
    pure redirects

assertFinalizationBoundary :: String -> PresolutionM ()
assertFinalizationBoundary phase = do
    st <- getPresolutionState
    let pendingWeakens = psPendingWeakens st
        pendingUnify = cUnifyEdges (psConstraint st)
    when (not (IntSet.null pendingWeakens) || not (null pendingUnify)) $
        do
            pendingOwners <- pendingWeakenOwners
            throwError $
                InternalError $
                    unlines
                        [ "presolution finalization boundary violation: " ++ phase
                        , "pending weakens: " ++ show (IntSet.toList pendingWeakens)
                        , "pending weaken owners: " ++ show pendingOwners
                        , "pending unify edges: " ++ show pendingUnify
                        ]

assertMaterializationCoverage :: IntMap NodeId -> PresolutionM ()
assertMaterializationCoverage mapping = do
    c <- getConstraint
    let missing =
            [ tnId node
            | node@TyExp{} <- NodeAccess.allNodes c
            , IntMap.notMember (getNodeId (tnId node)) mapping
            ]
    when (not (null missing)) $
        throwError $
            InternalError $
                "materialization coverage missing TyExp nodes: " ++ show missing

assertNoResidualTyExp :: String -> PresolutionM ()
assertNoResidualTyExp phase = do
    c <- getConstraint
    let residual =
            [ tnId node
            | node@TyExp{} <- NodeAccess.allNodes c
            ]
    when (not (null residual)) $
        throwError $
            InternalError $
                phase ++ ": residual TyExp nodes " ++ show residual

assertWitnessTraceDomain :: String -> PresolutionM ()
assertWitnessTraceDomain phase = do
    st <- getPresolutionState
    let witnessKeys = IntSet.fromList (IntMap.keys (psEdgeWitnesses st))
        traceKeys = IntSet.fromList (IntMap.keys (psEdgeTraces st))
    when (witnessKeys /= traceKeys) $
        throwError $
            InternalError $
                unlines
                    [ "presolution witness/trace domain mismatch at " ++ phase
                    , "witness keys: " ++ show (IntSet.toList witnessKeys)
                    , "trace keys: " ++ show (IntSet.toList traceKeys)
                    ]

validateReplayMapTraceContract
    :: (NodeId -> NodeId)
    -> Constraint
    -> Constraint
    -> Int
    -> EdgeTrace
    -> Either PresolutionError ()
validateReplayMapTraceContract canonical sourceConstraint finalConstraint eid tr = do
    let sourceDomain =
            IntSet.fromList
                [ getNodeId binder
                | (binder, _arg) <- etBinderArgs tr
                ]
        replayDomain =
            IntSet.fromList (IntMap.keys (etBinderReplayMap tr))
        replayBinderDomain =
            IntSet.fromList [getNodeId b | b <- replayBindersForTrace tr]
        missingReplay =
            IntSet.toList (IntSet.difference sourceDomain replayDomain)
        extraReplay =
            IntSet.toList (IntSet.difference replayDomain sourceDomain)
    when (not (null missingReplay) || not (null extraReplay)) $
        Left $
            InternalError $
                unlines
                    [ "edge replay-map domain mismatch"
                    , "edge: " ++ show (EdgeId eid)
                    , "trace binder-source domain: " ++ show (IntSet.toList sourceDomain)
                    , "replay-map domain: " ++ show (IntSet.toList replayDomain)
                    , "missing source keys: " ++ show missingReplay
                    , "extra source keys: " ++ show extraReplay
                    ]
    forM_ (IntMap.toList (etBinderReplayMap tr)) $ \(sourceKey, replayTarget) -> do
        let replayTargetC = canonical replayTarget
        when (IntSet.notMember (getNodeId replayTargetC) replayBinderDomain) $
            Left $
                InternalError $
                    unlines
                        [ "edge replay-map codomain target outside replay binder domain"
                        , "edge: " ++ show (EdgeId eid)
                        , "source key: " ++ show sourceKey
                        , "replay target: " ++ show replayTarget
                        ]
        case NodeAccess.lookupNode finalConstraint replayTargetC of
            Just TyVar{} -> pure ()
            _ ->
                Left $
                    InternalError $
                        unlines
                            [ "edge replay-map codomain contains non-TyVar target"
                            , "edge: " ++ show (EdgeId eid)
                            , "source key: " ++ show sourceKey
                            , "replay target: " ++ show replayTarget
                            ]
  where
    replayBindersForTrace trace =
        case Binding.orderedBinders canonical sourceConstraint (typeRef (etRoot trace)) of
            Left _ -> []
            Right binders -> map canonical binders

-- | Rewrite constraint by removing TyExp nodes, applying expansion mapping and
-- union-find canonicalization, collapsing duplicates (preferring structure over
-- vars), and clearing instantiation edges (consumed in presolution).
-- Returns the canonicalized redirect map.
rewriteConstraint :: IntMap NodeId -> PresolutionM (IntMap NodeId)
rewriteConstraint mapping = do
    (c, canonicalUf) <- getConstraintAndCanonical
    st <- getPresolutionState
    let edgeExpansions0 = psEdgeExpansions st
        edgeWitnesses0 = psEdgeWitnesses st
        edgeTraces0 = psEdgeTraces st

    -- If an identity `TyExp` wrapper is unified away (i.e. it is not the UF root),
    -- we still must redirect the whole UF class to the wrapper’s body, otherwise
    -- programs like `\y. let id = (\x. x) in id y` lose the “result = argument”
    -- relationship and get over-generalized types.
    identityRootMap <- do
        let exps = [ n | n@TyExp{} <- NodeAccess.allNodes c ]
        pairs <- forM exps $ \expNode -> do
            expn <- getExpansion (tnExpVar expNode)
            pure $ case expn of
                ExpIdentity ->
                    let root = canonicalUf (tnId expNode)
                    in Just (getNodeId root, tnBody expNode)
                _ -> Nothing
        let chooseMin a b = min a b
        pure $ IntMap.fromListWith chooseMin (catMaybes pairs)

    let canonical nid =
            let step n =
                    let r0 = canonicalUf n
                        r1 = fromMaybe r0 (IntMap.lookup (getNodeId r0) identityRootMap)
                        r2 = fromMaybe r1 (IntMap.lookup (getNodeId r1) mapping)
                    in r2
                go seen n =
                    let n' = step n
                    in if n' == n || IntSet.member (getNodeId n') seen
                        then n'
                        else go (IntSet.insert (getNodeId n') seen) n'
            in go IntSet.empty nid
        canon = canonicalizerFrom canonical

        newNodes = IntMap.fromListWith Canonicalize.chooseRepNode (mapMaybe (rewriteNode canonical) (NodeAccess.allNodes c))
        eliminated' = rewriteVarSet canonical newNodes (cEliminatedVars c)
        weakened' = rewriteVarSet canonical newNodes (cWeakenedVars c)
        genNodes' = rewriteGenNodes canonical newNodes (cGenNodes c)

        newExps = IntMap.map (canonicalizeExpansion canon) edgeExpansions0

        newWitnesses = IntMap.map (canonicalizeWitness canon) edgeWitnesses0

        newTraces0 = IntMap.map (canonicalizeTrace canon) edgeTraces0

        bindingEdges0 = cBindParents c
        cStruct = c { cNodes = NodeMap newNodes, cGenNodes = genNodes' }
        rootGenRef =
            case IntMap.keys (getGenNodeMap genNodes') of
                [] -> Nothing
                gids -> Just (genRef (GenNodeId (minimum gids)))

        incomingParents :: IntMap IntSet.IntSet
        incomingParents =
            let addOne parent child m =
                    IntMap.insertWith
                        IntSet.union
                        (getNodeId child)
                        (IntSet.singleton (getNodeId parent))
                        m

                addNode m node =
                    let parent = tnId node
                        addChild acc child
                            | child == parent = acc
                            | otherwise = addOne parent child acc
                    in foldl' addChild m (structuralChildrenWithBounds node)
            in IntMap.foldl' addNode IntMap.empty newNodes

        -- Canonicalize redirects (values in the map)
        -- mapping maps OldId -> NewId. NewId might be non-canonical.
        -- We want to return a map for ALL nodes that were redirected or merged.
        fullRedirects = IntMap.fromList
            [ (nid, canonical (NodeId nid))
            | nid <- map (getNodeId . fst) (toListNode (cNodes c))
            ]

    newBindParents <- do
        let genNodes0 = cGenNodes c
            genExists gid =
                IntMap.member (getGenNodeId gid) (getGenNodeMap genNodes0)
            typeExists nid = IntMap.member (getNodeId nid) newNodes
            schemeParents =
                IntMap.fromListWith
                    const
                    [ (getNodeId child, genRef gid)
                    | (childKey, (parent0, _flag)) <- IntMap.toList bindingEdges0
                    , let childRef0 = nodeRefFromKey childKey
                          childRef = canonicalRef childRef0
                          parentRef = canonicalRef parent0
                    , TypeRef child <- [childRef]
                    , GenRef gid <- [parentRef]
                    ]

            canonicalRef = Canonicalize.canonicalRef canonical
            entries0 =
                [ (childRef, parentRef, flag)
                | (childKey, (parent0, flag)) <- IntMap.toList bindingEdges0
                , let childRef0 = nodeRefFromKey childKey
                      childRef = canonicalRef childRef0
                      parentRef = canonicalRef parent0
                , case childRef of
                    TypeRef nid -> typeExists nid
                    GenRef gid -> genExists gid
                ]

            chooseBindParent :: NodeRef -> NodeRef -> PresolutionM NodeRef
            chooseBindParent childRef parent0 =
                case childRef of
                    GenRef _ ->
                        case parent0 of
                            GenRef gid
                                | genExists gid -> pure parent0
                            _ ->
                                throwError $
                                    BindingTreeError $
                                        InvalidBindingTree $
                                            "rewriteConstraint: gen node has non-gen parent "
                                                ++ show parent0
                    TypeRef child' -> do
                        let inNodes ref = case ref of
                                TypeRef nid -> typeExists nid
                                GenRef gid -> genExists gid
                            upper parent = case parent of
                                GenRef _ -> True
                                _ -> Binding.isUpper cStruct parent childRef

                            bindingAncestors :: NodeRef -> [NodeRef]
                            bindingAncestors start =
                                case Binding.bindingPathToRoot c start of
                                    Left _ -> []
                                    Right path -> drop 1 path

                            expBody :: NodeRef -> Maybe NodeRef
                            expBody ref =
                                case ref of
                                    TypeRef nid ->
                                        case NodeAccess.lookupNode c nid of
                                            Just TyExp{ tnBody = b } -> Just (TypeRef b)
                                            _ -> Nothing
                                    GenRef _ -> Nothing

                            structuralParent :: NodeRef -> Maybe NodeRef
                            structuralParent ref =
                                case ref of
                                    GenRef _ -> Nothing
                                    TypeRef nid ->
                                        case IntMap.lookup (getNodeId nid) incomingParents of
                                            Nothing -> Nothing
                                            Just ps ->
                                                case IntSet.toList ps of
                                                    [] -> Nothing
                                                    (p:_) -> Just (TypeRef (NodeId p))

                            candidates =
                                map canonicalRef $
                                    [ parent0 ]
                                        ++ maybe [] pure (IntMap.lookup (getNodeId child') schemeParents)
                                        ++ mapMaybe expBody [parent0]
                                        ++ bindingAncestors parent0
                                        ++ bindingAncestors childRef
                                        ++ maybe [] pure (structuralParent childRef)

                            firstValid = \case
                                [] -> Nothing
                                (p:ps) ->
                                    if p == childRef || not (inNodes p) || not (upper p)
                                        then firstValid ps
                                        else Just p

                        case firstValid candidates of
                            Just p -> pure p
                            Nothing ->
                                case rootGenRef of
                                    Just gref
                                        | gref /= childRef
                                        , inNodes gref
                                        , upper gref ->
                                            pure gref
                                    _ ->
                                        throwError $
                                            BindingTreeError $
                                                InvalidBindingTree $
                                                    "rewriteConstraint: could not find a valid binding parent for "
                                                        ++ show child'
                                                        ++ " (original parent "
                                                        ++ show parent0
                                                        ++ ")"

            insertOne :: BindParents -> (Int, (NodeRef, BindFlag)) -> PresolutionM BindParents
            insertOne bp (childKey, (parent, flag)) =
                case IntMap.lookup childKey bp of
                    Nothing -> pure (IntMap.insert childKey (parent, flag) bp)
                    Just (parent0, flag0)
                        | parent0 == parent ->
                            let flag' = max flag0 flag
                            in pure (IntMap.insert childKey (parent, flag') bp)
                        | otherwise ->
                            let flag' = max flag0 flag
                                childRef = nodeRefFromKey childKey
                                schemeParent =
                                    case childRef of
                                        TypeRef nid -> IntMap.lookup (getNodeId nid) schemeParents
                                        GenRef _ -> Nothing
                                pickParent0 =
                                    case schemeParent of
                                        Just sp -> sp
                                        Nothing -> parent0
                                pickParent =
                                    if Binding.isUpper cStruct pickParent0 childRef
                                        then pickParent0
                                        else parent
                                bp' = IntMap.insert childKey (pickParent, flag') bp
                                msg =
                                    "presolution: bind-parent conflict child="
                                        ++ show childRef
                                        ++ " parent0="
                                        ++ show parent0
                                        ++ " parent1="
                                        ++ show parent
                                        ++ " schemeParent="
                                        ++ show schemeParent
                                        ++ " pick="
                                        ++ show pickParent
                            in debugBindParents msg bp'

        entries' <- fmap concat $ forM entries0 $ \(childRef, parent0, flag) -> do
            parent <- chooseBindParent childRef parent0
            pure $ case parent == childRef of
                True -> []
                False -> [(nodeRefKey childRef, (parent, flag))]

        bp0 <- foldM insertOne IntMap.empty entries'

        let inNodesRef ref =
                case ref of
                    TypeRef nid -> typeExists nid
                    GenRef gid -> genExists gid
            bp0' =
                IntMap.filterWithKey
                    (\childKey (parent, _) ->
                        inNodesRef (nodeRefFromKey childKey) && inNodesRef parent
                    )
                    bp0
            rootGen =
                case IntMap.keys (getGenNodeMap genNodes') of
                    [] -> Nothing
                    gids -> Just (GenNodeId (minimum gids))

            addMissing bp nidInt = do
                let childRef = typeRef (NodeId nidInt)
                    childKey = nodeRefKey childRef
                if IntMap.member childKey bp
                    then pure bp
                    else do
                        let structuralParent =
                                case IntMap.lookup nidInt incomingParents of
                                    Nothing -> Nothing
                                    Just ps ->
                                        case IntSet.toList ps of
                                            [] -> Nothing
                                            (p:_) -> Just (typeRef (NodeId p))
                            parent =
                                case IntMap.lookup nidInt schemeParents of
                                    Just gp -> Just gp
                                    Nothing -> structuralParent
                            parent' =
                                case parent of
                                    Just p -> Just p
                                    Nothing ->
                                        case rootGen of
                                            Nothing -> Nothing
                                            Just gid -> Just (genRef gid)
                        case parent' of
                            Nothing -> pure bp
                            Just p ->
                                if p == childRef
                                    then pure bp
                                    else pure (IntMap.insert childKey (p, BindFlex) bp)

        bp1 <- foldM addMissing bp0' (IntMap.keys newNodes)
        let rootGenRefLocal = fmap genRef rootGen
            pickUpperParent childN =
                case IntMap.lookup (getNodeId childN) incomingParents of
                    Just ps ->
                        case IntSet.toList ps of
                            (p:_) -> Just (typeRef (NodeId p))
                            [] -> rootGenRefLocal
                    Nothing -> rootGenRefLocal
            fixUpper bp =
                IntMap.mapWithKey
                    (\childKey (parentRef, flag) ->
                        case nodeRefFromKey childKey of
                            GenRef _ -> (parentRef, flag)
                            TypeRef childN ->
                                case parentRef of
                                    GenRef _ -> (parentRef, flag)
                                    _ ->
                                        if Binding.isUpper cStruct parentRef (typeRef childN)
                                            then (parentRef, flag)
                                            else
                                                case pickUpperParent childN of
                                                    Just pRef ->
                                                        if pRef == typeRef childN
                                                            then
                                                                case rootGenRefLocal of
                                                                    Just gref -> (gref, flag)
                                                                    Nothing -> (parentRef, flag)
                                                            else (pRef, flag)
                                                    Nothing -> (parentRef, flag)
                    )
                    bp
        pure (fixUpper bp1)

    let c0' = c
            { cNodes = NodeMap newNodes
            , cInstEdges = []
            , cUnifyEdges = Canonicalize.rewriteUnifyEdges canonical (cUnifyEdges c)
            , cBindParents = newBindParents
            , cEliminatedVars = eliminated'
            , cWeakenedVars = weakened'
            , cGenNodes = genNodes'
            }

    let c' = c0'

    case Binding.checkBindingTree c' of
        Left err -> throwError (BindingTreeError err)
        Right () -> pure ()

    newTraces' <- do
        let updateTrace tr = do
                let root = etRoot tr
                    interiorRootRef = traceInteriorRootRef id c' root
                interior <- bindingToPresM (Binding.interiorOf c' interiorRootRef)
                let interiorNodes =
                        fromListInterior
                            [ nid
                            | key <- IntSet.toList interior
                            , TypeRef nid <- [nodeRefFromKey key]
                            ]
                pure tr { etInterior = interiorNodes }
        traverse updateTrace newTraces0

    putPresolutionState st
        { psConstraint = c'
        , psEdgeExpansions = newExps
        , psEdgeWitnesses = newWitnesses
        , psEdgeTraces = newTraces'
        }

    return fullRedirects
