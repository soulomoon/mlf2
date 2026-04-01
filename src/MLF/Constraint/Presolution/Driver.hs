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
    validateReplayMapTraceContract
) where

import Control.Monad.Except (throwError)
import Control.Monad (forM, forM_, when)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Util.Trace (TraceConfig)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Canonicalizer (canonicalizerFrom, chaseRedirectsStable)
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Types
import MLF.Constraint.Types.Witness (isStrictReplayContract)
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.Plan (buildGeneralizePlans)
import MLF.Constraint.Presolution.Rewrite (
    RebuildBindParentsEnv(..),
    canonicalizeExpansion,
    canonicalizeTrace,
    canonicalizeWitness,
    rebuildBindParents,
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
    runPresolutionLoop
    )
import MLF.Constraint.Presolution.EdgeUnify.Omega (pendingWeakenOwners)
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)
import MLF.Constraint.Acyclicity (AcyclicityResult(..))

-- | Main entry point: compute principal presolution.
computePresolution
    :: TraceConfig
    -> AcyclicityResult
    -> Constraint
    -> Either PresolutionError PresolutionResult
computePresolution traceCfg acyclicityResult constraint = do
    -- Initialize state
    let initialState = mkInitialPresolutionState constraint

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
            tyExpNodeIds finalConstraint
    when (not (null residualTyExpNodes)) $
        Left (ResidualTyExpNodes residualTyExpNodes)
    validateTranslatablePresolution finalConstraint

    let edgeArtifacts =
            dropTrivialSchemeEdges
                finalConstraint
                (EdgeArtifacts (psEdgeExpansions finalState) (psEdgeWitnesses finalState) (psEdgeTraces finalState))
        edgeWitnesses = eaEdgeWitnesses edgeArtifacts
        edgeTraces = eaEdgeTraces edgeArtifacts
        edgeExpansions = eaEdgeExpansions edgeArtifacts
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
            [ nid
            | nid <- tyExpNodeIds c
            , IntMap.notMember (getNodeId nid) mapping
            ]
    when (not (null missing)) $
        throwError $
            InternalError $
                "materialization coverage missing TyExp nodes: " ++ show missing

assertNoResidualTyExp :: String -> PresolutionM ()
assertNoResidualTyExp phase = do
    c <- getConstraint
    let residual = tyExpNodeIds c
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
validateReplayMapTraceContract canonical _sourceConstraint finalConstraint eid tr = do
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
        replayContract = etReplayContract tr
    if isStrictReplayContract replayContract
        then do
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
                let inReplayDomain =
                        IntSet.member (getNodeId replayTarget) replayBinderDomain
                when (not inReplayDomain) $
                    Left $
                        InternalError $
                            unlines
                                [ "edge replay-map codomain target outside replay binder domain"
                                , "edge: " ++ show (EdgeId eid)
                                , "source key: " ++ show sourceKey
                                , "replay target: " ++ show replayTarget
                                ]
                case NodeAccess.lookupNode finalConstraint replayTarget of
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
            case duplicateReplayTarget (etBinderReplayMap tr) of
                Nothing -> pure ()
                Just (sourceA, sourceB, target) ->
                    Left $
                        InternalError $
                            unlines
                                [ "edge replay-map codomain is non-injective"
                                , "edge: " ++ show (EdgeId eid)
                                , "first source key: " ++ show sourceA
                                , "second source key: " ++ show sourceB
                                , "shared target: " ++ show target
                                ]
        else do
            when (not (IntMap.null (etBinderReplayMap tr))) $
                Left $
                    InternalError $
                        unlines
                            [ "edge replay-map expected empty under ReplayContractNone"
                            , "edge: " ++ show (EdgeId eid)
                            , "replay-map domain: " ++ show (IntSet.toList replayDomain)
                            ]
            when (not (null (etBinderArgs tr))) $
                Left $
                    InternalError $
                        unlines
                            [ "edge binder-args expected empty under ReplayContractNone"
                            , "edge: " ++ show (EdgeId eid)
                            , "binder-arg keys: " ++ show (IntSet.toList sourceDomain)
                            ]
  where
    duplicateReplayTarget replayMap =
        let step (seen, dupFound) (sourceKey, replayTarget)
                | Just _ <- dupFound = (seen, dupFound)
                | otherwise =
                    let replayKey = getNodeId replayTarget
                    in case IntMap.lookup replayKey seen of
                        Nothing ->
                            (IntMap.insert replayKey sourceKey seen, Nothing)
                        Just sourceA ->
                            (seen, Just (sourceA, sourceKey, replayTarget))
            (_, dup) = foldl' step (IntMap.empty, Nothing) (IntMap.toList replayMap)
        in dup

    replayBindersForTrace trace =
        let rootC = canonical (etRoot trace)
            orderedUnder nid =
                case Binding.orderedBinders canonical finalConstraint (typeRef (canonical nid)) of
                    Left _ -> []
                    Right binders -> map canonical binders
            direct = orderedUnder rootC
        in case NodeAccess.lookupNode finalConstraint rootC of
            Just TyVar{ tnBound = Just bnd } ->
                let viaBound = orderedUnder bnd
                in if null direct then viaBound else direct
            _ -> direct

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
        let exps =
                [ (expNode, expVar, expBody)
                | nid <- tyExpNodeIds c
                , Just expNode@TyExp { tnExpVar = expVar, tnBody = expBody } <- [NodeAccess.lookupNode c nid]
                ]
        pairs <- forM exps $ \(expNode, expVar, expBody) -> do
            expn <- getExpansion expVar
            pure $ case expn of
                ExpIdentity ->
                    let root = canonicalUf (tnId expNode)
                    in Just (getNodeId root, expBody)
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

    newBindParents <-
        rebuildBindParents
            RebuildBindParentsEnv
                { rbpOriginalConstraint = c
                , rbpNewNodes = newNodes
                , rbpGenNodes = genNodes'
                , rbpCanonical = canonical
                , rbpIncomingParents = incomingParents
                }

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

mkInitialPresolutionState :: Constraint -> PresolutionState
mkInitialPresolutionState constraint =
    PresolutionState
        { psConstraint = constraint
        , psPresolution = Presolution IntMap.empty
        , psUnionFind = IntMap.empty
        , psNextNodeId = maxNodeIdKeyOr0 constraint + 1
        , psPendingWeakens = IntSet.empty
        , psPendingWeakenOwners = IntMap.empty
        , psBinderCache = IntMap.empty
        , psEdgeExpansions = IntMap.empty
        , psEdgeWitnesses = IntMap.empty
        , psEdgeTraces = IntMap.empty
        }

tyExpNodeIds :: Constraint -> [NodeId]
tyExpNodeIds c =
    [ tnId node
    | node@TyExp{} <- NodeAccess.allNodes c
    ]
