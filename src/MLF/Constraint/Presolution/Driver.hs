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
    processInstEdge
) where

import Control.Monad.State
import Control.Monad.Except (throwError)
import Control.Monad (foldM, forM, forM_, when)
import Data.Functor.Foldable (cata)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

import qualified MLF.Util.UnionFind as UnionFind
import qualified MLF.Util.Order as Order
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Witness.OmegaExec as OmegaExec
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.Plan (buildGeneralizePlans)
import MLF.Constraint.Presolution.Validation (
    validateTranslatablePresolution,
    rigidifyTranslatablePresolutionM,
    structuralInterior,
    translatableWeakenedNodes,
    bindingToPres,
    bindingToPresM
    )
import MLF.Constraint.Presolution.Ops (
    findRoot,
    getCanonicalNode,
    getNode,
    setBindParentM,
    setVarBound,
    )
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Traversal as Traversal
import MLF.Constraint.Presolution.Expansion (
    applyExpansion,
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
    normalizeInstanceStepsFull,
    OmegaNormalizeEnv(..),
    witnessFromExpansion
    )
import MLF.Constraint.Presolution.EdgeUnify (
    EdgeUnifyState(eusOps),
    flushPendingWeakens,
    initEdgeUnifyState,
    mkOmegaExecEnv,
    unifyAcyclicEdge,
    unifyStructureEdge
    )
import MLF.Constraint.Presolution.Unify (unifyAcyclic)
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import qualified MLF.Constraint.Inert as Inert
-- We will likely need unification logic from Normalize,
-- but for now we'll implement the structure.

-- | Main entry point: compute principal presolution.
computePresolution
    :: AcyclicityResult
    -> Constraint
    -> Either PresolutionError PresolutionResult
computePresolution acyclicityResult constraint = do
    -- Initialize state
    let initialState = PresolutionState
            { psConstraint = constraint
            , psPresolution = Presolution IntMap.empty
            , psUnionFind = IntMap.empty -- Should initialize from constraint if needed
            , psNextNodeId = maxNodeIdKeyOr0 constraint + 1
            , psPendingWeakens = IntSet.empty
            , psBinderCache = IntMap.empty
            , psEdgeExpansions = IntMap.empty
            , psEdgeWitnesses = IntMap.empty
            , psEdgeTraces = IntMap.empty
            }

    -- Run the presolution loop
    presState <- execStateT
        (runPresolutionLoop (arSortedEdges acyclicityResult))
        initialState

    -- Materialize expansions, rewrite TyExp away, and apply UF canonicalization.
    (redirects, finalState) <- runPresolutionM presState $ do
        mapping <- materializeExpansions
        flushPendingWeakens
        redirects <- rewriteConstraint mapping
        rigidifyTranslatablePresolutionM
        normalizeEdgeWitnessesM
        pure redirects

    validateTranslatablePresolution (psConstraint finalState)

    let (edgeWitnesses, edgeTraces, edgeExpansions) =
            dropTrivialSchemeEdges
                (psConstraint finalState)
                (psEdgeWitnesses finalState)
                (psEdgeTraces finalState)
                (psEdgeExpansions finalState)

    return PresolutionResult
        { prConstraint = psConstraint finalState
        , prEdgeExpansions = edgeExpansions
        , prEdgeWitnesses = edgeWitnesses
        , prEdgeTraces = edgeTraces
        , prRedirects = redirects
        , prPlanBuilder = PresolutionPlanBuilder buildGeneralizePlans
        }

-- | Finalize presolution by materializing expansions, rewriting TyExp away,
-- applying union-find canonicalization, and clearing consumed instantiation
-- edges. This prepares the constraint for Phase 5.
-- | Apply final expansions to all TyExp nodes and record their replacements.
materializeExpansions :: PresolutionM (IntMap NodeId)
materializeExpansions = do
    nodes <- gets (cNodes . psConstraint)
    let exps = [ n | n@TyExp{} <- IntMap.elems nodes ]
    uf <- gets psUnionFind
    fmap IntMap.fromList $ forM exps $ \expNode -> do
        let eid = tnId expNode
        expn <- getExpansion (tnExpVar expNode)
        nid' <- case expn of
            -- Identity expansions are erased by rewriting the wrapper to its body.
            ExpIdentity -> applyExpansion expn expNode
            -- For non-identity expansions, `processInstEdge` should already have
            -- materialized and unified the `TyExp` with its expansion result. Reuse
            -- that representative to avoid duplicating fresh nodes here.
            _ ->
                let root = frWith uf eid
                in if root /= eid
                    then pure root
                    else applyExpansion expn expNode
        pure (getNodeId eid, nid')

-- | Rewrite constraint by removing TyExp nodes, applying expansion mapping and
-- union-find canonicalization, collapsing duplicates (preferring structure over
-- vars), and clearing instantiation edges (consumed in presolution).
-- Returns the canonicalized redirect map.
rewriteConstraint :: IntMap NodeId -> PresolutionM (IntMap NodeId)
rewriteConstraint mapping = do
    st <- get
    let c = psConstraint st
        uf = psUnionFind st

    -- If an identity `TyExp` wrapper is unified away (i.e. it is not the UF root),
    -- we still must redirect the whole UF class to the wrapper’s body, otherwise
    -- programs like `\y. let id = (\x. x) in id y` lose the “result = argument”
    -- relationship and get over-generalized types.
    identityRootMap <- do
        let exps = [ n | n@TyExp{} <- IntMap.elems (cNodes c) ]
        pairs <- forM exps $ \expNode -> do
            expn <- getExpansion (tnExpVar expNode)
            pure $ case expn of
                ExpIdentity ->
                    let root = frWith uf (tnId expNode)
                    in Just (getNodeId root, tnBody expNode)
                _ -> Nothing
        let chooseMin a b = min a b
        pure $ IntMap.fromListWith chooseMin (catMaybes pairs)

    let canonical nid =
            let step n =
                    let r0 = frWith uf n
                        r1 = fromMaybe r0 (IntMap.lookup (getNodeId r0) identityRootMap)
                        r2 = fromMaybe r1 (IntMap.lookup (getNodeId r1) mapping)
                    in r2
                go seen n =
                    let n' = step n
                    in if n' == n || IntSet.member (getNodeId n') seen
                        then n'
                        else go (IntSet.insert (getNodeId n') seen) n'
            in go IntSet.empty nid

        rewriteNode :: TyNode -> Maybe (Int, TyNode)
        rewriteNode TyExp{} = Nothing
        rewriteNode n =
            let nid' = canonical (tnId n)
                node' = case n of
                    TyVar { tnBound = mb } -> TyVar { tnId = nid', tnBound = fmap canonical mb }
                    TyBottom {} -> TyBottom nid'
                    TyArrow { tnDom = d, tnCod = cod } -> TyArrow nid' (canonical d) (canonical cod)
                    TyBase { tnBase = b } -> TyBase nid' b
                    TyForall { tnBody = b } -> TyForall nid' (canonical b)
            in Just (getNodeId nid', node')

        -- traceCanonical n = let c = canonical n in trace ("Canonical " ++ show n ++ " -> " ++ show c) c

        newNodes = IntMap.fromListWith Canonicalize.chooseRepNode (mapMaybe rewriteNode (IntMap.elems (cNodes c)))
        eliminated' = rewriteEliminated canonical newNodes (cEliminatedVars c)
        weakened' = rewriteWeakened canonical newNodes (cWeakenedVars c)
        genNodes' = rewriteGenNodes canonical newNodes (cGenNodes c)

        -- Canonicalize edge expansions
        canonicalizeExp = cata alg
          where
            alg layer = case layer of
                ExpIdentityF -> ExpIdentity
                ExpInstantiateF args -> ExpInstantiate (map canonical args)
                ExpForallF levels -> ExpForall levels
                ExpComposeF exps -> ExpCompose exps

        newExps = IntMap.map canonicalizeExp (psEdgeExpansions st)

        canonicalizeOp :: InstanceOp -> InstanceOp
        canonicalizeOp op = case op of
            OpGraft sigma n -> OpGraft (canonical sigma) (canonical n)
            OpMerge a b -> OpMerge (canonical a) (canonical b)
            OpRaise n -> OpRaise (canonical n)
            OpWeaken n -> OpWeaken (canonical n)
            OpRaiseMerge n m -> OpRaiseMerge (canonical n) (canonical m)

        canonicalizeStep :: InstanceStep -> InstanceStep
        canonicalizeStep step = case step of
            StepOmega op -> StepOmega (canonicalizeOp op)
            StepIntro -> StepIntro

        canonicalizeWitness :: EdgeWitness -> EdgeWitness
        canonicalizeWitness w =
            let InstanceWitness ops = ewWitness w
            in w
                { ewLeft = canonical (ewLeft w)
                , ewRight = canonical (ewRight w)
                , ewRoot = canonical (ewRoot w)
                , ewSteps = map canonicalizeStep (ewSteps w)
                , ewWitness = InstanceWitness (map canonicalizeOp ops)
                }

        newWitnesses = IntMap.map canonicalizeWitness (psEdgeWitnesses st)

        canonicalizeTrace :: EdgeTrace -> EdgeTrace
        canonicalizeTrace tr =
            let canonPair (a, b) = (canonical a, canonical b)
                canonInterior =
                    IntSet.fromList
                        [ getNodeId (canonical (NodeId i))
                        | i <- IntSet.toList (etInterior tr)
                        ]
                canonCopyMap =
                    IntMap.fromListWith min
                        [ ( getNodeId (canonical (NodeId k))
                          , canonical v
                          )
                        | (k, v) <- IntMap.toList (etCopyMap tr)
                        ]
            in tr
                { etRoot = canonical (etRoot tr)
                , etBinderArgs = map canonPair (etBinderArgs tr)
                , etInterior = canonInterior
                , etCopyMap = canonCopyMap
                }

        newTraces0 = IntMap.map canonicalizeTrace (psEdgeTraces st)

        bindingEdges0 = cBindParents c
        cStruct = c { cNodes = newNodes, cGenNodes = genNodes' }
        rootGenRef =
            case IntMap.keys genNodes' of
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
            | nid <- IntMap.keys (cNodes c)
            ]

    newBindParents <- do
        let genNodes0 = cGenNodes c
            genExists gid = IntMap.member (getGenNodeId gid) genNodes0
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
                            in pure (debugBindParents msg bp')

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
                case IntMap.keys genNodes' of
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
            { cNodes = newNodes
            , cInstEdges = []
            , cUnifyEdges = Canonicalize.rewriteUnifyEdges canonical (cUnifyEdges c)
            , cBindParents = newBindParents
            , cEliminatedVars = eliminated'
            , cWeakenedVars = weakened'
            , cGenNodes = genNodes'
            }

    let c' = c0'

    let probeIds = [NodeId 2, NodeId 3]
        probeInfo =
            [ ( pid
              , NodeAccess.lookupNode c' pid
              , IntMap.lookup (nodeRefKey (typeRef pid)) (cBindParents c')
              )
            | pid <- probeIds
            ]
    debugBindParentsM ("rewriteConstraint: probe nodes " ++ show probeInfo)

    case Binding.checkBindingTree c' of
        Left err -> throwError (BindingTreeError err)
        Right () -> pure ()

    newTraces' <- do
        let updateTrace tr = do
                interior <- bindingToPresM (Binding.interiorOf c' (typeRef (etRoot tr)))
                let interiorNodes =
                        IntSet.fromList
                            [ getNodeId nid
                            | key <- IntSet.toList interior
                            , TypeRef nid <- [nodeRefFromKey key]
                            ]
                pure tr { etInterior = interiorNodes }
        traverse updateTrace newTraces0

    put st
        { psConstraint = c'
        , psEdgeExpansions = newExps
        , psEdgeWitnesses = newWitnesses
        , psEdgeTraces = newTraces'
        }

    return fullRedirects

-- | Normalize edge witnesses against the finalized presolution constraint.
normalizeEdgeWitnessesM :: PresolutionM ()
normalizeEdgeWitnessesM = do
    c0 <- gets psConstraint
    traces <- gets psEdgeTraces
    witnesses0 <- gets psEdgeWitnesses
    let rewriteNodeWith copyMap nid =
            IntMap.findWithDefault nid (getNodeId nid) copyMap
        weakenedOps =
            IntSet.fromList
                [ getNodeId (rewriteNodeWith copyMap n)
                | (eid, w0) <- IntMap.toList witnesses0
                , let copyMap = maybe IntMap.empty etCopyMap (IntMap.lookup eid traces)
                , StepOmega (OpWeaken n) <- ewSteps w0
                ]
        weakened =
            IntSet.union weakenedOps (translatableWeakenedNodes c0)
    witnesses <- forM (IntMap.toList witnesses0) $ \(eid, w0) -> do
        let (edgeRoot, copyMap, binderArgs0, traceInterior) =
                case IntMap.lookup eid traces of
                    Nothing -> (ewRoot w0, IntMap.empty, [], IntSet.empty)
                    Just tr -> (etRoot tr, etCopyMap tr, etBinderArgs tr, etInterior tr)
            rewriteNode = rewriteNodeWith copyMap
            binderArgs =
                IntMap.fromList
                    [ (getNodeId (rewriteNode bv), rewriteNode arg)
                    | (bv, arg) <- binderArgs0
                    ]
            copyToOriginal =
                IntMap.fromListWith min
                    [ (getNodeId copy, NodeId orig)
                    | (orig, copy) <- IntMap.toList copyMap
                    ]
            restoreNode nid =
                IntMap.findWithDefault nid (getNodeId nid) copyToOriginal
            rewriteOp op =
                case op of
                    OpGraft sigma n -> OpGraft (rewriteNode sigma) (rewriteNode n)
                    OpMerge n m -> OpMerge (rewriteNode n) (rewriteNode m)
                    OpRaise n -> OpRaise (rewriteNode n)
                    OpWeaken n -> OpWeaken (rewriteNode n)
                    OpRaiseMerge n m -> OpRaiseMerge (rewriteNode n) (rewriteNode m)
            rewriteStep step =
                case step of
                    StepOmega op -> StepOmega (rewriteOp op)
                    StepIntro -> StepIntro
            restoreOp op =
                case op of
                    OpGraft sigma n -> OpGraft (restoreNode sigma) (restoreNode n)
                    OpMerge n m -> OpMerge (restoreNode n) (restoreNode m)
                    OpRaise n -> OpRaise (restoreNode n)
                    OpWeaken n -> OpWeaken (restoreNode n)
                    OpRaiseMerge n m -> OpRaiseMerge (restoreNode n) (restoreNode m)
            restoreStep step =
                case step of
                    StepOmega op -> StepOmega (restoreOp op)
                    StepIntro -> StepIntro
        let interiorRoot = typeRef edgeRoot
            orderBase = edgeRoot
            orderRoot = orderBase
        interiorExact <-
            if IntSet.null traceInterior
                then case Binding.interiorOf c0 interiorRoot of
                    Left err -> throwError (BindingTreeError err)
                    Right s ->
                        pure $
                            IntSet.fromList
                                [ getNodeId nid
                                | key <- IntSet.toList s
                                , TypeRef nid <- [nodeRefFromKey key]
                                ]
                else pure traceInterior
        let interiorNorm =
                -- Normalize against an expansion-aware interior so ops on copied nodes
                -- (e.g., escaping binder metas) are retained for Raise/Merge witnesses.
                IntSet.union interiorExact $
                    IntSet.fromList (map getNodeId (IntMap.elems copyMap))
        let steps0 = map rewriteStep (ewSteps w0)
            orderKeys0 = Order.orderKeysFromConstraintWith id c0 orderRoot Nothing
            opTargets = \case
                OpGraft sigma n -> [sigma, n]
                OpWeaken n -> [n]
                OpMerge n m -> [n, m]
                OpRaise n -> [n]
                OpRaiseMerge n m -> [n, m]
            missingKeys =
                IntSet.fromList
                    [ getNodeId n
                    | StepOmega op <- steps0
                    , n <- opTargets op
                    , not (IntMap.member (getNodeId n) orderKeys0)
                    ]
            orderKeys =
                if IntSet.null missingKeys
                    then orderKeys0
                    else
                        -- Defensive: if an op targets a node outside the ≺ traversal,
                        -- assign a stable ordering key so normalization can proceed.
                        let existingFirsts =
                                mapMaybe
                                    (\k -> case Order.okPath k of
                                        [] -> Nothing
                                        (x:_) -> Just x
                                    )
                                    (IntMap.elems orderKeys0)
                            base =
                                case existingFirsts of
                                    [] -> 0
                                    _ -> maximum existingFirsts + 1
                            extraKeys =
                                IntMap.fromList
                                    [ (nidInt, Order.OrderKey { Order.okDepth = 0, Order.okPath = [base, idx] })
                                    | (idx, nidInt) <- zip [0 ..] (IntSet.toList missingKeys)
                                    ]
                        in IntMap.union orderKeys0 extraKeys
            env =
                OmegaNormalizeEnv
                    { oneRoot = edgeRoot
                    , interior = interiorNorm
                    , weakened = weakened
                    , orderKeys = orderKeys
                    , canonical = id
                    , constraint = c0
                    , binderArgs = binderArgs
                    }
        steps <- case normalizeInstanceStepsFull env steps0 of
            Right steps' -> pure steps'
            Left err ->
                throwError (InternalError ("normalizeInstanceStepsFull failed: " ++ show err))
        let stepsFinal = map restoreStep steps
            ops = [op | StepOmega op <- stepsFinal]
        pure (eid, w0 { ewSteps = stepsFinal, ewWitness = InstanceWitness ops })
    modify' $ \st -> st { psEdgeWitnesses = IntMap.fromList witnesses }

dropTrivialSchemeEdges
    :: Constraint
    -> IntMap EdgeWitness
    -> IntMap EdgeTrace
    -> IntMap Expansion
    -> (IntMap EdgeWitness, IntMap EdgeTrace, IntMap Expansion)
dropTrivialSchemeEdges constraint witnesses traces expansions =
    let dropEdgeIds = cLetEdges constraint
        keepEdge eid = not (IntSet.member eid dropEdgeIds)
        witnesses' = IntMap.filterWithKey (\eid _ -> keepEdge eid) witnesses
        traces' = IntMap.filterWithKey (\eid _ -> keepEdge eid) traces
        expansions' = IntMap.filterWithKey (\eid _ -> keepEdge eid) expansions
    in (witnesses', traces', expansions')

rewriteEliminated :: (NodeId -> NodeId) -> IntMap TyNode -> EliminatedVars -> EliminatedVars
rewriteEliminated canon nodes0 elims0 =
    IntSet.fromList
        [ getNodeId vC
        | vid <- IntSet.toList elims0
        , let vC = canon (NodeId vid)
        , case IntMap.lookup (getNodeId vC) nodes0 of
            Just TyVar{} -> True
            _ -> False
        ]

rewriteWeakened :: (NodeId -> NodeId) -> IntMap TyNode -> WeakenedVars -> WeakenedVars
rewriteWeakened canon nodes0 weakened0 =
    IntSet.fromList
        [ getNodeId vC
        | vid <- IntSet.toList weakened0
        , let vC = canon (NodeId vid)
        , case IntMap.lookup (getNodeId vC) nodes0 of
            Just TyVar{} -> True
            _ -> False
        ]

rewriteGenNodes :: (NodeId -> NodeId) -> IntMap TyNode -> IntMap.IntMap GenNode -> IntMap.IntMap GenNode
rewriteGenNodes canon nodes0 gen0 =
    let rewriteOne g =
            let (schemesRev, _seen) =
                    foldl'
                        (\(acc, seen) s ->
                            let s' = canon s
                                key = getNodeId s'
                            in if IntMap.member key nodes0 && not (IntSet.member key seen)
                                then (s' : acc, IntSet.insert key seen)
                                else (acc, seen)
                        )
                        ([], IntSet.empty)
                        (gnSchemes g)
                schemes' = reverse schemesRev
            in (genNodeKey (gnId g), g { gnSchemes = schemes' })
    in IntMap.fromListWith const (map rewriteOne (IntMap.elems gen0))

-- | Read-only chase like Solve.frWith
frWith :: IntMap NodeId -> NodeId -> NodeId
frWith = UnionFind.frWith

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
    -- For LHS, we check the raw node first to handle TyExp properly
    -- even if it has been unified/replaced in previous steps.
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
            -- n1 is an expansion node. We need to ensure s expands enough to cover n2.
            currentExp <- getExpansion s

            -- Decide required expansion based on n2
            let allowTrivialTarget = IntSet.member (getEdgeId edgeId) (cLetEdges constraint0)
            (reqExp, unifications) <- decideMinimalExpansion allowTrivialTarget n1 n2

            -- Merge with current expansion
            finalExp <- mergeExpansions s currentExp reqExp

            -- Update presolution
            setExpansion s finalExp
            recordEdgeExpansion edgeId finalExp

            -- Perform unifications requested by expansion decision
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
                    -- Eagerly materialize and unify to resolve the constraint immediately.
                    -- This ensures that the expansion result is unified with the target (n2),
                    -- and TyExp is unified with the result.
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

                    -- Flag reset: bind the frontier copies at the target binder.
                    pure ()
                    uf0 <- gets psUnionFind
                    let canonical = UnionFind.frWith uf0
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
                    ufInterior <- gets psUnionFind
                    let canonInterior =
                            IntSet.fromList
                                [ getNodeId (UnionFind.frWith ufInterior (NodeId i))
                                | i <- IntSet.toList interior0
                                ]
                    interiorExact <- edgeInteriorExact resNodeId
                    let interior = IntSet.union canonInterior interiorExact
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
                            lift $ bindExpansionArgs resNodeId bas
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
                    -- Keep the TyExp wrapper aligned with the target binder so the
                    -- canonical expansion root remains bound at the target binder.
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
            -- n1 is not an expansion node.
            -- This is a standard instantiation constraint (or just subtyping).
            -- "If the left hand side is not an expansion node, it must be equal to the right hand side"
            -- (Simplification for now, might need refinement for full MLF)
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
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
        canonInterior tr =
            let interior' =
                    IntSet.fromList
                        [ getNodeId (canonical (NodeId nid))
                        | nid <- IntSet.toList (etInterior tr)
                        ]
            in tr { etInterior = interior' }
    modify' $ \st -> st { psEdgeTraces = IntMap.map canonInterior (psEdgeTraces st) }

bindExpansionArgs :: NodeId -> [(NodeId, NodeId)] -> PresolutionM ()
bindExpansionArgs expansionRoot pairs = do
    uf0 <- gets psUnionFind
    c0 <- gets psConstraint
    let canonical = UnionFind.frWith uf0
        expansionRootC = canonical expansionRoot
        rootGen =
            let genIds = IntMap.keys (cGenNodes c0)
                pickRoot acc gidInt =
                    case acc of
                        Just _ -> acc
                        Nothing ->
                            let gref = genRef (GenNodeId gidInt)
                            in case Binding.lookupBindParentUnder canonical c0 gref of
                                Right Nothing -> Just gref
                                _ -> Nothing
            in foldl' pickRoot Nothing genIds
    forM_ pairs $ \(_bv, arg) -> do
        let argC = canonical arg
        case Binding.lookupBindParent c0 (typeRef argC) of
            Just _ -> pure ()
            Nothing ->
                case rootGen of
                    Just gref -> setBindParentM (typeRef argC) (gref, BindFlex)
                    Nothing ->
                        if Binding.isUpper c0 (typeRef expansionRootC) (typeRef argC)
                            then setBindParentM (typeRef argC) (typeRef expansionRootC, BindFlex)
                            else pure ()

-- | Build an edge witness from the chosen expansion recipe.
--
-- This is intentionally conservative: we record only the operations induced by
-- our current presolution lattice (Identity / Instantiate / ∀-intro / Compose).
-- More elaborate witnesses (raise/merge inside interiors, etc.) can be added
-- incrementally as the solver gains a more explicit instance-operation engine.
--
-- Normalization is deferred until the presolution constraint is finalized so
-- inert-locked weakening and canonicalization are reflected in the witness.
--
-- Note: we retain the TyExp body as the witness root for Φ/Σ translation,
-- and use the expansion root from EdgeTrace when normalizing.
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
--
-- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.2):
-- the interior I(r) is defined as all nodes transitively bound to r in the
-- binding tree. We compute this on the UF-quotient binding graph so it stays
-- consistent with presolution’s unification.
--
-- Requirements: 3.1, 3.2, 3.3
buildEdgeTrace :: EdgeId -> NodeId -> TyNode -> Expansion -> (CopyMap, InteriorSet, FrontierSet) -> PresolutionM EdgeTrace
buildEdgeTrace _eid left leftRaw expn (copyMap0, _interior0, _frontier0) = do
    bas <- binderArgsFromExpansion leftRaw expn
    root <- findRoot left
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
        canonicalizeInterior s =
            IntSet.fromList
                [ getNodeId (canonical (NodeId nid))
                | nid <- IntSet.toList s
                ]
    interiorRaw <- case Binding.interiorOfUnder canonical c0 (typeRef root) of
        Left err -> throwError (BindingTreeError err)
        Right s ->
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
        -- Fetch structure before merging
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
                uf0 <- gets psUnionFind
                let canonical = UnionFind.frWith uf0
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
        -- trace ("UnifyStructure " ++ show n1 ++ " " ++ show n2) $ return ()
        let isVarNode node = case node of
                TyVar{} -> True
                _ -> False
            trySetBound target bnd = do
                uf <- gets psUnionFind
                c0 <- gets psConstraint
                let canonical = UnionFind.frWith uf
                    targetC = canonical target
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
                -- Perform the merge
                unifyAcyclic n1 n2

                -- Recursively unify children if structures match
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
                        -- trace "Unifying Arrows" $ return ()
                        unifyStructure d1 d2
                        unifyStructure c1 c2
                    (TyForall { tnBody = b1 }, TyForall { tnBody = b2 }) -> do
                        -- trace "Unifying Foralls" $ return ()
                        unifyStructure b1 b2
                    -- Base types: no children to unify.
                    -- Mismatches: handled by Solve or suppressed (Presolution implies compatibility).
                    _ -> return ()

-- | Check if a node is a scheme root (directly under a GenNode).
isSchemeRootNode :: (NodeId -> NodeId) -> Constraint -> NodeId -> PresolutionM Bool
isSchemeRootNode canonical c0 nid =
    case Binding.lookupBindParentUnder canonical c0 (typeRef nid) of
        Left _ -> pure False
        Right (Just (GenRef gid, _)) ->
            case IntMap.lookup (genNodeKey gid) (cGenNodes c0) of
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
    uf <- gets psUnionFind
    c0 <- gets psConstraint
    let canonical = UnionFind.frWith uf
        lhsC = canonical lhs
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
    uf <- gets psUnionFind
    c0 <- gets psConstraint
    let canonical = UnionFind.frWith uf
        lhsC = canonical lhs
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

-- | Merge two expansions for the same variable.
-- This may trigger unifications if we merge two Instantiates.
-- (moved to MLF.Constraint.Presolution.Expansion)

debugBindParents :: String -> a -> a
debugBindParents msg value =
    if debugBindParentsEnabled
        then trace msg value
        else value

-- | Monadic version of debugBindParents for use in PresolutionM.
debugBindParentsM :: String -> PresolutionM ()
debugBindParentsM msg =
    when debugBindParentsEnabled (trace msg (pure ()))

nodeTag :: TyNode -> String
nodeTag node = case node of
    TyVar{} -> "TyVar"
    TyBottom{} -> "TyBottom"
    TyArrow{} -> "TyArrow"
    TyBase{} -> "TyBase"
    TyForall{} -> "TyForall"
    TyExp{} -> "TyExp"

debugBindParentsEnabled :: Bool
debugBindParentsEnabled =
    unsafePerformIO $ do
        enabled <- lookupEnv "MLF_DEBUG_BINDING"
        pure (maybe False (const True) enabled)
{-# NOINLINE debugBindParentsEnabled #-}
