{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{- |
Module      : MLF.Constraint.Presolution.Driver
Description : Presolution driver (Phase 4)
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the principal presolution phase of MLF type inference.
It processes instantiation edges in topological order to decide minimal
expansions for expansion variables.

Primary references:
  * Rémy & Yakobowski, "Graphic Type Constraints and Efficient Type
    Inference: from ML to MLF" (ICFP 2008) - §5 "Presolution"
-}
module MLF.Constraint.Presolution.Driver (
    computePresolution,
    processInstEdge
) where

import Control.Monad.State
import Control.Monad.Except (throwError)
import Control.Monad (foldM, forM, forM_)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe, mapMaybe)

import qualified MLF.Constraint.Root as ConstraintRoot
import qualified MLF.Util.UnionFind as UnionFind
import qualified MLF.Util.Order as Order
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Witness.OmegaExec as OmegaExec
import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.Inert as Inert
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.Ops (
    findRoot,
    getCanonicalNode,
    getNode,
    )
import MLF.Constraint.Presolution.Expansion (
    applyExpansion,
    applyExpansionEdgeTraced,
    bindExpansionRootLikeTarget,
    bindUnboundCopiedNodes,
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
-- We will likely need unification logic from Normalize,
-- but for now we'll implement the structure.

-- | Main entry point: compute principal presolution.
computePresolution
    :: AcyclicityResult
    -> Constraint
    -> Either PresolutionError PresolutionResult
computePresolution acyclicityResult constraint = do
    let constraint' = ConstraintRoot.ensureConstraintRoot constraint
    -- Initialize state
    let initialState = PresolutionState
            { psConstraint = constraint'
            , psPresolution = Presolution IntMap.empty
            , psUnionFind = IntMap.empty -- Should initialize from constraint if needed
            , psNextNodeId = maxNodeIdKeyOr0 constraint' + 1
            , psPendingWeakens = IntSet.empty
            , psEdgeExpansions = IntMap.empty
            , psEdgeWitnesses = IntMap.empty
            , psEdgeTraces = IntMap.empty
            }

    -- Run the presolution loop
    presState <- execStateT (runPresolutionLoop (arSortedEdges acyclicityResult)) initialState

    -- Materialize expansions, rewrite TyExp away, and apply UF canonicalization.
    (redirects, finalState) <- runPresolutionM presState $ do
        mapping <- materializeExpansions
        flushPendingWeakens
        redirects <- rewriteConstraint mapping
        weakenInertLockedNodesM
        normalizeEdgeWitnessesM
        pure redirects

    return PresolutionResult
        { prConstraint = psConstraint finalState
        , prEdgeExpansions = psEdgeExpansions finalState
        , prEdgeWitnesses = psEdgeWitnesses finalState
        , prEdgeTraces = psEdgeTraces finalState
        , prRedirects = redirects
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
        pure $ IntMap.fromListWith chooseMin (mapMaybe id pairs)

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
                    TyRoot { tnChildren = cs } -> TyRoot nid' (map canonical cs)
            in Just (getNodeId nid', node')

        -- traceCanonical n = let c = canonical n in trace ("Canonical " ++ show n ++ " -> " ++ show c) c

        newNodes = IntMap.fromListWith Canonicalize.chooseRepNode (mapMaybe rewriteNode (IntMap.elems (cNodes c)))
        eliminated' = rewriteEliminated canonical newNodes (cEliminatedVars c)
        genNodes' = rewriteGenNodes canonical newNodes (cGenNodes c)

        -- Canonicalize edge expansions
        canonicalizeExp ExpIdentity = ExpIdentity
        canonicalizeExp (ExpInstantiate args) = ExpInstantiate (map canonical args)
        canonicalizeExp (ExpForall levels) = ExpForall levels
        canonicalizeExp (ExpCompose exps) = ExpCompose (fmap canonicalizeExp exps)

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

        newTraces = IntMap.map canonicalizeTrace (psEdgeTraces st)

        bindingEdges0 = cBindParents c
        cStruct = c { cNodes = newNodes }

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
                    in foldl' addChild m (structuralChildren node)
            in IntMap.foldl' addNode IntMap.empty newNodes

        -- Canonicalize redirects (values in the map)
        -- mapping maps OldId -> NewId. NewId might be non-canonical.
        -- We want to return a map for ALL nodes that were redirected or merged.
        fullRedirects = IntMap.fromList
            [ (nid, canonical (NodeId nid))
            | nid <- IntMap.keys (cNodes c)
            ]

    newBindParents <- do
        let constraintRoot = ConstraintRoot.findConstraintRoot cStruct
            entries0 =
                [ (getNodeId child', (parent0, flag))
                | (childId, (parent0, flag)) <- IntMap.toList bindingEdges0
                , let child' = canonical (NodeId childId)
                , IntMap.member (getNodeId child') newNodes
                ]

            chooseBindParent :: NodeId -> NodeId -> PresolutionM (Maybe NodeId)
            chooseBindParent child' parent0 = do
                let inNodes nid = IntMap.member (getNodeId nid) newNodes
                    upper parent = Binding.isUpper cStruct parent child'

                    bindingAncestors :: NodeId -> [NodeId]
                    bindingAncestors start =
                        case Binding.bindingPathToRoot c start of
                            Left _ -> []
                            Right path -> drop 1 path

                    expBody :: NodeId -> Maybe NodeId
                    expBody nid =
                        case IntMap.lookup (getNodeId nid) (cNodes c) of
                            Just TyExp{ tnBody = b } -> Just b
                            _ -> Nothing

                    structuralParent :: NodeId -> Maybe NodeId
                    structuralParent nid =
                        case IntMap.lookup (getNodeId nid) incomingParents of
                            Nothing -> Nothing
                            Just ps ->
                                case IntSet.toList ps of
                                    [] -> Nothing
                                    (p:_) -> Just (NodeId p)

                    candidates =
                        map canonical $
                            [ parent0 ]
                                ++ mapMaybe expBody [parent0]
                                ++ bindingAncestors parent0
                                ++ maybe [] pure (structuralParent child')
                                ++ maybe [] pure constraintRoot

                    firstValid = \case
                        [] -> Nothing
                        (p:ps) ->
                            if p == child' || not (inNodes p) || not (upper p)
                                then firstValid ps
                                else Just p

                case firstValid candidates of
                    Just p -> pure (Just p)
                    Nothing ->
                        if IntMap.member (getNodeId child') incomingParents
                            then throwError $
                                BindingTreeError $
                                    InvalidBindingTree $
                                        "rewriteConstraint: could not find an upper binding parent for "
                                            ++ show child'
                                            ++ " (original parent "
                                            ++ show parent0
                                            ++ ")"
                            else pure Nothing

            insertOne :: BindParents -> (Int, (NodeId, BindFlag)) -> PresolutionM BindParents
            insertOne bp (childId, (parent, flag)) =
                case IntMap.lookup childId bp of
                    Nothing -> pure (IntMap.insert childId (parent, flag) bp)
                    Just (parent0, flag0)
                        | parent0 == parent ->
                            let flag' = max flag0 flag
                            in pure (IntMap.insert childId (parent, flag') bp)
                        | otherwise ->
                            -- UF canonicalization can transiently create multiple binding
                            -- parents for the same rewritten node. Resolve this
                            -- deterministically by keeping the first parent and taking
                            -- the max flag (matching the UF-rewrite strategy used in
                            -- Normalize/Solve).
                            let flag' = max flag0 flag
                            in pure (IntMap.insert childId (parent0, flag') bp)

        entries' <- fmap concat $ forM entries0 $ \(childId, (parent0, flag)) -> do
            let child' = NodeId childId
            mp <- chooseBindParent child' parent0
            pure $ case mp of
                Nothing -> []
                Just parent ->
                    if parent == child'
                        then []
                        else [(childId, (parent, flag))]

        foldM insertOne IntMap.empty entries'

    let c0' = c
            { cNodes = newNodes
            , cInstEdges = []
            , cUnifyEdges = Canonicalize.rewriteUnifyEdges canonical (cUnifyEdges c)
            , cBindParents = newBindParents
            , cEliminatedVars = eliminated'
            , cGenNodes = genNodes'
            }
        -- Keep the synthetic constraint root total after rewriting/canonicalization.
        c' = ConstraintRoot.ensureConstraintRoot c0'

    case Binding.checkBindingTree c' of
        Left err -> throwError (BindingTreeError err)
        Right () -> pure ()

    put st
        { psConstraint = c'
        , psEdgeExpansions = newExps
        , psEdgeWitnesses = newWitnesses
        , psEdgeTraces = newTraces
        }

    return fullRedirects

-- | Weaken inert-locked nodes to obtain a translatable presolution.
--
-- Thesis alignment: Corollary 15.2.5 removes inert-locked nodes, and
-- Definition 15.2.10 (condition 1) requires none remain.
weakenInertLockedNodesM :: PresolutionM ()
weakenInertLockedNodesM = do
    c0 <- gets psConstraint
    case Inert.weakenInertLockedNodes c0 of
        Left err -> throwError (BindingTreeError err)
        Right c1 -> do
            case Inert.inertLockedNodes c1 of
                Left err -> throwError (BindingTreeError err)
                Right locked ->
                    if IntSet.null locked
                        then modify' $ \st -> st { psConstraint = c1 }
                        else throwError (InternalError "weakenInertLockedNodesM: inert-locked nodes remain after weakening")

-- | Normalize edge witnesses against the finalized presolution constraint.
normalizeEdgeWitnessesM :: PresolutionM ()
normalizeEdgeWitnessesM = do
    c0 <- gets psConstraint
    traces <- gets psEdgeTraces
    witnesses0 <- gets psEdgeWitnesses
    let rewriteNodeWith copyMap nid =
            IntMap.findWithDefault nid (getNodeId nid) copyMap
        weakened =
            IntSet.fromList
                [ getNodeId (rewriteNodeWith copyMap n)
                | (eid, w0) <- IntMap.toList witnesses0
                , let copyMap = maybe IntMap.empty etCopyMap (IntMap.lookup eid traces)
                , StepOmega (OpWeaken n) <- ewSteps w0
                ]
    witnesses <- forM (IntMap.toList witnesses0) $ \(eid, w0) -> do
        let (edgeRoot, copyMap, binderArgs0) =
                case IntMap.lookup eid traces of
                    Nothing -> (ewRoot w0, IntMap.empty, [])
                    Just tr -> (etRoot tr, etCopyMap tr, etBinderArgs tr)
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
        let interiorRoot =
                case Binding.lookupBindParent c0 edgeRoot of
                    Just (parent, _) -> parent
                    Nothing -> edgeRoot
        interior <- case Binding.interiorOf c0 interiorRoot of
            Left err -> throwError (BindingTreeError err)
            Right s -> pure s
        let steps0 = map rewriteStep (ewSteps w0)
            orderKeys = Order.orderKeysFromConstraintWith id c0 interiorRoot Nothing
            env =
                OmegaNormalizeEnv
                    { oneRoot = edgeRoot
                    , interior = interior
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

rewriteEliminated :: (NodeId -> NodeId) -> IntMap TyNode -> EliminatedVars -> EliminatedVars
rewriteEliminated canon nodes0 elims0 =
    IntSet.fromList
        [ getNodeId vC
        | vid <- IntSet.toList elims0
        , let vC = canon (NodeId vid)
        , IntMap.member (getNodeId vC) nodes0
        ]

rewriteGenNodes :: (NodeId -> NodeId) -> IntMap TyNode -> IntMap.IntMap GenNode -> IntMap.IntMap GenNode
rewriteGenNodes canon nodes0 gen0 =
    let rewriteOne g =
            let gidC = canon (NodeId (genNodeKey (gnId g)))
                gid = getNodeId gidC
                schemes' =
                    [ canon s
                    | s <- gnSchemes g
                    , IntMap.member (getNodeId (canon s)) nodes0
                    ]
                g' = g { gnId = GenNodeId gid, gnSchemes = schemes' }
            in (gid, g')
    in IntMap.fromListWith (\a _ -> a) (map rewriteOne (IntMap.elems gen0))

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

    -- Resolve nodes.
    -- For LHS, we check the raw node first to handle TyExp properly
    -- even if it has been unified/replaced in previous steps.
    n1Raw <- getNode n1Id
    n2 <- getCanonicalNode n2Id

    case n1Raw of
        TyExp { tnExpVar = s, tnBody = _bodyId } -> do
            let n1 = n1Raw
            -- n1 is an expansion node. We need to ensure s expands enough to cover n2.
            currentExp <- getExpansion s

            -- Decide required expansion based on n2
            (reqExp, unifications) <- decideMinimalExpansion n1 n2

            -- Merge with current expansion
            finalExp <- mergeExpansions s currentExp reqExp

            -- Update presolution
            setExpansion s finalExp
            recordEdgeExpansion edgeId finalExp

            -- Perform unifications requested by expansion decision
            mapM_ (uncurry unifyAcyclic) unifications

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
                    let baseOps = [op | StepOmega op <- baseSteps]
                    -- Eagerly materialize and unify to resolve the constraint immediately.
                    -- This ensures that the expansion result is unified with the target (n2),
                    -- and TyExp is unified with the result.
                    (resNodeId, (copyMap0, interior0)) <- applyExpansionEdgeTraced finalExp n1
                    
                    -- Paper alignment (`papers/xmlf.txt` §3.2): bind the expansion root
                    -- at the same binder as the edge target. This ensures the expansion
                    -- root is in the correct interior I(r) for subsequent operations.
                    bindExpansionRootLikeTarget resNodeId (tnId n2)
                    
                    -- Bind all copied nodes that don't have binding parents to the
                    -- expansion root. This ensures the binding tree remains valid.
                    bindUnboundCopiedNodes copyMap0 interior0 resNodeId
                    
                    bas <- binderArgsFromExpansion n1 finalExp
                    binderMetas <- forM bas $ \(bv, _arg) ->
                        case IntMap.lookup (getNodeId bv) copyMap0 of
                            Just meta -> pure (bv, meta)
                            Nothing ->
                                throwError (InternalError ("processInstEdge: missing binder-meta copy for " ++ show bv))
                    interior <- edgeInteriorExact resNodeId
                    eu0 <- initEdgeUnifyState binderMetas interior resNodeId
                    let omegaEnv = mkOmegaExecEnv copyMap0
                    (_a, eu1) <- runStateT
                        (do
                            OmegaExec.executeOmegaBaseOpsPre omegaEnv baseOps
                            unifyStructureEdge resNodeId (tnId n2)
                            unifyAcyclicEdge (tnId n1) resNodeId
                            OmegaExec.executeOmegaBaseOpsPost omegaEnv baseOps
                        )
                        eu0
                    pure (resNodeId, (copyMap0, interior), eusOps eu1)

            tr <- buildEdgeTrace edgeId n1Id n1 finalExp expTrace
            recordEdgeTrace edgeId tr
            w <- buildEdgeWitness edgeId n1Id n2Id n1 finalExp extraOps edgeRoot
            recordEdgeWitness edgeId w

        _ -> do
            n1 <- getCanonicalNode n1Id
            -- n1 is not an expansion node.
            -- This is a standard instantiation constraint (or just subtyping).
            -- "If the left hand side is not an expansion node, it must be equal to the right hand side"
            -- (Simplification for now, might need refinement for full MLF)
            recordEdgeExpansion edgeId ExpIdentity
            let edgeRoot = tnId n1
            w <- buildEdgeWitness edgeId n1Id n2Id n1Raw ExpIdentity [] edgeRoot
            recordEdgeWitness edgeId w
            unifyStructure (tnId n1) (tnId n2)

-- | Record a witness for an instantiation edge.
recordEdgeWitness :: EdgeId -> EdgeWitness -> PresolutionM ()
recordEdgeWitness (EdgeId eid) w =
    modify $ \st -> st { psEdgeWitnesses = IntMap.insert eid w (psEdgeWitnesses st) }

recordEdgeTrace :: EdgeId -> EdgeTrace -> PresolutionM ()
recordEdgeTrace (EdgeId eid) tr =
    modify $ \st -> st { psEdgeTraces = IntMap.insert eid tr (psEdgeTraces st) }

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
    baseSteps <- witnessFromExpansion root leftRaw expn
    let steps0 = integratePhase2Steps baseSteps extraOps
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

-- | Build an edge trace.
--
-- Paper alignment (`papers/xmlf.txt` §3.2): the interior I(r) is defined as all
-- nodes transitively bound to r in the binding tree. We compute this on the
-- UF-quotient binding graph so it stays consistent with presolution’s unification.
--
-- Requirements: 3.1, 3.2, 3.3
buildEdgeTrace :: EdgeId -> NodeId -> TyNode -> Expansion -> (CopyMap, InteriorSet) -> PresolutionM EdgeTrace
buildEdgeTrace _eid left leftRaw expn (copyMap0, _interior0) = do
    bas <- binderArgsFromExpansion leftRaw expn
    root <- findRoot left
    interior <- edgeInteriorExact root
    pure EdgeTrace { etRoot = root, etBinderArgs = bas, etInterior = interior, etCopyMap = copyMap0 }

unifyStructure :: NodeId -> NodeId -> PresolutionM ()
unifyStructure n1 n2 = do
    root1 <- findRoot n1
    root2 <- findRoot n2
    if root1 == root2 then return ()
    else do
        -- Fetch structure before merging
        node1 <- getCanonicalNode n1
        node2 <- getCanonicalNode n2

        -- trace ("UnifyStructure " ++ show n1 ++ " " ++ show n2) $ return ()

        -- Perform the merge
        unifyAcyclic n1 n2

        -- Recursively unify children if structures match
        case (node1, node2) of
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

-- | Merge two expansions for the same variable.
-- This may trigger unifications if we merge two Instantiates.
-- (moved to MLF.Constraint.Presolution.Expansion)
