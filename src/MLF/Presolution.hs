{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{- |
Module      : MLF.Presolution
Description : Phase 4 - Principal Presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the principal presolution phase of MLF type inference.
It processes instantiation edges in topological order to decide minimal
expansions for expansion variables.

Primary references:
  * Rémy & Yakobowski, "Graphic Type Constraints and Efficient Type
    Inference: from ML to MLF" (ICFP 2008) - §5 "Presolution"
-}
module MLF.Presolution (
    -- * Main API
    computePresolution,
    PresolutionResult(..),
    PresolutionError(..),

    -- * Internal types (exported for testing)
    PresolutionState(..),
    EdgeTrace(..),
    runPresolutionM,

    -- * Building blocks (exported for testing)
    decideMinimalExpansion,
    processInstEdge,
    unifyAcyclicRawWithRaiseTrace,
    runEdgeUnifyForTest,
    instantiateScheme,
    instantiateSchemeWithTrace,
    mergeExpansions,
    normalizeInstanceOps,
    applyExpansion
) where

import Control.Monad.State
import Control.Monad.Except (throwError)
import Control.Monad (forM, forM_, zipWithM, zipWithM_, when, foldM)
import qualified Data.List.NonEmpty as NE
import Data.List (mapAccumL, partition, sortOn)
import Data.Ord (Down(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe, mapMaybe)

import qualified MLF.GNodeOps as GNodeOps
import qualified MLF.Order as Order
import qualified MLF.BindingAdjustment as BindingAdjustment
import qualified MLF.GraphOps as GraphOps
import qualified MLF.UnionFind as UnionFind
import qualified MLF.Binding as Binding
import qualified MLF.OmegaExec as OmegaExec
import qualified MLF.VarStore as VarStore
import MLF.Types
import MLF.Acyclicity (AcyclicityResult(..))
-- We will likely need unification logic from Normalize,
-- but for now we'll implement the structure.

-- | Result of the presolution phase.
data PresolutionResult = PresolutionResult
    { prConstraint :: Constraint
    , prEdgeExpansions :: IntMap Expansion
    , prEdgeWitnesses :: IntMap EdgeWitness
    , prEdgeTraces :: IntMap EdgeTrace
    , prRedirects :: IntMap NodeId -- ^ Map from old TyExp IDs to their replacement IDs
    } deriving (Eq, Show)

-- | Errors that can occur during presolution.
data PresolutionError
    = UnmatchableTypes NodeId NodeId String  -- ^ Type mismatch during expansion
    | UnresolvedExpVar ExpVarId              -- ^ ExpVar couldn't be resolved
    | MissingGNode GNodeId                   -- ^ Referenced generalization level absent
    | ArityMismatch String Int Int           -- ^ (context, expected, actual)
    | InstantiateOnNonForall NodeId          -- ^ Tried to instantiate a non-forall node
    | NodeLookupFailed NodeId                -- ^ Missing node in constraint
    | OccursCheckPresolution NodeId NodeId   -- ^ Unification would make node reachable from itself
    | BindingTreeError BindingError          -- ^ Invalid binding tree when binding edges are in use
    | InternalError String                   -- ^ Unexpected internal state
    deriving (Eq, Show)

-- | State maintained during the presolution process.
data PresolutionState = PresolutionState
    { psConstraint :: Constraint
    , psPresolution :: Presolution
    , psUnionFind :: IntMap NodeId
    , psNextNodeId :: Int
    , psEdgeExpansions :: IntMap Expansion
    , psEdgeWitnesses :: IntMap EdgeWitness
    , psEdgeTraces :: IntMap EdgeTrace
    }
    deriving (Eq, Show)

-- | Per-edge provenance for instantiation-related operations.
--
-- This is an internal aid for gradually aligning presolution witnesses with
-- `papers/xmlf.txt`’s normalized instance-operation language (Fig. 10). For now,
-- we only track the binder↦argument pairing chosen by `ExpInstantiate`.
data EdgeTrace = EdgeTrace
    { etRoot :: NodeId
    , etBinderArgs :: [(NodeId, NodeId)] -- ^ (binder node, instantiation argument node)
    , etInterior :: IntSet.IntSet -- ^ Nodes in I(r) (exact, from the binding tree).
    , etCopyMap :: IntMap NodeId -- ^ Provenance: original node -> copied/replaced node
    }
    deriving (Eq, Show)

type CopyMap = IntMap NodeId
type InteriorSet = IntSet.IntSet

emptyTrace :: (CopyMap, InteriorSet)
emptyTrace = (IntMap.empty, IntSet.empty)

unionTrace :: (CopyMap, InteriorSet) -> (CopyMap, InteriorSet) -> (CopyMap, InteriorSet)
unionTrace (m1, s1) (m2, s2) = (IntMap.union m1 m2, IntSet.union s1 s2)

data CopyState = CopyState
    { csCache :: IntMap NodeId
    , csCopyMap :: IntMap NodeId
    , csInterior :: IntSet.IntSet
    }

data EdgeUnifyState = EdgeUnifyState
    { eusInteriorRoots :: IntSet.IntSet
    , eusBindersByRoot :: IntMap IntSet.IntSet -- ^ UF-root -> binder NodeIds whose args live in that class
    , eusInteriorByRoot :: IntMap IntSet.IntSet -- ^ UF-root -> interior NodeIds (all nodes in I(r))
    , eusQuantLevel :: Maybe GNodeId -- ^ Quantifier level of the instantiated binders (if any)
    , eusEliminatedBinders :: IntSet.IntSet -- ^ binders eliminated by Merge/RaiseMerge ops we record
    , eusBinderMeta :: IntMap NodeId -- ^ source binder -> copied/meta node in χe
    , eusOrderKeys :: IntMap Order.OrderKey -- ^ order keys for meta nodes (edge-local ≺)
    , eusOps :: [InstanceOp]
    }

type EdgeUnifyM = StateT EdgeUnifyState PresolutionM

-- | The Presolution monad.
type PresolutionM = StateT PresolutionState (Either PresolutionError)

-- | Run a PresolutionM action with an initial state (testing helper).
runPresolutionM :: PresolutionState -> PresolutionM a -> Either PresolutionError (a, PresolutionState)
runPresolutionM st action = runStateT action st

-- | Testing helper: run a single edge-local unification and return the recorded
-- instance-operation witness slice.
--
-- This bypasses expansion copying and is intended for unit tests that want to
-- assert the precise `OpRaise` targets produced by binding-parent harmonization
-- (including the “no spray” behavior for interior nodes).
runEdgeUnifyForTest
    :: NodeId -- ^ edge root (for ≺ ordering keys)
    -> IntSet.IntSet -- ^ interior nodes (I(r))
    -> NodeId -- ^ left node to unify
    -> NodeId -- ^ right node to unify
    -> PresolutionM [InstanceOp]
runEdgeUnifyForTest edgeRoot interior n1 n2 = do
    requireValidBindingTree
    eu0 <- initEdgeUnifyState [] interior edgeRoot
    (_a, eu1) <- runStateT (unifyAcyclicEdge n1 n2) eu0
    pure (eusOps eu1)

requireValidBindingTree :: PresolutionM ()
requireValidBindingTree = do
    c0 <- gets psConstraint
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
    case Binding.checkBindingTreeUnder canonical c0 of
        Left err -> throwError (BindingTreeError err)
        Right () -> pure ()

edgeInteriorExact :: NodeId -> PresolutionM IntSet.IntSet
edgeInteriorExact root0 = do
    c0 <- gets psConstraint
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
    case Binding.interiorOfUnder canonical c0 root0 of
        Left err -> throwError (BindingTreeError err)
        Right interior -> pure interior

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
            , psNextNodeId = findMaxNodeId constraint + 1
            , psEdgeExpansions = IntMap.empty
            , psEdgeWitnesses = IntMap.empty
            , psEdgeTraces = IntMap.empty
            }

    -- Run the presolution loop
    presState <- execStateT (runPresolutionLoop (arSortedEdges acyclicityResult)) initialState

    -- Materialize expansions, rewrite TyExp away, and apply UF canonicalization.
    (redirects, finalState) <- runPresolutionM presState $ do
        mapping <- materializeExpansions
        rewriteConstraint mapping

    return PresolutionResult
        { prConstraint = psConstraint finalState
        , prEdgeExpansions = psEdgeExpansions finalState
        , prEdgeWitnesses = psEdgeWitnesses finalState
        , prEdgeTraces = psEdgeTraces finalState
        , prRedirects = redirects
        }

-- | Helper to find the next available NodeId
findMaxNodeId :: Constraint -> Int
findMaxNodeId c =
    let maxNode = if IntMap.null (cNodes c) then 0 else fst (IntMap.findMax (cNodes c))
    in maxNode

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

        isVar TyVar{} = True
        isVar _ = False

        choose new old = case (isVar old, isVar new) of
            (True, False) -> new
            (False, True) -> old
            _ -> old

        rewriteNode :: TyNode -> Maybe (Int, TyNode)
        rewriteNode TyExp{} = Nothing
        rewriteNode n =
            let nid' = canonical (tnId n)
                node' = case n of
                    TyVar { tnVarLevel = l } -> TyVar nid' l
                    TyArrow { tnDom = d, tnCod = cod } -> TyArrow nid' (canonical d) (canonical cod)
                    TyBase { tnBase = b } -> TyBase nid' b
                    TyForall { tnOwnerLevel = ownerLvl, tnQuantLevel = quantLvl, tnBody = b } ->
                        TyForall nid' quantLvl ownerLvl (canonical b)
            in Just (getNodeId nid', node')

        rewriteUnify (UnifyEdge l r) = UnifyEdge (canonical l) (canonical r)

        -- traceCanonical n = let c = canonical n in trace ("Canonical " ++ show n ++ " -> " ++ show c) c

        newNodes = IntMap.fromListWith choose (mapMaybe rewriteNode (IntMap.elems (cNodes c)))

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

        canonicalizeWitness :: EdgeWitness -> EdgeWitness
        canonicalizeWitness w =
            let InstanceWitness ops = ewWitness w
            in w
                { ewLeft = canonical (ewLeft w)
                , ewRight = canonical (ewRight w)
                , ewRoot = canonical (ewRoot w)
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

                addNode m node = case node of
                    TyArrow{ tnId = p, tnDom = d, tnCod = cod } ->
                        addOne p cod (addOne p d m)
                    TyForall{ tnId = p, tnBody = b } ->
                        addOne p b m
                    _ -> m
            in IntMap.foldl' addNode IntMap.empty newNodes

        -- Canonicalize redirects (values in the map)
        -- mapping maps OldId -> NewId. NewId might be non-canonical.
        -- We want to return a map for ALL nodes that were redirected or merged.
        fullRedirects = IntMap.fromList
            [ (nid, canonical (NodeId nid))
            | nid <- IntMap.keys (cNodes c)
            ]

    newBindParents <- do
        let entries0 =
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
                    bindingAncestors start = go IntSet.empty start
                      where
                        go seen nid
                            | IntSet.member (getNodeId nid) seen = []
                            | otherwise =
                                case Binding.lookupBindParent c nid of
                                    Nothing -> []
                                    Just (p, _) -> p : go (IntSet.insert (getNodeId nid) seen) p

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

    let c' = c
            { cNodes = newNodes
            , cInstEdges = []
            , cUnifyEdges = map rewriteUnify (cUnifyEdges c)
            , cBindParents = newBindParents
            }

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

            (expTrace, extraOps) <- if finalExp == ExpIdentity
                then pure (emptyTrace, [])
                else do
                    let root = case n1 of
                            TyExp{ tnBody = b } -> b
                            _ -> tnId n1
                    InstanceWitness baseOps <- witnessFromExpansion root n1 finalExp
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
                    pure ((copyMap0, interior), eusOps eu1)

            w <- buildEdgeWitness edgeId n1Id n2Id n1 finalExp extraOps
            recordEdgeWitness edgeId w
            tr <- buildEdgeTrace edgeId n1Id n1 finalExp expTrace
            recordEdgeTrace edgeId tr

        _ -> do
            n1 <- getCanonicalNode n1Id
            -- n1 is not an expansion node.
            -- This is a standard instantiation constraint (or just subtyping).
            -- "If the left hand side is not an expansion node, it must be equal to the right hand side"
            -- (Simplification for now, might need refinement for full MLF)
            recordEdgeExpansion edgeId ExpIdentity
            w <- buildEdgeWitness edgeId n1Id n2Id n1Raw ExpIdentity []
            recordEdgeWitness edgeId w
            unifyStructure (tnId n1) (tnId n2)

-- | Record a witness for an instantiation edge.
recordEdgeWitness :: EdgeId -> EdgeWitness -> PresolutionM ()
recordEdgeWitness (EdgeId eid) w =
    modify $ \st -> st { psEdgeWitnesses = IntMap.insert eid w (psEdgeWitnesses st) }

recordEdgeTrace :: EdgeId -> EdgeTrace -> PresolutionM ()
recordEdgeTrace (EdgeId eid) tr =
    modify $ \st -> st { psEdgeTraces = IntMap.insert eid tr (psEdgeTraces st) }

-- | Build an ω executor environment for χe base ops (Graft/Merge/Weaken).
--
-- This is used to execute the base operations induced directly by
-- `ExpInstantiate` as real χe transformations, but split into two phases so
-- that bounded binders can still trigger `RaiseMerge` during unification with
-- the edge target.
--
-- Paper alignment (`papers/xmlf.txt` §3.4): `Weaken` occurs after other
-- operations on nodes below it. Executing it eagerly can preempt the unification
-- that should be witnessed as `RaiseMerge`.
mkOmegaExecEnv :: CopyMap -> OmegaExec.OmegaExecEnv EdgeUnifyM
mkOmegaExecEnv copyMap =
    OmegaExec.OmegaExecEnv
        { omegaMetaFor = metaFor
        , omegaLookupMeta = \bv -> pure (IntMap.lookup (getNodeId bv) copyMap)
        , omegaSetVarBound = \meta mb -> lift $ setVarBound meta mb
        , omegaDropVarBind = \meta -> lift $ dropVarBind meta
        , omegaUnifyNoMerge = unifyAcyclicEdgeNoMerge
        , omegaRecordEliminate = recordEliminate
        , omegaIsEliminated = isEliminated
        , omegaEliminatedBinders = do
            elims <- gets eusEliminatedBinders
            pure (map NodeId (IntSet.toList elims))
        , omegaWeakenMeta = \meta -> do
            metaRoot <- lift $ findRoot meta
            -- Paper ω semantics: Weaken changes only the binding-edge flag
            -- (flex → rigid) and does not change the term-DAG.
            c0 <- lift $ gets psConstraint
            -- Redundant weakens can arise when multiple edges share the same
            -- expansion variable (merged χe). Treat "already rigid" as a no-op.
            case Binding.lookupBindParent c0 metaRoot of
                -- In our multi-root constraint graphs, unification can make a
                -- binder-meta equal to a binding root (no parent). Weaken is not
                -- defined for roots, so treat this as a no-op rather than
                -- failing the whole edge.
                Nothing -> pure ()
                Just (_p, BindRigid) -> pure ()
                Just _ ->
                    case GraphOps.applyWeaken metaRoot c0 of
                        Left err -> lift $ throwError (BindingTreeError err)
                        Right (c', _op) -> lift $ modify' (\st -> st { psConstraint = c' })
        }
  where
    metaFor :: NodeId -> EdgeUnifyM NodeId
    metaFor bv =
        case IntMap.lookup (getNodeId bv) copyMap of
            Just m -> pure m
            Nothing ->
                lift $ throwError (InternalError ("mkOmegaExecEnv: missing copy for binder " ++ show bv))

-- | Edge-local union like 'unifyAcyclicEdge', but without emitting merge-like
-- witness ops. This is used to *execute* base `Merge` operations (already
-- recorded in Ω) without accidentally introducing an opposing Phase-2 merge.
unifyAcyclicEdgeNoMerge :: NodeId -> NodeId -> EdgeUnifyM ()
unifyAcyclicEdgeNoMerge n1 n2 = do
    root1 <- lift $ findRoot n1
    root2 <- lift $ findRoot n2
    when (root1 /= root2) $ do
        st0 <- get
        let r1 = getNodeId root1
            r2 = getNodeId root2
            inInt1 = IntSet.member r1 (eusInteriorRoots st0)
            inInt2 = IntSet.member r2 (eusInteriorRoots st0)
            bs1 = IntMap.findWithDefault IntSet.empty r1 (eusBindersByRoot st0)
            bs2 = IntMap.findWithDefault IntSet.empty r2 (eusBindersByRoot st0)
            bs = IntSet.union bs1 bs2

        trace <- lift $ unifyAcyclicRootsWithRaiseTrace root1 root2
        let int1 = IntMap.findWithDefault IntSet.empty r1 (eusInteriorByRoot st0)
            int2 = IntMap.findWithDefault IntSet.empty r2 (eusInteriorByRoot st0)
            intAll = IntSet.union int1 int2
        recordRaisesFromTrace intAll trace

        modify $ \st ->
            let roots' =
                    if inInt1 || inInt2
                        then IntSet.insert r2 (IntSet.delete r1 (eusInteriorRoots st))
                        else eusInteriorRoots st
                binders' =
                    let m0 = eusBindersByRoot st
                        m1 = if IntSet.null bs then IntMap.delete r1 m0 else IntMap.insert r2 bs (IntMap.delete r1 m0)
                    in m1
                interior' =
                    let m0 = eusInteriorByRoot st
                        m1 = if IntSet.null intAll then IntMap.delete r1 m0 else IntMap.insert r2 intAll (IntMap.delete r1 m0)
                    in m1
            in st { eusInteriorRoots = roots', eusBindersByRoot = binders', eusInteriorByRoot = interior' }

-- | Build an edge witness from the chosen expansion recipe.
--
-- This is intentionally conservative: we record only the operations induced by
-- our current presolution lattice (Identity / Instantiate / ∀-intro / Compose).
-- More elaborate witnesses (raise/merge inside interiors, etc.) can be added
-- incrementally as the solver gains a more explicit instance-operation engine.
buildEdgeWitness :: EdgeId -> NodeId -> NodeId -> TyNode -> Expansion -> [InstanceOp] -> PresolutionM EdgeWitness
buildEdgeWitness eid left right leftRaw expn extraOps = do
    root <- case leftRaw of
        TyExp{ tnBody = b } -> pure b
        _ -> pure left
    forallIntros <- forallIntroSuffixCount expn
    InstanceWitness baseOps <- witnessFromExpansion root leftRaw expn
    let ops = normalizeInstanceOps (integratePhase2Ops baseOps extraOps)
        iw = InstanceWitness ops
    pure EdgeWitness
        { ewEdgeId = eid
        , ewLeft = left
        , ewRight = right
        , ewRoot = root
        , ewForallIntros = forallIntros
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

integratePhase2Ops :: [InstanceOp] -> [InstanceOp] -> [InstanceOp]
integratePhase2Ops baseOps extraOps =
    let isBarrier = \case
            OpRaise{} -> True
            _ -> False

        isGraft = \case
            OpGraft{} -> True
            _ -> False

        isWeaken = \case
            OpWeaken{} -> True
            _ -> False

        isMergeLike = \case
            OpMerge{} -> True
            OpRaiseMerge{} -> True
            _ -> False

        elimBinderByMerge = \case
            OpMerge n _ -> Just n
            OpRaiseMerge n _ -> Just n
            _ -> Nothing

        elimBinder = \case
            OpMerge n _ -> Just n
            OpRaiseMerge n _ -> Just n
            OpWeaken n -> Just n
            _ -> Nothing

        baseMerged =
            IntSet.fromList
                [ getNodeId n
                | op <- baseOps
                , Just n <- [elimBinderByMerge op]
                ]

        (extraRaises, extraOps') =
            partition
                (\case
                    OpRaise{} -> True
                    _ -> False
                )
                extraOps

        raisesByBinder0 =
            foldl'
                (\m op -> case op of
                    OpRaise n -> IntMap.insertWith (++) (getNodeId n) [op] m
                    _ -> m
                )
                IntMap.empty
                extraRaises

        extraElimOps =
            [ op
            | op <- extraOps'
            , Just n <- [elimBinder op]
            , not (IntSet.member (getNodeId n) baseMerged)
            ]

        extraElims =
            IntSet.fromList
                [ getNodeId n
                | op <- extraElimOps
                , Just n <- [elimBinder op]
                ]

        removeElimOps op = case op of
            OpGraft _ bv -> not (IntSet.member (getNodeId bv) extraElims)
            OpWeaken bv -> not (IntSet.member (getNodeId bv) extraElims)
            _ -> True

        baseOps' = filter removeElimOps baseOps

        (beforeBarrier, afterBarrier) = break isBarrier baseOps'

        grafts = [ op | op <- beforeBarrier, isGraft op ]
        weakens = [ op | op <- beforeBarrier, isWeaken op ]
        mergesBase = [ op | op <- beforeBarrier, isMergeLike op ]
        others = [ op | op <- beforeBarrier, not (isGraft op || isWeaken op || isMergeLike op) ]

        takeRaises raisesMap n =
            case IntMap.lookup (getNodeId n) raisesMap of
                Nothing -> ([], raisesMap)
                Just rs -> (rs, IntMap.delete (getNodeId n) raisesMap)

        mergeBlock raisesMap op = case elimBinder op of
            Just n ->
                let (rs, raisesMap') = takeRaises raisesMap n
                in (raisesMap', rs ++ [op])
            Nothing -> (raisesMap, [op])

        (raisesAfterBaseMerges, mergesBaseBlocks) =
            mapAccumL mergeBlock raisesByBinder0 mergesBase

        (raisesAfterExtraMerges, extraElimBlocks) =
            mapAccumL mergeBlock raisesAfterBaseMerges extraElimOps

        mergesAll = mergesBaseBlocks ++ extraElimBlocks

        elimKey op = case op of
            OpMerge n _ -> getNodeId n
            OpRaiseMerge n _ -> getNodeId n
            _ -> -1

        mergesSorted = concat (sortOn (Down . elimKey . last) mergesAll)
        (raisesAfterWeakens, weakensWithRaises) =
            foldl'
                (\(raisesMap, acc) op -> case elimBinder op of
                    Just n | isWeaken op ->
                        let (rs, raisesMap') = takeRaises raisesMap n
                        in (raisesMap', acc ++ rs ++ [op])
                    _ -> (raisesMap, acc ++ [op])
                )
                (raisesAfterExtraMerges, [])
                weakens

        leftoverRaises = concat (IntMap.elems raisesAfterWeakens)
    in grafts ++ mergesSorted ++ others ++ leftoverRaises ++ weakensWithRaises ++ afterBarrier

-- | Lightweight normalization of a recorded witness op sequence.
--
-- This is a conservative step toward `papers/xmlf.txt`’s “normalized” Ω language:
--  * ensure a binder is not eliminated twice (Weaken after Merge/RaiseMerge), and
--  * ensure `OpWeaken n` appears after other ops mentioning `n` when possible.
normalizeInstanceOps :: [InstanceOp] -> [InstanceOp]
normalizeInstanceOps ops0 =
    let ops1 = coalesceRaiseMerge ops0
    in go ops1
  where
    isBarrier = \case
        OpRaise{} -> True
        _ -> False

    isWeaken = \case
        OpWeaken{} -> True
        _ -> False

    isMergeLike = \case
        OpMerge{} -> True
        OpRaiseMerge{} -> True
        _ -> False

    elimBinder = \case
        OpMerge n _ -> Just n
        OpRaiseMerge n _ -> Just n
        OpWeaken n -> Just n
        _ -> Nothing

    normalizePreRaise :: [InstanceOp] -> [InstanceOp]
    normalizePreRaise ops =
        let mergeElims =
                IntSet.fromList
                    [ getNodeId n
                    | op <- ops
                    , Just n <- [case op of OpMerge n _ -> Just n; OpRaiseMerge n _ -> Just n; _ -> Nothing]
                    ]

            -- If an op eliminates a binder via Merge/RaiseMerge, do not also emit Weaken/Graft on it.
            opsNoRedundant =
                [ op
                | op <- ops
                , case op of
                    OpWeaken n -> not (IntSet.member (getNodeId n) mergeElims)
                    OpGraft _ n -> not (IntSet.member (getNodeId n) mergeElims)
                    _ -> True
                ]

            opsDedupElims = dedupElims opsNoRedundant

            grafts = [ op | op <- opsDedupElims, case op of OpGraft{} -> True; _ -> False ]
            merges = [ op | op <- opsDedupElims, isMergeLike op ]
            others = [ op | op <- opsDedupElims, not (case op of OpGraft{} -> True; _ -> False) && not (isMergeLike op) && not (isWeaken op) ]
            weakens = [ op | op <- opsDedupElims, isWeaken op ]

            elimKey = \case
                OpMerge n _ -> getNodeId n
                OpRaiseMerge n _ -> getNodeId n
                _ -> -1

            mergesSorted = sortOn (Down . elimKey) merges
        in grafts ++ mergesSorted ++ others ++ weakens

    go :: [InstanceOp] -> [InstanceOp]
    go ops = case break isBarrier ops of
        (chunk, []) -> normalizePreRaise chunk
        (chunk, barrierOp : rest) ->
            normalizePreRaise chunk ++ (barrierOp : go rest)

    dedupElims :: [InstanceOp] -> [InstanceOp]
    dedupElims = goDedup IntSet.empty
      where
        goDedup _ [] = []
        goDedup eliminated (op : rest) =
            case elimBinder op of
                Nothing -> op : goDedup eliminated rest
                Just n ->
                    let k = getNodeId n
                    in if IntSet.member k eliminated
                        then goDedup eliminated rest
                        else op : goDedup (IntSet.insert k eliminated) rest

-- | Coalesce the paper-shaped pattern “Raise(n); Merge(n, m)” into a single
-- `OpRaiseMerge(n, m)` operation.
--
-- Note: ∀-introduction (`O`) is not recorded in Ω; it is stored separately on
-- `EdgeWitness` as `ewForallIntros` and applied directly when constructing Φ(e).
coalesceRaiseMerge :: [InstanceOp] -> [InstanceOp]
coalesceRaiseMerge = go
  where
    go = \case
        (OpRaise n : rest) ->
            let isSameRaise = \case
                    OpRaise n' -> n' == n
                    _ -> False
                (_moreRaises, rest1) = span isSameRaise rest
            in case rest1 of
                (OpMerge n' m : rest2) | n' == n -> OpRaiseMerge n m : go rest2
                _ -> OpRaise n : go rest
        (op : rest) -> op : go rest
        [] -> []

initEdgeUnifyState :: [(NodeId, NodeId)] -> InteriorSet -> NodeId -> PresolutionM EdgeUnifyState
initEdgeUnifyState binderArgs interior edgeRoot = do
    roots <- forM (IntSet.toList interior) $ \i -> findRoot (NodeId i)
    let interiorRoots = IntSet.fromList (map getNodeId roots)
    bindersByRoot <- foldM
        (\m (bv, arg) -> do
            r <- findRoot arg
            let k = getNodeId r
                v = IntSet.singleton (getNodeId bv)
            pure (IntMap.insertWith IntSet.union k v m)
        )
        IntMap.empty
        binderArgs
    -- Build interior-by-root map: for each interior node, track which UF root it belongs to
    -- This allows us to record OpRaise for ALL interior nodes, not just binders
    interiorByRoot <- foldM
        (\m i -> do
            r <- findRoot (NodeId i)
            let k = getNodeId r
                v = IntSet.singleton i
            pure (IntMap.insertWith IntSet.union k v m)
        )
        IntMap.empty
        (IntSet.toList interior)
    quantLevel <- case binderArgs of
        [] -> pure Nothing
        ((bv, _) : _) -> do
            n <- getCanonicalNode bv
            case n of
                TyVar{ tnVarLevel = l } -> pure (Just l)
                _ -> pure Nothing
    nodes <- gets (cNodes . psConstraint)
    let binderMetaMap = IntMap.fromList [ (getNodeId bv, meta) | (bv, meta) <- binderArgs ]
        -- For edge-local ordering we use the expanded χe ids directly (no UF canonicalization),
        -- and restrict traversal to the interior set when possible.
        keys = Order.orderKeysFromRootWith id nodes edgeRoot (Just interior)
    pure EdgeUnifyState
        { eusInteriorRoots = interiorRoots
        , eusBindersByRoot = bindersByRoot
        , eusInteriorByRoot = interiorByRoot
        , eusQuantLevel = quantLevel
        , eusEliminatedBinders = IntSet.empty
        , eusBinderMeta = binderMetaMap
        , eusOrderKeys = keys
        , eusOps = []
        }

recordOp :: InstanceOp -> EdgeUnifyM ()
recordOp op = modify $ \st -> st { eusOps = eusOps st ++ [op] }

recordEliminate :: NodeId -> EdgeUnifyM ()
recordEliminate bv = modify $ \st -> st { eusEliminatedBinders = IntSet.insert (getNodeId bv) (eusEliminatedBinders st) }

isEliminated :: NodeId -> EdgeUnifyM Bool
isEliminated bv = gets (IntSet.member (getNodeId bv) . eusEliminatedBinders)

recordRaisesFromTrace :: IntSet.IntSet -> [NodeId] -> EdgeUnifyM ()
recordRaisesFromTrace interiorNodes trace =
    forM_ trace $ \nid ->
        when (IntSet.member (getNodeId nid) interiorNodes) $ do
            already <- isEliminated nid
            isLocked <- checkNodeLocked nid
            when (not already && not isLocked) $
                recordOp (OpRaise nid)

-- | Check if a node is under a rigid binder (locked) in the binding tree.
--
-- Paper alignment (`papers/xmlf.txt` §3.4): operations under rigidly bound nodes
-- should be absent or rejected in the normalized witness Ω.
--
-- Requirements: 5.2
checkNodeLocked :: NodeId -> EdgeUnifyM Bool
checkNodeLocked nid = do
    c <- lift $ gets psConstraint
    uf <- lift $ gets psUnionFind
    let canonical = UnionFind.frWith uf
        lookupParent :: NodeId -> EdgeUnifyM (Maybe (NodeId, BindFlag))
        lookupParent n =
            case Binding.lookupBindParentUnder canonical c n of
                Left err -> lift $ throwError (BindingTreeError err)
                Right p -> pure p

        -- Paper `locked`/“under rigid binder” check: consider only strict ancestors,
        -- so a restricted node (its own edge rigid) is not treated as locked
        -- solely because of that edge.
        goStrict :: NodeId -> EdgeUnifyM Bool
        goStrict n = do
            mbParent <- lookupParent n
            case mbParent of
                Nothing -> pure False
                Just (_, BindRigid) -> pure True
                Just (parent, BindFlex) -> goStrict parent

    mbSelf <- lookupParent nid
    case mbSelf of
        Nothing -> pure False
        Just (parent, _flag) -> goStrict parent

compareBinderIdsByPrec :: Int -> Int -> EdgeUnifyM Ordering
compareBinderIdsByPrec bid1 bid2 = do
    keys <- gets eusOrderKeys
    binderMeta <- gets eusBinderMeta
    let keyFor bid = do
            meta <- IntMap.lookup bid binderMeta
            IntMap.lookup (getNodeId meta) keys
        k1 = keyFor bid1
        k2 = keyFor bid2
    pure $ case (k1, k2) of
        (Just a, Just b) ->
            case Order.compareOrderKey a b of
                EQ -> compare bid1 bid2
                other -> other
        (Just _, Nothing) -> LT
        (Nothing, Just _) -> GT
        (Nothing, Nothing) -> compare bid1 bid2

pickRepBinderId :: IntSet.IntSet -> EdgeUnifyM Int
pickRepBinderId bs =
    case IntSet.toList bs of
        [] -> lift $ throwError (InternalError "pickRepBinderId: empty binder set")
        (x:xs) -> foldM pick x xs
  where
    pick best cand = do
        ord <- compareBinderIdsByPrec cand best
        pure $ case ord of
            LT -> cand
            _ -> best

recordMergesIntoRep :: IntSet.IntSet -> EdgeUnifyM ()
recordMergesIntoRep bs
    | IntSet.size bs <= 1 = pure ()
    | otherwise = do
        eliminated <- gets eusEliminatedBinders
        let live = IntSet.filter (\bid -> not (IntSet.member bid eliminated)) bs
        repId <- pickRepBinderId (if IntSet.null live then bs else live)
        let rep = NodeId repId
            others = filter (/= repId) (IntSet.toList bs)
        othersSorted <- sortByM (\a b -> compareBinderIdsByPrec b a) others
        forM_ othersSorted $ \bid -> do
            let b = NodeId bid
            already <- isEliminated b
            when (not already) $ do
                recordOp (OpMerge b rep)
                recordEliminate b
  where
    sortByM :: (a -> a -> EdgeUnifyM Ordering) -> [a] -> EdgeUnifyM [a]
    sortByM _ [] = pure []
    sortByM cmp xs = do
        let insertOne x [] = pure [x]
            insertOne x (y:ys) = do
                o <- cmp x y
                if o == LT then pure (x:y:ys) else (y:) <$> insertOne x ys
        foldM (\acc x -> insertOne x acc) [] xs

unifyAcyclicEdge :: NodeId -> NodeId -> EdgeUnifyM ()
unifyAcyclicEdge n1 n2 = do
    root1 <- lift $ findRoot n1
    root2 <- lift $ findRoot n2
    when (root1 /= root2) $ do
        st0 <- get
        let r1 = getNodeId root1
            r2 = getNodeId root2
            inInt1 = IntSet.member r1 (eusInteriorRoots st0)
            inInt2 = IntSet.member r2 (eusInteriorRoots st0)
            bs1 = IntMap.findWithDefault IntSet.empty r1 (eusBindersByRoot st0)
            bs2 = IntMap.findWithDefault IntSet.empty r2 (eusBindersByRoot st0)
            bs = IntSet.union bs1 bs2

        -- Paper alignment (xmlf.txt §3.4 / Fig. 10): `Raise(n)` is a real
        -- transformation of χe’s binding edges. We implement that effect via
        -- binding-edge harmonization and record the corresponding `OpRaise`
        -- steps in Ω.
        trace <- lift $ unifyAcyclicRootsWithRaiseTrace root1 root2

        let int1 = IntMap.findWithDefault IntSet.empty r1 (eusInteriorByRoot st0)
            int2 = IntMap.findWithDefault IntSet.empty r2 (eusInteriorByRoot st0)
            intAll = IntSet.union int1 int2

        -- Paper alignment (xmlf.txt §3.4): record Raise(n) for exactly the node(s)
        -- that were raised by binding-edge harmonization, restricted to I(r) and
        -- eliding operations under rigid binders.
        recordRaisesFromTrace intAll trace

        -- Update which UF roots correspond to classes containing interior nodes.
        modify $ \st ->
            let roots' =
                    if inInt1 || inInt2
                        then IntSet.insert r2 (IntSet.delete r1 (eusInteriorRoots st))
                        else eusInteriorRoots st
                binders' =
                    let m0 = eusBindersByRoot st
                        m1 = if IntSet.null bs then IntMap.delete r1 m0 else IntMap.insert r2 bs (IntMap.delete r1 m0)
                    in m1
                -- Also update interior-by-root map
                interior' =
                    let m0 = eusInteriorByRoot st
                        m1 = if IntSet.null intAll then IntMap.delete r1 m0 else IntMap.insert r2 intAll (IntMap.delete r1 m0)
                    in m1
            in st { eusInteriorRoots = roots', eusBindersByRoot = binders', eusInteriorByRoot = interior' }

        -- Record merges among binders that became aliased (inside the interior).
        recordMergesIntoRep bs

        -- Record RaiseMerge when a binder-class merges with an exterior TyVar-class.
        when (IntSet.size bs >= 1) $ do
            repId <- pickRepBinderId bs
            let rep = NodeId repId
            case (IntSet.null bs1, IntSet.null bs2) of
                (False, True) | inInt1 && not inInt2 -> do
                    node2 <- lift $ getCanonicalNode root2
                    case node2 of
                        TyVar{} -> do
                            should <- shouldRecordRaiseMerge rep root2
                            already <- isEliminated rep
                            when (should && not already) $ do
                                -- Paper-shaped RaiseMerge is a sequence (Raise(n))^k; Merge(n, m).
                                -- We record it in that explicit form and let `normalizeInstanceOps`
                                -- coalesce it back to `OpRaiseMerge`.
                                recordOp (OpRaise rep)
                                recordOp (OpMerge rep root2)
                                recordEliminate rep
                        _ -> pure ()
                (True, False) | inInt2 && not inInt1 -> do
                    node1 <- lift $ getCanonicalNode root1
                    case node1 of
                        TyVar{} -> do
                            should <- shouldRecordRaiseMerge rep root1
                            already <- isEliminated rep
                            when (should && not already) $ do
                                recordOp (OpRaise rep)
                                recordOp (OpMerge rep root1)
                                recordEliminate rep
                        _ -> pure ()
                _ -> pure ()

shouldRecordRaiseMerge :: NodeId -> NodeId -> EdgeUnifyM Bool
shouldRecordRaiseMerge binder ext = do
    st <- get
    case eusQuantLevel st of
        Nothing -> pure False
        Just qLvl -> do
            extNode <- lift $ getNode ext
            case extNode of
                TyVar{ tnVarLevel = extLvl } -> do
                    above <- lift $ isProperAncestorLevel extLvl qLvl
                    if not above
                        then pure False
                        else do
                            mbBound <- lift $ lookupVarBound binder
                            case mbBound of
                                Nothing ->
                                    -- Unbounded binder: RaiseMerge is not needed; InstApp is expressible.
                                    pure False
                                Just bnd -> do
                                    bndRoot <- lift $ findRoot bnd
                                    extRoot <- lift $ findRoot ext
                                    pure (bndRoot /= extRoot)
                _ -> pure False

lookupVarBound :: NodeId -> PresolutionM (Maybe NodeId)
lookupVarBound bv = do
    root <- findRoot bv
    c <- gets psConstraint
    pure (VarStore.lookupVarBound c root)

isProperAncestorLevel :: GNodeId -> GNodeId -> PresolutionM Bool
isProperAncestorLevel anc lvl =
    if anc == lvl
        then pure False
        else isAncestorLevel anc lvl

isAncestorLevel :: GNodeId -> GNodeId -> PresolutionM Bool
isAncestorLevel anc lvl
    | anc == lvl = pure True
    | otherwise = do
        gnodes <- gets (cGNodes . psConstraint)
        -- If there is no binding tree, we cannot classify ancestors.
        if IntMap.null gnodes
            then pure False
            else case IntMap.lookup (getGNodeId lvl) gnodes of
                Nothing -> pure False
                Just g -> case gParent g of
                    Nothing -> pure False
                    Just p -> isAncestorLevel anc p

unifyStructureEdge :: NodeId -> NodeId -> EdgeUnifyM ()
unifyStructureEdge n1 n2 = do
    root1 <- lift $ findRoot n1
    root2 <- lift $ findRoot n2
    if root1 == root2 then pure ()
    else do
        node1 <- lift $ getCanonicalNode n1
        node2 <- lift $ getCanonicalNode n2

        unifyAcyclicEdge n1 n2

        case (node1, node2) of
            (TyArrow { tnDom = d1, tnCod = c1 }, TyArrow { tnDom = d2, tnCod = c2 }) -> do
                unifyStructureEdge d1 d2
                unifyStructureEdge c1 c2
            (TyForall { tnBody = b1 }, TyForall { tnBody = b2 }) ->
                unifyStructureEdge b1 b2
            _ -> pure ()

-- | Extract binder→argument pairing information from an expansion recipe.
--
-- For now, this is only defined for instantiation steps at a `TyExp` root.
binderArgsFromExpansion :: TyNode -> Expansion -> PresolutionM [(NodeId, NodeId)]
binderArgsFromExpansion leftRaw expn = do
    let go = \case
            ExpIdentity -> pure []
            ExpForall _ -> pure []
            ExpCompose es -> concat <$> mapM go (NE.toList es)
            ExpInstantiate args ->
                case leftRaw of
                    TyExp{ tnBody = b } -> do
                        binders <- firstNonVacuousBinders b
                        if length binders /= length args
                            then throwError (ArityMismatch "binderArgsFromExpansion/ExpInstantiate" (length binders) (length args))
                            else pure (zip binders args)
                    _ -> pure []

        firstNonVacuousBinders :: NodeId -> PresolutionM [NodeId]
        firstNonVacuousBinders nid = do
            n <- getCanonicalNode nid
            case n of
                TyForall{ tnBody = inner, tnQuantLevel = q } -> do
                    bvs <- collectBoundVars inner q
                    if null bvs then firstNonVacuousBinders inner else pure bvs
                _ -> throwError (InstantiateOnNonForall (tnId n))

    go expn

-- | Convert our presolution expansion recipe into a (coarse) instance-operation witness.
witnessFromExpansion :: NodeId -> TyNode -> Expansion -> PresolutionM InstanceWitness
witnessFromExpansion _root leftRaw expn = do
    ops <- go expn
    pure (InstanceWitness ops)
  where
    go :: Expansion -> PresolutionM [InstanceOp]
    go ExpIdentity = pure []
    go (ExpCompose es) = concat <$> mapM go (NE.toList es)
    go (ExpForall _ls) = do
        -- `papers/xmlf.txt` does not include quantifier-introduction (`O`) in the
        -- witness language Ω (Figure 10). We record these separately on the
        -- `EdgeWitness` and apply them directly when constructing Φ(e).
        pure []
    go (ExpInstantiate args) = do
        -- If the TyExp body is a forall, instantiate its binders in the same order
        -- that `applyExpansion` uses (collectBoundVars + zip).
        case leftRaw of
            TyExp{ tnBody = b } -> do
                boundVars <- firstNonVacuousBinders b
                if length boundVars /= length args
                    then throwError (ArityMismatch "witnessFromExpansion/ExpInstantiate" (length boundVars) (length args))
                    else do
                        let pairs = zip args boundVars
                        (grafts, merges, weakens) <- foldM (classify boundVars) ([], [], []) pairs
                        -- Order:
                        --   • grafts first (update ⊥ bounds)
                        --   • then merges (alias + eliminate)
                        --   • then remaining weakens (eliminate using existing bounds)
                        --
                        -- This is closer to `papers/xmlf.txt` Fig. 10, and avoids emitting
                        -- invalid grafts under non-⊥ bounds (e.g. bounded variables).
                        pure (grafts ++ merges ++ weakens)
            _ -> do
                -- Instantiating a non-TyExp is unexpected in current pipeline; treat as empty.
                pure []
      where
        classify
            :: [NodeId] -- binders at this instantiation site
            -> ([InstanceOp], [InstanceOp], [InstanceOp])
            -> (NodeId, NodeId) -- (arg, binder)
            -> PresolutionM ([InstanceOp], [InstanceOp], [InstanceOp])
        classify binders (gAcc, mAcc, wAcc) (arg, bv) = do
            mbBound <- binderBound bv
            case mbBound of
                Nothing ->
                    -- Unbounded binder: graft then eliminate later via weaken.
                    pure (gAcc ++ [OpGraft arg bv], mAcc, wAcc ++ [OpWeaken bv])
                Just bnd -> do
                    isVarBound <- isTyVar bnd
                    if isVarBound && bnd `elem` binders
                        -- Bounded by an in-scope variable: alias + eliminate via Merge (Fig. 10).
                        then pure (gAcc, mAcc ++ [OpMerge bv bnd], wAcc)
                        -- Bounded by structure: eliminate via Weaken (substitute bound).
                        else pure (gAcc, mAcc, wAcc ++ [OpWeaken bv])

        firstNonVacuousBinders :: NodeId -> PresolutionM [NodeId]
        firstNonVacuousBinders nid = do
            n <- getCanonicalNode nid
            case n of
                TyForall{ tnBody = inner, tnQuantLevel = q } -> do
                    bvs <- collectBoundVars inner q
                    if null bvs then firstNonVacuousBinders inner else pure bvs
                _ -> throwError (InstantiateOnNonForall (tnId n))

        binderBound :: NodeId -> PresolutionM (Maybe NodeId)
        binderBound bv = do
            n <- getCanonicalNode bv
            case n of
                TyVar{} ->
                    lookupVarBound bv
                _ -> pure Nothing

        isTyVar :: NodeId -> PresolutionM Bool
        isTyVar nid = do
            n <- getCanonicalNode nid
            pure $ case n of
                TyVar{} -> True
                _ -> False

forallIntroSuffixCount :: Expansion -> PresolutionM Int
forallIntroSuffixCount expn =
    let parts = flatten expn
        (prefix, suffix) = splitTrailing isForall parts
    in if any isForall prefix
        then throwError (InternalError "forallIntroSuffixCount: ExpForall appears in non-suffix position")
        else pure (sum (map forallCount suffix))
  where
    isForall = \case
        ExpForall{} -> True
        _ -> False

    forallCount = \case
        ExpForall ls -> length (NE.toList ls)
        _ -> 0

    flatten = \case
        ExpCompose es -> concatMap flatten (NE.toList es)
        other -> [other]

    splitTrailing p xs =
        let (sufRev, preRev) = span p (reverse xs)
        in (reverse preRev, reverse sufRev)

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

getNode :: NodeId -> PresolutionM TyNode
getNode nid = do
    nodes <- gets (cNodes . psConstraint)
    case IntMap.lookup (getNodeId nid) nodes of
        Just n -> return n
        Nothing -> throwError $ NodeLookupFailed nid

-- | Get the current expansion for an expansion variable.
getExpansion :: ExpVarId -> PresolutionM Expansion
getExpansion s = do
    Presolution m <- gets psPresolution
    return $ fromMaybe ExpIdentity (IntMap.lookup (getExpVarId s) m)

-- | Set the expansion for an expansion variable.
setExpansion :: ExpVarId -> Expansion -> PresolutionM ()
setExpansion s expansion = do
    modify $ \st -> st { psPresolution = Presolution $ IntMap.insert (getExpVarId s) expansion (getAssignments (psPresolution st)) }

recordEdgeExpansion :: EdgeId -> Expansion -> PresolutionM ()
recordEdgeExpansion (EdgeId eid) expn =
    modify $ \st -> st { psEdgeExpansions = IntMap.insert eid expn (psEdgeExpansions st) }

-- | Merge two expansions for the same variable.
-- This may trigger unifications if we merge two Instantiates.
mergeExpansions :: ExpVarId -> Expansion -> Expansion -> PresolutionM Expansion
mergeExpansions v e1 e2 = case (e1, e2) of
    (ExpIdentity, _) -> pure e2
    (_, ExpIdentity) -> pure e1
    (ExpInstantiate args1, ExpInstantiate args2) -> do
        if length args1 /= length args2
            then throwError (ArityMismatch "ExpInstantiate merge" (length args1) (length args2))
            else do
                zipWithM_ unifyAcyclic args1 args2
                pure e1
    (ExpForall l1, ExpForall l2) -> do
        if l1 == l2 then pure e1
        else throwError (InternalError "Merging distinct Forall expansions not supported")
    (ExpCompose exps1, ExpCompose exps2) -> do
        if length exps1 /= length exps2
            then throwError (ArityMismatch "ExpCompose merge" (length exps1) (length exps2))
            else do
                merged <- zipWithM (mergeExpansions v) (NE.toList exps1) (NE.toList exps2)
                pure (ExpCompose (NE.fromList merged))
    _ -> throwError (InternalError ("Incompatible expansions: " ++ show e1 ++ " vs " ++ show e2))

{- Note [Minimal Expansion Decision]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Forall Level Mismatch → Compose].

Presolution chooses the least-committing expansion for an edge s · τ ≤ τ′ so
that E(τ) matches the shape of τ′ while keeping s as general as possible
(Rémy & Yakobowski, ICFP 2008, §5.2). We now exercise the full paper lattice:
identity, instantiation, ∀-introduction, and explicit composition.

Decision cases (as implemented):

1. ∀ ≤ ∀: if quantifier levels coincide, keep identity and unify bodies; if they
    differ, instantiate the source binders (fresh vars) then re-generalize to
    the target level via ExpCompose (ExpInstantiate · ExpForall).

2. ∀ ≤ structure: instantiate to expose the body. If there are no bound vars
    (degenerate ∀), reuse the body and just unify; otherwise allocate fresh
    nodes for each bound var and return ExpInstantiate. Unifications connect
    the exposed body to τ′ when appropriate.

3. Structure ≤ ∀: generalize to meet the target by wrapping the source body in
    ExpForall at the target level, while unifying the underlying body with the
    target’s body.

4. Structure ≤ structure: keep identity and emit component unifications
    (arrow dom/cod, base equality, etc.).

5. Var ≤ Var: same as structure—identity plus a unification of the two vars.

Level/scope notes: instantiation only introduces fresh nodes for the bound
variables of the source ∀; shared nodes beyond that scope stay shared. The
result is an Expansion (possibly composed) plus the deferred unifications
required for compatibility.
-}
{- Note [Forall Level Mismatch → Compose]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Paper §5 and Definition 5 ("expansion of g at g′") in
`papers/recasting-mlf-RR.txt` and the matching discussion in
`papers/Remy-Yakobowski@icfp08_mlf-type-inference.txt` describe how a forall
body is copied and re-bound at a destination generalization node. For an edge
s · (forall@ℓ α.σ) ≤ forall@ℓ′ β.τ with ℓ ≠ ℓ′ the minimal recipe is:

    1. instantiate the source binders at level ℓ (fresh metas, drop the old ∀)
    2. re-introduce ∀ at the demanded level ℓ′

The lattice offers only “instantiate” and “add ∀”, so changing the quantifier
level is spelled as ExpCompose (ExpInstantiate · ExpForall). Instantiation
alone would lose required polymorphism; ∀ alone would quantify at the wrong
level. The sequence preserves sharing outside the binder and remains the least
expansion that satisfies the edge (principality argument in §5).
-}
decideMinimalExpansion :: TyNode -> TyNode -> PresolutionM (Expansion, [(NodeId, NodeId)])
decideMinimalExpansion expNode@(TyExp { tnBody = bodyId }) targetNode = do
    body <- getCanonicalNode bodyId
    case body of
        TyForall { tnBody = forallBody, tnQuantLevel = lvl } -> do
            boundVars <- collectBoundVars forallBody lvl
            -- A TyForall node can be "vacuous" in our constraint graph: it marks a
            -- generalization boundary even when there are no binders at that level.
            -- In that case it should behave transparently for expansion decisions
            -- (otherwise we may unify a non-vacuous ∀ against structure and lose
            -- instantiation opportunities; see xmlf.txt §3.1 desugaring cases).
            if null boundVars then
                decideMinimalExpansion expNode { tnBody = forallBody } targetNode
            else case targetNode of
                TyForall { tnQuantLevel = lvl', tnBody = targetBody }
                    | lvl == lvl' ->
                        -- Note [Minimal Expansion Decision] case 1 (∀≤∀ same level)
                        return (ExpIdentity, [(forallBody, targetBody)])
                    | otherwise -> do
                        -- Note [Minimal Expansion Decision] case 1 (∀≤∀ level mismatch)
                        freshNodes <- mapM (const (createFreshVarAt lvl')) boundVars
                        -- instantiate then re-generalize to target level
                        let expn = ExpCompose (ExpInstantiate freshNodes NE.<| (ExpForall (lvl' NE.:| []) NE.:| []))
                        return (expn, [])
                _ -> do
                    -- target is not a forall → instantiate to expose structure
                    -- Note [Minimal Expansion Decision] case 2 (∀≤structure, with binders)
                    freshNodes <- mapM (const (createFreshVarAt lvl)) boundVars
                    return (ExpInstantiate freshNodes, [])

        TyArrow { tnDom = bDom, tnCod = bCod } -> do
            case targetNode of
                TyArrow { tnDom = tDom, tnCod = tCod } ->
                    -- Note [Minimal Expansion Decision] case 4 (structure≤structure, arrow)
                    return (ExpIdentity, [(bDom, tDom), (bCod, tCod)])
                TyForall { tnQuantLevel = lvl', tnBody = targetBody } -> do
                    -- need to generalize to meet target forall
                    -- Note [Minimal Expansion Decision] case 3 (structure≤∀)
                    let expn = ExpForall (lvl' NE.:| [])
                    return (expn, [(bodyId, targetBody)])
                _ -> return (ExpIdentity, [(bodyId, tnId targetNode)])

        _ -> case targetNode of
            TyForall { tnQuantLevel = lvl', tnBody = targetBody } -> do
                -- Note [Minimal Expansion Decision] case 3 (structure≤∀)
                let expn = ExpForall (lvl' NE.:| [])
                return (expn, [(bodyId, targetBody)])
            _ -> return (ExpIdentity, [(bodyId, tnId targetNode)])

decideMinimalExpansion _ _ = return (ExpIdentity, [])

-- | Get the level from a node if it has one.
-- Only TyVar and TyForall have level fields; others return root level (GNodeId 0).
getNodeLevel :: TyNode -> GNodeId
getNodeLevel node = case node of
    TyVar { tnVarLevel = l } -> l
    TyForall { tnOwnerLevel = l } -> l
    _ -> GNodeId 0  -- Default to root level for structure nodes

-- | Get the level from a TyExp's body by looking up the body node.
getLevelFromBody :: NodeId -> PresolutionM GNodeId
getLevelFromBody bodyId = do
    bodyNode <- getCanonicalNode bodyId
    return $ getNodeLevel bodyNode

-- | Apply an expansion to a TyExp node.
-- Note: this helper is used twice for distinct purposes.
--   • processInstEdge: enforce a single instantiation edge now (expansion choice
--     plus the unifications it triggers) so later edges see the refined graph.
--   • materializeExpansions: after all edges are processed, rewrite the graph to
--     erase TyExp nodes and clear inst edges before Solve.
applyExpansion :: Expansion -> TyNode -> PresolutionM NodeId
applyExpansion expansion expNode = case (expansion, expNode) of
    (ExpIdentity, TyExp { tnBody = b }) -> return b
    (ExpInstantiate args, expNode'@TyExp { tnBody = b }) -> do
        body <- getCanonicalNode b
        case body of
            TyForall { tnBody = innerBody, tnQuantLevel = q } -> do
                boundVars <- collectBoundVars innerBody q
                if null boundVars then
                    -- Vacuous ∀: skip it and instantiate the first non-vacuous one underneath.
                    if null args
                        then pure innerBody
                        else applyExpansion (ExpInstantiate args) expNode' { tnBody = innerBody }
                else if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansion" (length boundVars) (length args)
                    else instantiateScheme innerBody q (zip boundVars args)
            _ -> throwError $ InstantiateOnNonForall (tnId body)
    (ExpForall levels, TyExp { tnBody = b }) -> do
        -- Get level from the body (which should have a TyVar or TyForall)
        ownerLvl <- getLevelFromBody b
        wrapForall ownerLvl (NE.toList levels) b
    (ExpCompose exps, TyExp { tnBody = b }) -> do
        ownerLvl <- getLevelFromBody b
        foldM (\nid e -> do
                node <- getCanonicalNode nid
                applyExpansionOverNode ownerLvl e node nid)
              b
              (NE.toList exps)
    -- If expansion is applied to a non-expansion node (after composition), use the helper
    (expn, otherNode) -> do
        -- Get level from the node if it has one, or use root level as fallback
        let ownerLvl = getNodeLevel otherNode
        applyExpansionOverNode ownerLvl expn otherNode (tnId otherNode)
  where
    wrapForall :: GNodeId -> [GNodeId] -> NodeId -> PresolutionM NodeId
    wrapForall _ [] nid = return nid
    wrapForall ownerLvl (quantLvl:ls) nid = do
        newId <- createFreshNodeId
        let node = TyForall newId quantLvl ownerLvl nid
        registerNode newId node
        wrapForall ownerLvl ls newId

    -- Allow composing over an already-expanded node (not necessarily TyExp)
    applyExpansionOverNode :: GNodeId -> Expansion -> TyNode -> NodeId -> PresolutionM NodeId
    applyExpansionOverNode _ ExpIdentity _ nid = return nid
    applyExpansionOverNode ownerLvl (ExpForall ls) _ nid = wrapForall ownerLvl (NE.toList ls) nid
    applyExpansionOverNode ownerLvl (ExpCompose es) _ nid = foldM (\n e -> do
                                                            nNode <- getCanonicalNode n
                                                            applyExpansionOverNode ownerLvl e nNode n) nid (NE.toList es)
    applyExpansionOverNode ownerLvl (ExpInstantiate args) node _ =
        case node of
            TyExp{} -> throwError $ InstantiateOnNonForall (tnId node)
            TyForall { tnBody = innerBody, tnQuantLevel = q } -> do
                boundVars <- collectBoundVars innerBody q
                if null boundVars then
                    if null args
                        then pure innerBody
                        else do
                            innerNode <- getCanonicalNode innerBody
                            applyExpansionOverNode ownerLvl (ExpInstantiate args) innerNode innerBody
                else if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansionOverNode" (length boundVars) (length args)
                    else instantiateScheme innerBody q (zip boundVars args)
            _ -> throwError $ InstantiateOnNonForall (tnId node)

-- | Apply an expansion like 'applyExpansion', but also return a (coarse) trace
-- of the expansion interior I(r): instantiation args, copied nodes, and any
-- freshly introduced ∀ wrappers.
applyExpansionTraced :: Expansion -> TyNode -> PresolutionM (NodeId, (CopyMap, InteriorSet))
applyExpansionTraced expansion expNode = case (expansion, expNode) of
    (ExpIdentity, TyExp { tnBody = b }) -> pure (b, emptyTrace)
    (ExpInstantiate args, expNode'@TyExp { tnBody = b }) -> do
        body <- getCanonicalNode b
        case body of
            TyForall { tnBody = innerBody, tnQuantLevel = q } -> do
                boundVars <- collectBoundVars innerBody q
                if null boundVars then
                    -- Vacuous ∀: skip it and instantiate the first non-vacuous one underneath.
                    if null args
                        then pure (innerBody, emptyTrace)
                        else applyExpansionTraced (ExpInstantiate args) expNode' { tnBody = innerBody }
                else if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansionTraced" (length boundVars) (length args)
                    else do
                        (root, cmap, interior) <- instantiateSchemeWithTrace innerBody q (zip boundVars args)
                        pure (root, (cmap, interior))
            _ -> throwError $ InstantiateOnNonForall (tnId body)
    (ExpForall levels, TyExp { tnBody = b }) -> do
        ownerLvl <- getLevelFromBody b
        wrapForallTraced ownerLvl (NE.toList levels) b
    (ExpCompose exps, TyExp { tnBody = b }) -> do
        ownerLvl <- getLevelFromBody b
        foldM
            (\(nid, trAcc) e -> do
                node <- getCanonicalNode nid
                (nid', tr') <- applyExpansionOverNodeTraced ownerLvl e node nid
                pure (nid', unionTrace trAcc tr'))
            (b, emptyTrace)
            (NE.toList exps)
    (expn, otherNode) -> do
        let ownerLvl = getNodeLevel otherNode
        applyExpansionOverNodeTraced ownerLvl expn otherNode (tnId otherNode)
  where
    wrapForallTraced :: GNodeId -> [GNodeId] -> NodeId -> PresolutionM (NodeId, (CopyMap, InteriorSet))
    wrapForallTraced _ [] nid = pure (nid, emptyTrace)
    wrapForallTraced ownerLvl (quantLvl:ls) nid = do
        newId <- createFreshNodeId
        let node = TyForall newId quantLvl ownerLvl nid
        registerNode newId node
        (outer, (cmap, interior)) <- wrapForallTraced ownerLvl ls newId
        pure (outer, (cmap, IntSet.insert (getNodeId newId) interior))

    applyExpansionOverNodeTraced :: GNodeId -> Expansion -> TyNode -> NodeId -> PresolutionM (NodeId, (CopyMap, InteriorSet))
    applyExpansionOverNodeTraced _ ExpIdentity _ nid = pure (nid, emptyTrace)
    applyExpansionOverNodeTraced ownerLvl (ExpForall ls) _ nid =
        wrapForallTraced ownerLvl (NE.toList ls) nid
    applyExpansionOverNodeTraced ownerLvl (ExpCompose es) _ nid =
        foldM
            (\(n, trAcc) e -> do
                nNode <- getCanonicalNode n
                (n', tr') <- applyExpansionOverNodeTraced ownerLvl e nNode n
                pure (n', unionTrace trAcc tr'))
            (nid, emptyTrace)
            (NE.toList es)
    applyExpansionOverNodeTraced ownerLvl (ExpInstantiate args) node _ =
        case node of
            TyExp{} -> throwError $ InstantiateOnNonForall (tnId node)
            TyForall { tnBody = innerBody, tnQuantLevel = q } -> do
                boundVars <- collectBoundVars innerBody q
                if null boundVars then
                    if null args
                        then pure (innerBody, emptyTrace)
                        else do
                            innerNode <- getCanonicalNode innerBody
                            applyExpansionOverNodeTraced ownerLvl (ExpInstantiate args) innerNode innerBody
                else if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansionOverNodeTraced" (length boundVars) (length args)
                    else do
                        (root, cmap, interior) <- instantiateSchemeWithTrace innerBody q (zip boundVars args)
                        pure (root, (cmap, interior))
            _ -> throwError $ InstantiateOnNonForall (tnId node)

-- | Like 'applyExpansionTraced', but for edge processing: construct χe-style copies
-- so that Ω operations (Graft/Weaken/Merge) can be executed as graph transformations.
--
-- In particular, `ExpInstantiate` copies the body by substituting binders with
-- fresh binder-meta variables, and copies their instance bounds into `gBinds`.
applyExpansionEdgeTraced :: Expansion -> TyNode -> PresolutionM (NodeId, (CopyMap, InteriorSet))
applyExpansionEdgeTraced expansion expNode = case (expansion, expNode) of
    (ExpIdentity, TyExp { tnBody = b }) -> pure (b, emptyTrace)
    (ExpInstantiate args, expNode'@TyExp { tnBody = b }) -> do
        body <- getCanonicalNode b
        case body of
            TyForall { tnBody = innerBody, tnQuantLevel = q } -> do
                boundVars <- collectBoundVars innerBody q
                if null boundVars then
                    if null args
                        then pure (innerBody, emptyTrace)
                        else applyExpansionEdgeTraced (ExpInstantiate args) expNode' { tnBody = innerBody }
                else if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansionEdgeTraced" (length boundVars) (length args)
                    else do
                        metas <- zipWithM binderMetaAt args boundVars
                        (root, cmap0, interior0) <- instantiateSchemeWithTrace innerBody q (zip boundVars metas)
                        (cmapB, interiorB) <- copyBinderBounds q (zip boundVars metas)
                        pure (root, (IntMap.union cmap0 cmapB, IntSet.union interior0 interiorB))
            _ -> throwError $ InstantiateOnNonForall (tnId body)
    (ExpForall levels, TyExp { tnBody = b }) -> do
        ownerLvl <- getLevelFromBody b
        wrapForallTraced ownerLvl (NE.toList levels) b
    (ExpCompose exps, TyExp { tnBody = b }) -> do
        ownerLvl <- getLevelFromBody b
        foldM
            (\(nid, trAcc) e -> do
                node <- getCanonicalNode nid
                (nid', tr') <- applyExpansionOverNodeEdgeTraced ownerLvl e node nid
                pure (nid', unionTrace trAcc tr'))
            (b, emptyTrace)
            (NE.toList exps)
    (expn, otherNode) -> do
        let ownerLvl = getNodeLevel otherNode
        applyExpansionOverNodeEdgeTraced ownerLvl expn otherNode (tnId otherNode)
  where
    binderMetaAt :: NodeId -> NodeId -> PresolutionM NodeId
    binderMetaAt arg _bv = do
        argNode <- getCanonicalNode arg
        let lvl = case argNode of
                TyVar { tnVarLevel = l } -> l
                TyForall { tnOwnerLevel = l } -> l
                _ -> getNodeLevel argNode
        createFreshVarAt lvl

    wrapForallTraced :: GNodeId -> [GNodeId] -> NodeId -> PresolutionM (NodeId, (CopyMap, InteriorSet))
    wrapForallTraced _ [] nid = pure (nid, emptyTrace)
    wrapForallTraced ownerLvl (quantLvl:ls) nid = do
        newId <- createFreshNodeId
        let node = TyForall newId quantLvl ownerLvl nid
        registerNode newId node
        (outer, (cmap, interior)) <- wrapForallTraced ownerLvl ls newId
        pure (outer, (cmap, IntSet.insert (getNodeId newId) interior))

    applyExpansionOverNodeEdgeTraced :: GNodeId -> Expansion -> TyNode -> NodeId -> PresolutionM (NodeId, (CopyMap, InteriorSet))
    applyExpansionOverNodeEdgeTraced _ ExpIdentity _ nid = pure (nid, emptyTrace)
    applyExpansionOverNodeEdgeTraced ownerLvl (ExpForall ls) _ nid =
        wrapForallTraced ownerLvl (NE.toList ls) nid
    applyExpansionOverNodeEdgeTraced ownerLvl (ExpCompose es) _ nid =
        foldM
            (\(n, trAcc) e -> do
                nNode <- getCanonicalNode n
                (n', tr') <- applyExpansionOverNodeEdgeTraced ownerLvl e nNode n
                pure (n', unionTrace trAcc tr'))
            (nid, emptyTrace)
            (NE.toList es)
    applyExpansionOverNodeEdgeTraced ownerLvl (ExpInstantiate args) node _ =
        case node of
            TyExp{} -> throwError $ InstantiateOnNonForall (tnId node)
            TyForall { tnBody = innerBody, tnQuantLevel = q } -> do
                boundVars <- collectBoundVars innerBody q
                if null boundVars then
                    if null args
                        then pure (innerBody, emptyTrace)
                        else do
                            innerNode <- getCanonicalNode innerBody
                            applyExpansionOverNodeEdgeTraced ownerLvl (ExpInstantiate args) innerNode innerBody
                else if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansionOverNodeEdgeTraced" (length boundVars) (length args)
                    else do
                        metas <- zipWithM binderMetaAt args boundVars
                        (root, cmap0, interior0) <- instantiateSchemeWithTrace innerBody q (zip boundVars metas)
                        (cmapB, interiorB) <- copyBinderBounds q (zip boundVars metas)
                        pure (root, (IntMap.union cmap0 cmapB, IntSet.union interior0 interiorB))
            _ -> throwError $ InstantiateOnNonForall (tnId node)

-- | Copy (and re-bind) instance bounds from original binders onto fresh binder-metas.
copyBinderBounds :: GNodeId -> [(NodeId, NodeId)] -> PresolutionM (CopyMap, InteriorSet)
copyBinderBounds quantLevel substList = do
    let binderMap = IntMap.fromList [(getNodeId bv, meta) | (bv, meta) <- substList]
    foldM
        (\(cmapAcc, intAcc) (bv, meta) -> do
            mbBound <- lookupVarBound bv
            case mbBound of
                Nothing -> pure (cmapAcc, intAcc)
                Just bnd ->
                    case IntMap.lookup (getNodeId bnd) binderMap of
                        Just bndMeta -> do
                            setVarBound meta (Just bndMeta)
                            pure (cmapAcc, intAcc)
                        Nothing -> do
                            (bndCopy, cmapB, intB) <- instantiateSchemeWithTrace bnd quantLevel substList
                            setVarBound meta (Just bndCopy)
                            pure (IntMap.union cmapAcc cmapB, IntSet.union intAcc intB)
        )
        (IntMap.empty, IntSet.empty)
        substList

{- Note [instantiateScheme]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Goal
    Copy a ∀-body graph while substituting its bound vars with fresh nodes at the
    target level (per `papers/recasting-mlf-RR.txt` §5, Def. 5 and
    `papers/Remy-Yakobowski@icfp08_mlf-type-inference.txt` §4).

Guarantees
    • Bound vars substitute: `substList` replaces exactly the binders being
        instantiated.
    • Share outer scope: nodes with level < quantLevel are reused, not copied,
        preserving context and avoiding spurious polymorphism.
    • Preserve structure: arrows / foralls / expansions are recursively copied;
        bases may be shared as an optimization.
    • Preserve sharing: a StateT cache copies each source node at most once,
        keeping internal sharing and breaking cycles.
    • One pass, registered: `copyNode` both allocates fresh NodeIds and registers
        them into `cNodes`, so everything it creates is live in the constraint.
    • Necessity: plain ID substitution cannot simultaneously freshen binders,
        share outer nodes, and preserve internal sharing; `copyNode` implements the
        paper’s copy-with-subst traversal to do all three at once.

Failure mode
    • Missing node lookups raise `NodeLookupFailed` (tests cover this), keeping
        instantiation total on well-formed graphs.
-}
-- | Instantiate a scheme by copying the graph and replacing bound variables.
instantiateScheme :: NodeId -> GNodeId -> [(NodeId, NodeId)] -> PresolutionM NodeId
instantiateScheme bodyId quantLevel substList = do
    (root, _copyMap, _interior) <- instantiateSchemeWithTrace bodyId quantLevel substList
    pure root

-- | Like 'instantiateScheme', but also return:
--   • a copy provenance map (original node → copied/replaced node), and
--   • the expansion interior I(r) as an IntSet (computed from binding edges).
--
-- Paper alignment (`papers/xmlf.txt` §3.2): when expanding an instantiation edge,
-- we copy exactly the nodes "structurally strictly under g and in I(g)" and
-- preserve binding edges/flags for copied nodes. The expansion root is bound
-- at the same binder as the target node.
instantiateSchemeWithTrace :: NodeId -> GNodeId -> [(NodeId, NodeId)] -> PresolutionM (NodeId, IntMap NodeId, IntSet.IntSet)
instantiateSchemeWithTrace bodyId _quantLevel substList = do
    requireValidBindingTree
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0

    let bodyC = canonical bodyId
    when (IntMap.notMember (getNodeId bodyC) (cNodes c0)) $
        throwError (NodeLookupFailed bodyC)

    -- Paper (`xmlf.txt` §3.2): expansion copies nodes structurally under g that
    -- are in I(g). In our representation, we take g to be the (quotient) binding
    -- parent of the root we are copying, and compute I(g) on the quotient
    -- binding graph.
    --
    -- If the root is itself a binding root in the quotient relation (e.g. when
    -- copying a disconnected instance bound), we use it as g.
    copyInterior <- do
        mbParent <- case Binding.lookupBindParentUnder canonical c0 bodyC of
            Left err -> throwError (BindingTreeError err)
            Right p -> pure p
        let g = case mbParent of
                Nothing -> bodyC
                Just (g', _flag) -> g'
        case Binding.interiorOfUnder canonical c0 g of
            Left err -> throwError (BindingTreeError err)
            Right s -> pure s

    let subst = IntMap.fromList [(getNodeId k, v) | (k, v) <- substList]
        initialCopyMap = IntMap.fromList [(getNodeId k, v) | (k, v) <- substList]
        initialInterior = IntSet.fromList (map (getNodeId . snd) substList)
        st0 =
            CopyState
                { csCache = IntMap.empty
                , csCopyMap = initialCopyMap
                , csInterior = initialInterior
                }
    (root, st1) <- runStateT (copyNode copyInterior canonical subst bodyId) st0
    let cmap = csCopyMap st1
        interior = csInterior st1
    -- Ensure the binding tree remains valid after copying: substitution nodes
    -- (binder-metas) may become non-roots once referenced by fresh copies, and
    -- copied nodes whose original parents were not copied need a parent.
    bindUnboundCopiedNodes cmap interior root
    pure (root, cmap, interior)
  where
    recordNew :: NodeId -> StateT CopyState PresolutionM ()
    recordNew freshId =
        modify $ \st ->
            st { csInterior = IntSet.insert (getNodeId freshId) (csInterior st) }

    recordCopy :: NodeId -> NodeId -> StateT CopyState PresolutionM ()
    recordCopy src dst =
        modify $ \st ->
            st { csCopyMap = IntMap.insert (getNodeId src) dst (csCopyMap st) }

    cacheInsert :: NodeId -> NodeId -> StateT CopyState PresolutionM ()
    cacheInsert src dst =
        modify $ \st ->
            st { csCache = IntMap.insert (getNodeId src) dst (csCache st) }

    -- | Copy binding edge from source node to fresh node, translating parent through copyMap.
    --
    -- Paper alignment (`papers/xmlf.txt` §3.2): copied nodes preserve their binding
    -- edges/flags. If the original parent was also copied, we use the copied parent;
    -- otherwise we do NOT copy the binding edge (the fresh node becomes a root in
    -- the expansion graph, to be bound later by bindExpansionRootLikeTarget).
    --
    -- Note: We cannot use the original parent if it was not copied, because the
    -- fresh node is not in the term-DAG structure of the original parent, which
    -- would violate the "parent is upper than child" invariant.
    copyBindingEdge :: NodeId -> NodeId -> StateT CopyState PresolutionM ()
    copyBindingEdge srcNid freshId = do
        c <- lift $ gets psConstraint
        case Binding.lookupBindParent c srcNid of
            Nothing -> pure ()  -- Source is a root, fresh node becomes a root too
            Just (parentId, flag) -> do
                copyMap <- gets csCopyMap
                -- If parent was copied, use the copied parent; otherwise skip
                -- (the fresh node will be bound later by bindExpansionRootLikeTarget)
                case IntMap.lookup (getNodeId parentId) copyMap of
                    Just copiedParent -> lift $ setBindParentM freshId (copiedParent, flag)
                    Nothing -> pure ()  -- Parent not copied, don't set binding edge

    copyNode :: IntSet.IntSet -> (NodeId -> NodeId) -> IntMap NodeId -> NodeId -> StateT CopyState PresolutionM NodeId
    copyNode copyInterior canonical subst nid = do
        -- Check explicit substitution (bound vars)
        case IntMap.lookup (getNodeId nid) subst of
            Just freshId -> return freshId
            Nothing -> do
                -- Check cache (cycle breaking / sharing preservation)
                cache <- gets csCache
                case IntMap.lookup (getNodeId nid) cache of
                    Just copiedId -> return copiedId
                    Nothing -> do
                        -- Retrieve original node
                        node <- lift $ getCanonicalNode nid

                        -- Check level to decide whether to copy or share
                        let k = getNodeId (canonical nid)
                        shouldShare <- case node of
                            TyBase {} -> return True
                            _ -> return (not (IntSet.member k copyInterior))

                        if shouldShare then return nid
                        else do
                            -- Special handling for TyExp to inline expansions
                            case node of
                                TyExp { tnExpVar = s, tnBody = b } -> do
                                    expn <- lift $ getExpansion s
                                    case expn of
                                        ExpIdentity -> do
                                            -- Inline Identity expansion: just copy body
                                            -- Update cache after copy to handle cycles correctly?
                                            -- Actually, if we skip TyExp, the cycle is on the body.
                                            -- The cache for 'nid' (the TyExp) should point to the copy of 'b'.
                                            -- We can defer cache update or do it after.
                                            -- But if 'b' refers back to 'nid', we loop.
                                            -- 'b' referring to 'nid' means Body contains parent TyExp.
                                            -- Infinite type. MLF allows recursive types?
                                            -- If so, we need to be careful.
                                            -- Let's allocate a placeholder or use lazy tying?
                                            -- Simpler: just recurse. If cycle, 'copyNode b' will handle it via its own cache entry.
                                            -- But 'nid' cache entry is needed if 'b' refers to 'nid'.
                                            -- If we skip 'nid', then 'b' referring to 'nid' effectively refers to 'b' (or copy of b).
                                            -- This is knot-tying.
                                            -- For now, let's assume no cycles involving TyExp skipping.
                                            res <- copyNode copyInterior canonical subst b
                                            cacheInsert nid res
                                            recordCopy nid res
                                            return res
                                        _ -> do
                                            -- Non-identity expansion.
                                            -- We must materialize the expansion result.
                                            b' <- copyNode copyInterior canonical subst b
                                            (res, (cmap, interior)) <- lift $ applyExpansionTraced expn (TyExp (NodeId (-1)) s b')
                                            cacheInsert nid res
                                            recordCopy nid res
                                            modify $ \st ->
                                                st
                                                    { csCopyMap = IntMap.union (csCopyMap st) cmap
                                                    , csInterior = IntSet.union (csInterior st) interior
                                                    }
                                            return res

                                _ -> do
                                    -- Create fresh node shell
                                    freshId <- lift createFreshNodeId
                                    cacheInsert nid freshId
                                    recordCopy nid freshId
                                    recordNew freshId

                                    -- Copy binding edge from source to fresh node
                                    -- Paper alignment: preserve binding edges/flags for copied nodes
                                    copyBindingEdge nid freshId

                                    -- Recursively copy children
                                    newNode <- case node of
                                        TyArrow { tnDom = d, tnCod = c } -> do
                                            d' <- copyNode copyInterior canonical subst d
                                            c' <- copyNode copyInterior canonical subst c
                                            return $ TyArrow freshId d' c'
                                        TyForall { tnOwnerLevel = ownerLvl, tnQuantLevel = q, tnBody = b } -> do
                                            b' <- copyNode copyInterior canonical subst b
                                            return $ TyForall freshId q ownerLvl b'
                                        TyVar { tnVarLevel = l } -> do
                                            return $ TyVar freshId l
                                        TyBase { tnBase = b } -> do
                                            return $ TyBase freshId b

                                    -- Register new node in constraint
                                    lift $ registerNode freshId newNode
                                    return freshId

-- | Helper to create a fresh NodeId
createFreshNodeId :: PresolutionM NodeId
createFreshNodeId = do
    st <- get
    let nid = NodeId (psNextNodeId st)
    put $ st { psNextNodeId = psNextNodeId st + 1 }
    return nid

-- | Helper to register a node in the constraint
registerNode :: NodeId -> TyNode -> PresolutionM ()
registerNode nid node = do
    modify $ \st ->
        let c = psConstraint st
            nodes' = IntMap.insert (getNodeId nid) node (cNodes c)
        in st { psConstraint = c { cNodes = nodes' } }

-- | Helper to set a binding parent for a node in the constraint.
--
-- Paper alignment (`papers/xmlf.txt` §3.1): this updates the binding tree
-- to record that `child` is bound by `parent` with the given flag.
setBindParentM :: NodeId -> (NodeId, BindFlag) -> PresolutionM ()
setBindParentM child parentInfo = do
    modify $ \st ->
        let c = psConstraint st
            c' = Binding.setBindParent child parentInfo c
        in st { psConstraint = c' }

-- | Bind the expansion root at the same binder as the edge target.
--
-- Paper alignment (`papers/xmlf.txt` §3.2): "the root of the expansion is bound
-- at the same binder as the target". This ensures the expansion root is in the
-- correct interior I(r) for subsequent operations.
--
-- If the target has a binding parent, we copy that binding to the expansion root.
-- If the target is a binding root (no parent), the expansion root also becomes
-- a binding root (we don't set a binding parent for it).
bindExpansionRootLikeTarget :: NodeId -> NodeId -> PresolutionM ()
bindExpansionRootLikeTarget expansionRoot targetNode = do
    c <- gets psConstraint
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
    mbParentInfo <- case Binding.lookupBindParentUnder canonical c targetNode of
        Left err -> throwError (BindingTreeError err)
        Right p -> pure p
    case mbParentInfo of
        Just parentInfo -> setBindParentM expansionRoot parentInfo
        Nothing -> pure ()  -- Target is a root, expansion root also becomes a root

-- | Bind all copied nodes that don't have binding parents to the expansion root.
--
-- During expansion copying, some nodes may not get binding parents because their
-- original parents were not copied. This function ensures all copied nodes have
-- binding parents by binding unbound nodes to the expansion root.
--
-- This maintains the binding tree invariant that all non-term-dag-root nodes
-- have binding parents.
bindUnboundCopiedNodes :: IntMap NodeId -> IntSet.IntSet -> NodeId -> PresolutionM ()
bindUnboundCopiedNodes copyMap interior expansionRoot = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
        expansionRootC = canonical expansionRoot

    let copiedIds = IntSet.fromList (map getNodeId (IntMap.elems copyMap))
        candidateIds0 = IntSet.union copiedIds interior

        structuralChildren :: NodeId -> [NodeId]
        structuralChildren nid =
            case IntMap.lookup (getNodeId nid) (cNodes c0) of
                Just TyArrow { tnDom = d, tnCod = c } -> [d, c]
                Just TyForall { tnBody = b } -> [b]
                Just TyExp { tnBody = b } -> [b]
                _ -> []

        reachableFrom :: IntSet.IntSet -> IntSet.IntSet
        reachableFrom starts = go starts (IntSet.toList starts)
          where
            go seen [] = seen
            go seen (nid:work) =
                let next = structuralChildren (NodeId nid)
                    fresh = filter (\(NodeId k) -> not (IntSet.member k seen)) next
                    seen' = foldr (IntSet.insert . getNodeId) seen fresh
                    work' = work ++ map getNodeId fresh
                in go seen' work'

        -- Compute term-dag roots on the UF-quotient structure graph (matching
        -- `Binding.checkBindingTreeUnder`).
        termDagRootsUnder :: IntSet.IntSet
        termDagRootsUnder =
            let nodes0 = cNodes c0

                rootIdOf :: NodeId -> Int
                rootIdOf = getNodeId . canonical

                structuralKids :: TyNode -> [NodeId]
                structuralKids node = case node of
                    TyVar {} -> []
                    TyBase {} -> []
                    TyArrow _ dom cod -> [dom, cod]
                    TyForall _ _ _ body -> [body]
                    TyExp _ _ body -> [body]

                addStructEdges :: IntMap.IntMap IntSet.IntSet -> TyNode -> IntMap.IntMap IntSet.IntSet
                addStructEdges m node =
                    let parentRoot = rootIdOf (tnId node)
                        childRoots = IntSet.fromList (map rootIdOf (structuralKids node))
                    in if IntSet.null childRoots
                        then m
                        else IntMap.insertWith IntSet.union parentRoot childRoots m

                structEdges :: IntMap.IntMap IntSet.IntSet
                structEdges = IntMap.foldl' addStructEdges IntMap.empty nodes0

                allRoots :: IntSet.IntSet
                allRoots = IntSet.fromList [rootIdOf (NodeId nid) | nid <- IntMap.keys nodes0]

                incomingRoots :: IntSet.IntSet
                incomingRoots = IntSet.unions (IntMap.elems structEdges)
            in IntSet.difference allRoots incomingRoots

        candidateIds = reachableFrom candidateIds0

    -- Bind any copied/interior nodes that are not term-dag roots, do not already
    -- have a binding parent, and are not the expansion root itself.
    --
    -- Note: in a multi-root constraint graph, expansion copying may reuse nodes
    -- that were previously term-dag roots (disconnected components). Once a
    -- freshly-copied node points to such a reused node, it ceases to be a
    -- term-dag root in the quotient graph and must have a binding parent.
    --
    -- We conservatively bind any newly reachable unbound nodes to the expansion
    -- root to preserve binding-tree invariants.
    forM_ (IntSet.toList candidateIds) $ \nid -> do
        let node0 = NodeId nid
            nodeC = canonical node0
        c' <- gets psConstraint
        let needsParent = not (IntSet.member (getNodeId nodeC) termDagRootsUnder)
        when (needsParent && nodeC /= expansionRootC) $
            case Binding.lookupBindParent c' nodeC of
                Just _ -> pure ()
                Nothing -> setBindParentM nodeC (expansionRootC, BindFlex)

-- | Helper to create a fresh variable node
-- | Allocate a fresh variable at the given generalization level.
createFreshVarAt :: GNodeId -> PresolutionM NodeId
createFreshVarAt lvl = do
    ensureLevelExists lvl
    nid <- createFreshNodeId
    let node = TyVar nid lvl
    registerNode nid node
    -- Keep `gBinds` consistent with ConstraintGen/Normalize: any variable allocated
    -- at a level is tracked in that level's bind list.
    modify $ \st ->
        let c = psConstraint st
            gnodes' = GNodeOps.ensureVarBindAtLevel lvl nid (cGNodes c)
        in st { psConstraint = c { cGNodes = gnodes' } }
    return nid

-- | Update the instance bound of a type variable in the binding tree (if present).
setVarBound :: NodeId -> Maybe NodeId -> PresolutionM ()
setVarBound vid mb = do
    node <- getNode vid
    case node of
        TyVar{} -> do
            root <- findRoot vid
            mbRoot <- mapM findRoot mb
            modify' $ \st ->
                let c0 = psConstraint st
                    c1 = VarStore.setVarBound root mbRoot c0
                in st { psConstraint = c1 }
        _ -> pure ()

-- | Mark a type variable as eliminated so elaboration will not re-quantify it.
dropVarBind :: NodeId -> PresolutionM ()
dropVarBind vid = do
    node <- getNode vid
    case node of
        TyVar{} ->
            modify' $ \st ->
                let c0 = psConstraint st
                    c1 = VarStore.markEliminatedVar vid c0
                in st { psConstraint = c1 }
        _ -> pure ()

-- | Verify that the requested G-node exists, unless none are tracked.
ensureLevelExists :: GNodeId -> PresolutionM ()
ensureLevelExists lvl = do
    gnodes <- gets (cGNodes . psConstraint)
    when (not (IntMap.null gnodes) && IntMap.notMember (getGNodeId lvl) gnodes) $
        throwError $ MissingGNode lvl

-- | Helper to collect variables bound at a specific level
collectBoundVars :: NodeId -> GNodeId -> PresolutionM [NodeId]
collectBoundVars rootId level = do
    -- BFS/DFS to find TyVars with tnVarLevel == level
    -- We need to access the graph.
    st <- get
    let nodes = cNodes (psConstraint st)
    let vars = go nodes IntSet.empty [rootId]
    -- trace ("collectBoundVars root=" ++ show rootId ++ " level=" ++ show level ++ " found=" ++ show vars) $ return vars
    return vars
  where
    go _ _ [] = []
    go nodes visited (nid:rest)
        | IntSet.member (getNodeId nid) visited = go nodes visited rest
        | otherwise =
            let visited' = IntSet.insert (getNodeId nid) visited
                node = IntMap.lookup (getNodeId nid) nodes
                vars = case node of
                    Just (TyVar { tnId = vid, tnVarLevel = vLevel })
                        | vLevel == level -> [vid]
                    _ -> []
                children = case node of
                    Just n -> getChildren n
                    Nothing -> []
            in vars ++ go nodes visited' (children ++ rest)

    getChildren :: TyNode -> [NodeId]
    getChildren = \case
        TyArrow { tnDom = d, tnCod = c } -> [d, c]
        TyForall { tnBody = b } -> [b]
        TyExp { tnBody = b } -> [b]
        _ -> []

-- | Union-Find helpers
getCanonicalNode :: NodeId -> PresolutionM TyNode
getCanonicalNode nid = do
    rootId <- findRoot nid
    st <- get
    case IntMap.lookup (getNodeId rootId) (cNodes (psConstraint st)) of
        Just node -> return node
        Nothing -> throwError $ NodeLookupFailed rootId

-- | Lightweight reachability to prevent emitting a unification that would
-- immediately create a self-reference (occurs-check for presolution).
occursIn :: NodeId -> NodeId -> PresolutionM Bool
occursIn needle start = do
    needleRoot <- findRoot needle
    let go visited nid = do
            root <- findRoot nid
            if IntSet.member (getNodeId root) visited
                then return False
                else if root == needleRoot
                    then return True
                    else do
                        node <- getCanonicalNode root
                        let children = case node of
                                TyArrow { tnDom = d, tnCod = c } -> [d, c]
                                TyForall { tnBody = b } -> [b]
                                TyExp { tnBody = b } -> [b]
                                _ -> []
                        foldM
                            (\acc child -> if acc then return True else go (IntSet.insert (getNodeId root) visited) child)
                            False
                            children
    go IntSet.empty start

findRoot :: NodeId -> PresolutionM NodeId
findRoot nid = do
    uf <- gets psUnionFind
    let (root, uf') = UnionFind.findRootWithCompression uf nid
    modify $ \st -> st { psUnionFind = uf' }
    pure root

-- | Union-Find merge with occurs-check, returning the Raise trace induced by
-- binding-edge harmonization.
--
-- Paper anchor (`papers/xmlf.txt`): `Raise(n)` is a binding-edge raising
-- operation (a real χe graph transformation).
--
-- Returns the exact raised-node trace (with multiplicity) induced by binding-edge
-- harmonization. Presolution records `OpRaise` based on this trace (filtered to
-- interior nodes and not under rigid binders).
unifyAcyclicRawWithRaiseTrace :: NodeId -> NodeId -> PresolutionM [NodeId]
unifyAcyclicRawWithRaiseTrace n1 n2 = do
    root1 <- findRoot n1
    root2 <- findRoot n2
    if root1 == root2
        then pure []
        else unifyAcyclicRootsWithRaiseTrace root1 root2

unifyAcyclicRootsWithRaiseTrace :: NodeId -> NodeId -> PresolutionM [NodeId]
unifyAcyclicRootsWithRaiseTrace root1 root2 = do
    occurs12 <- occursIn root1 root2
    when occurs12 $ throwError $ OccursCheckPresolution root1 root2

    occurs21 <- occursIn root2 root1
    when occurs21 $ throwError $ OccursCheckPresolution root2 root1

    st0 <- get
    let c0 = psConstraint st0

    (c1, trace0) <-
        case BindingAdjustment.harmonizeBindParentsWithTrace root1 root2 c0 of
            Left err -> throwError (BindingTreeError err)
            Right result -> pure result

    put st0 { psConstraint = c1 }
    modify $ \st ->
        st { psUnionFind = IntMap.insert (getNodeId root1) root2 (psUnionFind st) }

    -- Keep the binding-parent relation representative-canonical after UF merges.
    -- This avoids “forest LCA” artifacts when two binders become equal via UF but
    -- `cBindParents` still mentions their pre-merge aliases.
    canonicalizeBindParentsWithUF

    pure trace0

canonicalizeBindParentsWithUF :: PresolutionM ()
canonicalizeBindParentsWithUF = do
    st0 <- get
    let c0 = psConstraint st0
        uf = psUnionFind st0
        canonical = UnionFind.frWith uf
    case Binding.canonicalizeBindParentsUnder canonical c0 of
        Left err -> throwError (BindingTreeError err)
        Right bp ->
            put st0 { psConstraint = c0 { cBindParents = bp } }

unifyAcyclicRawWithRaiseCounts :: NodeId -> NodeId -> PresolutionM (Int, Int)
unifyAcyclicRawWithRaiseCounts n1 n2 = do
    root1 <- findRoot n1
    root2 <- findRoot n2
    if root1 == root2
        then pure (0, 0)
        else do
            trace <- unifyAcyclicRootsWithRaiseTrace root1 root2
            let k1 = length (filter (== root1) trace)
                k2 = length (filter (== root2) trace)
            pure (k1, k2)

-- | Union-Find merge with occurs-check.
unifyAcyclicRaw :: NodeId -> NodeId -> PresolutionM ()
unifyAcyclicRaw n1 n2 = do
    _ <- unifyAcyclicRawWithRaiseCounts n1 n2
    pure ()

unifyAcyclic :: NodeId -> NodeId -> PresolutionM ()
unifyAcyclic = unifyAcyclicRaw
