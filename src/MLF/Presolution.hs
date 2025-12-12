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
    runPresolutionM,

    -- * Building blocks (exported for testing)
    decideMinimalExpansion,
    processInstEdge,
    instantiateScheme,
    mergeExpansions,
    applyExpansion
) where

import Control.Monad.State
import Control.Monad.Except (throwError)
import Control.Monad (forM, forM_, zipWithM, zipWithM_, when, foldM)
import qualified Data.List.NonEmpty as NE
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe, mapMaybe)

import MLF.Types
import MLF.Acyclicity (AcyclicityResult(..))
-- We will likely need unification logic from Normalize,
-- but for now we'll implement the structure.

-- | Result of the presolution phase.
data PresolutionResult = PresolutionResult
    { prConstraint :: Constraint
    , prEdgeExpansions :: IntMap Expansion
    , prEdgeWitnesses :: IntMap EdgeWitness
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
    }
    deriving (Eq, Show)

-- | The Presolution monad.
type PresolutionM = StateT PresolutionState (Either PresolutionError)

-- | Run a PresolutionM action with an initial state (testing helper).
runPresolutionM :: PresolutionState -> PresolutionM a -> Either PresolutionError (a, PresolutionState)
runPresolutionM st action = runStateT action st

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
finalizePresolution :: PresolutionM (IntMap NodeId)
finalizePresolution = do
    mapping <- materializeExpansions
    rewriteConstraint mapping

-- | Apply final expansions to all TyExp nodes and record their replacements.
materializeExpansions :: PresolutionM (IntMap NodeId)
materializeExpansions = do
    nodes <- gets (cNodes . psConstraint)
    let exps = [ n | n@TyExp{} <- IntMap.elems nodes ]
    fmap IntMap.fromList $ forM exps $ \expNode -> do
        let eid = tnId expNode
        expn <- getExpansion (tnExpVar expNode)
        nid' <- applyExpansion expn expNode
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

        chaseMap nid = case IntMap.lookup (getNodeId nid) mapping of
            Nothing -> nid
            Just n' -> chaseMap n'

        canonical nid = chaseMap (frWith uf nid)

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
                    TyVar { tnLevel = l } -> TyVar nid' l
                    TyArrow { tnDom = d, tnCod = cod } -> TyArrow nid' (canonical d) (canonical cod)
                    TyBase { tnBase = b } -> TyBase nid' b
                    TyForall { tnLevel = ownerLvl, tnQuantLevel = quantLvl, tnBody = b } ->
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

        -- Canonicalize redirects (values in the map)
        -- mapping maps OldId -> NewId. NewId might be non-canonical.
        -- We want to return a map for ALL nodes that were redirected or merged.
        fullRedirects = IntMap.fromList
            [ (nid, canonical (NodeId nid))
            | nid <- IntMap.keys (cNodes c)
            ]

    put st { psConstraint = c
                { cNodes = newNodes
                , cInstEdges = []
                , cUnifyEdges = map rewriteUnify (cUnifyEdges c)
                }
           , psEdgeExpansions = newExps
           , psEdgeWitnesses = newWitnesses
           }

    return fullRedirects

-- | Read-only chase like Solve.frWith
frWith :: IntMap NodeId -> NodeId -> NodeId
frWith uf nid = case IntMap.lookup (getNodeId nid) uf of
    Nothing -> nid
    Just p -> frWith uf p

-- | The main loop processing sorted instantiation edges.
runPresolutionLoop :: [InstEdge] -> PresolutionM ()
runPresolutionLoop edges = forM_ edges processInstEdge

-- | Process a single instantiation edge.
processInstEdge :: InstEdge -> PresolutionM ()
processInstEdge edge = do
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
            w <- buildEdgeWitness edgeId n1Id n2Id n1 finalExp
            recordEdgeWitness edgeId w

            -- Perform unifications requested by expansion decision
            mapM_ (uncurry unifyAcyclic) unifications

            -- Unify LHS (TyExp) directly with RHS.
            -- MaterializeExpansions will replace TyExp with its expansion, and
            -- RewriteConstraint will update this unify edge to point to the expansion.
            -- However, if expansion is Identity, TyExp effectively becomes its body.
            -- Unifying TyExp with body (or something unified with body) creates a cycle
            -- (TyExp -> body -> TyExp).
            -- So we skip explicit unification for Identity, relying on decideMinimalExpansion
            -- to have returned unifications between the body and the target.
            if finalExp == ExpIdentity
                then return ()
                else do
                    -- Eagerly materialize and unify to resolve the constraint immediately.
                    -- This ensures that the expansion result is unified with the target (n2),
                    -- and TyExp is unified with the result.
                    resNodeId <- applyExpansion finalExp n1
                    unifyStructure resNodeId (tnId n2)
                    unifyAcyclic (tnId n1) resNodeId

        _ -> do
            n1 <- getCanonicalNode n1Id
            -- n1 is not an expansion node.
            -- This is a standard instantiation constraint (or just subtyping).
            -- "If the left hand side is not an expansion node, it must be equal to the right hand side"
            -- (Simplification for now, might need refinement for full MLF)
            recordEdgeExpansion edgeId ExpIdentity
            w <- buildEdgeWitness edgeId n1Id n2Id n1Raw ExpIdentity
            recordEdgeWitness edgeId w
            unifyStructure (tnId n1) (tnId n2)

-- | Record a witness for an instantiation edge.
recordEdgeWitness :: EdgeId -> EdgeWitness -> PresolutionM ()
recordEdgeWitness (EdgeId eid) w =
    modify $ \st -> st { psEdgeWitnesses = IntMap.insert eid w (psEdgeWitnesses st) }

-- | Build an edge witness from the chosen expansion recipe.
--
-- This is intentionally conservative: we record only the operations induced by
-- our current presolution lattice (Identity / Instantiate / ∀-intro / Compose).
-- More elaborate witnesses (raise/merge inside interiors, etc.) can be added
-- incrementally as the solver gains a more explicit instance-operation engine.
buildEdgeWitness :: EdgeId -> NodeId -> NodeId -> TyNode -> Expansion -> PresolutionM EdgeWitness
buildEdgeWitness eid left right leftRaw expn = do
    root <- case leftRaw of
        TyExp{ tnBody = b } -> pure b
        _ -> pure left
    iw <- witnessFromExpansion root leftRaw expn
    pure EdgeWitness
        { ewEdgeId = eid
        , ewLeft = left
        , ewRight = right
        , ewRoot = root
        , ewWitness = iw
        }

-- | Convert our presolution expansion recipe into a (coarse) instance-operation witness.
witnessFromExpansion :: NodeId -> TyNode -> Expansion -> PresolutionM InstanceWitness
witnessFromExpansion root leftRaw expn = do
    ops <- go expn
    pure (InstanceWitness ops)
  where
    go :: Expansion -> PresolutionM [InstanceOp]
    go ExpIdentity = pure []
    go (ExpCompose es) = concat <$> mapM go (NE.toList es)
    go (ExpForall ls) = do
        -- Model ∀-introduction as repeated “raise” at the root.
        -- (xmlf uses O for ∀-intro; we refine the exact witness/translation later.)
        pure (replicate (length (NE.toList ls)) (OpRaise root))
    go (ExpInstantiate args) = do
        -- If the TyExp body is a forall, instantiate its binders in the same order
        -- that `applyExpansion` uses (collectBoundVars + zip).
        case leftRaw of
            TyExp{ tnBody = b } -> do
                bNode <- getCanonicalNode b
                case bNode of
                    TyForall{ tnBody = inner, tnQuantLevel = q } -> do
                        boundVars <- collectBoundVars inner q
                        if length boundVars /= length args
                            then throwError (ArityMismatch "witnessFromExpansion/ExpInstantiate" (length boundVars) (length args))
                            else do
                                let pairs = zip args boundVars
                                pure $ concatMap (\(arg, bv) -> [OpGraft arg bv, OpWeaken bv]) pairs
                    _ -> throwError (InstantiateOnNonForall (tnId bNode))
            _ -> do
                -- Instantiating a non-TyExp is unexpected in current pipeline; treat as empty.
                pure []

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
decideMinimalExpansion (TyExp { tnBody = bodyId }) targetNode = do
    body <- getCanonicalNode bodyId
    case body of
        TyForall { tnBody = forallBody, tnQuantLevel = lvl } -> do
            boundVars <- collectBoundVars forallBody lvl
            case targetNode of
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
                    if null boundVars then
                        -- Note [Minimal Expansion Decision] case 2 (∀≤structure, degenerate ∀)
                        return (ExpIdentity, [(forallBody, tnId targetNode)])
                    else do
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
-- Only TyVar and TyForall have tnLevel; others return root level (GNodeId 0).
getNodeLevel :: TyNode -> GNodeId
getNodeLevel node = case node of
    TyVar { tnLevel = l } -> l
    TyForall { tnLevel = l } -> l
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
    (ExpInstantiate args, TyExp { tnBody = b }) -> do
        body <- getCanonicalNode b
        case body of
            TyForall { tnBody = innerBody, tnQuantLevel = q } -> do
                boundVars <- collectBoundVars innerBody q
                if length boundVars /= length args
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
    applyExpansionOverNode _ (ExpInstantiate args) node _ =
        case node of
            TyExp{} -> throwError $ InstantiateOnNonForall (tnId node)
            TyForall { tnBody = innerBody, tnQuantLevel = q } -> do
                boundVars <- collectBoundVars innerBody q
                if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansionOverNode" (length boundVars) (length args)
                    else instantiateScheme innerBody q (zip boundVars args)
            _ -> throwError $ InstantiateOnNonForall (tnId node)

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
    let subst = IntMap.fromList [(getNodeId k, v) | (k, v) <- substList]
    evalStateT (copyNode subst bodyId) IntMap.empty
  where
    copyNode :: IntMap NodeId -> NodeId -> StateT (IntMap NodeId) PresolutionM NodeId
    copyNode subst nid = do
        -- Check explicit substitution (bound vars)
        case IntMap.lookup (getNodeId nid) subst of
            Just freshId -> return freshId
            Nothing -> do
                -- Check cache (cycle breaking / sharing preservation)
                cache <- get
                case IntMap.lookup (getNodeId nid) cache of
                    Just copiedId -> return copiedId
                    Nothing -> do
                        -- Retrieve original node
                        node <- lift $ getCanonicalNode nid

                        -- Check level to decide whether to copy or share
                        shouldShare <- case node of
                            TyVar { tnLevel = l } -> return (l < quantLevel)
                            TyBase {} -> return True -- Optimization: share base types
                            _ -> return False

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
                                            res <- copyNode subst b
                                            modify $ IntMap.insert (getNodeId nid) res
                                            return res
                                        _ -> do
                                            -- Non-identity expansion.
                                            -- We must materialize the expansion result.
                                            b' <- copyNode subst b
                                            res <- lift $ applyExpansion expn (TyExp (NodeId (-1)) s b')
                                            modify $ IntMap.insert (getNodeId nid) res
                                            return res

                                _ -> do
                                    -- Create fresh node shell
                                    freshId <- lift createFreshNodeId
                                    modify $ IntMap.insert (getNodeId nid) freshId

                                    -- Recursively copy children
                                    newNode <- case node of
                                        TyArrow { tnDom = d, tnCod = c } -> do
                                            d' <- copyNode subst d
                                            c' <- copyNode subst c
                                            return $ TyArrow freshId d' c'
                                        TyForall { tnLevel = ownerLvl, tnQuantLevel = q, tnBody = b } -> do
                                            b' <- copyNode subst b
                                            return $ TyForall freshId q ownerLvl b'
                                        TyVar { tnLevel = l } -> do
                                            return $ TyVar freshId l
                                        TyBase { tnBase = b } -> do
                                            return $ TyBase freshId b
                                        -- TyExp handled above
                                        TyExp {} -> error "Unreachable TyExp"

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
    modify $ \st -> st { psConstraint = (psConstraint st) { cNodes = IntMap.insert (getNodeId nid) node (cNodes (psConstraint st)) } }

-- | Helper to create a fresh variable node
-- | Allocate a fresh variable at the given generalization level.
createFreshVarAt :: GNodeId -> PresolutionM NodeId
createFreshVarAt lvl = do
    ensureLevelExists lvl
    nid <- createFreshNodeId
    let node = TyVar nid lvl
    registerNode nid node
    return nid

-- | Verify that the requested G-node exists, unless none are tracked.
ensureLevelExists :: GNodeId -> PresolutionM ()
ensureLevelExists lvl = do
    gnodes <- gets (cGNodes . psConstraint)
    when (not (IntMap.null gnodes) && IntMap.notMember (getGNodeId lvl) gnodes) $
        throwError $ MissingGNode lvl

-- | Helper to collect variables bound at a specific level
collectBoundVars :: NodeId -> GNodeId -> PresolutionM [NodeId]
collectBoundVars rootId level = do
    -- BFS/DFS to find TyVars with tnLevel == level
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
                    Just (TyVar { tnId = vid, tnLevel = vLevel })
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
    case IntMap.lookup (getNodeId nid) uf of
        Nothing -> return nid
        Just parent -> do
            root <- findRoot parent
            -- Path compression
            modify $ \st -> st { psUnionFind = IntMap.insert (getNodeId nid) root (psUnionFind st) }
            return root

unifyAcyclic :: NodeId -> NodeId -> PresolutionM ()
unifyAcyclic n1 n2 = do
    root1 <- findRoot n1
    root2 <- findRoot n2
    when (root1 /= root2) $ do
        -- trace ("unifyAcyclic " ++ show root1 ++ " " ++ show root2) $ return ()
        occurs12 <- occursIn root1 root2
        when occurs12 $ throwError $ OccursCheckPresolution root1 root2

        occurs21 <- occursIn root2 root1
        when occurs21 $ throwError $ OccursCheckPresolution root2 root1

        modify $ \st -> st { psUnionFind = IntMap.insert (getNodeId root1) root2 (psUnionFind st) }
        return ()

