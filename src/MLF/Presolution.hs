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
    instantiateScheme
) where

import Control.Monad.State
import Control.Monad.Except (throwError)
import Control.Monad (forM_, zipWithM_, when, foldM)
import qualified Data.List.NonEmpty as NE
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)

import MLF.Types
import MLF.Acyclicity (AcyclicityResult(..))
-- We will likely need unification logic from Normalize,
-- but for now we'll implement the structure.

-- | Result of the presolution phase.
data PresolutionResult = PresolutionResult
    { prPresolution :: Presolution        -- ^ The computed ExpVar → Expansion map
    , prConstraint :: Constraint          -- ^ Updated constraint (with new UnifyEdges)
    , prUnionFind :: IntMap NodeId        -- ^ Updated union-find from incremental unification
    }
    deriving (Eq, Show)

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
            }

    -- Run the presolution loop
    (finalState) <- execStateT (runPresolutionLoop (arSortedEdges acyclicityResult)) initialState

    return PresolutionResult
        { prPresolution = psPresolution finalState
        , prConstraint = psConstraint finalState
        , prUnionFind = psUnionFind finalState
        }

-- | Helper to find the next available NodeId
findMaxNodeId :: Constraint -> Int
findMaxNodeId c =
    let maxNode = if IntMap.null (cNodes c) then 0 else fst (IntMap.findMax (cNodes c))
    in maxNode

-- | The main loop processing sorted instantiation edges.
runPresolutionLoop :: [InstEdge] -> PresolutionM ()
runPresolutionLoop edges = forM_ edges processInstEdge

-- | Process a single instantiation edge.
processInstEdge :: InstEdge -> PresolutionM ()
processInstEdge edge = do
    let n1Id = instLeft edge
    let n2Id = instRight edge

    -- Resolve canonical nodes
    n1 <- getCanonicalNode n1Id
    n2 <- getCanonicalNode n2Id

    case n1 of
        TyExp { tnExpVar = s, tnBody = _bodyId } -> do
            -- n1 is an expansion node. We need to ensure s expands enough to cover n2.
            currentExp <- getExpansion s

            -- Decide required expansion based on n2
            (reqExp, unifications) <- decideMinimalExpansion n1 n2

            -- Merge with current expansion
            finalExp <- mergeExpansions s currentExp reqExp

            -- Update presolution
            setExpansion s finalExp

            -- Perform unifications requested by expansion decision
            mapM_ (uncurry unifyAcyclic) unifications

            -- Apply expansion to get the concrete node and unify with target
            expandedNodeId <- applyExpansion finalExp n1
            unifyAcyclic expandedNodeId (tnId n2)

        _ -> do
            -- n1 is not an expansion node.
            -- This is a standard instantiation constraint (or just subtyping).
            -- In MLF, non-expansion instantiation is just unification or check?
            -- "If the left hand side is not an expansion node, it must be equal to the right hand side"
            -- (Simplification for now, might need refinement for full MLF)
            unifyAcyclic (tnId n1) (tnId n2)

-- | Get the current expansion for an expansion variable.
getExpansion :: ExpVarId -> PresolutionM Expansion
getExpansion s = do
    Presolution m <- gets psPresolution
    return $ fromMaybe ExpIdentity (IntMap.lookup (getExpVarId s) m)

-- | Set the expansion for an expansion variable.
setExpansion :: ExpVarId -> Expansion -> PresolutionM ()
setExpansion s expansion = do
    modify $ \st -> st { psPresolution = Presolution $ IntMap.insert (getExpVarId s) expansion (getAssignments (psPresolution st)) }

-- | Merge two expansions for the same variable.
-- This may trigger unifications if we merge two Instantiates.
mergeExpansions :: ExpVarId -> Expansion -> Expansion -> PresolutionM Expansion
mergeExpansions _ ExpIdentity e2 = return e2
mergeExpansions _ e1 ExpIdentity = return e1
mergeExpansions _ (ExpInstantiate args1) (ExpInstantiate args2)
    | length args1 == length args2 = do
        zipWithM_ unifyAcyclic args1 args2
        return (ExpInstantiate args1)
    | otherwise = throwError $ ArityMismatch "ExpInstantiate merge" (length args1) (length args2)
mergeExpansions _ (ExpForall l1) (ExpForall l2)
    | l1 == l2  = return (ExpForall l1)
    | otherwise = return (ExpCompose (ExpForall l1 NE.:| [ExpForall l2]))
mergeExpansions _ (ExpCompose xs) (ExpCompose ys) = return (ExpCompose (xs <> ys))
mergeExpansions _ (ExpCompose xs) e2 = return (ExpCompose (xs <> pure e2))
mergeExpansions _ e1 (ExpCompose ys) = return (ExpCompose (e1 NE.<| ys))
mergeExpansions _ e1 e2 = return (ExpCompose (e1 NE.<| (e2 NE.:| [])))

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

-- | Apply an expansion to a TyExp node.
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
    (ExpForall levels, TyExp { tnBody = b }) ->
        wrapForall (NE.toList levels) b
    (ExpCompose exps, TyExp { tnBody = b }) ->
        foldM (\nid e -> do
                node <- getCanonicalNode nid
                applyExpansionOverNode e node nid)
              b
              (NE.toList exps)
    -- If expansion is applied to a non-expansion node (after composition), use the helper
    (expn, otherNode) -> applyExpansionOverNode expn otherNode (tnId otherNode)
  where
    wrapForall :: [GNodeId] -> NodeId -> PresolutionM NodeId
    wrapForall [] nid = return nid
    wrapForall (l:ls) nid = do
        newId <- createFreshNodeId
        let node = TyForall newId l nid
        registerNode newId node
        wrapForall ls newId

    -- Allow composing over an already-expanded node (not necessarily TyExp)
    applyExpansionOverNode :: Expansion -> TyNode -> NodeId -> PresolutionM NodeId
    applyExpansionOverNode ExpIdentity _ nid = return nid
    applyExpansionOverNode (ExpForall ls) _ nid = wrapForall (NE.toList ls) nid
    applyExpansionOverNode (ExpCompose es) _ nid = foldM (\n e -> do
                                                            nNode <- getCanonicalNode n
                                                            applyExpansionOverNode e nNode n) nid (NE.toList es)
    applyExpansionOverNode (ExpInstantiate args) node _ =
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
                            -- Create fresh node shell
                            freshId <- lift createFreshNodeId
                            modify $ IntMap.insert (getNodeId nid) freshId

                            -- Recursively copy children
                            newNode <- case node of
                                TyArrow { tnDom = d, tnCod = c } -> do
                                    d' <- copyNode subst d
                                    c' <- copyNode subst c
                                    return $ TyArrow freshId d' c'
                                TyForall { tnQuantLevel = q, tnBody = b } -> do
                                    b' <- copyNode subst b
                                    return $ TyForall freshId q b'
                                TyExp { tnExpVar = s, tnBody = b } -> do
                                    b' <- copyNode subst b
                                    return $ TyExp freshId s b'
                                TyVar { tnLevel = l } -> do
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
    return $ go nodes IntSet.empty [rootId]
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
        occurs12 <- occursIn root1 root2
        when occurs12 $ throwError $ OccursCheckPresolution root1 root2

        occurs21 <- occursIn root2 root1
        when occurs21 $ throwError $ OccursCheckPresolution root2 root1

        modify $ \st -> st { psUnionFind = IntMap.insert (getNodeId root1) root2 (psUnionFind st) }
        return ()

