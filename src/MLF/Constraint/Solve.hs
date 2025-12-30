{-# LANGUAGE LambdaCase #-}
{-|
Phase 5 — Global unification (Rémy–Yakobowski, TLDI 2007; ICFP 2008 §3/§5)

Consumes the pending monotype equalities @cUnifyEdges@ and merges the graphic
type DAG with a worklist union-find unifier:

1. Initialize the queue from @cUnifyEdges@.
2. Pop an edge, find canonical reps with path compression.
3. Cases: Var=Var union; Var=τ occurs-check then union; Arrow=Arrow union heads
    and enqueue dom/cod; Base=Base must match; Forall=Forall levels must match
    then enqueue bodies; TyExp is illegal here; otherwise constructor clash.
4. After the queue drains, rewrite the constraint to canonical reps so later
    phases see a normalized graph.

Subtle points (paper alignment / choices)
-----------------------------------------
* Occurs-check: The papers assume a well-scoped grafting discipline; we still
    enforce an explicit occurs-check when a var meets structure to prevent
    accidental self-cycles introduced by earlier merges.
* Forall = Forall: The paper’s “same level” requirement (§3, Fig. 5) is enforced
    strictly; we do not attempt sublevel unification here. Phase 4 must already
    align levels via expansions.
* TyExp presence: Expansions should be eliminated before Phase 5. Seeing a
    TyExp here signals a pipeline bug; we surface it as an error instead of
    silently collapsing.
* Representative choice: When collapsing duplicate node ids we prefer keeping a
    structured node over a var, so downstream phases can see concrete shapes for
    reconstruction (matches the “keep grafted structure” intuition from TLDI’07).
* Union-find heuristics: We only use path compression (no union-by-rank). The
    constraint sizes here are small; avoiding rank keeps the code simpler without
    observable asymptotic impact in practice.

We prefer keeping a structured node as the representative when collapsing
duplicates so downstream passes see concrete shapes instead of bare variables
whenever possible.

Note [Paper alignment: solveUnify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Paper touch points after re-reading the artifacts in `papers/`:
• ICFP’08 “Graphic Type Constraints…” (§4, “grafting and merging”) describes
    unification as graft/merge over graphic types; our worklist implements the
    same shape decomposition (Arrow→enqueue dom/cod, Forall→levels must match,
    Base→must agree) with an explicit occurs-check that the paper relies on earlier
    acyclicity for.
• The report “Recasting MLF” (RR-6228, Defs. for graphic constraints) assumes
    TyExp nodes are gone by this phase; we likewise surface TyExp as a hard error.
• Representative choice is left unspecified in the papers; we bias toward keeping
    structured reps (not vars) when collapsing duplicates so later phases can reuse
    grafted structure without re-traversing UF parents.

Note [Algorithm sketch: solveUnify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Input: constraint with pending `cUnifyEdges`; UF map initially empty (each node
is its own root). Output: UF plus constraint rewritten to canonical ids, with
unify queue drained.

Loop (worklist):
1. Pop next `UnifyEdge l r`; find canonical reps via `findRoot` (path compression).
2. If reps coincide → skip (already unified).
3. Fetch canonical nodes; inspect constructor pair:
     • Var/Var → union roots.
     • Var/Struct → occurs-check(var, struct); union var under struct.
     • Struct/Var → symmetric; occurs-check(var, struct); union var under struct.
     • Arrow/Arrow → union, then enqueue (dom, dom) and (cod, cod).
     • Base/Base → require same base else `BaseClash`.
     • Forall/Forall → require equal binder arity else `ForallArityMismatch`; union and enqueue bodies.
     • TyExp anywhere → `UnexpectedExpNode` (should not reach solve).
     • Anything else → `ConstructorClash`.
4. Repeat until queue empty.

Occurs-check details: graph reachability from `target` under current UF; if the
searched var root is found, raise `OccursCheckFailed`. This stays sound even after
prior unions because traversal uses canonical ids.

Rewriting: once the queue drains, rewrite all nodes/edges through UF via
`applyUFConstraint`, collapsing duplicates with a preference for structured reps
over vars. This exposes concrete structure to later phases and preserves sharing
via `IntMap.fromListWith choose`.

Note [Normalize vs Solve unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Phase 2 (`processUnifyEdges` in `MLF.Normalize`) vs Phase 5 (`processEdge` here):
• Goal: Normalize is a local simplifier; it tries easy merges and *keeps* unresolved
    edges for later. Solve is final and must succeed or raise a `SolveError`.
• Occurs-check: Normalize has none; Solve enforces an explicit occurs-check on
    Var=Structure (and symmetric Var paths), failing with `OccursCheckFailed`.
• Constructors: Normalize tolerates TyExp pairs (unifies bodies only if vars match)
    and preserves mismatches/arity clashes. Solve rejects TyExp outright and raises
    specific errors (`ConstructorClash`, `BaseClash`, `ForallArityMismatch`,
    `MissingNode`).
• Control flow: Normalize folds once over the list, appending decomposed edges.
    Solve uses a worklist queue, enqueuing dom/cod or forall bodies as it goes.
• Union-find: Normalize has no path compression and no structured-rep preference;
    it applies UF to the constraint once per normalize iteration. Solve compresses
    paths on find and rewrites the whole constraint at the end, preferring structured
    representatives over variables when collapsing duplicates.
-}
module MLF.Constraint.Solve (
    SolveError(..),
    SolveResult(..),
    solveUnify,
    validateSolvedGraph,
    validateSolvedGraphStrict,
    frWith
) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.State.Strict
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (mapMaybe)

import qualified MLF.Binding.Adjustment as BindingAdjustment
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Util.UnionFind as UnionFind
import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.Unify.Decompose as UnifyDecompose
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Types

-- | Errors that can arise during monotype unification.
data SolveError
    = MissingNode NodeId
    | ConstructorClash TyNode TyNode
    | BaseClash BaseTy BaseTy
    | ForallArityMismatch Int Int
    | OccursCheckFailed NodeId NodeId   -- ^ (var, target)
    | UnexpectedExpNode NodeId
    | BindingTreeError BindingError
    deriving (Eq, Show)

-- | Successful unification result.
data SolveResult = SolveResult
        { srConstraint :: Constraint        -- ^ Constraint rewritten to canonical node ids; unify edges drained.
        , srUnionFind :: IntMap NodeId      -- ^ Final union-find parent map.
        }
        deriving (Eq, Show)

{-
Note [SolveResult invariants]
-----------------------------
srConstraint is normalized with respect to the final UF:
    • All node ids/edges rewritten to canonical reps via applyUFConstraint
    • cUnifyEdges drained (should be empty on success)
    • Duplicate nodes collapsed, preferring structured reps over variables
    • cInstEdges also rewritten to reps (no stale ids)
srUnionFind is the parent map after solve; use `frWith`/`findRoot` to chase.

Note [Solved graph properties]
------------------------------
After solve succeeds, the constraint should satisfy:
    • No TyExp nodes remain; any survivor would have raised UnexpectedExpNode.
    • Instantiation is resolved: cInstEdges were consumed earlier (and rewritten);
        expansions were already materialized in presolution.
    • All unifications are discharged: cUnifyEdges is empty; equalities live in UF.
    • Canonical ids: every node/edge is rewritten through UF; duplicates collapsed
        with structured reps preferred over variables.
    • Constructor consistency: no arrow/base/forall clashes; forall arity aligned;
        base mismatches would have errored.
    • Occurs-safety: occurs-check in solve prevents a variable from being reachable
        from itself in the graph.
    • Sharing preserved: common substructure stays shared; distinct instantiations
        got fresh nodes as chosen in presolution.
-}

data SolveState = SolveState
    { suConstraint :: Constraint
    , suUnionFind :: IntMap NodeId
    , suQueue :: [UnifyEdge]
    }

type SolveM = StateT SolveState (Either SolveError)

-- | Drain all unification edges; assumes instantiation work was already done
-- by earlier phases. Returns the rewritten constraint and the final UF map.
-- See Note [Paper alignment: solveUnify] and Note [Algorithm sketch: solveUnify].
solveUnify :: Constraint -> Either SolveError SolveResult
solveUnify c0 = do
    case Binding.checkBindingTree c0 of
        Left err -> Left (BindingTreeError err)
        Right () -> do
            let st = SolveState { suConstraint = c0, suUnionFind = IntMap.empty, suQueue = cUnifyEdges c0 }
            final <- execStateT loop st
            let uf = suUnionFind final
                c' = applyUFConstraint uf (suConstraint final) { cUnifyEdges = [] }
            c'' <- case rewriteEliminatedBinders c' of
                Left err -> Left (BindingTreeError err)
                Right out -> Right out
            case Binding.checkBindingTree c'' of
                Left err -> Left (BindingTreeError err)
                Right () -> do
                    let res = SolveResult { srConstraint = c'', srUnionFind = uf }
                        violations = validateSolvedGraph res
                    if null violations
                        then pure res
                        else error ("validateSolvedGraph failed: " ++ unlines violations)
  where
    loop :: SolveM ()
    loop = do
        q <- gets suQueue
        case q of
            [] -> pure ()
            (e:rest) -> do
                modify' $ \s -> s { suQueue = rest }
                processEdge e
                loop

    enqueue :: [UnifyEdge] -> SolveM ()
    enqueue es = modify' $ \s ->
        s { suQueue = es ++ suQueue s }

    -- | Process one equality edge using canonical representatives, decomposing
    -- arrows/foralls as needed and performing occurs-checks for var = structure.
    processEdge :: UnifyEdge -> SolveM ()
    processEdge (UnifyEdge l r) = do
        lRoot <- findRoot l
        rRoot <- findRoot r
        if lRoot == rRoot
            then pure ()
            else do
                lNode <- lookupNode lRoot
                rNode <- lookupNode rRoot
                case (lNode, rNode) of
                    (TyVar{}, TyVar{}) -> do
                        -- Paper Raise(n) / binding-edge harmonization: keep scope well-formed
                        -- when merging variables from different binders.
                        cBefore <- gets suConstraint
                        case BindingAdjustment.harmonizeBindParentsWithTrace lRoot rRoot cBefore of
                            Left err -> lift $ Left (BindingTreeError err)
                            Right (c', _trace) -> modify' $ \s -> s { suConstraint = c' }
                        unionNodes lRoot rRoot

                    (TyVar{}, _) -> do
                        occursCheck lRoot rRoot
                        unionNodes lRoot rRoot

                    (_, TyVar{}) -> do
                        occursCheck rRoot lRoot
                        unionNodes rRoot lRoot

                    (TyForall { tnBody = b1 }, TyForall { tnBody = b2 }) -> do
                        cCur <- gets suConstraint
                        uf0 <- gets suUnionFind
                        let canonical = UnionFind.frWith uf0
                        case ( Binding.orderedBinders canonical cCur lRoot
                             , Binding.orderedBinders canonical cCur rRoot
                             ) of
                            (Right bs1, Right bs2)
                                | length bs1 == length bs2 -> do
                                    unionNodes lRoot rRoot
                                    enqueue [UnifyEdge b1 b2]
                                | otherwise ->
                                    lift $ Left (ForallArityMismatch (length bs1) (length bs2))
                            (Left err, _) -> lift $ Left (BindingTreeError err)
                            (_, Left err) -> lift $ Left (BindingTreeError err)

                    (TyExp{}, _) -> lift $ Left (UnexpectedExpNode lRoot)
                    (_, TyExp{}) -> lift $ Left (UnexpectedExpNode rRoot)

                    _ ->
                        case UnifyDecompose.decomposeUnifyChildren lNode rNode of
                            Right newEdges -> do
                                unionNodes lRoot rRoot
                                enqueue newEdges
                            Left (UnifyDecompose.MismatchBase b1 b2) ->
                                lift $ Left (BaseClash b1 b2)
                            Left _ ->
                                lift $ Left (ConstructorClash lNode rNode)

    -- | Lookup a node by id or fail with 'MissingNode'.
    lookupNode :: NodeId -> SolveM TyNode
    lookupNode nid = do
        nodes <- gets (cNodes . suConstraint)
        case IntMap.lookup (getNodeId nid) nodes of
            Just n -> pure n
            Nothing -> lift $ Left (MissingNode nid)

    -- Union-find helpers
    -- | Find canonical representative with path compression.
    findRoot :: NodeId -> SolveM NodeId
    findRoot nid = do
        uf <- gets suUnionFind
        let (root, uf') = UnionFind.findRootWithCompression uf nid
        modify' $ \s -> s { suUnionFind = uf' }
        pure root

    -- | Union two sets, pointing the first root at the second.
    unionNodes :: NodeId -> NodeId -> SolveM ()
    unionNodes a b = do
        aRoot <- findRoot a
        bRoot <- findRoot b
        when (aRoot /= bRoot) $ do
            cCur <- gets suConstraint
            let aElim = VarStore.isEliminatedVar cCur aRoot
                bElim = VarStore.isEliminatedVar cCur bRoot
                (fromRoot, toRoot) =
                    case (aElim, bElim) of
                        (False, True) -> (bRoot, aRoot)
                        _ -> (aRoot, bRoot)
            modify' $ \s ->
                s { suUnionFind = IntMap.insert (getNodeId fromRoot) toRoot (suUnionFind s) }

    -- | Reject unifying a variable with a structure that already contains it.
    -- Traverses under current UF representatives so it stays sound after merges.
    occursCheck :: NodeId -> NodeId -> SolveM ()
    occursCheck var target = do
        varRoot <- findRoot var
        targetRoot <- findRoot target
        when (varRoot == targetRoot) $ lift $ Left (OccursCheckFailed varRoot targetRoot)
        nodes <- gets (cNodes . suConstraint)
        uf <- gets suUnionFind
        let canonical = frWith uf
            lookupTyNode nid = IntMap.lookup (getNodeId nid) nodes
        case Traversal.occursInUnder canonical lookupTyNode varRoot targetRoot of
            Left _ -> lift $ Left (OccursCheckFailed varRoot targetRoot)
            Right True -> lift $ Left (OccursCheckFailed varRoot targetRoot)
            Right False -> pure ()

-- | Rewrite every node/edge to UF representatives and collapse duplicates,
-- preferring structured nodes when both sides share an id.
applyUFConstraint :: IntMap NodeId -> Constraint -> Constraint
applyUFConstraint uf c =
    let canonical = frWith uf
        nodes' = IntMap.fromListWith Canonicalize.chooseRepNode (map rewriteNode (IntMap.elems (cNodes c)))
        bindParents0 = cBindParents c
        bindParents' =
            Canonicalize.rewriteBindParentsLenient
                canonical
                (\childC -> IntMap.member (getNodeId childC) nodes')
                bindParents0
        eliminated' = rewriteEliminated canonical nodes' (cEliminatedVars c)
    in c
        { cNodes = nodes'
        , cInstEdges = Canonicalize.rewriteInstEdges canonical (cInstEdges c)
        , cUnifyEdges = Canonicalize.rewriteUnifyEdges canonical (cUnifyEdges c)
        , cBindParents = bindParents'
        , cEliminatedVars = eliminated'
        }
  where
    rewriteNode :: TyNode -> (Int, TyNode)
    rewriteNode n =
        let nid' = frWith uf (tnId n)
        in (getNodeId nid', case n of
            TyVar{ tnBound = mb } -> TyVar { tnId = nid', tnBound = fmap (frWith uf) mb }
            TyBottom{} -> TyBottom nid'
            TyArrow { tnDom = d, tnCod = cod } -> TyArrow nid' (frWith uf d) (frWith uf cod)
            TyBase { tnBase = b } -> TyBase nid' b
            TyForall { tnBody = b } -> TyForall nid' (frWith uf b)
            TyExp { tnExpVar = s, tnBody = b } -> TyExp nid' s (frWith uf b)
            TyRoot { tnChildren = cs } -> TyRoot nid' (map (frWith uf) cs)
            )

    rewriteEliminated :: (NodeId -> NodeId) -> IntMap TyNode -> EliminatedVars -> EliminatedVars
    rewriteEliminated canon nodes0 elims0 =
        IntSet.fromList
            [ getNodeId vC
            | vid <- IntSet.toList elims0
            , let vC = canon (NodeId vid)
            , IntMap.member (getNodeId vC) nodes0
            ]

rewriteEliminatedBinders :: Constraint -> Either BindingError Constraint
rewriteEliminatedBinders c0
    | IntSet.null elims0 = Right c0
    | otherwise = do
        let nodes0 = cNodes c0
            bindParents0 = cBindParents c0
            maxId = maxNodeIdKeyOr0 c0
            elimList = IntSet.toList elims0

        forM_ elimList $ \vid ->
            unless (IntMap.member vid nodes0) $
                Left $
                    InvalidBindingTree $
                        "rewriteEliminatedBinders: eliminated node " ++ show vid ++ " not in cNodes"

        let unbounded =
                [ vid
                | vid <- elimList
                , VarStore.lookupVarBound c0 (NodeId vid) == Nothing
                ]
            bottomIds = [NodeId i | i <- [maxId + 1 .. maxId + length unbounded]]
            bottomMap = IntMap.fromList (zip unbounded bottomIds)
            bottomNodes =
                IntMap.fromList
                    [ (getNodeId bid, TyBottom bid)
                    | bid <- bottomIds
                    ]
            bottomSet = IntSet.fromList (map getNodeId bottomIds)

            resolve :: IntSet.IntSet -> NodeId -> Either BindingError NodeId
            resolve seen nid
                | not (IntSet.member (getNodeId nid) elims0) =
                    if IntMap.member (getNodeId nid) nodes0
                        then Right nid
                        else Left $
                            InvalidBindingTree $
                                "rewriteEliminatedBinders: missing node " ++ show nid
                | IntSet.member (getNodeId nid) seen =
                    Left $
                        InvalidBindingTree "rewriteEliminatedBinders: cycle in eliminated-binder bounds"
                | otherwise =
                    case VarStore.lookupVarBound c0 nid of
                        Nothing ->
                            case IntMap.lookup (getNodeId nid) bottomMap of
                                Just b -> Right b
                                Nothing ->
                                    Left $
                                        InvalidBindingTree $
                                            "rewriteEliminatedBinders: missing bottom mapping for " ++ show nid
                        Just bnd ->
                            resolve (IntSet.insert (getNodeId nid) seen) bnd

        substPairs <- forM elimList $ \vid -> do
            rep <- resolve IntSet.empty (NodeId vid)
            pure (vid, rep)
        let subst = IntMap.fromList substPairs
            substNode nid = IntMap.findWithDefault nid (getNodeId nid) subst

            rewriteNode node = case node of
                TyVar{ tnId = nid, tnBound = mb } ->
                    let mb' =
                            case mb of
                                Nothing -> Nothing
                                Just b ->
                                    let b' = substNode b
                                    in if IntSet.member (getNodeId b') bottomSet
                                        then Nothing
                                        else Just b'
                    in TyVar { tnId = nid, tnBound = mb' }
                TyBottom{} -> node
                TyArrow { tnId = nid, tnDom = d, tnCod = cod } ->
                    TyArrow nid (substNode d) (substNode cod)
                TyBase{} -> node
                TyForall { tnId = nid, tnBody = b } ->
                    TyForall nid (substNode b)
                TyExp { tnId = nid, tnExpVar = s, tnBody = b } ->
                    TyExp nid s (substNode b)
                TyRoot { tnId = nid, tnChildren = cs } ->
                    TyRoot nid (map substNode cs)

            nodes1 =
                IntMap.fromList
                    [ (getNodeId (tnId n), rewriteNode n)
                    | n <- IntMap.elems nodes0
                    ]
            nodes' = IntMap.union nodes1 bottomNodes
            inNodes nid = IntMap.member (getNodeId nid) nodes'
        bindEntries <- forM (IntMap.toList bindParents0) $ \(childId, (parent0, flag)) ->
            if IntSet.member childId elims0
                then pure Nothing
                else if IntSet.member (getNodeId parent0) elims0
                    then Left $
                        InvalidBindingTree $
                            "rewriteEliminatedBinders: binding parent eliminated for child "
                                ++ show childId
                    else do
                        let parent' = substNode parent0
                        if parent' == NodeId childId
                            then pure Nothing
                            else if not (inNodes (NodeId childId))
                                then pure Nothing
                                else if not (inNodes parent')
                                    then Left $
                                        InvalidBindingTree $
                                            "rewriteEliminatedBinders: missing binding parent "
                                                ++ show parent'
                                                ++ " for child "
                                                ++ show childId
                                    else pure (Just (childId, (parent', flag)))

        let bottomParents =
                [ (getNodeId bid, (parent0, flag))
                | (vid, bid) <- zip unbounded bottomIds
                , Just (parent0, flag) <- [IntMap.lookup vid bindParents0]
                , not (IntSet.member (getNodeId parent0) elims0)
                , inNodes parent0
                ]
            bindParents' = IntMap.fromList (mapMaybe id bindEntries ++ bottomParents)

            instEdges' =
                [ InstEdge eid (substNode l) (substNode r)
                | InstEdge eid l r <- cInstEdges c0
                ]
            unifyEdges' =
                [ UnifyEdge (substNode l) (substNode r)
                | UnifyEdge l r <- cUnifyEdges c0
                ]

        pure c0
            { cNodes = nodes'
            , cBindParents = bindParents'
            , cInstEdges = instEdges'
            , cUnifyEdges = unifyEdges'
            , cEliminatedVars = IntSet.empty
            }
  where
    elims0 = cEliminatedVars c0

-- Note [frWith]
-- --------------
-- `frWith` is the canonicalization helper used during `applyUFConstraint` to
-- rewrite every node/edge through the union-find map. It intentionally performs
-- a read-only chase (no path compression) because `applyUFConstraint` builds a
-- new constraint immutably; compression would only mutate the temporary map and
-- is unnecessary for a single pass over the nodes/edges. All hot-path finds in
-- `solveUnify` use the compressed `findRoot` instead.
frWith :: IntMap NodeId -> NodeId -> NodeId
frWith = UnionFind.frWith

-- | Debug validator for Note [Solved graph properties]. Returns a list of
-- violation messages; the list is empty when the solved graph satisfies the
-- note's claims. Intended for assertions in tests or exploratory debugging,
-- not for production code paths.
validateSolvedGraph :: SolveResult -> [String]
validateSolvedGraph SolveResult { srConstraint = c, srUnionFind = uf } =
    validateWith ValidateOpts { voCheckInstEdges = False }
        SolveResult { srConstraint = c, srUnionFind = uf }

-- | Strict variant that enforces empty inst edges.
validateSolvedGraphStrict :: SolveResult -> [String]
validateSolvedGraphStrict = validateWith ValidateOpts { voCheckInstEdges = True }

data ValidateOpts = ValidateOpts
    { voCheckInstEdges :: Bool
    }

validateWith :: ValidateOpts -> SolveResult -> [String]
validateWith opts SolveResult { srConstraint = c, srUnionFind = uf } =
        concat
            [ tyExpViolations
        , instEdgeViolations
        , unifyEdgeViolations
            , canonicalViolations
            , childPresenceViolations
            , occursViolations
            ]
    where
        nodes = cNodes c
        canonical :: NodeId -> NodeId
        canonical = frWith uf

        childRefs :: TyNode -> [NodeId]
        childRefs = structuralChildren

        -- 1. No TyExp nodes remain.
        tyExpViolations =
                [ msg "Unexpected TyExp node" [nid]
                | TyExp { tnId = nid } <- IntMap.elems nodes
                ]

        -- 2. Instantiation resolved (optional in lax mode, required in strict).
        instEdgeViolations =
            [ msg "Residual instantiation edge" [instLeft e, instRight e]
            | voCheckInstEdges opts
            , e <- cInstEdges c
            ]

        -- 3. All unifications discharged (queue empty).
        unifyEdgeViolations =
                [ msg "Residual unification edge" [uniLeft e, uniRight e]
                | e <- cUnifyEdges c
                ]

        -- 4. Canonical ids: nodes/edges already rewritten through UF and keys match ids.
        canonicalViolations =
                concat
                        [ [ msg "Node key/id mismatch" [tnId n]
                            | n <- IntMap.elems nodes
                            , IntMap.lookup (getNodeId (tnId n)) nodes /= Just n
                            ]
                        , [ msg "Non-canonical node id" [tnId n]
                            | n <- IntMap.elems nodes
                            , let cid = canonical (tnId n)
                            , cid /= tnId n
                            ]
                        , [ msg "Non-canonical child id" [parent, child]
                            | n <- IntMap.elems nodes
                            , parent <- [tnId n]
                            , child <- childRefs n
                            , canonical child /= child
                            ]
                        , [ msg "Non-canonical inst edge" [instLeft e, instRight e]
                            | e <- cInstEdges c
                            , canonical (instLeft e) /= instLeft e || canonical (instRight e) /= instRight e
                            ]
                        , [ msg "Non-canonical unify edge" [uniLeft e, uniRight e]
                            | e <- cUnifyEdges c
                            , canonical (uniLeft e) /= uniLeft e || canonical (uniRight e) /= uniRight e
                            ]
                        ]

        -- 5. Child references must point to existing nodes.
        childPresenceViolations =
                [ msg "Missing child node" [tnId n, child]
                | n <- IntMap.elems nodes
                , child <- childRefs n
                , IntMap.notMember (getNodeId child) nodes
                ]

        -- 6. Occurs-safety: no variable is reachable from itself via structure edges.
        occursViolations =
                [ msg "Occurs-check violation" [varId]
                | var <- [ n | n@TyVar{} <- IntMap.elems nodes ]
                , let varId = tnId var
                , reachesSelf varId
                ]

        reachesSelf :: NodeId -> Bool
        reachesSelf start = go IntSet.empty start
            where
                go seen nid
                    | nid == start && not (IntSet.null seen) = True
                    | IntSet.member (getNodeId nid) seen = False
                    | otherwise = case IntMap.lookup (getNodeId nid) nodes of
                        Nothing -> False
                        Just node ->
                            let seen' = IntSet.insert (getNodeId nid) seen
                                next = map canonical (childRefs node)
                            in any (go seen') next

        msg :: Show a => String -> [a] -> String
        msg label payload = label ++ ": " ++ unwords (map show payload)
