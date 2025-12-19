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
     • Forall/Forall → require equal quant levels else `ForallLevelMismatch`; union and enqueue bodies.
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
    and preserves mismatches/level clashes. Solve rejects TyExp outright and raises
    specific errors (`ConstructorClash`, `BaseClash`, `ForallLevelMismatch`,
    `MissingNode`).
• Control flow: Normalize folds once over the list, appending decomposed edges.
    Solve uses a worklist queue, enqueuing dom/cod or forall bodies as it goes.
• Union-find: Normalize has no path compression and no structured-rep preference;
    it applies UF to the constraint once per normalize iteration. Solve compresses
    paths on find and rewrites the whole constraint at the end, preferring structured
    representatives over variables when collapsing duplicates.
-}
module MLF.Solve (
    SolveError(..),
    SolveResult(..),
    solveUnify,
    validateSolvedGraph,
    validateSolvedGraphStrict,
    frWith
) where

import Control.Monad (foldM, when)
import Control.Monad.State.Strict
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.BindingAdjustment as BindingAdjustment
import qualified MLF.Binding as Binding
import qualified MLF.UnionFind as UnionFind
import MLF.Types

-- | Errors that can arise during monotype unification.
data SolveError
    = MissingNode NodeId
    | ConstructorClash TyNode TyNode
    | BaseClash BaseTy BaseTy
    | ForallLevelMismatch GNodeId GNodeId
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
    • Constructor consistency: no arrow/base/forall clashes; forall levels aligned;
        base mismatches would have errored.
    • Occurs-safety: occurs-check in solve prevents a variable from being reachable
        from itself in the graph.
    • Sharing preserved: common substructure stays shared; distinct instantiations
        got fresh nodes as chosen in presolution.
    • G-node invariant unchanged: levels remain in cGNodes/cGForest; type nodes
        only reference levels via level fields (tnVarLevel/tnOwnerLevel/tnQuantLevel),
        never embed them.
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
            case Binding.checkBindingTree c' of
                Left err -> Left (BindingTreeError err)
                Right () -> do
                    let res = SolveResult { srConstraint = c', srUnionFind = uf }
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
    enqueue es = modify' $ \s -> s { suQueue = es ++ suQueue s }

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

                    (TyArrow { tnDom = d1, tnCod = c1 }, TyArrow { tnDom = d2, tnCod = c2 }) -> do
                        unionNodes lRoot rRoot
                        enqueue [UnifyEdge d1 d2, UnifyEdge c1 c2]

                    (TyBase { tnBase = b1 }, TyBase { tnBase = b2 }) ->
                        if b1 == b2 then unionNodes lRoot rRoot else lift $ Left (BaseClash b1 b2)

                    (TyForall { tnQuantLevel = l1, tnBody = b1 }, TyForall { tnQuantLevel = l2, tnBody = b2 }) ->
                        if l1 == l2
                            then do
                                unionNodes lRoot rRoot
                                enqueue [UnifyEdge b1 b2]
                            else lift $ Left (ForallLevelMismatch l1 l2)

                    (TyExp{}, _) -> lift $ Left (UnexpectedExpNode lRoot)
                    (_, TyExp{}) -> lift $ Left (UnexpectedExpNode rRoot)

                    _ -> lift $ Left (ConstructorClash lNode rNode)

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
        when (aRoot /= bRoot) $
            modify' $ \s -> s { suUnionFind = IntMap.insert (getNodeId aRoot) bRoot (suUnionFind s) }

    -- | Reject unifying a variable with a structure that already contains it.
    -- Traverses under current UF representatives so it stays sound after merges.
    occursCheck :: NodeId -> NodeId -> SolveM ()
    occursCheck var target = do
        varRoot <- findRoot var
        targetRoot <- findRoot target
        when (varRoot == targetRoot) $ lift $ Left (OccursCheckFailed varRoot targetRoot)
        nodes <- gets (cNodes . suConstraint)
        uf <- gets suUnionFind
        let canonical nid = frWith uf nid
            go visited nid =
                let cnid = canonical nid
                in if IntSet.member (getNodeId cnid) visited
                    then Right visited
                    else do
                        when (cnid == varRoot) $ Left ()
                        n <- maybe (Left ()) Right (IntMap.lookup (getNodeId cnid) nodes)
                        let children = case n of
                                TyArrow { tnDom = d, tnCod = cod } -> [d, cod]
                                TyForall { tnBody = b } -> [b]
                                TyExp { tnBody = b } -> [b]
                                _ -> []
                        foldM go (IntSet.insert (getNodeId cnid) visited) children
        case go IntSet.empty targetRoot of
            Left _ -> lift $ Left (OccursCheckFailed varRoot targetRoot)
            Right _ -> pure ()

-- | Rewrite every node/edge to UF representatives and collapse duplicates,
-- preferring structured nodes when both sides share an id.
applyUFConstraint :: IntMap NodeId -> Constraint -> Constraint
applyUFConstraint uf c =
    let nodes' = IntMap.fromListWith choose (map rewriteNode (IntMap.elems (cNodes c)))
        bindParents0 = cBindParents c
        bindParents' =
            let entries0 =
                    [ (getNodeId child', (parent', flag))
                    | (childId, (parent0, flag)) <- IntMap.toList bindParents0
                    , let child' = frWith uf (NodeId childId)
                    , let parent' = frWith uf parent0
                    , child' /= parent'
                    ]
                entries =
                    [ (childId, parentInfo)
                    | (childId, parentInfo) <- entries0
                    , IntMap.member childId nodes'
                    ]
                combine (pNew, fNew) (pOld, fOld)
                    | pNew == pOld = (pOld, max fNew fOld)
                    | otherwise = (pOld, max fNew fOld)
            in IntMap.fromListWith combine entries
    in c
        { cNodes = nodes'
        , cInstEdges = map (rewriteInst uf) (cInstEdges c)
        , cUnifyEdges = map (rewriteUnify uf) (cUnifyEdges c)
        , cBindParents = bindParents'
        }
  where
    choose new old = case (isVar old, isVar new) of
        (True, False) -> new
        (False, True) -> old
        (True, True) -> old -- Tie-break vars (arbitrary but deterministic)
        (False, False) -> old -- Tie-break structures (arbitrary)

    isVar TyVar{} = True
    isVar _ = False

    rewriteNode :: TyNode -> (Int, TyNode)
    rewriteNode n =
        let nid' = frWith uf (tnId n)
        in (getNodeId nid', case n of
            TyVar { tnVarLevel = l } -> TyVar nid' l
            TyArrow { tnDom = d, tnCod = cod } -> TyArrow nid' (frWith uf d) (frWith uf cod)
            TyBase { tnBase = b } -> TyBase nid' b
            TyForall { tnOwnerLevel = ownerLvl, tnQuantLevel = quantLvl, tnBody = b } ->
                TyForall nid' quantLvl ownerLvl (frWith uf b)
            TyExp { tnExpVar = s, tnBody = b } -> TyExp nid' s (frWith uf b)
            )

    rewriteInst :: IntMap NodeId -> InstEdge -> InstEdge
    rewriteInst m (InstEdge eid l r) = InstEdge eid (frWith m l) (frWith m r)

    rewriteUnify :: IntMap NodeId -> UnifyEdge -> UnifyEdge
    rewriteUnify m (UnifyEdge l r) = UnifyEdge (frWith m l) (frWith m r)

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
    validateWith ValidateOpts { voCheckInstEdges = False, voRequireGNodes = not (IntMap.null (cGNodes c)) }
        SolveResult { srConstraint = c, srUnionFind = uf }

-- | Strict variant that enforces empty inst edges and requires G-node presence.
validateSolvedGraphStrict :: SolveResult -> [String]
validateSolvedGraphStrict = validateWith ValidateOpts { voCheckInstEdges = True, voRequireGNodes = True }

data ValidateOpts = ValidateOpts
    { voCheckInstEdges :: Bool
    , voRequireGNodes :: Bool
    }

validateWith :: ValidateOpts -> SolveResult -> [String]
validateWith opts SolveResult { srConstraint = c, srUnionFind = uf } =
        concat
            [ tyExpViolations
        , instEdgeViolations
        , unifyEdgeViolations
            , canonicalViolations
            , childPresenceViolations
            , forallLevelViolations
            , occursViolations
            ]
    where
        nodes = cNodes c
        canonical :: NodeId -> NodeId
        canonical = frWith uf

        childRefs :: TyNode -> [NodeId]
        childRefs TyArrow { tnDom = d, tnCod = cod } = [d, cod]
        childRefs TyForall { tnBody = b } = [b]
        childRefs TyExp { tnBody = b } = [b]
        childRefs _ = []

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

        -- 6. All level values must exist in cGNodes when required.
        -- Only TyVar and TyForall carry level fields.
        gnodes = cGNodes c
        forallLevelViolations =
            if not (voRequireGNodes opts)
                then []
                else
                    [ msg "Missing GNode for level" [GNodeId (getGNodeId lvl)]
                    | n <- IntMap.elems nodes
                    , lvl <- case n of
                        TyVar { tnVarLevel = l } -> [l]
                        TyForall { tnOwnerLevel = ownerLvl, tnQuantLevel = quantLvl } -> [ownerLvl, quantLvl]
                        _ -> []
                    , IntMap.notMember (getGNodeId lvl) gnodes
                    ]

        -- 7. Occurs-safety: no variable is reachable from itself via structure edges.
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
