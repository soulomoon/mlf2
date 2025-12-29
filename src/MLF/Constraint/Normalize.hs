{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Normalize
Description : Phase 2 - Local constraint transformations
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the normalization phase of MLF type inference,
which applies semantics-preserving local transformations to put constraints
into "locally-solved form."

Primary references:
  * Rémy & Yakobowski, "Graphic Type Constraints and Efficient Type
    Inference: from ML to MLF" (ICFP 2008) - §4 "Solving constraints locally"
  * Rémy & Yakobowski, "A graphical representation of MLF types with a
    linear-time incremental unification algorithm" (TLDI 2007) - §3

Related work (for later phases):
  * Rémy & Yakobowski, "A Church-Style Intermediate Language for MLF"
    (FLOPS 2010) - describes xMLF elaboration (Phase 6)
  * Yakobowski, PhD thesis (2008) - comprehensive treatment
-}
module MLF.Constraint.Normalize (
    normalize,
    NormalizeState (..),
    -- * Individual transformation rules (exported for testing)
    dropReflexiveInstEdges,
    dropReflexiveUnifyEdges,
    graftInstEdges,
    mergeUnifyEdges
) where

import Control.Monad (when, foldM)
import Control.Monad.State.Strict
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Adjustment as BindingAdjustment
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Util.UnionFind as UnionFind
import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.Unify.Decompose as UnifyDecompose
import MLF.Constraint.Types (NodeId(..), TyNode(..), Constraint(..), InstEdge(..), UnifyEdge(..), BindFlag(..), maxNodeIdKeyOr0, lookupNodeIn)

{- Note [Normalization / Local Transformations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Phase 2 applies semantics-preserving rewrite rules to the constraint graph
until a fixed point is reached.

Paper reference: Rémy & Yakobowski, "Graphic Type Constraints and Efficient
Type Inference: from ML to MLF" (ICFP 2008), §4 "Solving constraints locally".

The paper describes several "local transformations" (§4, p.8):

  1. Drop reflexive edges: T ≤ T and T = T contribute no information.
     (Implicit in the paper, stated explicitly here.)

  2. Grafting: when an instantiation edge has a variable on the left and
     structure on the right, copy that structure onto the variable.
     (§4, "grafting" - copies the structure of τ onto α when α ≤ τ.)

  3. Merging: process unification edges by merging nodes via union-find.
     (§4, implicit in unification handling; explicit in TLDI'07 paper.)

  4. Collapse identity expansions: when s · τ must match τ exactly.
     (§4, "collapsing identity expansions" - requires presolution.)

  5. Binding-tree hygiene: the paper’s “push/pull binding nodes” is implicit
     in our representation (binding edges live outside the term DAG). We only
     maintain a valid binding tree when nodes are merged (paper Raise(n)).

We iterate these rules until no changes occur. The paper calls this
"putting constraints in locally-solved form" which is a prerequisite
for the global solving phases (acyclicity checking and presolution).

See also:
  - Yakobowski's PhD thesis (2008), Chapter 4 for extended treatment
  - TLDI'07 paper for the graphical unification algorithm
-}

-- | State maintained during normalization.
data NormalizeState = NormalizeState
    { nsNextNodeId :: !Int
      -- ^ Counter for allocating fresh nodes during grafting.
    , nsUnionFind :: !(IntMap NodeId)
      -- ^ Union-find structure: maps node IDs to their canonical representative.
    , nsConstraint :: !Constraint
      -- ^ The constraint being normalized.
    }
    deriving (Eq, Show)

type NormalizeM a = State NormalizeState a

-- | Apply all normalization rules until a fixed point is reached.
normalize :: Constraint -> Constraint
normalize c = nsConstraint finalState
  where
    initialState = NormalizeState
        { nsNextNodeId = maxNodeIdKeyOr0 c + 1
        , nsUnionFind = IntMap.empty
        , nsConstraint = c
        }
    finalState = execState normalizeLoop initialState

-- | Main normalization loop: apply transformations until fixed point.
normalizeLoop :: NormalizeM ()
normalizeLoop = do
    before <- gets nsConstraint

    -- Apply transformations in order
    modify' $ \s -> s { nsConstraint = dropReflexiveInstEdges (nsConstraint s) }
    modify' $ \s -> s { nsConstraint = dropReflexiveUnifyEdges (nsConstraint s) }
    graftInstEdges
    mergeUnifyEdges
    applyUnionFindToConstraint

    after <- gets nsConstraint
    -- Continue if something changed
    when (before /= after) normalizeLoop

-- | Remove instantiation edges where left == right (T ≤ T).
dropReflexiveInstEdges :: Constraint -> Constraint
dropReflexiveInstEdges c = c { cInstEdges = filter (not . isReflexive) (cInstEdges c) }
  where
    isReflexive edge = instLeft edge == instRight edge

-- | Remove unification edges where left == right (T = T).
dropReflexiveUnifyEdges :: Constraint -> Constraint
dropReflexiveUnifyEdges c = c { cUnifyEdges = filter (not . isReflexive) (cUnifyEdges c) }
  where
    isReflexive edge = uniLeft edge == uniRight edge

{- Note [Grafting]
~~~~~~~~~~~~~~~~~~
Grafting is one of the core semantic-preserving transformations described in
Rémy & Yakobowski (ICFP 2008) §4, under "Local transformations."

Paper terminology: "We say that τ is grafted onto α in the constraint
C[α ≤ τ] when C[α = τ] is formed by replacing α ≤ τ with α = τ."

More precisely (from the paper, §4 p.8):
  "When the type τ is rigid, i.e. when it is known from the constraint
   that τ begins with a type constructor κ, then the variable α must
   also begin with κ. This constraint can be propagated as follows:
   we graft a copy of τ's structure onto α."

The key insight is that an instantiation constraint α ≤ τ where τ has
constructor structure (arrow, base type) forces α to have that same structure.
Rather than keeping this as an instantiation edge, we "graft" the structure
onto α by creating a copy with fresh variables, then unifying.

Example:   α ≤ (Int → β)

Step-by-step grafting (paper §4):
  1. Create fresh variable nodes α₁, α₂ at α's binding level
  2. Create a fresh arrow node (α₁ → α₂)
  3. Emit unification edge: α = (α₁ → α₂)
  4. Emit unification edges: α₁ = Int, α₂ = β
  5. Remove the original instantiation edge

Constraint transformation:

    Before:  α ≤ (Int → β)               -- instantiation edge

    After:   α = (α₁ → α₂)               -- α must be an arrow
             α₁ = Int                    -- domain matches Int
             α₂ = β                      -- codomain matches β

After merging (union-find) resolves these unification edges:

    α  ↦  (Int → β)                      -- α is now (Int → β)

Why fresh variables? We create α₁, α₂ rather than directly unifying α with
(Int → β) because:
  1. The right-hand side nodes may be shared with other constraints
  2. Fresh nodes at α's level preserve correct scoping for generalization
  3. It keeps the graph structure clean (α points to its own arrow node)

The paper notes that grafting must respect binding levels: fresh nodes are
created at the same level as α to preserve the scoping discipline.

See also:
  - Note [Grafting Cases] for the complete case analysis
  - Yakobowski PhD thesis §4.4 for extended discussion of grafting
  - TLDI'07 paper §3.2 for the underlying graphical unification
-}

-- | Process instantiation edges by grafting structure onto variables.
graftInstEdges :: NormalizeM ()
graftInstEdges = do
    c <- gets nsConstraint
    let edges = cInstEdges c
        nodes = cNodes c

    -- Partition edges into those we can graft and those we keep
    (toGraft, toKeep) <- partitionGraftable edges nodes

    -- Process graftable edges, collecting unify edges and type errors
    results <- mapM graftEdge toGraft
    let (errors, successes) = partitionResults (zip toGraft results)
        newUnifyEdges = concat successes

    -- Update constraint: keep non-graftable edges + error edges
    modify' $ \s ->
        let c' = nsConstraint s
        in s { nsConstraint = c'
                 { cInstEdges = toKeep ++ errors
                 , cUnifyEdges = cUnifyEdges c' ++ newUnifyEdges
                 }
             }
  where
    -- Separate type errors (Nothing) from successful grafts (Just edges)
    partitionResults :: [(InstEdge, Maybe [UnifyEdge])] -> ([InstEdge], [[UnifyEdge]])
    partitionResults = foldr go ([], [])
      where
        go (edge, Nothing) (errs, succs) = (edge : errs, succs)
        go (_, Just unifyEdges) (errs, succs) = (errs, unifyEdges : succs)

-- | Partition edges into (graftable, non-graftable).
-- An edge is graftable if we can structurally transform it without needing
-- presolution decisions. See Note [Grafting Cases].
partitionGraftable :: [InstEdge] -> IntMap TyNode -> NormalizeM ([InstEdge], [InstEdge])
partitionGraftable edges nodes = do
    uf <- gets nsUnionFind
    let isGraftable edge =
            let leftId = findRoot uf (instLeft edge)
                rightId = findRoot uf (instRight edge)
                leftNode = IntMap.lookup (getNodeId leftId) nodes
                rightNode = IntMap.lookup (getNodeId rightId) nodes
            in case (leftNode, rightNode) of
                -- Variable ≤ Structure: graft the structure
                (Just TyVar {}, Just TyArrow {}) -> True
                (Just TyVar {}, Just TyBase {}) -> True
                (Just TyVar {}, Just TyBottom {}) -> True
                -- Structure ≤ Structure: decompose or check compatibility
                (Just TyArrow {}, Just TyArrow {}) -> True
                (Just TyBase {}, Just TyBase {}) -> True
                (Just TyBottom {}, Just TyBottom {}) -> True
                -- Incompatible structures: type error, but we process them
                (Just TyArrow {}, Just TyBase {}) -> True
                (Just TyBase {}, Just TyArrow {}) -> True
                (Just TyArrow {}, Just TyBottom {}) -> True
                (Just TyBottom {}, Just TyArrow {}) -> True
                (Just TyBase {}, Just TyBottom {}) -> True
                (Just TyBottom {}, Just TyBase {}) -> True
                -- Variable ≤ Variable: can't graft, keep for Phase 4
                -- TyForall/TyExp cases: require presolution, keep for Phase 4
                _ -> False
    pure (filter isGraftable edges, filter (not . isGraftable) edges)

{- Note [Grafting Cases]
~~~~~~~~~~~~~~~~~~~~~~~~
Grafting transforms instantiation edges (≤) into unification edges (=) by
copying constructor structure. This is part of the normalization phase
(ICFP'08 §4) that puts constraints into "locally-solved form."

Paper reference (§4, p.8): "The type τ is rigid, i.e. [...] begins with
a type constructor κ, then the variable α must also begin with κ."

Cases handled during normalization (Phase 2):
──────────────────────────────────────────────
  Var ≤ Arrow    → Graft: rewrite α into (α₁ → α₂), then unify α₁/α₂ with
                   the arrow's components (core grafting operation).

  Var ≤ Base     → Unify variable with base type directly
                   (Degenerate case: base types have no substructure.)

  Arrow ≤ Arrow  → Decompose: emit (dom₁ = dom₂) and (cod₁ = cod₂)
                   (Standard unification of constructors with same head.)

  Base ≤ Base    → If same, satisfied; if different, type error
                   (Nullary constructors unify only with themselves.)

  Arrow ≤ Base   → Type error (incompatible constructors)
  Base ≤ Arrow   → Type error (incompatible constructors)
                   (Paper §5: "type error detection" during constraint
                    solving identifies these clashes.)

Cases requiring Phase 4 (presolution):
──────────────────────────────────────
The paper (§4.3) discusses these cases under "expansion variables":

  Var ≤ Forall   → The ∀ introduces a polymorphic type. We must decide
                   whether to instantiate it (choose s := inst) or keep
                   the polymorphism (s := ∀). This is the "presolution"
                   phase described in §4.3 "Computing the presolution."

  Var ≤ Exp      → Expansion nodes (s · τ) are placeholders for
                   "expansion variables" (§4.1). Their meaning is
                   determined by presolution. We can't graft until we
                   know what expansion s represents.

  Forall ≤ *     → A polymorphic type on the left must be instantiated.
                   The presolution decides how deeply to instantiate.
                   (Paper: "each use may require a different expansion".)

  Exp ≤ *        → Expansion on the left: presolution must resolve s.
                   (The expansion variable s is constrained by both
                    the left and right context.)

  Var ≤ Var      → Neither side has structure yet. The instantiation
                   relationship is kept for Phase 4, which builds the
                   "instance graph" (§4.3) to compute minimal expansions.

These unresolved edges are kept as InstEdges for Phase 4 to process
after building the dependency graph and computing minimal expansions.

Implementation note: Type errors are kept as InstEdges rather than
raising an exception, allowing us to collect multiple errors or defer
error reporting to a later phase with better diagnostics.
-}

-- | Graft a single edge, returning new unification edges.
-- Returns Nothing if the edge represents a type error that should be kept.
graftEdge :: InstEdge -> NormalizeM (Maybe [UnifyEdge])
graftEdge edge = do
    c <- gets nsConstraint
    uf <- gets nsUnionFind
    let nodes = cNodes c
        leftId = findRoot uf (instLeft edge)
        rightId = findRoot uf (instRight edge)
        leftNode = IntMap.lookup (getNodeId leftId) nodes
        rightNode = IntMap.lookup (getNodeId rightId) nodes

    case (leftNode, rightNode) of
        -- Variable ≤ Arrow: graft arrow structure onto the variable node
        (Just TyVar {}, Just (TyArrow { tnDom = rDom, tnCod = rCod }))
          | occursIn nodes uf leftId rightId -> pure Nothing
          | otherwise -> do
            domVar <- freshVar
            codVar <- freshVar
            -- Rewrite the variable node in-place to an arrow node.
            insertNode TyArrow { tnId = leftId, tnDom = domVar, tnCod = codVar }
            -- Bind fresh children to the new arrow node.
            setBindParentNorm domVar leftId BindFlex
            setBindParentNorm codVar leftId BindFlex
            pure $
                Just
                    [ UnifyEdge domVar (findRoot uf rDom)
                    , UnifyEdge codVar (findRoot uf rCod)
                    ]

        -- Variable ≤ Base: unify directly
        (Just TyVar {}, Just TyBase {}) ->
            pure $ Just [UnifyEdge leftId rightId]

        -- Variable ≤ Bottom: unify directly
        (Just TyVar {}, Just TyBottom {}) ->
            pure $ Just [UnifyEdge leftId rightId]

        -- Arrow ≤ Arrow: decompose into unification of components
        (Just (TyArrow { tnDom = lDom, tnCod = lCod }),
         Just (TyArrow { tnDom = rDom, tnCod = rCod })) ->
            pure $ Just [ UnifyEdge (findRoot uf lDom) (findRoot uf rDom)
                        , UnifyEdge (findRoot uf lCod) (findRoot uf rCod)
                        ]

        -- Base ≤ Base: check equality
        (Just (TyBase { tnBase = lBase }), Just (TyBase { tnBase = rBase }))
            | lBase == rBase -> pure $ Just []  -- Same type, trivially satisfied
            | otherwise -> pure Nothing  -- Type error: keep edge to report later

        -- Bottom ≤ Bottom: trivially satisfied
        (Just TyBottom {}, Just TyBottom {}) ->
            pure $ Just []

        -- Arrow ≤ Base or Base ≤ Arrow: type error (incompatible structures)
        (Just TyArrow {}, Just TyBase {}) -> pure Nothing
        (Just TyBase {}, Just TyArrow {}) -> pure Nothing
        (Just TyArrow {}, Just TyBottom {}) -> pure Nothing
        (Just TyBottom {}, Just TyArrow {}) -> pure Nothing
        (Just TyBase {}, Just TyBottom {}) -> pure Nothing
        (Just TyBottom {}, Just TyBase {}) -> pure Nothing

        -- Other cases: shouldn't reach here (filtered by partitionGraftable)
        _ -> pure $ Just []

-- | Check whether target (under UF reps) contains the variable.
occursIn :: IntMap TyNode -> IntMap NodeId -> NodeId -> NodeId -> Bool
occursIn nodes uf var target =
    case Traversal.occursInUnder (findRoot uf) (lookupNodeIn nodes) var target of
        Left _ -> False
        Right ok -> ok

-- | Allocate a fresh variable node.
freshVar :: NormalizeM NodeId
freshVar = do
    nid <- freshNodeId
    let node = TyVar { tnId = nid }
    insertNode node
    -- Note: binding parent is set by the caller when needed.
    pure nid

-- | Set the binding parent for a node in the constraint.
setBindParentNorm :: NodeId -> NodeId -> BindFlag -> NormalizeM ()
setBindParentNorm child parent flag =
    when (child /= parent) $
        modify' $ \s ->
            let c = nsConstraint s
                bp = cBindParents c
                bp' = IntMap.insert (getNodeId child) (parent, flag) bp
            in s { nsConstraint = c { cBindParents = bp' } }

-- | Get a fresh NodeId.
freshNodeId :: NormalizeM NodeId
freshNodeId = do
    n <- gets nsNextNodeId
    modify' $ \s -> s { nsNextNodeId = n + 1 }
    pure (NodeId n)

-- | Insert a node into the constraint.
insertNode :: TyNode -> NormalizeM ()
insertNode node = modify' $ \s ->
    let c = nsConstraint s
        nodes' = IntMap.insert (getNodeId (tnId node)) node (cNodes c)
    in s { nsConstraint = c { cNodes = nodes' } }

{- Note [Merging]
~~~~~~~~~~~~~~~~~
Merging processes unification edges (=) by maintaining a union-find structure.
This is the core of the "graphical unification" algorithm for MLF types.

Paper references:
  * Rémy & Yakobowski (TLDI 2007), "A graphical representation of MLF types
    with a linear-time incremental unification algorithm" — §3 describes the
    incremental unification algorithm on graphic types.
  * Rémy & Yakobowski (ICFP 2008) §4 — refers to merging as part of the
    local transformations that put constraints into "locally-solved form."

Algorithm (from TLDI'07 §3):
  1. Find the canonical representatives of both sides using path compression.
  2. If they're the same node, the edge is redundant (already unified).
  3. If one is a variable (⊥ node) and one has structure, point the variable
     to the structured node. The paper calls this "grafting the structure."
  4. If both have structure with the same head constructor (e.g., both →),
     merge them and recursively unify their children.
  5. If both have structure with different head constructors, this is a
     type error (constructor clash).

The TLDI'07 paper proves this algorithm is:
  * Linear time: O(n · α(n)) where α is the inverse Ackermann function,
    using union-find with path compression and union-by-rank.
  * Incremental: new unification edges can be added at any time without
    reprocessing the entire constraint.
  * Sound and complete for graphic type unification.

Representative selection (from the paper):
  "We always prefer structured nodes as representatives over variables."
  This ensures that looking up a type gives the most informative view.
  In our implementation, when unifying Var with Structure, we do:
      unionNodes varId structureId  -- var points to structure

Occurs check:
  The paper notes that occurs check is handled structurally by the graph
  representation. Since nodes are shared (not copied), a cycle in the
  graph directly represents an infinite type. The algorithm detects this
  when it would create a self-loop during unification.

Implementation note:
  We process unification edges iteratively. When unifying two arrows,
  we generate new UnifyEdges for their components and add them to the
  worklist. This continues until all edges are processed or an error
  is detected.

The union-find is applied to the constraint at the end of each iteration
to update all node references to their canonical representatives.

Current implementation limitations:
  * No path compression: `findRoot` traverses without updating parent
    pointers. This is O(depth) per find rather than amortized O(α(n)).
    Could be added later for performance if needed.
  * No explicit occurs check: We rely on the graph structure to prevent
    infinite types. Self-loops are caught by `left == right` check, but
    cycles through arrow children would show as divergence. A proper
    occurs check could be added for better error messages.

See also:
  - Note [Grafting] for how InstEdges generate UnifyEdges
  - Yakobowski PhD thesis Chapter 3 for the full formal treatment
-}

-- | Process unification edges using union-find.
mergeUnifyEdges :: NormalizeM ()
mergeUnifyEdges = do
    c <- gets nsConstraint
    let edges = cUnifyEdges c
    -- Process each edge
    remainingEdges <- processUnifyEdges edges

    -- Update constraint with remaining edges
    modify' $ \s ->
        let c' = nsConstraint s
        in s { nsConstraint = c' { cUnifyEdges = remainingEdges } }

-- | Process unification edges, returning any that couldn't be resolved.
processUnifyEdges :: [UnifyEdge] -> NormalizeM [UnifyEdge]
processUnifyEdges = foldM processOne []
  where
    -- Handles Var=?, Arrow=Arrow, Base=Base (same head), Forall=Forall with
    -- matching levels, and Exp=Exp with matching expansion vars. Any other
    -- pairing (missing nodes, constructor clash, mismatched levels/exp vars)
    -- is preserved in the accumulator so later phases can surface an error.
    processOne acc edge = do
        uf <- gets nsUnionFind
        c <- gets nsConstraint
        let nodes = cNodes c
            left = findRoot uf (uniLeft edge)
            right = findRoot uf (uniRight edge)

        if left == right
            then pure acc  -- Already unified
            else do
                let leftNode = IntMap.lookup (getNodeId left) nodes
                    rightNode = IntMap.lookup (getNodeId right) nodes
                case (leftNode, rightNode) of
                    -- TyExp vs TyVar:
                    --
                    -- Prefer keeping the *structured* node (`TyExp`) as representative so we
                    -- don't lose information (notably: λ-parameters are referenced through
                    -- fresh `TyExp` nodes created by `EVar`, and application relies on
                    -- propagating those wrappers through unification).
                    --
                    -- However, if the expansion node's body is (up to UF) the variable itself,
                    -- then unifying Var ~ (s·Var) would create a self-loop if we made Var point
                    -- to the `TyExp`. In that special case, we collapse the wrapper by making
                    -- the `TyExp` point to the variable (this corresponds to forcing `s` to be
                    -- the identity expansion).
                    (Just TyVar {}, Just (TyExp { tnBody = b })) -> do
                        let bRoot = findRoot uf b
                        if bRoot == left
                            then unionNodes right left   -- collapse (s·Var) ~ Var
                            else unionNodes left right   -- Var points to TyExp (keep structure)
                        pure acc
                    (Just (TyExp { tnBody = b }), Just TyVar {}) -> do
                        let bRoot = findRoot uf b
                        if bRoot == right
                            then unionNodes left right   -- collapse (s·Var) ~ Var
                            else unionNodes right left   -- Var points to TyExp (keep structure)
                        pure acc

                    -- Var = Var: before unioning, harmonize binding parents so
                    -- scope stays well-formed (paper Raise(n)).
                    (Just TyVar {}, Just TyVar {}) -> do
                        modify' $ \s ->
                            let c0 = nsConstraint s
                                c1 = BindingAdjustment.harmonizeBindParents left right c0
                            in s { nsConstraint = c1 }
                        unionNodes left right
                        pure acc

                    -- Variable = anything: point variable to other
                    (Just TyVar {}, _) -> do
                        unionNodes left right
                        pure acc
                    (_, Just TyVar {}) -> do
                        unionNodes right left
                        pure acc

                    -- Forall = Forall: unify bodies when binder arity matches
                    (Just (TyForall { tnBody = b1 }), Just (TyForall { tnBody = b2 })) -> do
                        c0 <- gets nsConstraint
                        let canonical = findRoot uf
                            arityOf nid =
                                case Binding.orderedBinders canonical c0 nid of
                                    Right bs -> Just (length bs)
                                    Left _ -> Nothing
                        case (arityOf left, arityOf right) of
                            (Just k1, Just k2)
                                | k1 == k2 -> do
                                    unionNodes left right
                                    pure (UnifyEdge b1 b2 : acc)
                            _ ->
                                -- Arity mismatch (or invalid binder): type error, keep edge
                                pure (edge : acc)

                    (Just node1, Just node2) ->
                        case UnifyDecompose.decomposeUnifyChildren node1 node2 of
                            Right newEdges -> do
                                unionNodes left right
                                case node1 of
                                    TyArrow{} -> pure (acc ++ newEdges)
                                    TyExp{} -> pure (newEdges ++ acc)
                                    _ -> pure acc
                            Left _ ->
                                pure (edge : acc)

                    -- Missing nodes / incompatible structures: keep edge to signal error
                    _ ->
                        pure (edge : acc)

    -- Union-find link; caller is responsible for any scope maintenance (e.g.
    -- binding-edge harmonization / paper Raise(n) for Var/Var unions).
    unionNodes :: NodeId -> NodeId -> NormalizeM ()
    unionNodes = unionNodesRaw

    unionNodesRaw :: NodeId -> NodeId -> NormalizeM ()
    unionNodesRaw from to = modify' $ \s ->
        s { nsUnionFind = IntMap.insert (getNodeId from) to (nsUnionFind s) }

-- | Find the root/canonical representative of a node.
findRoot :: IntMap NodeId -> NodeId -> NodeId
findRoot = UnionFind.frWith

-- | Apply the union-find substitution to all node references in the constraint.
applyUnionFindToConstraint :: NormalizeM ()
applyUnionFindToConstraint = do
    uf <- gets nsUnionFind
    when (not (IntMap.null uf)) $ modify' $ \s ->
        let c = nsConstraint s
            -- Update nodes: dereference variables to their canonical structure
            nodes' = IntMap.map (applyToNode uf (cNodes c)) (cNodes c)
            -- Update instantiation edges
            instEdges' = Canonicalize.rewriteInstEdges (findRoot uf) (cInstEdges c)
            -- Update unification edges
            unifyEdges' = Canonicalize.rewriteUnifyEdges (findRoot uf) (cUnifyEdges c)
            -- Canonicalize binding parents through UF reps
            bindParents' =
                Canonicalize.rewriteBindParentsLenient (findRoot uf) (const True) (cBindParents c)
        in s { nsConstraint = c
                 { cNodes = nodes'
                 , cInstEdges = instEdges'
                 , cUnifyEdges = unifyEdges'
                 , cBindParents = bindParents'
                 }
             }

-- | Apply union-find to a node.
-- If the node is a variable that has been unified with a structure,
-- we replace the variable with that structure (preserving the variable's ID).
applyToNode :: IntMap NodeId -> IntMap TyNode -> TyNode -> TyNode
applyToNode uf nodes node =
    let nid = tnId node
        rootId = findRoot uf nid
    in if nid /= rootId
        then
            -- Node has been merged. Check if the root has structure we should copy.
            case IntMap.lookup (getNodeId rootId) nodes of
                Just rootNode ->
                    -- If root is structure (not Var), copy it to this node ID.
                    -- If root is also Var, we keep this node as Var (but canonicalized?).
                    -- Actually, if root is Var, it doesn't matter much (it's a var alias).
                    -- But if root is structure, we MUST copy it so 'nid' behaves like structure.
                    case rootNode of
                        TyVar {} -> node -- Keep as var (alias)
                        _ -> applyToStructure uf (rootNode { tnId = nid })
                Nothing -> node -- Should not happen
        else
            -- Node is a root (or not in UF). Update its children.
            applyToStructure uf node

-- | Update children of a structure node using UF.
applyToStructure :: IntMap NodeId -> TyNode -> TyNode
applyToStructure uf node = case node of
    TyVar {} -> node
    TyBottom {} -> node
    TyArrow { tnId = nid, tnDom = dom, tnCod = cod } ->
        TyArrow { tnId = nid, tnDom = findRoot uf dom, tnCod = findRoot uf cod }
    TyBase {} -> node
    TyForall { tnId = nid, tnBody = body } ->
        TyForall { tnId = nid, tnBody = findRoot uf body }
    TyExp { tnId = nid, tnExpVar = ev, tnBody = body } ->
        TyExp { tnId = nid, tnExpVar = ev, tnBody = findRoot uf body }
    TyRoot { tnId = nid, tnChildren = cs } ->
        TyRoot { tnId = nid, tnChildren = map (findRoot uf) cs }

-- Helper to get NodeId as Int
