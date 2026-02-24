# Equivalence-Class Abstraction Layer Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace the union-find + canonical rewriting solver output with an opaque `Solved` abstraction backed by equivalence classes that preserve full node identity and structural edges.

**Architecture:** Opaque `MLF.Constraint.Solved` module with smart API. Phase 1 wraps current `SolveResult`. Phase 2 swaps to equivalence-class backend. Big-bang consumer migration, zero regression per milestone.

**Tech Stack:** Haskell, GHC 9.12.2, IntMap-based equivalence classes, existing union-find for solving internals.

**Design doc:** `docs/plans/2026-02-25-equivalence-class-abstraction-design.md`

---

## Milestone 1: Define `MLF.Constraint.Solved` Module

### Task 1: Create the `Solved` module with Phase 1 API

**Files:**
- Create: `src/MLF/Constraint/Solved.hs`
- Modify: `mlf2.cabal:87` (add module to exposed-modules)

**Step 1: Add module to cabal file**

In `mlf2.cabal`, after line 87 (`MLF.Constraint.Solve,`), add:
```
MLF.Constraint.Solved,
```

**Step 2: Write the module**

Create `src/MLF/Constraint/Solved.hs` with the opaque type and Phase 1 API.
The internal representation is `SolveResult` — all queries delegate directly.

```haskell
module MLF.Constraint.Solved
  ( Solved
  , fromSolveResult
  -- Core queries
  , canonical
  , lookupNode
  , allNodes
  , lookupBindParent
  , bindParents
  , instEdges
  , schemeRoots
  , genNodes
  , boundEdges
  -- Escape hatch (deprecated in Phase 2)
  , unionFind
  -- Extended queries (degraded in Phase 1)
  , classMembers
  , originalNode
  , originalBindParent
  , wasOriginalBinder
  ) where

import Data.IntMap.Strict (IntMap)
import MLF.Constraint.Solve (SolveResult(..), frWith)
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Graph.Binding (BindParents, BindFlag)
import MLF.Constraint.Types.Graph.NodeEdge (NodeId(..), NodeRef, GenNodeId)
import qualified MLF.Constraint.NodeAccess as NodeAccess

data Solved = Solved
  { sConstraint :: Constraint
  , sUnionFind  :: IntMap NodeId
  }

fromSolveResult :: SolveResult -> Solved
fromSolveResult sr = Solved
  { sConstraint = srConstraint sr
  , sUnionFind  = srUnionFind sr
  }

canonical :: Solved -> NodeId -> NodeId
canonical s = frWith (sUnionFind s)

lookupNode :: Solved -> NodeId -> Maybe TyNode
lookupNode s nid = NodeAccess.lookupNode (sConstraint s) (canonical s nid)

allNodes :: Solved -> [TyNode]
allNodes s = NodeAccess.allNodes (sConstraint s)

lookupBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent s ref =
  Binding.lookupBindParent (sConstraint s) ref

bindParents :: Solved -> BindParents
bindParents s = cBindParents (sConstraint s)

instEdges :: Solved -> [InstEdge]
instEdges s = cInstEdges (sConstraint s)

schemeRoots :: Solved -> [SchemeRoot]
schemeRoots s = cSchemeRoots (sConstraint s)  -- or equivalent field

genNodes :: Solved -> IntMap GenNode
genNodes s = cGenNodes (sConstraint s)  -- adapt to actual field type

boundEdges :: Solved -> [BoundEdge]
boundEdges s = cBoundEdges (sConstraint s)  -- adapt to actual field

unionFind :: Solved -> IntMap NodeId
unionFind s = sUnionFind s

-- Phase 1 degraded implementations
classMembers :: Solved -> NodeId -> [NodeId]
classMembers s nid = [canonical s nid]

originalNode :: Solved -> NodeId -> Maybe TyNode
originalNode = lookupNode

originalBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
originalBindParent = lookupBindParent

wasOriginalBinder :: Solved -> NodeId -> Bool
wasOriginalBinder _ _ = False
```

Note: Exact imports and field names must be adapted to match the actual
`Constraint` type definition at `src/MLF/Constraint/Types/Graph.hs:110-165`.
Check which fields exist (e.g. `cSchemeRoots` may be named differently or
live in a different accessor). The code above is the structural template.

**Step 3: Verify it compiles**

Run: `cabal build mlf2-internal`
Expected: compiles with no errors

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Solved.hs mlf2.cabal
git commit -m "feat: add MLF.Constraint.Solved opaque abstraction (Phase 1 backend)"
```

### Task 2: Unit tests for the `Solved` module

**Files:**
- Create: `test/Constraint/SolvedSpec.hs`
- Modify: `mlf2.cabal` (add test module)

**Step 1: Write tests**

Test each API function against a known `SolveResult`. Construct a small constraint
with a union-find that merges two nodes, wrap it in `fromSolveResult`, and verify:

- `canonical` returns the canonical rep
- `lookupNode` finds nodes by non-canonical id (canonicalizes internally)
- `classMembers` returns singleton (Phase 1 degraded)
- `wasOriginalBinder` returns False (Phase 1 degraded)
- `unionFind` returns the raw parent map
- `bindParents`, `instEdges`, etc. delegate correctly

Use existing test helpers from `test/SpecUtil.hs:63` which already imports
`MLF.Constraint.Solve`.

**Step 2: Run tests**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass including new SolvedSpec

**Step 3: Commit**

```bash
git add test/Constraint/SolvedSpec.hs mlf2.cabal
git commit -m "test: unit tests for MLF.Constraint.Solved Phase 1 API"
```

---

## Milestone 2: Migrate All Consumers to `Solved` API

### Task 3: Migrate `Elaborate.hs`

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs:32-53` (imports), lines 95-96, 117, 208, 361, 387, 390, 1031, 1057, 1112, 1164-1165

**Step 1: Update imports**

Replace:
```haskell
import MLF.Constraint.Solve (SolveResult(..))
import qualified MLF.Constraint.Solve as Solve (frWith)
```
With:
```haskell
import MLF.Constraint.Solve (SolveResult, solveUnify)  -- keep SolveResult type, not fields
import qualified MLF.Constraint.Solved as Solved
```

**Step 2: Migrate call sites**

Pattern for each call site:
- `srConstraint res` → use `Solved` queries on `fromSolveResult res`
- `Solve.frWith (srUnionFind res)` → `Solved.canonical solved`
- `NodeAccess.lookupNode (srConstraint res) (canonical nid)` → `Solved.lookupNode solved nid`
- `VarStore.lookupVarBound (srConstraint res)` → needs a `lookupVarBound` addition to `Solved` API, or pass `Solved` and use internal constraint

Key call sites (line numbers from current code):
- Line 95-96: `let constraint = srConstraint res; canonical = Solve.frWith (srUnionFind res)`
  → `let solved = fromSolveResult res`
- Line 208: `canonical = Solve.frWith (srUnionFind resReify)`
  → `let solvedReify = fromSolveResult resReify`
- Line 387, 390, 1112: `NodeAccess.lookupNode (srConstraint resReify)`
  → `Solved.lookupNode solvedReify`
- Line 1164-1165: passes `srConstraint res` and `Solve.frWith (srUnionFind res)` to helpers
  → pass `solved` instead

Note: `VarStore.lookupVarBound` is used at lines 361, 1057. Either add
`lookupVarBound :: Solved -> NodeId -> Maybe NodeId` to the `Solved` API,
or expose a `constraint :: Solved -> Constraint` accessor for read-only
access to fields not yet abstracted. Prefer adding to the API.

**Step 3: Run tests**

Run: `cabal test --test-show-details=direct`
Expected: all 780+ tests pass

**Step 4: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Constraint/Solved.hs
git commit -m "refactor: migrate Elaborate.hs to Solved API"
```

### Task 4: Migrate `Phi/Translate.hs`

**Files:**
- Modify: `src/MLF/Elab/Phi/Translate.hs:55-56` (imports), lines 71, 434, 446, 513, 691, 739, 775

**Step 1: Update imports**

Replace:
```haskell
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
```
With:
```haskell
import MLF.Constraint.Solve (SolveResult, solveUnify)  -- keep what's needed for solving
import qualified MLF.Constraint.Solved as Solved
```

**Step 2: Migrate call sites**

Key patterns:
- Line 71: `Solve.frWith (srUnionFind res) nid` → `Solved.canonical solved nid`
- Line 434: `(srConstraint res, srConstraint res, schemeCanonical)` → pass `solved`
- Line 446: `canonicalNode = Solve.frWith (srUnionFind res)` → `canonicalNode = Solved.canonical solved`
- Line 513: `NodeAccess.lookupNode (srConstraint res)` → `Solved.lookupNode solved`
- Line 691: `Binding.orderedBinders canonicalNode (srConstraint res)` → needs `Solved` to expose
  constraint for `Binding` module calls, or add `orderedBinders` to `Solved` API
- Line 739: `NodeAccess.allGenNodes (srConstraint res)` → add `allGenNodes :: Solved -> [GenNode]`
  to `Solved` API, or use `genNodes solved`
- Line 775: `Binding.lookupBindParentUnder canonicalNode (srConstraint res)` → add to `Solved` API

Note: Translate.hs passes `srConstraint` to `Binding.orderedBinders` and
`Binding.lookupBindParentUnder` which take a raw `Constraint`. Two options:
(a) Add these as `Solved` API functions, or (b) add a read-only
`solvedConstraint :: Solved -> Constraint` accessor for binding-tree operations
that haven't been abstracted yet. Option (b) is pragmatic for Phase 1 — mark
it as internal/deprecated.

**Step 3: Run tests**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass

**Step 4: Commit**

```bash
git add src/MLF/Elab/Phi/Translate.hs src/MLF/Constraint/Solved.hs
git commit -m "refactor: migrate Phi/Translate.hs to Solved API"
```

### Task 5: Migrate `Omega.hs` (heaviest consumer)

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs:36` (import), lines 55-70 (OmegaContext), ~30 call sites

**Step 1: Update OmegaContext record**

At `Omega.hs:55-70`, the `OmegaContext` record holds:
```haskell
ocResult :: SolveResult
ocCanonicalNode :: NodeId -> NodeId
```

Replace with:
```haskell
ocSolved :: Solved
```

Remove `ocCanonicalNode` — it becomes `Solved.canonical (ocSolved ctx)`.
The `ocResult` field is replaced entirely by `ocSolved`.

**Step 2: Update imports**

Replace:
```haskell
import MLF.Constraint.Solve (SolveResult(..))
```
With:
```haskell
import MLF.Constraint.Solve (SolveResult)
import qualified MLF.Constraint.Solved as Solved
```

**Step 3: Migrate call sites**

This is the largest migration (~30 call sites). The pattern is uniform:

| Old pattern | New pattern |
|-------------|-------------|
| `srConstraint res` | use `Solved` queries on `ocSolved ctx` |
| `canonicalNode nid` | `Solved.canonical (ocSolved ctx) nid` |
| `NodeAccess.lookupNode (srConstraint res) nidC` | `Solved.lookupNode (ocSolved ctx) nid` |
| `lookupBindParent (srConstraint res) ref` | `Solved.lookupBindParent (ocSolved ctx) ref` |
| `cBindParents (srConstraint res)` | `Solved.bindParents (ocSolved ctx)` |
| `cNodes (srConstraint res)` | use `Solved.lookupNode` or `Solved.allNodes` |
| `NodeAccess.allGenNodes (srConstraint res)` | use `Solved.genNodes` or add `allGenNodes` |
| `VarStore.lookupVarBound (srConstraint res)` | add `lookupVarBound` to `Solved` API |
| `Binding.orderedBinders canonicalNode (srConstraint res)` | add to `Solved` API or use `solvedConstraint` |
| `Binding.bindingLCA (srConstraint res)` | add to `Solved` API or use `solvedConstraint` |

Define local helpers in Omega.hs to reduce noise:
```haskell
let solved = ocSolved ctx
    canon = Solved.canonical solved
    lkNode nid = Solved.lookupNode solved nid
```

**Step 4: Update IdentityBridge construction**

At the call site where `IB.mkIdentityBridge` is constructed (around line 88-103):
```haskell
-- Old:
ib = IB.mkIdentityBridge canonicalNode mTrace copyMap
-- New:
ib = IB.mkIdentityBridge (Solved.canonical solved) mTrace copyMap
```

This uses the `unionFind` escape hatch indirectly (canonical still works).
IdentityBridge itself is not migrated until Milestone 5.

**Step 5: Run tests**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass

**Step 6: Commit**

```bash
git add src/MLF/Elab/Phi/Omega.hs src/MLF/Constraint/Solved.hs
git commit -m "refactor: migrate Omega.hs to Solved API"
```

### Task 6: Migrate test infrastructure

**Files:**
- Modify: `test/SpecUtil.hs:63` (import)
- Modify: `test/ElaborationSpec.hs:50` (import, ~38 construction sites)
- Modify: `test/SolveSpec.hs:9` (import)
- Modify: `test/PipelineSpec.hs:32` (import)

**Step 1: Update SpecUtil.hs**

Replace:
```haskell
import MLF.Constraint.Solve (SolveResult, solveUnify)
```
With:
```haskell
import MLF.Constraint.Solve (SolveResult, solveUnify)
import qualified MLF.Constraint.Solved as Solved
```

If SpecUtil provides helpers that extract from `SolveResult`, update them to
return or accept `Solved` instead.

**Step 2: Update ElaborationSpec.hs**

At line 50:
```haskell
import MLF.Constraint.Solve (SolveResult(..), solveUnify)
```
Change to:
```haskell
import MLF.Constraint.Solve (SolveResult, solveUnify)
import qualified MLF.Constraint.Solved as Solved
```

The ~38 construction sites (lines 880, 912, 944, ...) that pattern-match on
`SolveResult{..}` need to use `fromSolveResult` and then `Solved` queries.
If tests inspect `srConstraint` directly for assertions, they can use
`Solved.lookupNode`, `Solved.canonical`, etc.

**Step 3: Update SolveSpec.hs and PipelineSpec.hs**

Same pattern. SolveSpec.hs tests the solver itself — it may legitimately need
`SolveResult(..)` for white-box testing of solver internals. If so, keep the
field access in SolveSpec only and document why.

**Step 4: Run tests**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass

**Step 5: Commit**

```bash
git add test/SpecUtil.hs test/ElaborationSpec.hs test/SolveSpec.hs test/PipelineSpec.hs
git commit -m "refactor: migrate test infrastructure to Solved API"
```

### Task 7: Hide `SolveResult` fields and verify encapsulation

**Files:**
- Modify: `src/MLF/Constraint/Solve.hs:125-131` (module exports)

**Step 1: Stop exporting `SolveResult` field accessors**

At `Solve.hs:126`, change:
```haskell
SolveResult(..),
```
To:
```haskell
SolveResult,
```

This makes `srConstraint` and `srUnionFind` inaccessible outside `Solve.hs`
and `Solved.hs`. Any remaining direct field access will become a compile error.

Exception: `Solved.hs` imports `SolveResult(..)` — this is fine, it's the
one module allowed to see the internals.

**Step 2: Verify it compiles**

Run: `cabal build`
Expected: compiles. If any module still accesses `srConstraint`/`srUnionFind`
directly, it will fail here — fix those remaining call sites.

**Step 3: Run full test suite**

Run: `cabal test --test-show-details=direct`
Expected: all 780+ tests pass

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Solve.hs
git commit -m "refactor: hide SolveResult fields — all access through Solved API"
```

This is the **Milestone 2 gate**. At this point, no consumer module can access
`SolveResult` internals. The abstraction boundary is enforced at compile time.

---

## Milestone 3: Build Equivalence-Class Representation

### Task 8: Add equivalence-class fields to `Solved`

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs`

**Step 1: Extend the internal representation**

Add a second constructor or extend the record with optional equivalence-class
fields. Using a sum type keeps Phase 1 and Phase 2 backends coexisting during
development:

```haskell
data SolvedBackend
  = LegacyBackend
      { lbConstraint :: Constraint
      , lbUnionFind  :: IntMap NodeId
      }
  | EquivBackend
      { ebCanonicalMap        :: IntMap NodeId       -- original -> canonical
      , ebCanonicalNodes      :: IntMap TyNode       -- canonical -> node data
      , ebEquivClasses        :: IntMap (Set NodeId)  -- canonical -> original members
      , ebOriginalNodes       :: IntMap TyNode       -- original -> pre-solving node data
      , ebOriginalBindParents :: BindParents         -- binding tree in original ids
      , ebCanonicalBindParents :: BindParents        -- binding tree in canonical ids
      , ebInstEdges           :: [InstEdge]
      , ebSchemeRoots         :: [SchemeRoot]
      , ebGenNodes            :: IntMap GenNode
      , ebBoundEdges          :: [BoundEdge]
      }

newtype Solved = Solved { unSolved :: SolvedBackend }
```

**Step 2: Update all API functions to pattern-match on both backends**

Each function dispatches on `LegacyBackend` vs `EquivBackend`:

```haskell
canonical :: Solved -> NodeId -> NodeId
canonical (Solved (LegacyBackend _ uf)) nid = frWith uf nid
canonical (Solved (EquivBackend{ebCanonicalMap = cm})) nid =
  fromMaybe nid (IntMap.lookup (getNodeId nid) cm)

lookupNode :: Solved -> NodeId -> Maybe TyNode
lookupNode (Solved (LegacyBackend c uf)) nid =
  NodeAccess.lookupNode c (frWith uf nid)
lookupNode (Solved (EquivBackend{ebCanonicalNodes = cn, ebCanonicalMap = cm})) nid =
  let nidC = fromMaybe nid (IntMap.lookup (getNodeId nid) cm)
  in IntMap.lookup (getNodeId nidC) cn

classMembers :: Solved -> NodeId -> [NodeId]
classMembers (Solved (LegacyBackend _ uf)) nid = [frWith uf nid]
classMembers (Solved (EquivBackend{ebEquivClasses = ec, ebCanonicalMap = cm})) nid =
  let nidC = fromMaybe nid (IntMap.lookup (getNodeId nid) cm)
  in maybe [nidC] Set.toList (IntMap.lookup (getNodeId nidC) ec)

wasOriginalBinder :: Solved -> NodeId -> Bool
wasOriginalBinder (Solved (LegacyBackend _ _)) _ = False
wasOriginalBinder (Solved (EquivBackend{ebOriginalNodes = on})) nid =
  case IntMap.lookup (getNodeId nid) on of
    Just TyVar{} -> True
    _ -> False
```

**Step 3: Verify it compiles**

Run: `cabal build`
Expected: compiles. No consumer changes needed — the API is unchanged.

**Step 4: Run tests**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass (still using `LegacyBackend` via `fromSolveResult`)

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Solved.hs
git commit -m "feat: add EquivBackend constructor to Solved (not yet wired)"
```

### Task 9: Build snapshot construction from pre-rewrite state

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs` (add `fromPreRewriteState`)
- Modify: `src/MLF/Constraint/Solve.hs` (expose pre-rewrite snapshot point)

**Step 1: Identify the snapshot point in Solve.hs**

At `Solve.hs:274`, the solver constructs `SolveResult`:
```haskell
let res = SolveResult { srConstraint = c''', srUnionFind = uf' }
```

Where `c'''` is the result of `applyUFConstraint uf' c''`. The snapshot must
capture `c''` (pre-rewrite constraint) and `uf'` (final union-find) BEFORE
`applyUFConstraint` is called.

Add an internal function or export the pre-rewrite state:
```haskell
-- In Solve.hs, around line 274:
let preRewriteConstraint = c''  -- before applyUFConstraint
    finalUF = uf'
    rewrittenConstraint = applyUFConstraint finalUF preRewriteConstraint
    res = SolveResult { srConstraint = rewrittenConstraint, srUnionFind = finalUF }
```

Export a new function or data type that carries both:
```haskell
data SolveOutput = SolveOutput
  { soResult :: SolveResult          -- existing rewritten result
  , soPreRewriteConstraint :: Constraint  -- pre-rewrite snapshot
  }
```

**Step 2: Build `fromPreRewriteState` in Solved.hs**

```haskell
fromPreRewriteState :: IntMap NodeId -> Constraint -> Solved
fromPreRewriteState uf preRewrite =
  let canonMap = buildCanonicalMap uf (NodeAccess.allNodes preRewrite)
      equivClasses = buildEquivClasses canonMap
      canonNodes = buildCanonicalNodes canonMap (NodeAccess.allNodes preRewrite)
      origNodes = buildOriginalNodeMap (NodeAccess.allNodes preRewrite)
      origBP = cBindParents preRewrite
      canonBP = rewriteBindParents uf origBP
  in Solved $ EquivBackend
      { ebCanonicalMap = canonMap
      , ebCanonicalNodes = canonNodes
      , ebEquivClasses = equivClasses
      , ebOriginalNodes = origNodes
      , ebOriginalBindParents = origBP
      , ebCanonicalBindParents = canonBP
      , ebInstEdges = rewriteInstEdges uf (cInstEdges preRewrite)
      , ebSchemeRoots = rewriteSchemeRoots uf (cSchemeRoots preRewrite)
      , ebGenNodes = cGenNodes preRewrite
      , ebBoundEdges = rewriteBoundEdges uf (cBoundEdges preRewrite)
      }

buildCanonicalMap :: IntMap NodeId -> [TyNode] -> IntMap NodeId
buildCanonicalMap uf nodes =
  IntMap.fromList [ (getNodeId (tnId n), frWith uf (tnId n)) | n <- nodes ]

buildEquivClasses :: IntMap NodeId -> IntMap (Set NodeId)
buildEquivClasses canonMap =
  IntMap.fromListWith Set.union
    [ (getNodeId canon, Set.singleton (NodeId origKey))
    | (origKey, canon) <- IntMap.toList canonMap
    ]

buildCanonicalNodes :: IntMap NodeId -> [TyNode] -> IntMap TyNode
buildCanonicalNodes canonMap nodes =
  IntMap.fromListWith Canonicalize.chooseRepNode
    [ (getNodeId (frWith canonMap (tnId n)), rewriteNodeFields canonMap n)
    | n <- nodes
    ]

buildOriginalNodeMap :: [TyNode] -> IntMap TyNode
buildOriginalNodeMap nodes =
  IntMap.fromList [ (getNodeId (tnId n), n) | n <- nodes ]
```

Note: `rewriteBindParents`, `rewriteInstEdges`, `rewriteSchemeRoots`,
`rewriteBoundEdges`, and `rewriteNodeFields` follow the same pattern as
`applyUFConstraint` in `Solve.hs:573-625` — rewrite all `NodeId` fields
through `frWith uf`. Factor these out or duplicate the logic.

**Step 3: Verify it compiles**

Run: `cabal build`
Expected: compiles

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Solved.hs src/MLF/Constraint/Solve.hs
git commit -m "feat: add fromPreRewriteState for equivalence-class construction"
```

### Task 10: Unit tests for equivalence-class queries

**Files:**
- Modify: `test/Constraint/SolvedSpec.hs`

**Step 1: Write tests for EquivBackend**

Construct a small constraint with known structure:
- Two nodes: α (TyVar, binder) and Int (TyBase)
- Union-find merges α → Int
- A forall node with α as its binder

Build `Solved` via `fromPreRewriteState` and verify:

```haskell
describe "EquivBackend" $ do
  it "classMembers returns all original nodes in equivalence class" $ do
    let solved = fromPreRewriteState uf preRewriteConstraint
    Solved.classMembers solved intNodeId `shouldContain` [alphaNodeId]
    Solved.classMembers solved alphaNodeId `shouldContain` [alphaNodeId]

  it "wasOriginalBinder returns True for unified-away binder" $ do
    let solved = fromPreRewriteState uf preRewriteConstraint
    Solved.wasOriginalBinder solved alphaNodeId `shouldBe` True

  it "originalNode returns pre-solving node data" $ do
    let solved = fromPreRewriteState uf preRewriteConstraint
    case Solved.originalNode solved alphaNodeId of
      Just TyVar{} -> pure ()
      other -> expectationFailure ("expected TyVar, got " ++ show other)

  it "originalBindParent preserves pre-solving binding tree" $ do
    let solved = fromPreRewriteState uf preRewriteConstraint
    Solved.originalBindParent solved (typeRef alphaNodeId)
      `shouldBe` Just (typeRef forallNodeId, BindBound)

  it "canonical matches legacy backend" $ do
    let legacy = fromSolveResult solveResult
        equiv = fromPreRewriteState uf preRewriteConstraint
    for_ allNodeIds $ \nid ->
      Solved.canonical equiv nid `shouldBe` Solved.canonical legacy nid

  it "lookupNode matches legacy backend" $ do
    let legacy = fromSolveResult solveResult
        equiv = fromPreRewriteState uf preRewriteConstraint
    for_ allNodeIds $ \nid ->
      Solved.lookupNode equiv nid `shouldBe` Solved.lookupNode legacy nid
```

The last two tests are the **comparison oracle** — they verify that the
EquivBackend produces identical core query results as the LegacyBackend.

**Step 2: Run tests**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass

**Step 3: Commit**

```bash
git add test/Constraint/SolvedSpec.hs
git commit -m "test: equivalence-class backend unit tests with legacy comparison"
```

This is the **Milestone 3 gate**. The EquivBackend exists, is tested in
isolation, and produces identical core results to the LegacyBackend.

---

## Milestone 4: Swap Backend

### Task 11: Wire equivalence-class construction into Solve.hs

**Files:**
- Modify: `src/MLF/Constraint/Solve.hs:256-274` (solver output path)
- Modify: `src/MLF/Constraint/Solved.hs` (add `fromSolveOutput`)

**Step 1: Capture pre-rewrite state in Solve.hs**

At `Solve.hs:274`, the solver currently does:
```haskell
let res = SolveResult { srConstraint = applyUFConstraint uf' c'', srUnionFind = uf' }
```

Change to also capture the pre-rewrite constraint:
```haskell
let preRewrite = c''
    rewritten = applyUFConstraint uf' preRewrite
    res = SolveResult { srConstraint = rewritten, srUnionFind = uf' }
    solved = fromPreRewriteState uf' preRewrite
```

Export `solved` alongside `res` via a new return type or by having `solveUnify`
return `Solved` directly. The cleanest approach: change `solveUnify` to return
`Solved`, and provide `toSolveResult :: Solved -> SolveResult` for any code
that still needs the legacy type during transition.

**Step 2: Update solveUnify return type**

Option A (minimal): Add a parallel function:
```haskell
solveUnifyToSolved :: ... -> SolveM Solved
```

Option B (cleaner): Change `solveUnify` to return `Solved`, add
`toSolveResult` for backward compat. Since all consumers already use `Solved`
API (Milestone 2), this should be straightforward.

Prefer Option B.

**Step 3: Run full test suite**

Run: `cabal test --test-show-details=direct`
Expected: all 780+ tests pass

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Solve.hs src/MLF/Constraint/Solved.hs
git commit -m "feat: wire equivalence-class backend into solver output path"
```

### Task 12: Comparison testing across full test suite

**Files:**
- Modify: `test/Constraint/SolvedSpec.hs` or create a temporary comparison harness

**Step 1: Write a comparison test**

Temporarily modify the pipeline to produce BOTH backends for every test case
and compare core query outputs:

```haskell
describe "Backend equivalence" $ do
  forM_ allTestCases $ \(name, input) -> do
    it ("legacy == equiv for: " ++ name) $ do
      let (uf, preRewrite, rewritten) = solveAndCapture input
          legacy = fromSolveResult (SolveResult rewritten uf)
          equiv = fromPreRewriteState uf preRewrite
          nodeIds = allNodeIdsFrom preRewrite
      for_ nodeIds $ \nid -> do
        Solved.canonical equiv nid `shouldBe` Solved.canonical legacy nid
        Solved.lookupNode equiv nid `shouldBe` Solved.lookupNode legacy nid
      Solved.instEdges equiv `shouldBe` Solved.instEdges legacy
      Solved.bindParents equiv `shouldBe` Solved.bindParents legacy
```

This runs against the full test suite's constraint graphs, not just
hand-crafted examples.

**Step 2: Run comparison**

Run: `cabal test --test-show-details=direct -p "Backend equivalence"`
Expected: all comparisons pass

**Step 3: If divergences found**

Debug each divergence. Common causes:
- Node deduplication order differs (fix: ensure `chooseRepNode` is applied
  identically in both paths)
- Binding tree edge rewriting drops different self-edges
- InstEdge rewriting order differs (fix: sort before compare)

**Step 4: Commit**

```bash
git add test/Constraint/SolvedSpec.hs
git commit -m "test: full-suite backend equivalence comparison"
```

### Task 13: Remove `applyUFConstraint` from output path

**Files:**
- Modify: `src/MLF/Constraint/Solve.hs`
- Modify: `src/MLF/Constraint/Solved.hs`

**Step 1: Stop calling `applyUFConstraint` for the returned result**

Now that `Solved` is built from `fromPreRewriteState`, the rewritten
`Constraint` inside `SolveResult` is no longer needed by consumers.

If `solveUnify` now returns `Solved` directly (Task 11, Option B), then
`applyUFConstraint` is only called if `toSolveResult` is used. Keep
`applyUFConstraint` as an internal function but remove it from the
primary output path.

**Step 2: Run full test suite**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass

**Step 3: Commit**

```bash
git add src/MLF/Constraint/Solve.hs src/MLF/Constraint/Solved.hs
git commit -m "refactor: remove applyUFConstraint from primary output path"
```

This is the **Milestone 4 gate**. The solver produces equivalence-class
output. All tests pass. `fromSolveResult` / `toSolveResult` remain as
escape hatches but are not used in the primary path.

---

## Milestone 5: Eliminate Deviations and Harvest Benefits

### Task 14: Rework OpWeaken handling in Omega.hs

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs:790-814` (OpWeaken case)

**Step 1: Understand the current skip logic**

At `Omega.hs:790-800`, the current code:
```haskell
(OpWeaken bv : rest) -> do
    bvReplay <- resolveTraceBinderTarget False "OpWeaken" bv
    let bvC = canonicalNode bvReplay
        rootC = canonicalNode orderRoot
    if bvC == rootC
        then go ...  -- skip: binder is root
        else if not (isBinderNode binderKeys bvReplay)
            -- DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP
            then go ...  -- skip: binder solved away
            else ...     -- emit InstElim
```

The second branch (`not (isBinderNode ...)`) is the deviation. The binder
was unified away by solving, so it's not in the replay type's VSpine.

**Step 2: Use `wasOriginalBinder` to recover the binder**

With the EquivBackend, we can detect that the canonical rep was originally
a binder:

```haskell
(OpWeaken bv : rest) -> do
    bvReplay <- resolveTraceBinderTarget False "OpWeaken" bv
    let bvC = Solved.canonical solved bvReplay
        rootC = Solved.canonical solved orderRoot
    if bvC == rootC
        then go ...  -- skip: binder is root (unchanged)
        else if not (isBinderNode binderKeys bvReplay)
            then if Solved.wasOriginalBinder solved bvReplay
                then do
                    -- Binder was solved away but we can recover it.
                    -- The quantifier elimination was already performed by
                    -- solving; emit a trivial InstElim to stay thesis-exact.
                    -- Find the binder's position via classMembers.
                    let origBinders = Solved.classMembers solved bvC
                    -- ... reconstruct binder position in VSpine
                    -- ... emit InstElim
                else go ...  -- genuinely not a binder
            else ...  -- existing InstElim emission
```

The exact reconstruction logic depends on how VSpine tracks binder identity.
The key insight: `classMembers` gives us the original binder node id, and
`originalBindParent` tells us where it sat in the binding tree. From there
we can find its position in the VSpine.

This is the most complex step in the plan. It may require:
- Adding a VSpine lookup that works with original (pre-solving) node ids
- Adjusting `lookupBinderIndex` to consult `classMembers`
- Possibly extending VSpine construction to include solved-away binders
  as phantom entries

**Step 3: Write failing test first**

Find or create a test case where:
- Source type: `∀(α ≥ Int). α → α`
- Target type: `Int → Int`
- Currently produces a skip (no InstElim)
- Should produce InstElim after this change

The test in `ElaborationSpec.hs` or `WitnessSpec.hs` should assert that
the elaborated phi term contains `InstElim` where it currently doesn't.

**Step 4: Implement and iterate**

This task may take multiple iterations. The zero-regression constraint
means each sub-step must pass the full suite.

**Step 5: Run tests**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass, new test asserts thesis-exact InstElim

**Step 6: Commit**

```bash
git add src/MLF/Elab/Phi/Omega.hs test/ElaborationSpec.hs
git commit -m "fix: emit thesis-exact InstElim for solved-away binders (eliminate DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP)"
```

### Task 15: Rebuild IdentityBridge on `Solved` queries

**Files:**
- Modify: `src/MLF/Elab/Phi/IdentityBridge.hs:71-85` (mkIdentityBridge)
- Modify: `src/MLF/Elab/Phi/Omega.hs` (construction site)

**Step 1: Change `mkIdentityBridge` to accept `Solved`**

Replace:
```haskell
mkIdentityBridge
    :: (NodeId -> NodeId)
    -> Maybe EdgeTrace
    -> IntMap.IntMap NodeId
    -> IdentityBridge
```
With:
```haskell
mkIdentityBridge
    :: Solved
    -> Maybe EdgeTrace
    -> IntMap.IntMap NodeId
    -> IdentityBridge
```

Internally, replace `ibCanonical` usage with `Solved.canonical solved`.
Use `Solved.classMembers` for reverse lookups instead of building
`ibReverseCopyByCanonical` from the copy map alone.

**Step 2: Update Omega.hs construction site**

Around `Omega.hs:88-103`:
```haskell
-- Old:
ib = IB.mkIdentityBridge canonicalNode mTrace copyMap
-- New:
ib = IB.mkIdentityBridge solved mTrace copyMap
```

**Step 3: Run tests**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass

**Step 4: Commit**

```bash
git add src/MLF/Elab/Phi/IdentityBridge.hs src/MLF/Elab/Phi/Omega.hs
git commit -m "refactor: rebuild IdentityBridge on Solved queries"
```

### Task 16: Remove `unionFind` escape hatch

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs` (remove `unionFind` export)

**Step 1: Remove the export**

Remove `unionFind` from the module's export list. Any remaining call site
will become a compile error.

**Step 2: Verify it compiles**

Run: `cabal build`
Expected: compiles. If anything still uses `unionFind`, it was missed in
Tasks 14-15 — fix those call sites first.

**Step 3: Run tests**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Solved.hs
git commit -m "refactor: remove unionFind escape hatch from Solved API"
```

### Task 17: Update deviation documentation

**Files:**
- Modify: `docs/thesis-deviations.yaml:111-136` (remove DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP)
- Modify: `docs/thesis-claims.yaml` (update claim status if applicable)

**Step 1: Remove the deviation entry**

Delete the `DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP` entry from
`docs/thesis-deviations.yaml`. Update the deviation count in any
summary headers.

**Step 2: Audit for other deviations eliminated by node identity preservation**

Search `thesis-deviations.yaml` for any other deviations whose root cause
is "node identity lost during solving" or similar. If any exist, evaluate
whether the EquivBackend resolves them and remove/update accordingly.

**Step 3: Update thesis-claims.yaml**

If any claims were marked as partially satisfied due to the deviation,
update their status to fully satisfied.

**Step 4: Commit**

```bash
git add docs/thesis-deviations.yaml docs/thesis-claims.yaml
git commit -m "docs: remove DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP — thesis-exact with equivalence classes"
```

This is the **Milestone 5 gate**. The deviation is eliminated, the escape
hatch is removed, IdentityBridge uses the `Solved` API, and documentation
reflects the new state.

---

## Risk Register

| Milestone | Risk | Likelihood | Impact | Mitigation |
|-----------|------|-----------|--------|------------|
| M2 | Subtle semantic drift in API translation | Medium | High | 780+ tests as oracle; each site is direct translation |
| M2 | `VarStore.lookupVarBound` and `Binding.*` need many new `Solved` API functions | Medium | Medium | Use `solvedConstraint` escape hatch for Phase 1; abstract incrementally |
| M3 | Snapshot captures wrong moment (pending unifications) | Low | High | Assert union-find is closed; compare against legacy backend |
| M3 | `chooseRepNode` dedup order differs between paths | Medium | Medium | Use identical dedup logic; comparison tests catch divergence |
| M4 | Performance regression from richer representation | Low | Medium | Benchmark before/after; lazy fields if needed |
| M4 | Binding tree rewriting drops different edges | Medium | Medium | Comparison tests on `bindParents` output |
| M5 | VSpine doesn't have slots for solved-away binders | High | High | May need VSpine extension to include phantom binder entries |
| M5 | OpWeaken rework breaks existing phi translation | Medium | High | TDD: write failing test first, iterate under full suite |
| M5 | IdentityBridge reverse lookups change behavior | Medium | Medium | Comparison tests on trace resolution outputs |

## Rollback Strategy

At any milestone, if regressions prove intractable:

- **M1-M2**: Revert consumer migrations, re-export `SolveResult(..)` fields
- **M3**: Delete `EquivBackend` constructor, keep `LegacyBackend` only
- **M4**: Revert `Solve.hs` to produce via `fromSolveResult`; consumers
  are unaffected since they use the `Solved` API
- **M5**: Keep the deviation; revert Omega/IdentityBridge changes; re-add
  `unionFind` escape hatch

The `fromSolveResult` constructor is the key safety valve — it lets the
entire pipeline fall back to the legacy backend without touching consumers.
