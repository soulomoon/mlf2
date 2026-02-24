# Equivalence-Class Abstraction Layer Design

## Goal

Move the solver's output representation from union-find + canonical rewriting
toward the thesis's graphic type model, where node identity is preserved through
equivalence classes. Full node identity and structural edge preservation.

## Approach: Opaque Module with Smart API

Introduce `MLF.Constraint.Solved` exporting an opaque `Solved` type. All
downstream consumers query through this module's API. The internal representation
is hidden behind the module boundary, enabling a backend swap from union-find to
equivalence classes without touching consumers.

Two phases:
- **Phase 1**: All consumers migrate to the `Solved` API, backed by the current
  `SolveResult` (union-find + rewritten constraint).
- **Phase 2**: Swap the backend to equivalence classes that preserve all original
  nodes and structural edges.

## API Surface

### Core queries (Phase 1)

```haskell
module MLF.Constraint.Solved
  ( Solved
  , fromSolveResult        -- :: SolveResult -> Solved
  , canonical              -- :: Solved -> NodeId -> NodeId
  , lookupNode             -- :: Solved -> NodeId -> Maybe TyNode
  , allNodes               -- :: Solved -> [TyNode]
  , lookupBindParent       -- :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
  , bindParents            -- :: Solved -> BindParents
  , instEdges              -- :: Solved -> [InstEdge]
  , schemeRoots            -- :: Solved -> [SchemeRoot]
  , genNodes               -- :: Solved -> IntMap GenNode
  , boundEdges             -- :: Solved -> [BoundEdge]
  , unionFind              -- :: Solved -> IntMap NodeId  (escape hatch, deprecated Phase 2)
  ) where
```

### Extended queries (Phase 2)

```haskell
  , classMembers           -- :: Solved -> NodeId -> [NodeId]
  , originalNode           -- :: Solved -> NodeId -> Maybe TyNode
  , originalBindParent     -- :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
  , unifiedBinders         -- :: Solved -> NodeId -> [NodeId]
  , wasOriginalBinder      -- :: Solved -> NodeId -> Bool
  , originalEdges          -- :: Solved -> NodeId -> [(NodeId, EdgeKind)]
```

Phase 1 implements extended queries with degraded answers: `classMembers` returns
`[canonical nid]`, `wasOriginalBinder` returns `False` for solved-away binders.
Phase 2 makes them real.

## Internal Representation

### Phase 1 (thin wrapper)

```haskell
data Solved = Solved
  { sConstraint :: Constraint    -- rewritten-to-canonical constraint
  , sUnionFind  :: IntMap NodeId -- parent map
  }
```

All queries delegate directly to existing `Constraint` / `frWith` operations.

### Phase 2 (equivalence classes)

```haskell
data Solved = Solved
  { sCanonicalMap          :: IntMap NodeId       -- original -> canonical rep
  , sCanonicalNodes        :: IntMap TyNode       -- canonical rep -> node data
  , sEquivClasses          :: IntMap (Set NodeId) -- canonical rep -> original members
  , sOriginalNodes         :: IntMap TyNode       -- original id -> pre-solving node data
  , sOriginalBindParents   :: BindParents         -- binding tree in original node ids
  , sCanonicalBindParents  :: BindParents         -- binding tree in canonical reps
  , sInstEdges             :: [InstEdge]
  , sSchemeRoots           :: [SchemeRoot]
  , sGenNodes              :: IntMap GenNode
  , sBoundEdges            :: [BoundEdge]
  }
```

Built by snapshotting all nodes before `applyUFConstraint` would rewrite them,
then computing equivalence classes from the union-find. The solver still uses
union-find internally for unification — the change is in what it *outputs*.

## Consumer Migration Map

| Module | Call sites | Pattern |
|--------|-----------|---------|
| Elaborate.hs | 3 | `frWith` → `canonical`, `lookupNode`, `schemeRoots`, `genNodes` |
| Omega.hs | ~20 | Heaviest consumer. `canonicalNode`, `lookupNode`, `lookupBindParent` |
| Sigma.hs | moderate | Same pattern as Omega |
| OmegaContext | record | Replace `srConstraint` + `canonicalNode` fields with `Solved` |
| TypeCheck.hs | few | Read-only canonical + node lookup |
| Reduce.hs | few | Read-only canonical + node lookup |
| Solve.hs | producer | Returns `Solved` via `fromSolveResult` (Phase 1), direct construction (Phase 2) |
| Test infra | various | `SolveResult` construction → `fromSolveResult` |

Presolution/Ops.hs is pre-solve code — no migration needed.

IdentityBridge uses `unionFind` escape hatch in Phase 1, rebuilt on `Solved`
queries (`classMembers`, `wasOriginalBinder`) in Phase 2.

## Milestones

All milestones are zero-regression: full test suite (780+ tests) must pass before
proceeding.

### Milestone 1: Define `MLF.Constraint.Solved` module

- Create module with opaque type and full Phase 1 API
- Implement backed by `SolveResult` (thin delegation)
- Extended queries return degraded answers (single-element classes, etc.)
- Gate: module compiles, unit tests for each API function
- Risk: Low. No existing code changes.

### Milestone 2: Migrate all consumers to `Solved` API

- Big-bang migration of Elaborate, Omega, Sigma, OmegaContext, TypeCheck, Reduce, tests
- Remove direct imports of `srConstraint` / `srUnionFind` from consumer modules
- Keep `unionFind` escape hatch for IdentityBridge
- Gate: all tests pass, no module imports `SolveResult` fields directly
- Risk: Medium. Wide but mechanical. Tests are the oracle.

### Milestone 3: Build equivalence-class representation

- New internal `Solved` constructor with original nodes, equiv classes, original binding tree
- Snapshot before `applyUFConstraint`: after union-find is complete, before rewriting
- Unit tests for `classMembers`, `originalNode`, `wasOriginalBinder`, `originalBindParent`
- Gate: new representation passes its own unit tests. Existing tests untouched.
- Risk: Medium. Snapshot timing must be after union-find closure, before rewriting.

### Milestone 4: Swap backend

- `Solve.hs` produces `Solved` via equivalence-class constructor
- `applyUFConstraint` no longer called on output path (may be kept for validation)
- Optional: comparison test harness verifying output equivalence before committing
- Gate: full test suite green against new backend
- Risk: Highest. New backend must produce identical `canonical`/`lookupNode` results.

### Milestone 5: Eliminate deviations and harvest benefits

- `DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP`: Omega uses `wasOriginalBinder`/`classMembers`
  to find unified-away binders, emits `InstElim` instead of skipping
- Remove `unionFind` escape hatch, rebuild IdentityBridge on `Solved` queries
- Audit other deviations stemming from lost node identity
- Gate: deviation removed from `thesis-deviations.yaml`, thesis-exact behavior verified
- Risk: Medium. OpWeaken handling rework; VSpine/replay type may surface secondary issues.

## Compatibility Strategy

- `fromSolveResult` remains available through Milestone 4 as a fallback — revert
  `Solve.hs` to produce via `fromSolveResult` without touching consumers
- `unionFind` escape hatch is the only API that breaks between phases — deprecated
  at Milestone 2, removed at Milestone 5
- Test suite is the single source of truth at every gate

## Risks Summary

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Subtle semantic drift in API translation | Medium | High | Tests as oracle; each call site is direct translation |
| Snapshot timing wrong (pending unifications) | Low | High | Assert union-find is closed before snapshot |
| Equivalence-class backend diverges from canonical | Medium | High | Comparison harness; `fromSolveResult` fallback |
| VSpine/trace resolution secondary issues at M5 | Medium | Medium | Incremental OpWeaken rework; can defer individual deviations |
| Performance regression from richer representation | Low | Medium | Benchmark before/after M4; lazy fields if needed |
