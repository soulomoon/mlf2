# Task: Eliminate Solved Indirection — Elaborate Directly from χp

**Goal**: Remove the `Solved` wrapper from the elaboration path. Elaborate directly from `PresolutionResult` (the translatable presolution χp) + per-edge witness derivations (`EdgeTrace`), replacing `buildSolvedFromPresolutionSnapshot` and `eeSolved`.

**Thesis alignment**: §15.3.6 elaborates directly from a translatable presolution χp. The current `Solved` intermediate re-solves/replays the presolution snapshot to build equivalence classes and canonical maps, which is an unnecessary indirection.

---

## Architecture Summary

### What `Solved` actually provides

`Solved` is a newtype over `SolvedBackend` containing:
1. **`ebCanonicalMap`** — `IntMap NodeId` (union-find: node → canonical representative)
2. **`ebOriginalConstraint`** — the pre-rewrite `Constraint` (= `prConstraint` from presolution)
3. **`ebCanonical*` fields** — post-solve canonical nodes/edges/bind-parents/gen-nodes
4. **`ebEquivClasses`** — `IntMap [NodeId]` (equivalence classes)

Only `prConstraint` and `prUnionFind` from `PresolutionResult` feed into `Solved` construction. All other presolution artifacts (`prEdgeExpansions`, `prEdgeWitnesses`, `prEdgeTraces`, `prRedirects`, `prPlanBuilder`) already bypass `Solved` and are threaded independently.

### Core insight

The `Solved` module provides ~15 query functions, but all of them can be decomposed into:
- **`canonical :: NodeId -> NodeId`** — chase the union-find (from `prUnionFind`)
- **Constraint queries** — `lookupNode`, `lookupVarBound`, `bindParents`, `genNodes`, `allNodes` (from `prConstraint`)
- **Canonical-domain queries** — `weakenedVars`, `isEliminatedVar`, `canonicalBindParents` (derivable from constraint + UF)
- **Mutation** — `rebuildWithConstraint`, `rebuildWithNodes`, `pruneBindParentsSolved` (only used in pipeline wiring, not in core elaboration)

A thin `PresolutionView` record can replace all of these.

---

## Phased Plan

### Phase 1: Introduce `PresolutionView` (non-breaking shim)

**Files**: New module `src/MLF/Constraint/PresolutionView.hs`

Create a record that bundles the presolution's directly-usable data:

```haskell
data PresolutionView = PresolutionView
    { pvConstraint  :: Constraint       -- χp (= prConstraint)
    , pvCanonical   :: NodeId -> NodeId  -- chase function over prUnionFind
    , pvCanonicalMap :: IntMap NodeId    -- raw UF map (prUnionFind collapsed)
    , pvWeakenedVars :: IntSet          -- derived: nodes whose binder was weakened
    , pvEliminatedVars :: IntSet        -- derived: eliminated var IDs
    }
```

Provide the same query API as `Solved` but backed by the presolution data:
- `canonical`, `lookupNode`, `lookupVarBound`, `bindParents`, `genNodes`, `originalConstraint` → direct field access / constraint queries + canonicalizing via `pvCanonical`
- `canonicalConstraint` → lazily reconstructed from `pvConstraint` + `pvCanonicalMap`
- `canonicalBindParents` → derived from `pvConstraint.cBindParents` + `pvCanonicalMap`

**Construct from `PresolutionResult`**:
```haskell
fromPresolutionResult :: PresolutionResult -> PresolutionView
fromPresolutionResult pr = PresolutionView
    { pvConstraint = prConstraint pr
    , pvCanonical = equivCanonical uf
    , pvCanonicalMap = uf
    , pvWeakenedVars = deriveWeakenedVars (prConstraint pr) uf
    , pvEliminatedVars = deriveEliminatedVars (prConstraint pr) uf
    }
  where uf = sanitizeSnapshotUf (prConstraint pr) (getPresolutionUf (prUnionFind pr))
```

**Status**: NOT STARTED

### Phase 2: Create `Solved`-compatible wrapper (mechanical migration)

**Purpose**: Allow incremental migration — `PresolutionView` implements the same query interface as `Solved`, so consumers can switch one at a time.

Create a typeclass or type alias:
```haskell
-- Option A: typeclass
class HasConstraintView a where
    viewCanonical :: a -> NodeId -> NodeId
    viewConstraint :: a -> Constraint
    viewLookupNode :: a -> NodeId -> Maybe TyNode
    ...

-- Option B: just use PresolutionView directly
```

**Decision**: Option B is simpler and sufficient. No typeclass needed — just replace `Solved` with `PresolutionView` everywhere.

**Status**: NOT STARTED

### Phase 3: Replace `eeSolved` in `ElabEnv`

**Files**: `src/MLF/Elab/Elaborate.hs`

Change:
```haskell
data ElabEnv = ElabEnv
    { eeSolved          :: Solved           -- REMOVE
    , eePresolutionView :: PresolutionView  -- ADD
    , eeGaParents       :: GaBindParents
    , eeEdgeWitnesses   :: IntMap EdgeWitness
    , eeEdgeTraces      :: IntMap EdgeTrace
    , eeEdgeExpansions  :: IntMap Expansion
    , eeScopeOverrides  :: IntMap NodeRef
    }
```

Update all call sites in `elaborateWithEnv` / `elabAlg` that use `eeSolved`:
- `Solved.canonical solved` → `pvCanonical (eePresolutionView env)`
- `Solved.lookupNode solved nid` → `lookupNode (pvConstraint view) (pvCanonical view nid)`
- `Solved.lookupVarBound solved nid` → `lookupVarBound (pvConstraint view) (pvCanonical view nid)`
- `Solved.originalConstraint solved` → `pvConstraint (eePresolutionView env)`
- etc.

**Affected modules** (direct `eeSolved` usage):
| Module | ~Calls | Notes |
|---|---|---|
| `Elab/Elaborate.hs` | 3 | Field decl, construction, destructure |
| `Elab/Run/Pipeline.hs` | 1 | Construction of ElabEnv |

**Status**: NOT STARTED

### Phase 4: Migrate Phi/Omega/Reify consumers

These are the heaviest consumers of `Solved.*` APIs:

| Module | Distinct APIs | ~Total calls |
|---|---|---|
| `Elab/Phi/Omega.hs` | 5 | ~30 |
| `Reify/Core.hs` | 7 | ~25 |
| `Elab/Elaborate.hs` | 4 | ~12 |
| `Elab/Run/Pipeline.hs` | 6 | ~10 |
| `Elab/Run/ResultType/Fallback.hs` | 6 | ~8 |
| `Elab/Run/Scope.hs` | 2 | ~4 |
| `Elab/Run/Generalize.hs` | 2 | ~3 |
| `Elab/Run/Generalize/Constraint.hs` | 1 | ~1 |
| `Elab/Run/TypeOps.hs` | 2 | ~2 |
| `Elab/Run/ResultType/Ann.hs` | 2 | ~4 |
| `Elab/Legacy.hs` | 2 | ~2 |
| `Elab/Phi/Translate.hs` | 2 | ~4 |
| `Elab/Phi/Context.hs` | 1 | ~1 |
| `Elab/Phi/Env.hs` | 1 | field decl |
| `Elab/Phi/IdentityBridge.hs` | 1 | ~1 |
| `Elab/Phi/TestOnly.hs` | 1 | ~1 |
| `Elab/Generalize.hs` | 1 | ~1 |
| `Reify/TypeOps.hs` | 1 | ~1 |
| `Util/Order.hs` | 2 | ~4 |
| `Presolution/Plan.hs` | 3 | ~5 |
| `Presolution/Plan/Context.hs` | 1 | ~1 |
| `Presolution/Plan/ReifyPlan.hs` | 1 | field |
| `Presolution/Plan/BinderPlan/Types.hs` | 1 | field |

**Migration approach**: Sub-phases by module cluster:

- **4a**: Phi cluster — `Phi/Env.hs` (field swap), `Phi/Context.hs`, `Phi/Translate.hs`, `Phi/Omega.hs`, `Phi/IdentityBridge.hs`, `Phi/TestOnly.hs`
- **4b**: Reify cluster — `Reify/Core.hs`, `Reify/TypeOps.hs`
- **4c**: Run cluster — `Run/Pipeline.hs`, `Run/Scope.hs`, `Run/Generalize.hs`, `Run/Generalize/Constraint.hs`, `Run/TypeOps.hs`, `Run/ResultType/*.hs`
- **4d**: Support — `Util/Order.hs`, `Legacy.hs`, `Generalize.hs`
- **4e**: Presolution Plan — `Presolution/Plan.hs`, `Plan/Context.hs`, `Plan/ReifyPlan.hs`, `Plan/BinderPlan/Types.hs`

**Status**: NOT STARTED

### Phase 5: Remove `buildSolvedFromPresolutionSnapshot` and pipeline solved-builder

**Files**: `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Constraint/Solved.hs`

- Remove `buildSolvedFromPresolutionSnapshot` function
- Remove `runPipelineElabWithSolvedBuilder` (or inline the simplified version)
- Pipeline now: presolution → `fromPresolutionResult` → `PresolutionView` → `ElabEnv`
- Remove `Solved.fromPreRewriteState` / `solveResultFromSnapshot` (no longer called)

Handle the pipeline mutation steps that currently use Solved:
- `pruneBindParentsSolved` → operate on `PresolutionView.pvConstraint.cBindParents` directly
- `rebuildWithConstraint` → update `pvConstraint` field (simple record update)
- `setSolvedConstraint` for generalization → update `pvConstraint` with the generalization-modified constraint
- `validateCanonicalGraphStrict` → validate directly on `pvConstraint` + `pvCanonicalMap`

**Status**: NOT STARTED

### Phase 6: Remove `Solved` module (or reduce to test-only)

**Files**: `src/MLF/Constraint/Solved.hs`, `mlf2.cabal`

- If all production code has migrated, move `Solved` to test-only or delete it
- `mkTestSolved` (used in `Reify/Core.hs` for test helpers) can be replaced with `PresolutionView` test constructors
- Update `mlf2.cabal` module lists

**Test-only consumers**:
| Module | Usage |
|---|---|
| `test/Constraint/SolvedSpec.hs` | dedicated spec — may be rewritten or removed |
| `test/AlignmentInvariantSpec.hs` | uses Solved in helpers |
| `test/ElaborationSpec.hs` | pipeline helpers |
| `test/PipelineSpec.hs` | pipeline helpers |
| `test/ConstraintGenSpec.hs` | pipeline helpers |
| `test/SpecUtil.hs` | shared test utilities |
| `test/Parity/FrozenArtifacts.hs` | frozen parity artifacts |
| `test/Phi/IdentityBridgeSpec.hs` | identity bridge tests |

**Status**: NOT STARTED

### Phase 7: Validate & clean up

- `cabal build all && cabal test` must pass
- Remove any dead `Solved` re-exports from `MLF.Constraint.Types`, `MLF.Elab.Pipeline`, etc.
- Update `implementation_notes.md` and `docs/thesis-deviations.yaml`
- Update the transformation mechanism table: set "Elaboration input" row to `Thesis-exact = Yes`
- CHANGELOG entry

**Status**: NOT STARTED

---

## Risk Assessment

| Risk | Severity | Mitigation |
|---|---|---|
| `canonicalConstraint` relied on reconstructed canonical fields, not just UF+original | Medium | `PresolutionView` can lazily reconstruct the same thing; verify presolution's post-unification constraint already has canonical structure |
| `rebuildWithConstraint` / mutation patterns in pipeline | Medium | Replace with simple record-update on `PresolutionView`; pipeline steps that mutate constraint for generalization work the same way |
| `Presolution/Plan` uses `Solved` in closures (`prPlanBuilder`) | High | Plan-builder captures `Solved` at construction time; must change capture to `PresolutionView` or factor out the needed query functions |
| `mkTestSolved` usage in Reify for test helpers | Low | Provide `mkTestPresolutionView` |
| Frozen parity tests depend on `Solved`-based pipeline | Low | These are regression artifacts; update them or accept breakage as intentional |

## Decision Log

| Decision | Rationale |
|---|---|
| Use a plain record (`PresolutionView`) instead of a typeclass | Simpler, fewer abstraction layers, matches thesis's direct-access model |
| Phase incrementally via shim | Allows `cabal test` to pass after each phase; bisectable history |
| Keep `canonicalConstraint` capability | Some consumers (Presolution/Plan, Reify, Generalize) need the canonical-domain constraint; derive it lazily from UF + original |
| Don't remove Solved.hs in first pass | Leave it for test-only use until all production code migrates cleanly |
