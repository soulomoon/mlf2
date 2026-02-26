# Remaining solvedConstraint Migration Plan

**Goal:** Eliminate the last 5 `solvedConstraint` callers and remove `ebCanonicalConstraint` from `EquivBackend`.

**Status:** Completed — 12 of 12 production callers migrated, test callers migrated, and `ebCanonicalConstraint` removed from `EquivBackend` (2026-02-26).

**Prerequisite:** Branch `equiv-class-abstraction` (M5a-M5d complete).

---

## Execution Update (2026-02-26)

- Tier 1-4 migration targets listed in this plan have been applied in production modules (`Pipeline`, `Reify/Core`, `Fallback`, `Presolution/Plan`, `Elab/Run/Generalize`).
- Production and test code no longer call `Solved.solvedConstraint`; remaining references are compatibility alias/docs-only.
- Phase 6 is complete: `MLF.Constraint.Solved.EquivBackend` no longer stores `ebCanonicalConstraint`; canonical graph state is stored as explicit canonical slices and reconstructed via `canonicalConstraint`.

## Problem Analysis

All 5 remaining callers share two patterns:

1. **Canonical-domain node lookups:** They extract `cNodes` from the canonical constraint and look up nodes by canonical IDs. The original constraint's `cNodes` is keyed by pre-merge IDs, so `lookupNodeIn nodes (canonical nid)` fails for merged nodes.

2. **Post-solve state dependencies:** They rely on var bounds, weakened vars, and eliminated-node status that only exist in the canonical (post-solve) constraint.

## Architecture: Expand the Opaque Solved API

Rather than making callers work with raw `originalConstraint`, expand the `Solved` API to cover the operations these callers need. The Solved module already canonicalizes internally — callers should use it instead of extracting the raw constraint.

### New Solved API surface needed

```haskell
-- Already exists:
canonical       :: Solved -> NodeId -> NodeId
lookupNode      :: Solved -> NodeId -> Maybe TyNode      -- canonicalizes, looks up in original
lookupVarBound  :: Solved -> NodeId -> Maybe NodeId       -- canonicalizes, looks up in original
lookupBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
-- Needed: canonical-domain iteration and post-solve state
allCanonicalNodes   :: Solved -> [TyNode]           -- nodes keyed by canonical IDs
canonicalNodes      :: Solved -> NodeMap TyNode      -- raw canonical node map (for iteration)
isEliminatedVar     :: Solved -> NodeId -> Bool      -- post-solve eliminated status
weakenedVars        :: Solved -> IntSet              -- post-solve weakened var set
canonicalBindParents :: Solved -> BindParents         -- bind parents in canonical domain
canonicalGenNodes   :: Solved -> GenNodeMap GenNode   -- gen nodes in canonical domain

-- For read-write callers:
rebuildWithNodes    :: Solved -> NodeMap TyNode -> Solved  -- replace canonical nodes
rebuildWithBindParents :: Solved -> BindParents -> Solved  -- replace canonical bind parents
```

The key insight: callers that iterate `cNodes` need the *canonical* node map,
not the original. But they should get it through the Solved API, not by
extracting the raw constraint.

---

## Caller-by-Caller Migration Strategy

### Tier 1: Moderate — Pipeline.hs (2 call sites)

**Current pattern:**
```haskell
pruneBindParentsConstraint (Solved.solvedConstraint solvedView)
Solved.solvedConstraint solvedClean  -- for validateSolvedGraphStrict
```

**Migration:**
1. Add `pruneBindParentsSolved :: Solved -> Solved` that operates through the API
2. Change `validateSolvedGraphStrict` to accept `Solved` directly (or add a
   `validateSolved :: Solved -> Maybe SolveError` wrapper)
3. Remove both `solvedConstraint` calls

**Complexity:** Moderate. Both functions are self-contained.

### Tier 2: Moderate-Hard — Reify/Core.hs `reifyWith` (1 call site)

**Current pattern:**
```haskell
constraint = Solved.solvedConstraint solved
nodes = cNodes constraint
-- then ~50 lookupNodeIn nodes calls throughout 650 lines
```

**Migration:**
1. Replace `lookupNodeIn nodes nid` with `Solved.lookupNode solved nid`
2. Replace `VarStore.lookupVarBound constraint nid` with `Solved.lookupVarBound solved nid`
3. Replace `cGenNodes constraint` with `Solved.genNodes solved`
4. Replace `cWeakenedVars constraint` with `Solved.weakenedVars solved` (new API)
5. Replace `canonicalizeBindParentsUnder canonical constraint` with
   `Solved.canonicalBindParents solved` or equivalent
6. Handle `VarStore.isEliminatedVar` via new `Solved.isEliminatedVar`

**Complexity:** Moderate-Hard. Many call sites but uniform pattern. The risk is
copy nodes (created during instantiation) that don't exist in original — need
to verify `Solved.lookupNode` handles these.

**Blocker investigation:** Do copy nodes exist in the original constraint?
Copy nodes are created during presolution (before solving), so they should
exist in the pre-rewrite snapshot. Verify by checking `fromPreRewriteStateStrict`.

### Tier 3: Hard — Fallback.hs (5 call sites)

**Current pattern:**
```haskell
solvedConstraintOf = Solved.solvedConstraint
-- READ: cNodes iteration, lookupNodeIn, cBindParents, allGenNodes
-- WRITE: node patch at canonical ID, rebuild via setSolvedConstraint
```

**Migration:**
1. Replace read-only lookups with Solved API calls
2. For the write pattern (line 473 — patching a TyVar bound):
   - Use `Solved.lookupNode solved nid` to find the node
   - Build patched node
   - Use new `rebuildWithNodes` or `patchNode :: Solved -> NodeId -> TyNode -> Solved`
3. Replace `bindingScopeRef constraint` with Solved-based binding query
4. Replace `allGenNodes constraint` with `Solved.genNodes solved`

**Complexity:** Hard. The write pattern requires a new `patchNode` API.
The 250-line function has many interleaved reads.

### Tier 4: Hard — Generalize.hs + Phases 1-4 + Finalize (1 call site, 16 ops across 6 files)

**Current pattern:**
```haskell
geSolvedConstraint = Solved.solvedConstraint solved
-- Phases build merged constraint: solvedConstraint { cNodes = ..., cBindParents = ..., cGenNodes = ... }
-- upperConstraint pattern for isUpperRef checks
```

**Migration approach A — Pass Solved through phases:**
1. Store `Solved` in `GeneralizeEnv` instead of raw constraint
2. Phases use Solved API for reads
3. For the `upperConstraint` pattern, add `Solved.withNodes :: Solved -> NodeMap -> Solved`
   that creates a temporary Solved with modified nodes
4. For the final `constraintForGen` output, use `rebuildWithNodes` + `rebuildWithBindParents`

**Migration approach B — Keep canonical constraint for Generalize only:**
1. Accept that Generalize fundamentally operates in canonical domain
2. Rename `solvedConstraint` to `canonicalConstraint` (semantic clarity)
3. Restrict its use to Generalize pipeline only
4. This is pragmatic but doesn't fully eliminate the canonical constraint

**Recommended:** Approach B for now. The Generalize pipeline's job IS to merge
base and solved structure — it's the one place where canonical domain is natural.
Approach A is possible but high-risk for a 6-file refactor.

### Tier 4b: Hard — Plan.hs (2 call sites, 19 test failures on swap)

**Current pattern:**
```haskell
constraint = Solved.solvedConstraint solved
-- cNodes, cBindParents, cGenNodes, VarStore queries, cWeakenedVars
```

**Migration:** Same as Generalize — Plan.hs feeds into the Generalize pipeline.
If Generalize keeps canonical constraint (Approach B), Plan.hs keeps it too.
If Generalize migrates (Approach A), Plan.hs migrates with it.

---

## Recommended Execution Order

### Phase 1: Expand Solved API (prerequisite for all)

**Task A: Add canonical-domain query exports**
- `weakenedVars :: Solved -> IntSet`
- `isEliminatedVar :: Solved -> NodeId -> Bool`
- `canonicalBindParents :: Solved -> BindParents` (canonicalized from original)
- `canonicalGenNodes :: Solved -> GenNodeMap GenNode`

**Task B: Add mutation helpers**
- `patchNode :: Solved -> NodeId -> (TyNode -> TyNode) -> Solved`
- `pruneBindParentsSolved :: Solved -> Solved`

### Phase 2: Migrate Tier 1 — Pipeline.hs

Migrate `pruneBindParentsConstraint` and `validateSolvedGraphStrict`.
Estimated: 1 task, moderate.

### Phase 3: Migrate Tier 2 — Reify/Core.hs

Replace ~50 `lookupNodeIn` calls with `Solved.lookupNode`.
Estimated: 1-2 tasks, moderate-hard.

### Phase 4: Migrate Tier 3 — Fallback.hs

Replace reads with Solved API, use `patchNode` for write.
Estimated: 1-2 tasks, hard.

### Phase 5: Rename or migrate Tier 4 — Generalize + Plan

Either rename `solvedConstraint` → `canonicalConstraint` (pragmatic) or
do the full Approach A migration (ambitious).
Estimated: 1 task (rename) or 3-5 tasks (full migration).

### Phase 6: Remove ebCanonicalConstraint

Once all callers are migrated or using renamed API, remove the field.
Estimated: 1 task.

---

## Summary

| Phase | Files | Complexity | Tasks |
|-------|-------|-----------|-------|
| 1. Expand Solved API | Solved.hs | Moderate | 2 |
| 2. Pipeline.hs | 1 file | Moderate | 1 |
| 3. Reify/Core.hs | 1 file | Moderate-Hard | 1-2 |
| 4. Fallback.hs | 1 file | Hard | 1-2 |
| 5. Generalize + Plan | 8 files | Hard (or trivial rename) | 1-5 |
| 6. Remove field | Solved.hs | Trivial | 1 |
| **Total** | | | **7-13 tasks** |

The pragmatic path (Phases 1-4 + rename in Phase 5) is ~8 tasks.
The thesis-exact path (Phases 1-4 + full Approach A in Phase 5) is ~13 tasks.
```
