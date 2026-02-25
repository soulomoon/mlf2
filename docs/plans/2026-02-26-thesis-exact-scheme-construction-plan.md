# Thesis-Exact Scheme Construction Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Eliminate the DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP deviation by making the original (pre-solving) constraint primary for all post-solve operations, remove LegacyBackend, and remove all escape hatches.

**Architecture:** Remove LegacyBackend first (M5a) so subsequent changes touch one code path. Then migrate Solved.hs queries to original constraint (M5b). Then pass original constraint + identity canonical to scheme reification (M5c). Finally remove deviation docs (M5d).

**Tech Stack:** Haskell, cabal, hspec test framework

**Design doc:** `docs/plans/2026-02-26-thesis-exact-scheme-construction-design.md`

---

## Milestone 5a: Remove LegacyBackend and Escape Hatches

### Task 1: Add mkTestSolved helper to Solved.hs

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs`

**Step 1: Add mkTestSolved that constructs EquivBackend from constraint + union-find**

Add to the export list and implement:

```haskell
-- Export list: add mkTestSolved
    mkTestSolved,

-- | Test helper: construct an EquivBackend from a constraint and
-- union-find map. Same interface as 'mkSolved' but produces the
-- EquivBackend. The constraint serves as both original and canonical
-- (no pre-rewrite snapshot needed for tests with empty union-find).
mkTestSolved :: Constraint -> IntMap NodeId -> Solved
mkTestSolved c uf =
    let canonicalMap = buildCanonicalMap uf c
        equivClasses = buildEquivClasses canonicalMap c
    in Solved EquivBackend
        { ebCanonicalMap = canonicalMap
        , ebCanonicalConstraint = c
        , ebEquivClasses = equivClasses
        , ebOriginalConstraint = c
        }
```

Note: For tests with empty union-find, original == canonical constraint.
For tests with non-empty union-find, we still use the same constraint
as both original and canonical — the canonical map captures the merges.

**Step 2: Run tests**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS (no callers changed yet)

**Step 3: Commit**

```bash
git add src/MLF/Constraint/Solved.hs
git commit -m "feat: add mkTestSolved helper for EquivBackend construction"
```

### Task 2: Migrate ElaborationSpec.hs from mkSolved to mkTestSolved

**Files:**
- Modify: `test/ElaborationSpec.hs`

**Step 1: Replace mkSolved alias with mkTestSolved**

At line 107, change:
```haskell
mkSolved = Solved.mkSolved
```
to:
```haskell
mkSolved = Solved.mkTestSolved
```

Update the import at the top of the file to include `mkTestSolved`
instead of (or in addition to) `mkSolved`.

This is a mechanical change — all ~18 call sites in ElaborationSpec.hs
use `mkSolved c IntMap.empty`, which `mkTestSolved` handles identically.

**Step 2: Run ElaborationSpec tests**

Run: `cabal test --test-show-details=direct --test-option='-m "Elaboration"' 2>&1 | tail -20`
Expected: PASS

**Step 3: Commit**

```bash
git add test/ElaborationSpec.hs
git commit -m "test: migrate ElaborationSpec from mkSolved to mkTestSolved"
```

### Task 3: Migrate IdentityBridgeSpec.hs from mkSolved to mkTestSolved

**Files:**
- Modify: `test/Phi/IdentityBridgeSpec.hs`

**Step 1: Replace mkTestSolved helper**

At line 19, change:
```haskell
mkTestSolved uf = mkSolved emptyConstraint uf
```
to use `Solved.mkTestSolved`:
```haskell
mkTestSolved uf = Solved.mkTestSolved emptyConstraint uf
```

Update imports to use `mkTestSolved` from Solved instead of `mkSolved`.

**Step 2: Run IdentityBridgeSpec tests**

Run: `cabal test --test-show-details=direct --test-option='-m "IdentityBridge"' 2>&1 | tail -20`
Expected: PASS

**Step 3: Commit**

```bash
git add test/Phi/IdentityBridgeSpec.hs
git commit -m "test: migrate IdentityBridgeSpec from mkSolved to mkTestSolved"
```

### Task 4: Migrate SolvedSpec.hs from mkSolved to mkTestSolved

**Files:**
- Modify: `test/Constraint/SolvedSpec.hs`

**Step 1: Replace mkSolved calls**

Lines 81, 257, 357 use `mkSolved`. Replace with `mkTestSolved`.
Line 357 also uses `toSolveResult` — leave that for now, migrate
in the toSolveResult removal task.

**Step 2: Run SolvedSpec tests**

Run: `cabal test --test-show-details=direct --test-option='-m "Solved"' 2>&1 | tail -20`
Expected: PASS

**Step 3: Commit**

```bash
git add test/Constraint/SolvedSpec.hs
git commit -m "test: migrate SolvedSpec from mkSolved to mkTestSolved"
```

### Task 5: Migrate SolveSpec.hs from mkSolved to mkTestSolved

**Files:**
- Modify: `test/SolveSpec.hs`

**Step 1: Replace mkSolveResult helper**

Line 30: `mkSolveResult c uf = Solved.toSolveResult (Solved.mkSolved c uf)`

This uses both `mkSolved` and `toSolveResult`. Replace with
`mkTestSolved` and find an alternative to `toSolveResult`.
If `toSolveResult` is still needed here, keep it for now and
migrate in the toSolveResult removal task.

Lines 24, 27 use `fromSolveResult` — these extract constraint and
union-find from SolveResult for assertions. Replace with direct
Solved API calls.

**Step 2: Run SolveSpec tests**

Run: `cabal test --test-show-details=direct --test-option='-m "Solve"' 2>&1 | tail -20`
Expected: PASS

**Step 3: Commit**

```bash
git add test/SolveSpec.hs
git commit -m "test: migrate SolveSpec from mkSolved to mkTestSolved"
```

### Task 6: Migrate PipelineSpec.hs from escape hatches

**Files:**
- Modify: `test/PipelineSpec.hs`

**Step 1: Replace unionFind calls with Solved.canonical**

Lines 79, 203, 240, 333, 339 use `Solved.unionFind`. Replace:
- `canonical (Solved.unionFind res) rootRedirected` → `Solved.canonical res rootRedirected`
- `makeCanonicalizer (Solved.unionFind solved) ...` → use `Solved.canonical` directly or build canonicalizer from canonical function
- `IntMap.null (Solved.unionFind solved)` → alternative check (e.g., always assume canonicalization may be needed, or add a `hasEquivalences :: Solved -> Bool` query)

Lines 83, 174, 188, 272 use `Solved.solvedConstraint`. Replace with
`Solved.originalConstraint` (once available) or keep as-is until M5b.

Lines 87, 201 use `Solved.toSolveResult`. Replace with Solved API.

**Step 2: Run PipelineSpec tests**

Run: `cabal test --test-show-details=direct --test-option='-m "Pipeline"' 2>&1 | tail -20`
Expected: PASS

**Step 3: Commit**

```bash
git add test/PipelineSpec.hs
git commit -m "test: migrate PipelineSpec from escape hatches to Solved API"
```

### Task 7: Migrate production unionFind callers

**Files:**
- Modify: `src/MLF/Elab/Run/Pipeline.hs:91,100`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs:179`
- Modify: `src/MLF/Constraint/Presolution/Plan/Context.hs:310`
- Modify: `src/MLF/Elab/Generalize.hs:408`

**Step 1: Audit each caller**

Each of these callers uses `Solved.unionFind` to reconstruct a Solved
with a modified constraint: `Solved.mkSolved newConstraint (Solved.unionFind old)`.

The pattern is: take the union-find from one Solved, pair it with a
different constraint, and create a new Solved. Replace with
`Solved.mkTestSolved newConstraint (Solved.unionFind old)` temporarily,
then replace `unionFind` with a new `rebuildWithConstraint` helper:

```haskell
-- | Rebuild a Solved with a different constraint, preserving the
-- canonical map. Used by callers that modify the constraint
-- (e.g., alias insertion, canonicalization).
rebuildWithConstraint :: Solved -> Constraint -> Solved
rebuildWithConstraint (Solved EquivBackend { ebCanonicalMap = cm, ebEquivClasses = ec, ebOriginalConstraint = orig }) c =
    Solved EquivBackend
        { ebCanonicalMap = cm
        , ebCanonicalConstraint = c
        , ebEquivClasses = ec
        , ebOriginalConstraint = orig
        }
```

Add this to Solved.hs exports.

**Step 2: Replace each caller**

- `Pipeline.hs:93`: `Solved.toSolveResult (Solved.mkSolved cCanon uf)` → use `rebuildWithConstraint` + remove `toSolveResult`
- `Fallback.hs:179`: `Solved.mkSolved c' (Solved.unionFind ...)` → `rebuildWithConstraint solved c'`
- `Plan/Context.hs:310`: `Solved.mkSolved constraintForReify (Solved.unionFind solvedRes)` → `rebuildWithConstraint solvedRes constraintForReify`
- `Generalize.hs:408`: `Solved.mkSolved constraintAlias (Solved.unionFind solvedRes)` → `rebuildWithConstraint solvedRes constraintAlias`

**Step 3: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Solved.hs src/MLF/Elab/Run/Pipeline.hs \
    src/MLF/Elab/Run/ResultType/Fallback.hs \
    src/MLF/Constraint/Presolution/Plan/Context.hs \
    src/MLF/Elab/Generalize.hs
git commit -m "refactor: replace unionFind+mkSolved pattern with rebuildWithConstraint"
```

### Task 8: Migrate production toSolveResult callers

**Files:**
- Modify: `src/MLF/Reify/Core.hs:710,780,785`
- Modify: `src/MLF/Elab/Phi/Omega.hs:443`
- Modify: `src/MLF/Elab/Phi/Context.hs:66`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs:180`

**Step 1: Audit each caller**

These callers convert Solved → SolveResult to pass to reification
functions (`reifyTypeWithNamesNoFallback`, `reifyBoundWithNames`,
`Order.orderKeysFromRoot`). The reification functions accept
`SolveResult` but only extract constraint + canonical from it.

Approach: Change reification functions to accept `Solved` directly,
or provide `Solved`-based wrappers. The cleanest path is to add
Solved-accepting overloads in Reify/Core.hs.

**Step 2: Add Solved-based reification wrappers**

In `src/MLF/Reify/Core.hs`, add:

```haskell
reifyTypeWithNamesNoFallbackSolved :: Solved -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallbackSolved solved subst nid =
    reifyTypeWithNamesNoFallback (Solved.toSolveResult solved) subst nid

reifyBoundWithNamesSolved :: Solved -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNamesSolved solved subst nid =
    reifyBoundWithNames (Solved.toSolveResult solved) subst nid
```

These are temporary bridges — they still use `toSolveResult` internally
but callers no longer need to. In M5c we'll change the internals.

**Step 3: Migrate callers to Solved-based wrappers**

- `Omega.hs:443`: `reifyBoundWithNames (Solved.toSolveResult solved)` → `reifyBoundWithNamesSolved solved`
- `Context.hs:66`: `Order.orderKeysFromRoot (Solved.toSolveResult res)` → needs Order.hs to accept Solved (add wrapper there too)

**Step 4: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS

**Step 5: Commit**

```bash
git add src/MLF/Reify/Core.hs src/MLF/Elab/Phi/Omega.hs \
    src/MLF/Elab/Phi/Context.hs src/MLF/Util/Order.hs
git commit -m "refactor: add Solved-based reification wrappers, migrate toSolveResult callers"
```

### Task 9: Migrate production solvedConstraint callers (batch 1 — Omega/Translate)

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs` (9 call sites)
- Modify: `src/MLF/Elab/Phi/Translate.hs` (4 call sites)

**Step 1: Add originalConstraint export to Solved.hs**

```haskell
-- | The original (pre-solving) constraint. Primary for all
-- post-solve operations (thesis-exact).
originalConstraint :: Solved -> Constraint
originalConstraint (Solved EquivBackend { ebOriginalConstraint = c }) = c
originalConstraint (Solved LegacyBackend { lbConstraint = c }) = c
```

Add to export list. The LegacyBackend branch returns `lbConstraint`
as a temporary fallback (removed when LegacyBackend is deleted).

**Step 2: Replace solvedConstraint with originalConstraint in Omega.hs**

Replace all 9 uses of `Solved.solvedConstraint solved` with
`Solved.originalConstraint solved` at lines:
156, 165, 214, 219, 425, 441, 866, 1144, 1157

Note: At this stage, for EquivBackend, `originalConstraint` returns
`ebOriginalConstraint` while `solvedConstraint` returns
`ebCanonicalConstraint`. These may differ. Run tests after each
replacement to catch breakage early. If a specific caller breaks,
it may need `ebCanonicalConstraint` — add a `canonicalConstraint`
query for those cases.

**Step 3: Replace solvedConstraint with originalConstraint in Translate.hs**

Replace all 4 uses at lines: 433, 690, 738, 774

**Step 4: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS (or failures that need caller fixes)

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Solved.hs src/MLF/Elab/Phi/Omega.hs \
    src/MLF/Elab/Phi/Translate.hs
git commit -m "refactor: migrate Omega/Translate from solvedConstraint to originalConstraint"
```

### Task 10: Migrate production solvedConstraint callers (batch 2 — remaining)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs:116,1161`
- Modify: `src/MLF/Elab/Legacy.hs:48`
- Modify: `src/MLF/Elab/Phi/Context.hs:58`
- Modify: `src/MLF/Elab/Run/Generalize.hs:106`
- Modify: `src/MLF/Elab/Run/Pipeline.hs:96,130`
- Modify: `src/MLF/Elab/Run/ResultType/Ann.hs:99,121,292,303`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs:175,191`
- Modify: `src/MLF/Elab/Run/ResultType.hs:47`
- Modify: `src/MLF/Elab/Run/Scope.hs:85`
- Modify: `src/MLF/Elab/Run/TypeOps.hs:57`
- Modify: `src/MLF/Reify/Core.hs:97,791,835`
- Modify: `src/MLF/Reify/TypeOps.hs:354`
- Modify: `src/MLF/Util/Order.hs:43,48`

**Step 1: Replace each solvedConstraint call with originalConstraint**

Work through each file. For each replacement, consider whether the
caller needs original or canonical constraint semantics:

- Node lookups (`NodeAccess.lookupNode`, `cNodes`) → original is fine
  (all nodes exist in original)
- Gen node queries (`NodeAccess.allGenNodes`) → original is fine
- Binding tree queries (`Binding.orderedBinders`, `bindingPathToRoot`,
  `bindingLCA`) → original is fine (thesis model)
- Order key computation (`Order.hs`) → may need canonical constraint
  if order keys depend on solved structure. Test and see.
- Reification (`Reify/Core.hs:97`) → will be changed to original in
  M5c anyway, but safe to switch now

**Step 2: Run full test suite after each file**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS (fix failures as they arise)

**Step 3: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Legacy.hs \
    src/MLF/Elab/Phi/Context.hs src/MLF/Elab/Run/Generalize.hs \
    src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType/Ann.hs \
    src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType.hs \
    src/MLF/Elab/Run/Scope.hs src/MLF/Elab/Run/TypeOps.hs \
    src/MLF/Reify/Core.hs src/MLF/Reify/TypeOps.hs src/MLF/Util/Order.hs
git commit -m "refactor: migrate remaining solvedConstraint callers to originalConstraint"
```

### Task 11: Migrate test solvedConstraint callers

**Files:**
- Modify: `test/ElaborationSpec.hs:394,422,465,2245,2277`
- Modify: `test/PipelineSpec.hs:83,174,188,272`
- Modify: `test/SolveSpec.hs:24`

**Step 1: Replace solvedConstraint with originalConstraint in tests**

Same mechanical replacement. Tests that extract the constraint for
assertions may need updating if the original constraint differs from
the canonical constraint.

**Step 2: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS

**Step 3: Commit**

```bash
git add test/ElaborationSpec.hs test/PipelineSpec.hs test/SolveSpec.hs
git commit -m "test: migrate test solvedConstraint callers to originalConstraint"
```

### Task 12: Remove LegacyBackend constructor and old APIs

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs`

**Step 1: Remove LegacyBackend from SolvedBackend**

Delete the `LegacyBackend` constructor and all pattern matches on it.
This includes removing:
- `LegacyBackend` data constructor (lines 87-90)
- All `LegacyBackend` branches in: `canonical`, `lookupNode`,
  `allNodes`, `lookupBindParent`, `bindParents`, `instEdges`,
  `genNodes`, `lookupVarBound`, `toSolveResult`, `unionFind`,
  `solvedConstraint`, `classMembers`, `originalNode`,
  `originalBindParent`, `wasOriginalBinder`, `originalConstraint`

**Step 2: Remove old constructors and escape hatches**

Remove from exports and implementation:
- `fromSolveResult` — no longer needed
- `mkSolved` — replaced by `mkTestSolved`
- `toSolveResult` — all callers migrated
- `unionFind` — all callers migrated
- `solvedConstraint` — all callers migrated to `originalConstraint`

**Step 3: Clean up imports**

Remove `SolveResult(..)` import from Solved.hs if no longer needed.
Remove `frWith` import if no longer needed.

**Step 4: Build**

Run: `cabal build 2>&1 | tail -30`
Expected: BUILD SUCCESS. If there are compile errors, fix remaining
callers that still reference removed APIs.

**Step 5: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS

**Step 6: Commit**

```bash
git add src/MLF/Constraint/Solved.hs
git commit -m "refactor: remove LegacyBackend constructor and escape hatches"
```

### Task 13: Verify no LegacyBackend references remain

**Files:**
- All files in `src/` and `test/`

**Step 1: Grep for LegacyBackend references**

Run: `grep -r "LegacyBackend\|fromSolveResult\|mkSolved\b\|toSolveResult\|unionFind\|solvedConstraint" src/ test/ --include='*.hs' -l`

Expected: No matches (or only the Solved.hs module itself if
re-exports exist for deprecation warnings).

If matches remain, fix them.

**Step 2: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS (821+ tests)

**Step 3: Commit (if any fixes needed)**

```bash
git commit -am "chore: clean up remaining LegacyBackend references"
```

---

## Milestone 5b: Original Constraint as Primary in Solved.hs

### Task 14: Switch lookupNode to use ebOriginalConstraint

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs`

**Step 1: Change lookupNode implementation**

With LegacyBackend removed, `lookupNode` has one branch. Change:

```haskell
lookupNode s@(Solved EquivBackend { ebCanonicalConstraint = c }) nid =
    NA.lookupNode c (canonical s nid)
```
to:
```haskell
lookupNode s@(Solved EquivBackend { ebOriginalConstraint = c }) nid =
    NA.lookupNode c (canonical s nid)
```

Note: Still canonicalizes the nid first. The canonical representative
exists in the original constraint because solving doesn't create nodes.

**Step 2: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS or failures from callers that assumed canonical-view
node structure. Fix any failures.

**Step 3: Commit**

```bash
git add src/MLF/Constraint/Solved.hs
git commit -m "refactor: switch lookupNode to original constraint"
```

### Task 15: Switch allNodes to use ebOriginalConstraint

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs`

**Step 1: Change allNodes implementation**

```haskell
allNodes (Solved EquivBackend { ebOriginalConstraint = c }) = NA.allNodes c
```

**Step 2: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS

**Step 3: Commit**

```bash
git add src/MLF/Constraint/Solved.hs
git commit -m "refactor: switch allNodes to original constraint"
```

### Task 16: Switch lookupBindParent and bindParents to original

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs`

**Step 1: Change both implementations**

```haskell
lookupBindParent (Solved EquivBackend { ebOriginalConstraint = c }) ref =
    NA.lookupBindParent c ref

bindParents (Solved EquivBackend { ebOriginalConstraint = c }) = cBindParents c
```

**Step 2: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS or failures from binding tree walkers. Fix any failures
by having callers canonicalize parent references after lookup.

**Step 3: Commit**

```bash
git add src/MLF/Constraint/Solved.hs
git commit -m "refactor: switch lookupBindParent/bindParents to original constraint"
```

### Task 17: Switch instEdges, genNodes, lookupVarBound to original

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs`

**Step 1: Change all three implementations**

```haskell
instEdges (Solved EquivBackend { ebOriginalConstraint = c }) = cInstEdges c

genNodes (Solved EquivBackend { ebOriginalConstraint = c }) = cGenNodes c

lookupVarBound s@(Solved EquivBackend { ebOriginalConstraint = c }) nid =
    NA.lookupVarBound c (canonical s nid)
```

**Step 2: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS or failures from bound reification. Fix any failures.
If `lookupVarBound` causes widespread breakage, add
`lookupCanonicalVarBound` that preserves old behavior and migrate
incrementally.

**Step 3: Commit**

```bash
git add src/MLF/Constraint/Solved.hs
git commit -m "refactor: switch instEdges/genNodes/lookupVarBound to original constraint"
```

### Task 18: Remove ebCanonicalConstraint field

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs`

**Step 1: Audit remaining uses of ebCanonicalConstraint**

Search for `ebCanonicalConstraint` in Solved.hs. If no queries read
from it anymore, it can be removed from the EquivBackend record.

If `rebuildWithConstraint` (Task 7) still writes to it, update that
helper to only modify `ebOriginalConstraint`, or keep both fields
in sync.

**Step 2: Remove field if safe**

Remove `ebCanonicalConstraint` from the `EquivBackend` record.
Update `fromPreRewriteStateStrict`, `mkTestSolved`,
`rebuildWithConstraint` to not set it.

If callers still need the canonical constraint for specific cases,
keep the field but mark it as deprecated.

**Step 3: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Solved.hs
git commit -m "refactor: remove ebCanonicalConstraint field from EquivBackend"
```

---

## Milestone 5c: Original Constraint in Generalize.hs and Reify

### Task 19: Pass identity canonical to scheme type reification

**Files:**
- Modify: `src/MLF/Elab/Generalize.hs`
- Modify: `src/MLF/Reify/Core.hs`

This is the critical change that preserves solved-away binders in
scheme types. The reifier must NOT follow canonical links when
rendering the type body for scheme construction.

**Step 1: Understand the reification data flow**

In Generalize.hs, the reification environment is built at lines 345-372.
The `geCanonical` field provides the canonical function. The constraint
comes from `geConstraint`.

There are two reification paths (lines 431-436):
- `useConstraintReify = True`: calls `reifyTypeWithNamesNoFallbackOnConstraint`
  which creates a temporary Solved with empty union-find (identity canonical)
- `useConstraintReify = False`: calls `reifyTypeWithNamesNoFallback` which
  uses the full Solved with real canonical function

The `useConstraintReify` path already uses identity canonical! The
question is whether this path is always taken for scheme construction.

**Step 2: Verify useConstraintReify is set for scheme construction**

Read `src/MLF/Elab/Generalize.hs` to find where `useConstraintReify`
is set. If it's always true for the scheme reification path, then
the identity canonical is already in place and the issue is that
`reifyTypeWithNamesNoFallbackOnConstraint` receives the solved
constraint instead of the original constraint.

**Step 3: Change constraint source for scheme reification**

In `reifyTypeWithNamesNoFallbackOnConstraint` (Reify/Core.hs:708-711):

```haskell
reifyTypeWithNamesNoFallbackOnConstraint constraint subst nid =
    let resBase = Solved.toSolveResult (Solved.mkSolved constraint IntMap.empty)
    in reifyTypeWithNamesNoFallback resBase subst nid
```

This already uses `IntMap.empty` as union-find (identity canonical).
The constraint passed in comes from `Generalize.hs`. If we ensure
that `Generalize.hs` passes `originalConstraint` instead of
`solvedConstraint`, the reifier will:
1. Use the original constraint (all binders present)
2. Use identity canonical (no collapsing)
3. Produce `∀α. α → α` instead of `Int → Int`

Change in Generalize.hs where the constraint is set up for the
reification environment — ensure it uses `originalConstraint solved`
(which should already be the case after Task 10 migrated
`solvedConstraint` → `originalConstraint`).

Similarly for `reifyBoundWithNamesOnConstraint` (Reify/Core.hs:778-781).

**Step 4: Change the non-constraint reification path**

For the `useConstraintReify = False` path, `reifyTypeWithNamesNoFallback`
uses the full Solved with real canonical. If this path is used for
scheme construction, it needs to use identity canonical too.

Option A: Always use the constraint-based path for scheme reification.
Option B: Add a new `reifyTypePreservingBinders` that uses original
constraint + identity canonical.

**Step 5: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: Some test failures — scheme types now include solved-away
binders. Update test expectations.

**Step 6: Commit**

```bash
git add src/MLF/Elab/Generalize.hs src/MLF/Reify/Core.hs
git commit -m "feat: pass original constraint with identity canonical to scheme reification"
```

### Task 20: Verify BinderPlan provides all binders

**Files:**
- Read: `src/MLF/Constraint/Presolution/Plan/BinderPlan.hs`
- Read: `src/MLF/Constraint/Presolution/Plan.hs`

**Step 1: Verify bpOrderedBinderIds includes solved-away binders**

BinderPlan is built during presolution (before solving). Check that
`bpOrderedBinderIds` contains ALL binder IDs, including ones that
will be solved away during constraint solving.

Read the BinderPlan construction code and trace where
`bpOrderedBinderIds` comes from. If it's built from the pre-solve
constraint, it should have all binders.

**Step 2: Write a test that verifies binder preservation**

Add a test in `test/ElaborationSpec.hs` or a new test file:

```haskell
it "scheme type preserves solved-away binders" $ do
    -- Construct a constraint with ∀α. α → α where α = Int
    -- Verify the scheme type is ∀α. α → α, not Int → Int
    ...
```

The exact test depends on the constraint construction API. The test
should:
1. Build a constraint with a binder that gets solved away
2. Run generalization
3. Assert the scheme type includes the binder

**Step 3: Run the test**

Run: `cabal test --test-show-details=direct --test-option='-m "preserves solved-away"' 2>&1 | tail -20`
Expected: PASS (if Task 19 is correct)

**Step 4: Commit**

```bash
git add test/ElaborationSpec.hs
git commit -m "test: verify scheme type preserves solved-away binders"
```

### Task 21: Verify finalizeScheme keeps solved-away binders

**Files:**
- Read: `src/MLF/Constraint/Presolution/Plan/Finalize.hs`

**Step 1: Trace finalizeScheme filtering logic**

At Finalize.hs lines 282-296, binders are kept if their name appears
in `usedNames` (free variables of the type body + bounds) or
`namedBinderNames`.

With the original constraint + identity canonical:
- The type body is `α → α` (not `Int → Int`)
- `α` appears as a free variable in the body
- `finalizeScheme` keeps it

Verify this by reading the code and confirming the data flow.

**Step 2: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS

**Step 3: No commit needed (verification only)**

### Task 22: Fix test expectations for scheme types

**Files:**
- Modify: `test/ElaborationSpec.hs` (multiple test cases)
- Modify: `test/PipelineSpec.hs` (if affected)

**Step 1: Identify failing tests**

After Tasks 19-21, some tests will fail because they expect scheme
types without solved-away binders. Run the full suite and collect
failures.

Run: `cabal test --test-show-details=direct 2>&1 | grep -A5 "FAIL"`

**Step 2: Update test expectations**

For each failing test, update the expected scheme type to include
the solved-away binders. For example:
- Expected `Int → Int` → now `∀α. α → α`
- Expected `∀β. Int → β` → now `∀α. ∀β. α → β`

**Step 3: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS

**Step 4: Commit**

```bash
git add test/ElaborationSpec.hs test/PipelineSpec.hs
git commit -m "test: update scheme type expectations for thesis-exact binder preservation"
```

---

## Milestone 5d: Remove Deviation and Update Documentation

### Task 23: Verify OpWeaken emits InstElim for solved-away binders

**Files:**
- Read: `src/MLF/Elab/Phi/Omega.hs:788-813`
- Read: `test/ElaborationSpec.hs:1431-1478`

**Step 1: Verify the deviation no longer applies**

With scheme types now including all original quantifiers:
- VSpine has slots for solved-away binders
- `isBinderNode` returns True for solved-away binders (they exist
  in the original constraint)
- OpWeaken finds the binder in the VSpine
- `atBinderWith` emits InstElim

Read the OpWeaken case in Omega.hs (lines 788-813) and trace through
the logic with a solved-away binder to confirm.

**Step 2: Write or update a test**

Find the test at ElaborationSpec.hs:1431-1478 that currently asserts
OpWeaken produces `ε` (identity) for solved-away binders. Update it
to assert `InstElim` instead.

If no such test exists, write one:

```haskell
it "OpWeaken on solved-away binder emits InstElim" $ do
    -- Build constraint with ∀α. α → α, solve α = Int
    -- Run phi translation with OpWeaken α
    -- Assert result contains InstElim, not ε
    ...
```

**Step 3: Run the test**

Run: `cabal test --test-show-details=direct --test-option='-m "OpWeaken"' 2>&1 | tail -20`
Expected: PASS

**Step 4: Commit**

```bash
git add test/ElaborationSpec.hs
git commit -m "test: verify OpWeaken emits InstElim for solved-away binders"
```

### Task 24: Remove DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP from deviations

**Files:**
- Modify: `docs/thesis-deviations.yaml`

**Step 1: Remove the deviation entry**

Find the DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP entry (around lines 111-136)
and delete it entirely.

**Step 2: Commit**

```bash
git add docs/thesis-deviations.yaml
git commit -m "docs: remove DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP deviation"
```

### Task 25: Audit DEV-PHI-KEEP-BINDER-WEAKEN-SUPPRESSION

**Files:**
- Read: `docs/thesis-deviations.yaml`
- Read: `src/MLF/Elab/Phi/Omega.hs` (keepBinderKeys logic)

**Step 1: Check if the deviation is still needed**

DEV-PHI-KEEP-BINDER-WEAKEN-SUPPRESSION suppresses InstElim for
OpWeaken when the binder is in `keepBinderKeys`. This was a
workaround for lost binder identity.

With original constraint as primary, binder identity is preserved.
Check whether `keepBinderKeys` suppression is still triggered in
any test case. If not, it may be dead code.

**Step 2: If no longer needed, remove**

Remove the `keepBinderKeys` suppression logic from Omega.hs and
the deviation entry from `thesis-deviations.yaml`.

If still needed for some cases, document why and leave it.

**Step 3: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: PASS

**Step 4: Commit**

```bash
git add src/MLF/Elab/Phi/Omega.hs docs/thesis-deviations.yaml
git commit -m "docs: audit and resolve DEV-PHI-KEEP-BINDER-WEAKEN-SUPPRESSION"
```

### Task 26: Update thesis-claims.yaml if needed

**Files:**
- Modify: `docs/thesis-claims.yaml`

**Step 1: Check for references to removed deviations**

Search `thesis-claims.yaml` for references to:
- DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP
- DEV-PHI-KEEP-BINDER-WEAKEN-SUPPRESSION (if removed)

Update any claims that reference these deviations.

**Step 2: Update claim status if applicable**

If any claims were marked as partially-satisfied due to these
deviations, update their status to satisfied.

**Step 3: Commit**

```bash
git add docs/thesis-claims.yaml
git commit -m "docs: update thesis claims for resolved deviations"
```

### Task 27: Final verification — full test suite + audit

**Files:**
- All

**Step 1: Run full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -30`
Expected: ALL PASS

**Step 2: Grep for remaining escape hatches**

Run: `grep -r "toSolveResult\|unionFind\|solvedConstraint\|LegacyBackend\|fromSolveResult\|mkSolved\b" src/ test/ --include='*.hs' -l`
Expected: No matches (except possibly Solved.hs itself for internal use)

**Step 3: Grep for deviation references**

Run: `grep -r "DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP" docs/ src/ test/ -l`
Expected: No matches

**Step 4: Commit any final cleanup**

```bash
git commit -am "chore: final cleanup after thesis-exact scheme construction"
```

---

## Summary

| Milestone | Tasks | Key Change |
|-----------|-------|------------|
| M5a | 1-13 | Remove LegacyBackend, escape hatches, migrate all callers |
| M5b | 14-18 | Switch Solved.hs queries to `ebOriginalConstraint` |
| M5c | 19-22 | Identity canonical for scheme reification, fix test expectations |
| M5d | 23-27 | Remove deviation, audit related deviations, final verification |
