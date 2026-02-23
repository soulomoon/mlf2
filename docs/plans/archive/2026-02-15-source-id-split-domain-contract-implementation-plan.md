# Source-ID Split-Domain Phi Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Close BUG-2026-02-14-003 by enforcing thesis-exact source-ID authority for Phi binder semantics, while preserving canonical-ID structural/typecheck behavior and keeping strict non-binder rejection.

**Architecture:** Introduce a single internal IdentityBridge module that centralizes source<->canonical reconciliation and deterministic binder ranking. Wire both `MLF.Elab.Phi.Translate` and `MLF.Elab.Phi.Omega` to this bridge so binder membership/index decisions share one contract and one tie-break policy. Keep structural graph operations canonical and provenance semantics source-authoritative.

**Tech Stack:** Haskell (GHC 9.12/Cabal), Hspec, existing MLF elaboration/presolution pipeline.

---

### Task 1: Re-lock Baseline Before Refactor

**Files:**
- Modify: `tasks/todo/2026-02-15-bug-2026-02-14-003-identity-drift/progress.md`
- Test: `test/ElaborationSpec.hs`, `test/CanonicalizerSpec.hs`, `test/PipelineSpec.hs`

**Step 1: Run strict BUG-003 anchors (expected current behavior)**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002 strict target matrix"'
```
Expected: current baseline result recorded (do not assume pass/fail).

**Step 2: Run BUG-002/BUG-004 guardrails**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V4|BUG-004"'
```
Expected: baseline status recorded for both anchors.

**Step 3: Run provenance/canonicalization anchors**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "tracks instantiation copy maps for named binders|witness/trace/expansion canonicalization"'
```
Expected: baseline status recorded.

**Step 4: Record evidence in progress log**

Add command outputs and failure signatures to progress log.

**Step 5: Commit baseline notes**

```bash
git add tasks/todo/2026-02-15-bug-2026-02-14-003-identity-drift/progress.md
git commit -m "chore: lock baseline anchors for source-id split-domain refactor"
```

### Task 2: Add IdentityBridge Skeleton with Failing Unit Tests

**Files:**
- Create: `src/MLF/Elab/Phi/IdentityBridge.hs`
- Create: `test/Phi/IdentityBridgeSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal`

**Step 1: Write failing bridge spec for deterministic key ranking**

Add tests for:
- source key de-duplication,
- trace-order priority over numeric order,
- canonical alias fallback ordering.

**Step 2: Run new spec only and confirm fail**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "IdentityBridge"'
```
Expected: FAIL (missing module/functions or incomplete behavior).

**Step 3: Implement minimal bridge data type + ranking helpers**

Implement:
- bridge construction from `EdgeTrace` + canonical function,
- pure helpers for candidate extraction and ranking.

**Step 4: Re-run IdentityBridge spec**

Run same command.
Expected: PASS for ranking tests.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Phi/IdentityBridge.hs test/Phi/IdentityBridgeSpec.hs test/Main.hs mlf2.cabal
git commit -m "phi: add IdentityBridge with deterministic source-key ranking"
```

### Task 3: Add Failing Tests for Binder Index Resolution Semantics

**Files:**
- Modify: `test/Phi/IdentityBridgeSpec.hs`

**Step 1: Add failing tests for binder index resolution policy**

Cases:
- exact source-key match beats canonical alias match,
- deterministic tie-break by trace order,
- no match returns `Nothing`.

**Step 2: Run IdentityBridge spec and verify fail**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "IdentityBridge.*binder index|IdentityBridge"'
```
Expected: FAIL on new cases.

**Step 3: Implement minimal binder-index helper**

Add a bridge API that receives scheme binder keys + spine IDs and returns stable index.

**Step 4: Re-run spec to pass**

Run same command.
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Phi/IdentityBridge.hs test/Phi/IdentityBridgeSpec.hs
git commit -m "phi: implement source-authoritative binder index lookup in IdentityBridge"
```

### Task 4: Move Translate Remap/Hydration Ranking onto Bridge

**Files:**
- Modify: `src/MLF/Elab/Phi/Translate.hs`
- Modify: `src/MLF/Elab/Phi/IdentityBridge.hs`
- Test: `test/CanonicalizerSpec.hs`, `test/ElaborationSpec.hs`

**Step 1: Add/extend failing test for under-populated `siSubst` hydration**

Add a regression in elaboration specs where trace has full binder args but incoming subst under-covers names.

**Step 2: Run target test to confirm fail**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002 strict target matrix|BUG-002-V4"'
```
Expected: FAIL in at least one target before bridge-backed hydration.

**Step 3: Refactor `Translate` to call bridge for ranking/candidate selection**

Keep behavior:
- source-keyed remap,
- deterministic hydration,
- no arity-drop-to-Nothing fallback.

**Step 4: Re-run targeted tests**

Run same command.
Expected: target regressions improved/passing with no strictness weakening.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Phi/Translate.hs src/MLF/Elab/Phi/IdentityBridge.hs test/ElaborationSpec.hs
git commit -m "phi-translate: use IdentityBridge for source-key remap and hydration"
```

### Task 5: Move Omega Membership/Index Logic onto Bridge

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs`
- Modify: `src/MLF/Elab/Phi/IdentityBridge.hs`
- Test: `test/ElaborationSpec.hs`

**Step 1: Add failing strict-negative and strict-positive assertions**

Add/confirm tests:
- true non-binder target still fails with `PhiTranslatabilityError`,
- valid source-binder target succeeds even when canonical representative is structural.

**Step 2: Run strict matcher before Omega refactor**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002 strict target matrix|BUG-003-V|out-of-scheme target"'
```
Expected: FAIL on intended positive case before refactor.

**Step 3: Replace local Omega reconciliation helpers with bridge calls**

Remove duplicated local logic for:
- source key extraction,
- binder membership tests,
- binder index ranking.

Keep strict behavior unchanged.

**Step 4: Re-run matcher**

Run same command.
Expected: PASS for positive source-ID case; strict negatives remain failing as expected (where intended by tests).

**Step 5: Commit**

```bash
git add src/MLF/Elab/Phi/Omega.hs src/MLF/Elab/Phi/IdentityBridge.hs test/ElaborationSpec.hs
git commit -m "phi-omega: use IdentityBridge for source-authoritative binder membership"
```

### Task 6: Boundary Contract Guard in Canonicalization Tests

**Files:**
- Modify: `test/CanonicalizerSpec.hs`
- Test: `src/MLF/Elab/Run/Util.hs` (behavior lock only)

**Step 1: Add failing assertions for split-domain boundary**

Assert all of:
- witness op targets unchanged (source IDs),
- trace binder/copy fields unchanged (source IDs),
- root/interior/left/right still canonicalized.

**Step 2: Run canonicalization matcher and confirm fail if assertions are new**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "witness/trace/expansion canonicalization|Canonicalizer"'
```
Expected: FAIL before updates or PASS if already covered exactly.

**Step 3: Update tests to lock final contract**

Make assertions explicit and deterministic.

**Step 4: Re-run matcher**

Run same command.
Expected: PASS.

**Step 5: Commit**

```bash
git add test/CanonicalizerSpec.hs
git commit -m "test: lock split-domain canonicalization boundary for trace/witness"
```

### Task 7: Run Sequential Validation Matrix

**Files:**
- Modify: `tasks/todo/2026-02-15-bug-2026-02-14-003-identity-drift/progress.md`

**Step 1: Run targeted anchors sequentially**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002 strict target matrix"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V4"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "tracks instantiation copy maps for named binders"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "witness/trace/expansion canonicalization"'
```
Expected: all targeted anchors green.

**Step 2: Run full validation gate**

Run:
```bash
cabal build all && cabal test
```
Expected: full suite green.

**Step 3: Record all results in progress log**

Capture example counts and failures exactly.

**Step 4: Commit validation log updates**

```bash
git add tasks/todo/2026-02-15-bug-2026-02-14-003-identity-drift/progress.md
git commit -m "chore: record split-domain contract validation matrix"
```

### Task 8: Close Tracker and Sync Docs (Only After Full Green)

**Files:**
- Modify: `Bugs.md`
- Modify: `CHANGELOG.md`
- Modify: `implementation_notes.md`
- Modify: `TODO.md`
- Modify: `tasks/todo/2026-02-15-bug-2026-02-14-003-identity-drift/task_plan.md`
- Modify: `tasks/todo/2026-02-15-bug-2026-02-14-003-identity-drift/findings.md`

**Step 1: Update bug tracker entry**

Move BUG-2026-02-14-003 to Resolved with:
- root cause summary,
- verification command list,
- regression test references.

**Step 2: Add changelog and implementation note entry**

Document split-domain contract, authority boundaries, and strictness guardrails.

**Step 3: Update TODO priorities**

Remove BUG-003 closure item and leave only next active buckets.

**Step 4: Update task files and archive**

Mark all phases complete; move task folder from `tasks/todo/...` to `tasks/archive/...`.

**Step 5: Commit docs closure**

```bash
git add Bugs.md CHANGELOG.md implementation_notes.md TODO.md tasks/todo/2026-02-15-bug-2026-02-14-003-identity-drift
git commit -m "docs: close BUG-2026-02-14-003 with split-domain source-id contract"
```

## Execution Notes

1. Run Cabal commands sequentially only; do not run parallel test jobs in this workspace.
2. Keep strict Phi/Omega non-binder rejection semantics unchanged.
3. Do not modify `src-public` modules.
4. Prefer small commits after each task to simplify rollback/review.
5. If full gate still fails after targeted anchors are green, stop and open new bug bucket(s) rather than broadening this fix scope.
