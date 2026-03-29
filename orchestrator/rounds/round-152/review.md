# Round 152 Review — Fix reifyInst TyMu without authoritative binder child

**Reviewer:** automatic reviewer (round-152)
**Date:** 2026-03-29
**Verdict:** APPROVED

## Baseline Checks

| Check | Command | Result |
|-------|---------|--------|
| Whitespace/conflict damage | `git diff --check codex/automatic-recursive-type-inference..orchestrator/round-152-fix-reifyinst-tymu-binder` | ✅ PASS (no output) |
| Valid JSON state | `python3 -m json.tool orchestrator/state.json >/dev/null` | ✅ PASS ("VALID JSON") |
| Roadmap bundle resolves | `test -f "$roadmap_dir/roadmap.md" && ...` | ✅ PASS ("ROADMAP BUNDLE RESOLVES") |
| Full build + test gate | `cabal build all && cabal test` | ✅ PASS — **1176 examples, 0 failures** |

## Item-2 Specific Checks

| Check | Evidence | Result |
|-------|----------|--------|
| `reifyInst` handles 0-binder TyMu without `PhiTranslatabilityError` | New `[]` arm in `goFull` synthesizes binder from TyMu node `n` instead of erroring | ✅ PASS |
| Existing local TyMu reification (1-binder path) unchanged | `[bndr]` arm has identical logic (extracted to self-contained arm, behavior preserved) | ✅ PASS |
| New targeted test validates non-local TyMu reification | `test/PipelineSpec.hs:426–456` — "reifies TyMu without binder child (non-local proxy fallback)" | ✅ PASS |
| `cabal test --test-option='-m' --test-option='non-local proxy'` | 3 examples, 0 failures | ✅ PASS |
| `cabal test --test-option='-m' --test-option='TyMu without binder'` | 1 example, 0 failures | ✅ PASS |
| No existing test regressions | Full suite: 1176 examples, 0 failures (was 1175 before; +1 from new test) | ✅ PASS |

## Diff Analysis Against Plan

### Step 1: Fix the `[]` case in TyMu branch — ✅

**Plan:** Replace the `[] -> Left $ PhiTranslatabilityError ...` arm (lines 357–362) with a synthesized-binder fallback using `synthBinder = n`.

**Actual:** The implementer restructured the entire TyMu branch from a `binder <- case binders of ...` extraction pattern into three self-contained `case binders of` arms. This is a **justified structural deviation**: the plan's replacement code returns `(Cache, ElabType)` but the original binding expected `Either ElabError NodeId` for the `binder <-` pattern. The restructuring is the correct fix.

**Behavior preserved:**
- `[bndr]` arm: identical logic (`canonical bndr`, `namedExtra'`, `vChild`, `TMu`, `cacheInsertLocal`, `markDone`)
- `[]` arm: implements plan exactly (`synthBinder = n`, `namedExtra'` with `getNodeId synthBinder`, `TMu (varName synthBinder) bodyTy`)
- Multi-binder error arm: unchanged (`BindingTreeError $ InvalidBindingTree`)

**Net change:** +61/-22 lines across 3 files. All changes are within the scope defined by the plan.

### Step 2: Add targeted unit test — ✅

New test at `test/PipelineSpec.hs:426–456` matches the plan's conceptual test code almost exactly:
- Constructs minimal constraint with TyMu (body = arrow: muVar → Int)
- No binding-tree entry for muVarId under muId → `orderedFlexChildren` returns `[]`
- Verifies `reifyType` produces `TMu` instead of error
- Correctly uses `SolvedTest.mkTestSolved` and `PresolutionViewBoundary.fromSolved`

### Step 3: Assess impact on existing tests — ✅

**Plan anticipated two tests potentially affected:**

1. `test/PipelineSpec.hs:2303–2315` — NOT affected. `expectStrictPipelineFailure` already accepts any `PhiTranslatabilityError` regardless of site. Confirmed by full suite passing.

2. `test/Research/P5ClearBoundarySpec.hs:91` — NOT affected. Still passes as-is (confirmed by implementation notes and full suite).

**Unexpected but valid change:** `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` — the plan did not anticipate this file needing changes. After the fix, `sameLaneAliasFrameClearBoundaryExpr` no longer fails at the reifyInst TyMu site but instead reaches type-checking, producing `TCLetTypeMismatch`. The implementer widened the predicate to accept either error class. This is a **correct and necessary adjustment** — the test's intent is to assert a current-architecture blocker, not a specific error site.

### Steps 4–5: Build and targeted verification — ✅

- `cabal build all && cabal test`: 1176 examples, 0 failures
- `cabal test --test-option='-m' --test-option='non-local proxy'`: 3 examples, 0 failures
- `cabal test --test-option='-m' --test-option='TyMu without binder'`: 1 example, 0 failures

## Code Quality Assessment

- **Comment quality:** The `[]` arm has clear inline comments explaining the non-local proxy scenario
- **Naming:** `synthBinder` is descriptive and distinguishes from the `[bndr]` path's `binder`
- **Pattern exhaustiveness:** All three arms are covered (`[bndr]`, `[]`, `_`)
- **Cache/markDone consistency:** Both success arms follow identical cache patterns
- **No unrelated changes:** Diff is minimal and scoped to the fix

## Decision

**APPROVED** — All baseline checks pass, all item-2 specific checks pass, no test regressions, implementation faithfully realizes the plan's intent with a justified structural deviation, and the unexpected SameLaneRetainedChildRepresentativeGapSpec adjustment is correct.
