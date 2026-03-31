# Round 167 — Review: Public API Enrichment (item-8)

**Reviewer**: automated reviewer
**Branch**: `orchestrator/round-167-public-api-enrichment`
**Base**: `codex/automatic-recursive-type-inference` @ `9ec1618`
**Implementation commit**: `765a06e`

---

## Baseline Checks

### 1. Build gate

```
$ cabal build all
```

**Result**: exit 0, no warnings. Only the test suite was incrementally rebuilt
(library and internal targets were already up to date).

**Verdict**: ✅ PASS

### 2. Test gate

```
$ cabal test
```

**Result**: exit 0, **1302 examples, 0 failures**.

**Verdict**: ✅ PASS

### 3. Thesis conformance gate

```
$ ./scripts/thesis-conformance-gate.sh
```

**Result**: exit 0. All 107 thesis obligation anchors matched and passed
individually. Final line: `[thesis-gate] PASS: thesis conformance anchors are green`.

**Verdict**: ✅ PASS

### 4. No regressions — test count

Pre-round baseline: **1296** examples.
Post-implementation: **1302** examples (net +6).

1302 ≥ 1296. ✅ PASS

---

## Task-Specific Checks (item-8)

### 1. Haddock doc-comments on every new export

| Export | Module | Haddock comment | Line |
|--------|--------|-----------------|------|
| `pipelineErrorPhase` | `MLF.Pipeline` | `-- \| Extract the numeric pipeline phase where the error occurred.` | 98 |
| `pipelineErrorPhaseName` | `MLF.Pipeline` | `-- \| Human-readable name of the pipeline phase where the error occurred.` | 117 |
| `formatPipelineError` | `MLF.Pipeline` | `-- \| Structured, multi-line 'Text' rendering of a 'PipelineError'.` | 127 |
| `constraintNodeCount` | `MLF.API` | `-- \| Number of type nodes in a constraint graph.` | 95 |
| `constraintEdgeCount` | `MLF.API` | `-- \| Total number of edges (instantiation + unification) in a constraint graph.` | 99 |
| `lookupNode` | `MLF.API` (re-export) | Has Haddock in source module `MLF.Constraint.Types.Graph` | — |

Additionally:
- `MLF.API` module header updated to mention "constraint graph introspection" (line 8).
- Section header `-- * Constraint graph introspection` added (line 41).
- `pipelineErrorPhase` includes phase-number mapping in its Haddock block.
- `formatPipelineError` includes output format example in `@...@` code block and cross-references `renderPipelineError`.

**Verdict**: ✅ PASS — all 6 new exports have Haddock doc-comments.

### 2. Test coverage for new exported functions

| Function | Test description | Test file |
|----------|-----------------|-----------|
| `formatPipelineError` | "formatPipelineError produces structured Text output" | `PublicSurfaceSpec.hs` |
| `pipelineErrorPhase` | "pipelineErrorPhase returns correct phase numbers" | `PublicSurfaceSpec.hs` |
| `pipelineErrorPhaseName` | "pipelineErrorPhaseName returns human-readable names" | `PublicSurfaceSpec.hs` |
| `constraintNodeCount` | "constraintNodeCount returns a positive count for a real graph" | `PublicSurfaceSpec.hs` |
| `constraintEdgeCount` | "constraintEdgeCount returns a non-negative count" | `PublicSurfaceSpec.hs` |
| `lookupNode` | "lookupNode retrieves the root node from a constraint graph" | `PublicSurfaceSpec.hs` |

**Verdict**: ✅ PASS — 6/6 new functions have at least one test.

### 3. No internal behavior changes

Files modified in the diff:

| File | Change type |
|------|-------------|
| `mlf2.cabal` | Added `text` dependency only (2 stanzas) |
| `src-public/MLF/Pipeline.hs` | New exports + new function definitions only |
| `src-public/MLF/API.hs` | New exports + new function definitions + cosmetic reformatting |
| `test/ConstraintGenSpec.hs` | Added `hiding (lookupNode)` to resolve name clash |
| `test/PublicSurfaceSpec.hs` | Added 6 new test cases + cosmetic reformatting |

No internal modules (`src/MLF/*`) were modified. No existing function behavior was altered. All changes are additive public surface enrichment.

**Verdict**: ✅ PASS

### 4. Scope compliance

The implementation stays within item-8 scope boundaries defined in the plan:
- ✅ New exports for error formatting and constraint introspection
- ✅ Haddock for all new exports
- ✅ Tests for all new functions
- ❌ No behavioral changes to pipeline internals
- ❌ No changes to `renderPipelineError`
- ❌ No new error constructors
- ❌ No changes to `PipelineConfig` fields

**Verdict**: ✅ PASS

---

## Plan Compliance Check (6 steps)

### Step 1 — Add `text` dependency to cabal

- `library` stanza: `text` added at line 300 of `mlf2.cabal` ✅
- `test-suite mlf2-test` stanza: `text` added at line 443 ✅

### Step 2 — Three new functions in `MLF.Pipeline`

- `pipelineErrorPhase :: PipelineError -> Int` — exported (line 47), defined (lines 108–115) ✅
- `pipelineErrorPhaseName :: PipelineError -> String` — exported (line 48), defined (lines 118–125) ✅
- `formatPipelineError :: PipelineError -> Text` — exported (line 46), defined (lines 137–153) ✅
- All three pattern-match on all 6 `PipelineError` constructors (total under `-Wall`) ✅

### Step 3 — Constraint graph introspection in `MLF.API`

- `Constraint(..)` + 10 related types/type aliases re-exported ✅
- `lookupNode` re-exported from `MLF.Constraint.Types.Graph` ✅
- `constraintNodeCount` defined locally (line 96–97) ✅
- `constraintEdgeCount` defined locally (lines 100–101) ✅
- Import of `MLF.Constraint.Types.Graph` with all needed symbols ✅

### Step 4 — 6 new tests in `PublicSurfaceSpec.hs`

- 3 tests under `"MLF.Pipeline (error formatting)"` ✅
- 3 tests under `"MLF.API (constraint graph introspection)"` ✅
- All 6 tests pass as part of the 1302-example suite ✅

### Step 5 — `CycleError(..)` re-export added

- `CycleError(..)` added to `MLF.Pipeline` export list (line 44) ✅
- Import from `MLF.Constraint.Acyclicity` added (line 62) ✅
- Used in test: `Pipeline.CycleError [] "test"` in `pipelineErrorPhase` test ✅

### Step 6 — Final verification gate

- `cabal build all`: exit 0, no warnings ✅
- `cabal test`: 1302 examples, 0 failures ✅
- `./scripts/thesis-conformance-gate.sh`: exit 0 ✅

---

## Observations (non-blocking)

1. **`MLF.API` formatting change**: The export list and imports in `MLF.API` were reformatted from leading-comma to trailing-comma style (with `where` on its own line). `MLF.Pipeline` preserved leading-comma style per the `RepoGuardSpec` guardrail. This creates a minor style inconsistency between the two public modules. Not blocking — all guardrail tests pass.

2. **`ConstraintGenSpec.hs` name clash fix**: Adding `lookupNode` to `MLF.API` exports caused an ambiguity in `ConstraintGenSpec.hs` (which imports both `MLF.API` and `SpecUtil`). Resolved with `hiding (lookupNode)`. Correct fix, documented in implementation-notes.

---

## Decision

**APPROVED**

All baseline checks pass. All task-specific checks pass. The implementation matches the plan step-by-step with no deviations. Test count increased from 1296 to 1302 (net +6). No internal behavior changes. All new exports have Haddock comments and test coverage.
