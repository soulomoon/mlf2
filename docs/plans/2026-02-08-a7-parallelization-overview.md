# A7 Duplicate-Logic Abstractions Parallelization Overview

## Objective
Execute `A7 (P2)` via independent tracks that minimize merge conflicts while preserving paper-faithful behavior.

## Grouping

### Group 1: Binding Core Abstractions
- Scope: `MLF.Binding.*` + `MLF.Constraint.BindingUtil` + `MLF.Constraint.Presolution.Base`
- Target outcomes:
  - Single source for binding-path traversal and gen-ancestor discovery
  - Single source for node-ref enumeration/existence checks
  - Single source for scope-graph and bound-children helpers
- Primary files:
  - `src/MLF/Binding/Queries.hs`
  - `src/MLF/Binding/Validation.hs`
  - `src/MLF/Binding/Tree.hs`
  - `src/MLF/Binding/Canonicalization.hs`
  - `src/MLF/Constraint/BindingUtil.hs`
  - `src/MLF/Constraint/Presolution/Base.hs`

### Group 2: Frontend/Elab Traversal + Scope Wiring Abstractions
- Scope: constraint-generation scope lifecycle helpers + shared annotation traversal fold.
- Target outcomes:
  - Remove repeated scope push/pop/rebind + bind-parent wiring boilerplate in `Translate`
  - Remove repeated `AnnExpr` `cata` constructor-walks in Elab run helpers
- Primary files:
  - `src/MLF/Frontend/ConstraintGen/Translate.hs`
  - `src/MLF/Elab/Run/Annotation.hs`
  - `src/MLF/Elab/Run/Debug.hs`

### Group 3: Test Harness Deduplication
- Scope: shared test helpers for normalize + pipeline solve chain + error decoration.
- Target outcomes:
  - Remove duplicated `unsafeNormalize`, `firstShow`, and solve-chain plumbing
  - Improve readability and lower future maintenance cost in test suite
- Primary files:
  - `test/SpecUtil.hs`
  - `test/PipelineSpec.hs`
  - `test/ElaborationSpec.hs`
  - `test/ConstraintGenSpec.hs`

## Parallel Execution Model
- Group 1 can run independently.
- Group 2 can run independently.
- Group 3 can run independently.
- Merge order recommendation:
  1. Group 1
  2. Group 2
  3. Group 3

## Worktree / Branch Layout
- `../mlf4-a7-binding` on `codex/a7-binding-core-abstractions`
- `../mlf4-a7-frontend-elab` on `codex/a7-frontend-elab-abstractions`
- `../mlf4-a7-test-harness` on `codex/a7-test-harness-dedup`

## Verification Gates
- Per-group targeted tests first.
- Final integration gate after merging all groups:
  - `cabal build all && cabal test`

## Detailed Plans
- `docs/plans/2026-02-08-a7-group-1-binding-core-abstractions-implementation-plan.md`
- `docs/plans/2026-02-08-a7-group-2-frontend-elab-abstractions-implementation-plan.md`
- `docs/plans/2026-02-08-a7-group-3-test-harness-dedup-implementation-plan.md`
