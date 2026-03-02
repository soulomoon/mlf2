# Progress

## 2026-03-03 Plan Capture
- Created task folder: `tasks/todo/2026-03-03-solved-compat-read-reduction/`.
- Added planning files:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Transcribed the full agent-team wave plan into `task_plan.md` including:
  - scope/out-of-scope
  - wave-by-wave execution and exit criteria
  - required new tests + focused carry-forward checks
  - risk controls and documentation hygiene rules
- Seeded findings with initial risks and pending baseline checklist.
- Current phase state: Wave 0 marked `in_progress` (baseline hotspot/gate capture not yet executed).

## 2026-03-03 Wave 0 Baseline
- Audited solved-read hotspots in:
  - `src/MLF/Elab/Run/ResultType/*`
  - `src/MLF/Elab/Generalize.hs`
  - `src/MLF/Constraint/Presolution/Plan/Context.hs`
  - `src/MLF/Elab/Run/Generalize.hs`
- Baseline gate:
  - `cabal build all && cabal test` -> PASS (`913 examples, 0 failures`).

## 2026-03-03 Wave 1 Implementation
- Updated `Context.hs`:
  - removed `gcConstraintForReify` and `rbConstraintForReify` + associated plumbing.
  - added invariant trace on unexpected `SolvedToBaseMissing` for base-domain nodes.
- Updated `Generalize.hs`:
  - gated alias `Solved.rebuildWithConstraint` under non-OnConstraint reify branch.
  - switched explicit-bound helper reify to OnConstraint bound path when structural-scheme path is authoritative.
- Added tests in `ElaborationSpec.hs`:
  - `generalizeWithPlan falls back from GA to no-GA on SchemeFreeVars`
  - `generalizeWithPlan falls back to reifyType after double SchemeFreeVars`

## 2026-03-03 Wave 2 Implementation
- Added new module: `src/MLF/Elab/Run/ResultType/View.hs`.
- Wired `MLF.Elab.Run.ResultType.View` into `mlf2.cabal`.
- Refactored result-type internals to consume view accessors:
  - `src/MLF/Elab/Run/ResultType.hs`
  - `src/MLF/Elab/Run/ResultType/Ann.hs`
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
- Confined `rtcSolveLike` use to `buildResultTypeView`.

## 2026-03-03 Wave 3 Implementation
- Added integrated fallback-core mapping coverage in `ElaborationSpec.hs`:
  - `result-type fallback core handles gaSolvedToBase same-domain roots`
  - `result-type fallback core handles gaSolvedToBase missing roots`

## 2026-03-03 Wave 4 Implementation
- Removed local fallback `rebuildWithNodes` patch path.
- Implemented overlay-based bound adjustment at view boundary (`rtvWithBoundOverlay` + view materialization).
- Preserved base-domain bind-parent mutation path (`bindParentsGaFinal`) for bound-target cases.

## 2026-03-03 Verification
- Focused carry-forward + new regression matrix: PASS (16 matcher runs).
- Canonical gate after implementation:
  - `cabal build all && cabal test` -> PASS (`917 examples, 0 failures`).
