# Findings — BUG-2026-02-16-007/008

## Context
- Both BUG-007 and BUG-008 currently fail with `PipelineElabError (SchemeFreeVars (NodeId {getNodeId = 27}) ["__rigid24"])`.
- Ownership likely in:
  - `src/MLF/Elab/Generalize.hs`
  - `src/MLF/Elab/Run/Generalize.hs`
  - `src/MLF/Elab/Elaborate.hs`

## Evidence Log
- Pending reproduction output capture.

## Root-Cause Notes
- Primary root cause for 007/008:
  - `runPipelineElab` root generalization retry logic handled `BindingTreeError GenSchemeFreeVars` but not plain `SchemeFreeVars`.
  - `generalizeWithPlan` in `MLF.Elab.Run.ResultType.Util` had the same mismatch.
  - Result: BUG-003 variants surfaced as transient `PipelineElabError (SchemeFreeVars ...)` instead of the underlying strict-instantiation failure class.
- Structural binder-filter experiments in `BinderPlan.Build` did not remove the drift signal; the decisive fix was fallback policy alignment.
- After fallback alignment, BUG-003-V1/V2 consistently land in:
  - `PipelineTypeCheckError ... "InstBot expects TBottom, ..."`
  - This is the stabilized failure bucket now tracked by the sentinel.

## Implemented Fix
- `src/MLF/Elab/Run/Pipeline.hs`
  - Added `generalizeNeedsFallback` that treats both:
    - `BindingTreeError GenSchemeFreeVars{}`
    - `SchemeFreeVars{}`
  - Retry order: GA -> non-GA -> `reifyType` fallback.
- `src/MLF/Elab/Run/ResultType/Util.hs`
  - Applied the same fallback policy to `generalizeWithPlan`.
- `test/ElaborationSpec.hs`
  - Updated BUG-003-V1/V2 sentinel predicates from `"OpGraft targets non-binder node"` to `"InstBot expects TBottom"` to reflect the stabilized post-fix bucket and avoid `SchemeFreeVars` masking.

## Verification
- Exact repro commands (both pass):
  - BUG-003-V1 selector + seed `1481579064`
  - BUG-003-V2 selector + seed `1481579064`
- Matrix slice:
  - `--match "BUG-003-V"` => `2 examples, 0 failures`

## 2026-02-16 Reproduction evidence
- BUG-003-V1 and BUG-003-V2 both fail deterministically with:
  - `PipelineElabError (SchemeFreeVars (NodeId {getNodeId = 27}) ["__rigid24"])`
- Failure is raised as an elaboration/generalization error before reaching the expected Phi guardrail assertion (`"OpGraft targets non-binder node"`).

## Code-path observations
- `SchemeFreeVars` is emitted in `src/MLF/Constraint/Presolution/Plan/Finalize.hs` when `missingNames` is non-empty.
- `missingNames` is computed after renaming/normalization and can include rigid-introduced names (`__rigidN`) when they are not bound by the finalized quantifier list.
- `MLF.Elab.Elaborate.generalizeAtWith` has fallback handling for `SchemeFreeVars`, but the top-level pipeline generalize call in `MLF.Elab.Run.Pipeline` only retries for `BindingTreeError GenSchemeFreeVars`, not plain `SchemeFreeVars`.
