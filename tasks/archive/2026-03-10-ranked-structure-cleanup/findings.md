# Findings

## Initial Notes
- Implementation started from the approved ranked cleanup plan.
- Existing untracked artifact: `tasks/archive/2026-03-09-code-structure-review/`.

## Implementation Findings
- The real `MLF.Constraint.Types` + `MLF.Constraint.Types.Graph` dual-import problem was small once imports were parsed exactly: only `src/MLF/Elab/Run/Pipeline.hs` and `src/MLF/Constraint/Presolution/Witness.hs` needed cleanup.
- `MLF.Constraint.Presolution` mixed true runtime exports (`computePresolution`, `PresolutionResult`, `PresolutionView`, `PresolutionPlanBuilder`, `PresolutionError`, `EdgeTrace`) with clearly test-support helpers; the smallest clean split was a new `MLF.Constraint.Presolution.TestSupport` entrypoint.
- `test/Main.hs` intentionally mixes direct spec registration with grouped spec aggregators, so the harness guard needs to distinguish direct entry specs from nested spec modules registered via an umbrella spec.
- `MLF.Constraint.Presolution.Driver` already had the right bind-parent rebuild extraction available in `MLF.Constraint.Presolution.Rewrite`; the cleanup mostly needed to route the existing assembly through it.
