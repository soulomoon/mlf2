# Findings — Thesis-Exact Fallback Rework Review

## Working Notes

- Pending review.


- Review result: the design appears correctly implemented in the current tree; I did not find a surviving live fallback path from the design's residual-audit list.
- `MLF.Elab.Elaborate` now uses a single authoritative let-generalization result for `ALet` and no longer contains the chooser/fallback symbols called out by the design.
- `MLF.Elab.Run.Generalize` calls `applyGeneralizePlan` directly without a recursive fallback callback, and `MLF.Elab.Generalize` uses direct structural scheme reification when scheme scope differs instead of recursively generalizing another scope.
- `reifyInst` no longer uses `targetArgs <|> expansionArgs`; refinement comes from witness/domain-owned authority (`ewLeft`/`ewRight` and trace binder args), and it now fails with `PhiTranslatabilityError` when a nontrivial instantiation is demanded but no authoritative translation exists.
- Semantic regression coverage matches the design intent: guard tests confirm the removed fallback families are absent, and targeted semantic tests assert strict failure for the old fallback-dependent behaviors (`SchemeFreeVars` retries, expansion-derived instantiation recovery, nested-let annotated-lambda path, and dual annotated coercion consumers).
- Documentation sync also matches the design: `TODO.md`, `implementation_notes.md`, `docs/architecture.md`, `CHANGELOG.md`, and the archived task artifacts all describe the same strict fail-fast policy.
- Minor review note: the let-level semantic coverage is mostly integration-level (`PipelineSpec`) rather than an isolated `ALet` unit, but it still exercises the intended behavior and passed.

## 2026-03-08 tracker cleanup
- Completed review; findings are final and the corresponding implementation task is already archived.
