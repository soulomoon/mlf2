# Findings

## 2026-03-03 Initial audit
- Production run path still depends on replay APIs through `src/MLF/Elab/Run/PipelineBoundary.hs` (`Solved.fromPreRewriteState`, `solveResultFromSnapshot`) and `src/MLF/Elab/Run/Pipeline.hs` imports/calls.
- `rtcSolveLike` in `src/MLF/Elab/Run/ResultType/Types.hs` currently returns `Right` without validation.
- Migration tests in `test/PipelineSpec.hs` and `test/ElaborationSpec.hs` include multiple fragile source-string assertions that can pass despite runtime semantic regressions.
- Existing `migration guardrail: thesis-core boundary matches legacy outcome` test does not directly compare key solved artifacts between thesis-core and legacy pathways.

## 2026-03-03 Agent-team implementation round
- Added `src/MLF/Constraint/Finalize.hs` and rewired `src/MLF/Elab/Run/Pipeline.hs` to a replay-free runtime finalization path.
- Removed `src/MLF/Elab/Run/PipelineBoundary.hs`; updated `mlf2.cabal` module lists accordingly.
- `rtcSolveLike` now validates solved materialization and can return `Left (ValidationFailed ...)`.
- `test/ElaborationSpec.hs` migration guards moved from source-text checks to runtime semantic checks.
- `test/PipelineSpec.hs` now includes a production run-path callgraph guard for forbidden replay APIs and stronger legacy-vs-thesis-core semantic comparisons.
- Active blocker: strengthened migration guardrail currently fails with a canonical-map mismatch (`legacy` vs thesis-core view path), requiring next-round triage.

## 2026-03-03 Continuation round (blocker triage + closure)
- Root cause of migration guardrail mismatch:
  - legacy snapshot replay canonical-map carries eliminated-node-only key mappings (outside live snapshot constraint domain),
  - thesis-core presolution view canonical-map is live-domain-only by construction.
- Guardrail hardening update:
  - canonical-map comparison now projects both sides to shared live-node domain;
  - canonical-constraint equality and solved query parity checks remain strict.
- Root cause of post-cutover Phase 6 regressions:
  - replay-free `MLF.Constraint.Finalize` path initially skipped eliminated-binder rewrite / UF substitution propagation and bind-parent pruning steps used by legacy snapshot finalization.
- Runtime fix:
  - introduced shared `Solve.finalizeConstraintWithUF` and routed `Finalize.finalizeSolvedFromSnapshot` / `Finalize.finalizeSolvedForConstraint` through full snapshot finalization semantics.
- Verification status:
  - checkpoint slices green (`migration guardrail`, `Dual-path verification`, `Phase 6 — Elaborate`);
  - full gate green (`cabal build all && cabal test`).
