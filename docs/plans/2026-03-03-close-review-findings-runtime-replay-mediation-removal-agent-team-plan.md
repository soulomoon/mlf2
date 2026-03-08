# Parallel Work Plan: Close Review Findings and Achieve True Runtime Replay/Mediation Removal

## Summary
We will fix all review findings with a two-stage safety rollout and a strict runtime-semantics target:
1. Stage 1 hardens tests and validation so regressions are detected semantically (not by file-string checks).
2. Stage 2 removes replay/mediation behavior from production runtime paths, then revalidates full behavior gates and docs claims.

This plan is decision-complete and can be executed by agent teams in checkpointed batches.

## Scope and Success Criteria
1. Remove replay/mediation from production runtime semantics, not only from `Pipeline.hs` text.
2. Remove false-positive structural tests and replace with semantic contract tests.
3. Restore explicit failure signaling for result-type solved materialization.
4. Eliminate dead contract scaffolding from runtime code.
5. Align docs with actual semantics only after code/test gates are green.

Done means all targeted and full gates pass, and no production module in `src/MLF/Elab/Run` calls `Solved.fromPreRewriteState` or `solveResultFromSnapshot`.

## Team Topology and Ownership
1. Team A (`runtime-boundary`) owns runtime boundary semantics and pipeline cutover.
2. Team B (`finalization-core`) owns shared graph-finalization logic extraction from solve replay internals.
3. Team C (`elab-resulttype`) owns elaboration/result-type solved materialization + validation behavior.
4. Team D (`tests-guards`) owns semantic guard tests and migration-equivalence assertions.
5. Team E (`docs-claims`) owns docs/changelog/plan closeout updates after green gates.
6. Team F (`independent-review`) runs post-change code review and blocker triage.

## Stage 1: Safety Harness and Validation Hardening
1. Team D replaces fragile grep-only closeout tests in `PipelineSpec.hs` and `ElaborationSpec.hs` with semantic assertions.
Details: keep at most one structural smoke check; all acceptance tests must compare runtime behavior or query results.
2. Team D fixes `migration guardrail: thesis-core boundary matches legacy outcome` to actually compare thesis-core and legacy artifacts.
Details: compare at least canonical map, canonical constraint validation, and key solved queries on a fixed corpus.
3. Team C updates `Types.hs` so `rtcSolveLike` validates solved graph and returns `Left ValidationFailed` on invalid materialization.
4. Checkpoint A gate:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "migration guardrail: thesis-core boundary matches legacy outcome"'`
`cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
`cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`

## Stage 2: Runtime Semantics Cutover (No Replay/Mediation in Production)
1. Team B extracts replay-finalization mechanics into a shared non-replay finalization API.
Files: new internal module under `src/MLF/Constraint/` (for example `MLF.Constraint.Finalize`).
Details: own these transformations as named finalization steps, not solve-replay helpers; expose one pure API callable by presolution/pipeline.
2. Team A rewires `Pipeline.hs` to use presolution-authoritative + shared finalization, with no runtime call to replay APIs.
Details: remove `PipelineBoundary.solvedFromSnapshotReplay` and `PipelineBoundary.rebuildSolvedForConstraint` from production path.
3. Team A deletes or reduces `PipelineBoundary.hs` to non-replay utilities only; remove dead `_boundaryContract*` scaffolding.
4. Team C verifies `Elaborate.hs` and `Types.hs` remain compatible with no-replay runtime inputs; patch callsites if scope/root logic expects replay-shaped bind-parent normalization.
5. Team D adds a runtime-callgraph guard test scanning production run-path modules (`src/MLF/Elab/Run/*.hs`) for forbidden replay APIs.
Forbidden in production run-path: `Solved.fromPreRewriteState`, `solveResultFromSnapshot`.
6. Checkpoint B gate:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "row1 boundary uses thesis-core elaboration input contract"'`
`cabal test mlf2-test --test-show-details=direct --test-options='--match "row1 boundary validates-only and does not mediate input"'`
`cabal test mlf2-test --test-show-details=direct --test-options='--match "final row1 state uses single thesis-core boundary path"'`
`cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline (Phases 1-5)"'`
`cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`

## Stage 3: Docs and Claim Consistency
1. Team E updates:
`docs/plans/2026-03-02-runtime-thesis-exact-elab-input-implementation-plan.md`
`docs/notes/2026-02-27-transformation-mechanism-table.md`
`implementation_notes.md`
`CHANGELOG.md`
2. Team E ensures docs claim runtime replay/mediation removal only if production call graph confirms it.
3. Checkpoint C gate:
`cabal build all && cabal test`

## Independent Review and Merge Gate
1. Team F performs code review focused on the five original findings.
2. Block merge on any remaining critical/important issue in runtime semantics or guard-test validity.
3. Merge readiness requires:
all Stage A/B/C gates green;
no production replay API usage in run-path;
semantic migration guardrail compares legacy vs thesis-core artifacts directly.

## Public API / Interface / Type Changes
1. No exposed public package API changes expected in `src-public/*`.
2. Internal interface changes expected:
`MLF.Elab.Run.PipelineBoundary` will lose replay constructors or be removed.
A new internal shared finalization API under `MLF.Constraint.*` will be introduced.
`rtcSolveLike` behavior changes from unconditional `Right` to validated `Either ElabError`.
3. Cabal `other-modules` must be updated to reflect final module moves/deletions.

## Required Test Scenarios
1. Legacy-vs-thesis-core equivalence corpus for lambda, let-polymorphism, and annotation-heavy expressions.
2. Regression corpus currently sensitive to boundary shape:
`BUG-002-V1..V4`, `BUG-004-V1..V3`, nested let + annotated lambda cases, polymorphic instantiation examples.
3. Checked-authoritative parity:
`runPipelineElab` vs `runPipelineElabChecked` on representative corpus.
4. Structural safety guard:
no replay APIs used in production run-path modules.
5. Full suite:
`cabal build all && cabal test`.

## Assumptions and Defaults
1. Chosen scope: Runtime semantics removal is mandatory (not file-local cosmetic removal).
2. Chosen rollout: 2-stage safety is mandatory (harden tests first, then cutover).
3. Behavioral compatibility target: preserve current passing semantics for Phase 6 and pipeline baselines while removing replay/mediation runtime dependency.
4. Documentation policy: do not mark closeout complete until runtime-callgraph and full gate are green.
