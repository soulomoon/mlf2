# Task Plan: Systematic Bug Variant Regression Matrix

## Goal
Add high-value variant regressions for resolved BUG-002/003/004 paths and US-010 witness normalization invariants so future refactors are guarded by realistic failure-pattern mutations.

## Scope
- Add BUG-002 variants (V1..V4) focused on polymorphic factory/generalization drift.
- Add BUG-004 variants (V1..V4) focused on checked-vs-unchecked parity and annotated-lambda acceptance.
- Add BUG-003 variants (V1..V2) focused on bounded alias/RaiseMerge translatability stability.
- Add US-010 witness-level variants (V1..V2) focused on step-boundary coalescing and single-binder widening guard.
- Run targeted and full validation gates.
- Sync bug tracker/docs for any newly discovered regressions.

## Phases
1. [complete] Map variants to concrete test locations and assertion style
2. [complete] Implement new regression tests
3. [complete] Run targeted suites and full `cabal build all && cabal test`
4. [complete] Update tracker/docs (`Bugs.md`, `CHANGELOG.md`, `TODO.md`)

## Decisions
- Extended existing regression sections in `test/ElaborationSpec.hs` and `test/Presolution/WitnessSpec.hs` to keep bug history locality.
- Kept passing variants as strict success assertions; converted newly failing variants to explicit sentinel assertions keyed by deterministic error fragments.
- Opened new bug IDs (`BUG-2026-02-11-002`..`004`) in `Bugs.md` in the same iteration, as required by repo bug-tracking rules.

## Errors Encountered
- `mv` while replacing `task_plan.md` emitted `set owner/group ... Operation not permitted`; file content still updated. Recovery: verify file contents and proceed.
- Compile failure after insertion in `test/ElaborationSpec.hs` (`The last statement in a 'do' block must be an expression`) due mis-indented `let` block in existing explicit-forall test. Recovery: corrected layout indentation.
- `US-010-V2` initially asserted `RaiseNotUnderRoot`; actual normalization drops exterior `OpRaise` first and fails on `OpOutsideInterior (OpMerge ...)`. Recovery: updated test expectation to deterministic current behavior.
