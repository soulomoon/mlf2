# Automatic Iso-Recursive Type Inference — Completion Roadmap

## Context

- This roadmap family succeeds
  `2026-03-29-00-automatic-iso-recursive-type-inference-implementation`
  (rounds 139–142). That family established:
  - Cycle detection and automatic `TyMu` introduction in the constraint solver
    (`breakCyclesAndCheckAcyclicity` in `MLF.Constraint.Acyclicity`)
  - Automatic μ-introduction wired into the pipeline with focused test coverage
    (simple self-recursion, nested lets, recursive data patterns)
  - Reification producing correct `TMu` types and elaboration emitting
    `ERoll`/`EUnroll` coercions accepted by the Phase 7 type checker
  - Edge-case hardening for nested recursion, polymorphic recursion with
    annotation, μ/∀ interaction, higher-order recursion, and explicit-μ
    stability (1168 examples, 0 failures)
- What remains for automatic iso-recursive type inference readiness:
  - End-to-end validation including Phase 7 reduction of auto-inferred
    recursive terms (roll/unroll reduction)
  - Documentation, changelog, and readiness recording
- The production baseline: all 1168 tests pass, automatic μ-inference works
  for unannotated recursive definitions through elaboration and type checking,
  but reduction behavior for auto-inferred recursive terms is not yet
  validated.
- Thesis alignment: iso-recursive types are an extension beyond the core
  thesis (which assumes acyclic constraint graphs). This extension is
  documented and tested. Any deviation from thesis behavior for non-recursive
  programs is already covered by the existing test suite.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] End-to-end validation: Phase 7 reduction of iso-recursive elaborated terms
   Item id: `item-1`
   Depends on: none
   Parallel safe: no
   Parallel group: none
   Merge after: none
   Completion notes: complete when the full pipeline (inference -> elaboration ->
   type checking -> reduction) works end-to-end for automatically-inferred
   recursive types. Specifically:
   - `runPipelineElabChecked` succeeds for recursive definitions
   - the elaborated term type-checks via Phase 7
   - `step`/`normalize` can reduce recursive applications (including
     roll/unroll reduction steps)
   - add integration tests in `test/PipelineSpec.hs` (or a new
     `test/TypeSoundnessSpec.hs` if appropriate) proving type soundness for
     automatically inferred recursive types
   - verify that non-recursive programs still produce identical results
   All tests pass via `cabal build all && cabal test`.

2. [done] Update documentation and record iso-recursive inference readiness
   Item id: `item-2`
   Depends on: `item-1`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-1`
   Completion notes: complete when:
   - `implementation_notes.md` records that automatic iso-recursive type
     inference is implemented and tested, with a summary of the mechanism
     (cycle detection -> TyMu introduction -> reification -> roll/unroll
     elaboration -> Phase 7 acceptance)
   - `roadmap.md` records iso-recursive inference as a completed capability
   - `TODO.md` records the completion of the iso-recursive inference campaign
   - `CHANGELOG.md` records the new capability
   - `docs/thesis-deviations.yaml` is created or updated if the automatic
     μ-introduction deviates from the thesis assumption of acyclic constraint
     graphs (document the extension clearly)
   - Final `cabal build all && cabal test` gate passes with zero regressions

3. [done] Final readiness gate: clean up orchestrator state and declare readiness
   Item id: `item-3`
   Depends on: `item-2`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-2`
   Completion notes: complete when:
   - all orchestrator worktrees for this campaign are cleaned up
   - `orchestrator/state.json` reflects terminal completion
   - the base branch has all changes merged and passes `cabal build all &&
     cabal test`
   - a final readiness summary is recorded confirming automatic iso-recursive
     type inference is production-ready
