# Automatic Iso-Recursive Type Inference — Implementation Roadmap

## Context

- This roadmap family succeeds all prior bounded research/settlement families
  (18 roadmap families, rounds 001–138). Those families established that:
  - `TyMu`, `STMu`, `TMu`, `ERoll`/`EUnroll` all exist in the codebase
  - Explicit iso-recursive annotations work end-to-end through the pipeline
  - The constraint graph, solver, reifier, and elaborator all handle `TyMu` nodes
  - The acyclicity check currently rejects type-level cycles as hard errors
  - No automatic path exists from cycle detection to `TyMu` introduction
- The goal of this family is **implementation**, not further research or
  settlement surfaces. Every item produces working code and passing tests.
- The production baseline: explicit `μ` annotations work; unannotated
  recursive definitions (e.g., `let f = λx. f x`) fail at the acyclicity
  check or produce incorrect types.
- Thesis alignment: the thesis assumes well-typed ML programs produce acyclic
  constraint graphs. Iso-recursive types are an extension beyond the core
  thesis, but the existing `TyMu` infrastructure was designed to support them.
  Any deviation from thesis behavior for non-recursive programs must be
  documented and tested.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Implement cycle detection and automatic μ-introduction in the constraint solver
   Item id: `item-1`
   Depends on: none
   Parallel safe: no
   Parallel group: none
   Merge after: none
   Completion notes: complete when the acyclicity check (`MLF.Constraint.Acyclicity`)
   or a new post-acyclicity pass can detect type-level cycles in the constraint
   graph and automatically introduce `TyMu` nodes to break them, converting
   cyclic type structures into iso-recursive `μ` types. The existing
   `checkAcyclicity` may be extended or a new `breakCycles` pass added between
   normalization and presolution. Non-recursive programs must continue to work
   identically. The implementation must pass `cabal build all && cabal test`
   with zero regressions.

2. [done] Wire automatic μ-introduction into the pipeline and add focused test coverage
   Item id: `item-2`
   Depends on: `item-1`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-1`
   Completion notes: complete when `MLF.Elab.Run.Pipeline` (or the relevant
   pipeline entry point) integrates the new cycle-breaking pass so that
   `runPipelineElab` and `runPipelineElabChecked` automatically infer
   iso-recursive types for unannotated recursive definitions. Add focused
   tests in `test/PipelineSpec.hs` covering:
   - simple self-recursive function (`let f = λx. f x`)
   - mutually recursive via nested lets
   - recursive data-like patterns (`let lst = λx. λxs. (x, xs)` used recursively)
   - non-recursive programs still produce identical results
   All tests pass via `cabal build all && cabal test`.

3. [pending] Ensure reification and elaboration produce correct μ-types and roll/unroll terms
   Item id: `item-3`
   Depends on: `item-2`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-2`
   Completion notes: complete when automatically inferred `TyMu` nodes
   reify to `TMu` types in elaborated output, elaborated terms include
   appropriate `ERoll`/`EUnroll` coercions at recursive use sites, and
   the Phase 7 type checker (`MLF.Elab.TypeCheck`) accepts the elaborated
   terms. Add focused tests verifying:
   - elaborated type of a self-recursive function includes `μ`
   - `ERoll`/`EUnroll` appear in the elaborated term
   - `typeCheck` succeeds on the elaborated term
   - `runPipelineElabChecked` succeeds end-to-end
   All tests pass via `cabal build all && cabal test`.

4. [pending] Harden edge cases: nested recursion, polymorphic recursion, interaction with ∀
   Item id: `item-4`
   Depends on: `item-3`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-3`
   Completion notes: complete when the automatic iso-recursive inference handles
   edge cases correctly:
   - nested recursive lets
   - recursive functions with polymorphic type annotations
   - interaction between `μ` and `∀` quantifiers (recursive polymorphic types)
   - recursive functions that return functions (higher-order recursion)
   - already-annotated `μ` types continue to work unchanged
   Add regression tests for each case. All tests pass via
   `cabal build all && cabal test`.

5. [pending] End-to-end validation: Phase 7 reduction of iso-recursive elaborated terms
   Item id: `item-5`
   Depends on: `item-4`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-4`
   Completion notes: complete when the full pipeline (inference → elaboration →
   type checking → reduction) works end-to-end for automatically-inferred
   recursive types. Specifically:
   - `runPipelineElabChecked` succeeds for recursive definitions
   - the elaborated term type-checks via Phase 7
   - `step`/`normalize` can reduce recursive applications (including
     roll/unroll reduction)
   - add integration tests in `test/TypeSoundnessSpec.hs` proving type
     soundness for automatically inferred recursive types
   All tests pass via `cabal build all && cabal test`.

6. [pending] Update documentation and record readiness
   Item id: `item-6`
   Depends on: `item-5`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-5`
   Completion notes: complete when `implementation_notes.md`, `roadmap.md`,
   `TODO.md`, and `CHANGELOG.md` record that automatic iso-recursive type
   inference is implemented and tested. Update `docs/thesis-deviations.yaml`
   if the automatic μ-introduction deviates from the thesis assumption of
   acyclic constraint graphs. Final `cabal build all && cabal test` gate passes.
