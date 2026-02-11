# Task Plan: BUG-2026-02-11-003 Systematic Debugging

## Goal
Find and fix the root cause of BUG-2026-02-11-003 (BUG-004 V2/V4 failures) so checked/unchecked pipelines accept the intended annotated instantiation paths and return `Int`.

## Scope
- Follow `systematic-debugging` phases strictly.
- Reproduce both failing variants with deterministic evidence.
- Trace failure path across pipeline stages (constraint -> presolution witnesses -> phi/elab/typecheck).
- Add strict success regressions for BUG-004-V2 and BUG-004-V4 after fix.
- Run targeted + full validation.

## Phases
1. [completed] Phase 1 root-cause investigation (repro + trace + evidence)
2. [completed] Phase 2 pattern analysis (working-vs-broken comparison)
3. [completed] Phase 3 hypothesis and minimal validation
4. [completed] Phase 4 implementation + tests + verification
5. [completed] Update docs/tracker/task artifacts

## Decisions
- Start by instrumenting through existing test helpers instead of ad hoc code changes.
- Keep V2 and V4 as separate reproduction lanes until evidence shows common root cause.
- Keep the V2-focused Phi reorder/identity fixes (`Finalize` + `Omega` scheme-arity reorder requirement).
- Revert broad Omega behavior relaxations (`binderKeys` early return, unconditional weaken keep-all, graft skip outside keep-set) after they triggered unrelated gate regressions.
- Fix V4 via a narrow elaboration + typecheck combination:
  - collapse closed bounded-identity lambda annotation types (`∀a⩾τ.a` with closed `τ`) in desugared-ann-lambda parameter recovery;
  - accept `InstBot` when argument is alpha-equal to the current bound in `TypeCheck` (matching `applyInstantiation` behavior).

## Errors Encountered
- A broad first attempt (`TypeCheck` relax + wider Omega changes) regressed unrelated suites (`PipelineSpec` make path + Φ witness soundness + thesis target baselines).
- Recovery: isolate with targeted tests, restore Omega semantics to near-baseline (keeping only V2-relevant reorder adjustment), and re-validate incrementally before full gate.
