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
1. [in_progress] Phase 1 root-cause investigation (repro + trace + evidence)
2. [pending] Phase 2 pattern analysis (working-vs-broken comparison)
3. [pending] Phase 3 hypothesis and minimal validation
4. [pending] Phase 4 implementation + tests + verification
5. [pending] Update docs/tracker/task artifacts

## Decisions
- Start by instrumenting through existing test helpers instead of ad hoc code changes.
- Keep V2 and V4 as separate reproduction lanes until evidence shows common root cause.

## Errors Encountered
- None so far.
