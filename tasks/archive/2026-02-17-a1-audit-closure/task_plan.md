# A1 Audit + Closure — Task Plan

## Goal
Audit TODO item A1 (`P1` strict Ω normalization in production witness path) against implementation/tests and close it if acceptance criteria are satisfied.

## Scope
- Verify no permissive fallback acceptance remains for malformed merge-direction witness normalization.
- Verify production path fails fast with explicit error.
- Verify/refresh regression tests and docs/trackers required for closure.

## Phases
- [completed] Phase 1: Locate A1 acceptance criteria and current implementation state.
- [completed] Phase 2: Execute targeted and full verification commands for A1 evidence.
- [completed] Phase 3: Apply minimal code/test/doc updates needed for closure.
- [completed] Phase 4: Sync trackers/docs (`TODO.md`, `implementation_notes.md`, optional `CHANGELOG.md`) and archive task.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `cabal test ... --test-options='--match fails fast ...'` parsed as separate CLI args (`unexpected argument 'fast'`) | 1 | Used nested quoting: `--test-options=\"--match=\\\"fails fast ...\\\"\"` |
