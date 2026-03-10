# Task Plan

## Goal
Decide whether retiring redundant `preferGenScope` from `MLF.Elab.Run.Scope` is still needed, bounded, worthwhile, and thesis-safe.

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| Initialize verifier context | complete | Created task notes and aligned with adjacent round artifacts |
| Inspect scope implementation and call sites | complete | Confirmed live export, single production caller, and bounded test surface |
| Check thesis and tracking docs | complete | Re-read thesis anchor and repo tracking docs; no positive semantic justification found |
| Evaluate verdict | complete | Compile blocker was later fixed on `master`; cleanup is already landed and guarded in the current tree |

## Decisions
- Follow `papers/these-finale-english.txt` as the primary source of truth and consult `papers/xmlf.txt` only if the thesis is silent.
- Keep this verifier pass read-only with respect to implementation files; respect unrelated working-tree edits already present in `src/MLF/Constraint/Solved.hs` and `test/SolvedFacadeTestUtil.hs`.
- Reuse nearby round-5 verifier and round-6 thinker artifacts for context, but base the verdict on the live repository state.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| `cabal test mlf2-test --test-show-details=direct --test-options='--match "ga scope"'` failed during test-suite compilation because `Solved.fromPreRewriteState` is no longer exported | 1 | Treat as unrelated workspace blocker owned by `MLF.Constraint.Solved`/test harness; do not modify it in this verifier pass |
