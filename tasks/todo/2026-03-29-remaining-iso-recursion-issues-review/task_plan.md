# Remaining Iso-Recursion Issues Review

## Goal

Determine whether the two remaining issues named in the live iso-recursive gap-fix roadmap are fixable, and if so whether they require implementation changes or documentation/status correction only.

## Phases

| Phase | Status | Notes |
| --- | --- | --- |
| 1. Read current roadmap/round artifacts | complete | Inspected roadmap plus round-146..150 plans/reviews/implementation notes |
| 2. Inspect live implementation/tests for both issues | complete | Traced nested-forall μ absorption and non-local proxy PhiTranslatabilityError through tests and source |
| 3. Judge fixability and scope | complete | Nested-forall case is status/semantics, non-local proxy case is a real elaboration-layer gap |
| 4. Synthesize answer | complete | Final answer prepared with concrete recommendations and caveats |

## Decision Notes

- Treat code and tests as primary truth; use orchestrator artifacts as intent/evidence, not as proof by themselves.

## Errors Encountered

| Error | Attempt | Resolution |
| --- | --- | --- |
| Parallel focused `cabal test` reruns collided in `dist-newstyle`; one successful focused run also ended with sandbox-denied write to `/Users/ares/.cache/cabal/logs/build.log`. | 1 | Treat as harness noise; rely on prior full gate, sequential focused evidence, and source inspection rather than parallel reruns. |
