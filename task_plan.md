# Task Plan

## Summary
Goal: implement deferred `.mlfp` program obligations so all 7 current
pending-success rows in `ProgramSpec` become strict runtime-success tests,
without widening public eMLF syntax or adding a direct `.mlfp -> ElabTerm`
route.

## Current Phase
Completed: deferred `.mlfp` program obligations are implemented and validated.

## Phases
1. Discovery and obligation design. - completed
2. Refactor `.mlfp` lowering/finalization for deferred program obligations. - completed
3. Move pending rows to strict matrix and add negative guards. - completed
4. Run focused validation and repair failures. - completed
5. Run full validation and update docs. - completed

## Decisions Made
| Decision | Rationale |
|----------|-----------|
| Keep public eMLF parser/API unchanged | User plan and repo invariants require `.mlfp`-internal obligations only |
| Preserve existing Church ADT runtime representation | Current recursive ADT/program runtime depends on this encoding |
| Use post-eMLF finalization as the dispatch point | Matches the existing deferred overload path and lets eMLF infer terms first |
| Defer ordinary ADT constructor applications, not constructor definitions | Source constructor uses can wait for eMLF inference while runtime Church constructor bindings remain the executable representation; GADT/existential constructors stay direct until obligations carry constructor-local `forall` evidence |

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| Installed `haskell-pro` path from `AGENTS.md` is missing | 1 | Follow repo formatting conventions directly |
| Planning skill relative path under worktree was absent | 1 | Read installed skill at `/Users/ares/.codex/skills/planning-with-files/SKILL.md` |
| Recursive overloaded method calls hit locked-node failures when resolved through synthetic local wrappers or reannotated pattern variables | 2 | Finalize instance method bodies as their runtime binding, defer overloaded runtime resolution, and leave pattern-bound recursive fields unannotated |
