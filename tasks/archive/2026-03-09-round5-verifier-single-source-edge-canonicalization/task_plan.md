# Task Plan

## Goal
Decide whether the proposed simplification "Single-source edge witness/trace canonicalization" is still needed, bounded, worthwhile, and thesis-safe.

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| Unrelated test-suite compile failure (`Solved.fromPreRewriteState` no longer exported) while running targeted verifier tests | 1 | Treat as external to this simplification; do not modify user workspace |
| Initialize verifier context | complete | Created task notes and collected repo/doc targets |
| Review papers and project guidance | complete | Checked thesis translation constraints and repo guidance/docs |
| Inspect current implementation | complete | Confirmed duplicate witness/trace helper bodies and live call sites |
| Evaluate verdict | complete | Verdict: YES — still needed, bounded to witness/trace only, and thesis-safe if expansion canonicalization stays local |

## Decisions
- Accept the simplification as a bounded follow-up: move only `canonicalizeWitness` and `canonicalizeTrace` into `MLF.Constraint.Canonicalization.Shared`, keep `canonicalizeExpansion` where it is because the presolution and runtime variants intentionally differ on `ExpForall`.
- Keep `MLF.Constraint.Presolution.Rewrite` and `MLF.Elab.Run.Util` as thin consumers so `Driver` and `Pipeline` data flow remains unchanged.
- Use `test/CanonicalizerSpec.hs` for both behavior-preservation assertions and a direct source guard that prevents new local `canonicalizeWitness` / `canonicalizeTrace` copies from reappearing in the two consumer modules.
- Update `implementation_notes.md` and `CHANGELOG.md` in the same change because the canonicalization ownership/documented architecture moves, even though runtime behavior should not.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| Unrelated test-suite compile failure (`Solved.fromPreRewriteState` no longer exported) while running targeted verifier tests | 1 | Treat as external to this simplification; do not modify user workspace |
| `mv` into `tasks/todo/...` tried to preserve owner/group metadata and failed with `Operation not permitted` | 1 | Rewrote the markdown files in place with shell redirection instead of `mv` |
