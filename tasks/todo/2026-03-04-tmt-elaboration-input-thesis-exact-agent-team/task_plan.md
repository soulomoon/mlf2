# Task Plan: TMT elaboration-input thesis-exact closeout

## Goal
Execute `/Volumes/src/mlf4/docs/plans/2026-03-04-tmt-elaboration-input-thesis-exact-agent-team-implementation-plan.md` with agent teams to make the Elaboration input row thesis-exact while preserving checked-authoritative behavior.

## Phases
- [x] Wave 0 / Team A: add elaboration-input thesis-exact guards + parity characterization.
- [x] Gate A.
- [x] Wave 1 / Team B: chi-native helper surfaces (generalize/scope/typeops/reify).
- [x] Wave 1 / Team C: eliminate elaboration-path internal `chiSolved` dependency + callback signature migration.
- [x] Gate B.
- [x] Wave 2 / Team D: pipeline/result-type callsite alignment.
- [x] Wave 2 / Teams C+D: cleanup sweep + guard hardening.
- [x] Gate C.
- [x] Wave 3 / Team E: docs + tracker closeout.
- [x] Final gate: `cabal build all && cabal test`.

## Errors Encountered
| Time | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-04 | None | 1 | N/A |
| 2026-03-04 | `git` unavailable in shell PATH during initial worktree setup | 1 | Switched to `/usr/bin/git` for all git invocations |
| 2026-03-04 | Initial Team B/C worktrees were contaminated with unexpected commits/dirty state | 1 | Recreated clean team worktrees (`-v2`/`-v3`) from integrated head |
| 2026-03-04 | Parallel Cabal matcher runs caused package-dir contention (`package.conf.inplace` / `.tmp` collisions) | 1 | Re-ran affected matchers sequentially with `-j1` and captured non-empty evidence |
| 2026-03-04 | Team D/Task 4 wiring already aligned before edits (no source delta in Team D file set) | 1 | Recorded explicit no-op Task 4 commit (`--allow-empty`) with required message |
| 2026-03-04 | Team C/Task 3 cherry-pick on integration head produced empty patch | 1 | Verified equivalent changes already present on integrated head and accepted via Gate B evidence |

## Notes
- Thesis source-of-truth: `papers/these-finale-english.txt` §15.3.5 Def. 15.3.12, §15.3.6.
- Preserve checked-authoritative output as a hard invariant.
