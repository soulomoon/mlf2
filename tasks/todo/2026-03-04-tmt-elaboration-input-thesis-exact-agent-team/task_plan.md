# Task Plan: TMT elaboration-input thesis-exact closeout

## Goal
Execute `/Volumes/src/mlf4/docs/plans/2026-03-04-tmt-elaboration-input-thesis-exact-agent-team-implementation-plan.md` with agent teams to make the Elaboration input row thesis-exact while preserving checked-authoritative behavior.

## Phases
- [ ] Wave 0 / Team A: add elaboration-input thesis-exact guards + parity characterization.
- [ ] Gate A.
- [ ] Wave 1 / Team B: chi-native helper surfaces (generalize/scope/typeops/reify).
- [ ] Wave 1 / Team C: eliminate elaboration-path internal `chiSolved` dependency + callback signature migration.
- [ ] Gate B.
- [ ] Wave 2 / Team D: pipeline/result-type callsite alignment.
- [ ] Wave 2 / Teams C+D: cleanup sweep + guard hardening.
- [ ] Gate C.
- [ ] Wave 3 / Team E: docs + tracker closeout.
- [ ] Final gate: `cabal build all && cabal test`.

## Errors Encountered
| Time | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-04 | None | 1 | N/A |

## Notes
- Thesis source-of-truth: `papers/these-finale-english.txt` §15.3.5 Def. 15.3.12, §15.3.6.
- Preserve checked-authoritative output as a hard invariant.
