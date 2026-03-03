# Task Plan: TMT row2 result-type/elab adapter retirement

## Goal
Execute `/Volumes/src/mlf4/docs/plans/2026-03-04-tmt-row2-resulttype-adapter-retirement-agent-team-implementation-plan.md` with agent teams to retire `rtcSolvedCompat`, `rtcSolveLike`, and `ElabConfig.ecSolved` while preserving checked-authoritative output.

## Phases
- [ ] Wave 0 / Team A: add row2 closeout guard tests + parity characterization.
- [ ] Gate A: row2 closeout guard + checked-authoritative parity slices.
- [ ] Wave 1 / Team B: remove `rtcSolvedCompat`/`rtcSolveLike` from result-type boundary.
- [ ] Wave 1 / Team C: remove `ElabConfig.ecSolved` compatibility handle.
- [ ] Gate B: row2 ResultType + Phase 6 Elaborate slices.
- [ ] Wave 2 / Team D (+ Team A support): wire pipeline/result-type callsites + test fixtures.
- [ ] Wave 2 / Teams C+D: cleanup sweep and source guard hardening.
- [ ] Gate C: Pipeline + Dual-path verification + checked-authoritative slices.
- [ ] Wave 3 / Team E: docs/changelog/todo/task evidence closeout.
- [ ] Final gate: `cabal build all && cabal test`.

## Team Ownership
- Team A (`contracts`): `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`
- Team B (`resulttype-view`): `src/MLF/Elab/Run/ResultType/Types.hs`, `src/MLF/Elab/Run/ResultType/View.hs`
- Team C (`elab-phi`): `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run/ChiQuery.hs`, `src/MLF/Elab/Phi/Translate.hs`, `src/MLF/Elab/Phi/TestOnly.hs`
- Team D (`pipeline-wire`): `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType.hs`, `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, `src/MLF/Elab/Run/ResultType/Util.hs`
- Team E (`verifier`): verification + docs + task trackers

## Errors Encountered
| Time | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-04 | None | 1 | N/A |

## Notes
- Primary source of truth remains `papers/these-finale-english.txt`.
- Preserve checked-authoritative output policy as hard invariant through all waves.
- Gate commands should use non-empty matcher evidence; if a matcher yields zero examples, run the fallback commands listed in the plan.
