# Task Plan: TMT row2 result-type/elab adapter retirement

## Goal
Execute `/Volumes/src/mlf4/docs/plans/2026-03-04-tmt-row2-resulttype-adapter-retirement-agent-team-implementation-plan.md` with agent teams to retire `rtcSolvedCompat`, `rtcSolveLike`, and `ElabConfig.ecSolved` while preserving checked-authoritative output.

## Phases
- [x] Wave 0 / Team A: add row2 closeout guard tests + parity characterization.
- [x] Gate A: row2 closeout guard + checked-authoritative parity slices.
- [x] Wave 1 / Team B: remove `rtcSolvedCompat`/`rtcSolveLike` from result-type boundary.
- [x] Wave 1 / Team C: remove `ElabConfig.ecSolved` compatibility handle.
- [x] Gate B: row2 ResultType + Phase 6 Elaborate slices.
- [x] Wave 2 / Team D (+ Team A support): wire pipeline/result-type callsites + test fixtures.
- [x] Wave 2 / Teams C+D: cleanup sweep and source guard hardening.
- [x] Gate C: Pipeline + Dual-path verification + checked-authoritative slices.
- [x] Wave 3 / Team E: docs/changelog/todo/task evidence closeout.
- [x] Final gate: `cabal build all && cabal test`.

## Team Ownership
- Team A (`contracts`): `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`
- Team B (`resulttype-view`): `src/MLF/Elab/Run/ResultType/Types.hs`, `src/MLF/Elab/Run/ResultType/View.hs`
- Team C (`elab-phi`): `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run/ChiQuery.hs`, `src/MLF/Elab/Phi/Translate.hs`, `src/MLF/Elab/Phi/TestOnly.hs`
- Team D (`pipeline-wire`): `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType.hs`, `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, `src/MLF/Elab/Run/ResultType/Util.hs`
- Team E (`verifier`): verification + docs + task trackers

## Wave/Gate Evidence (Team E verification pass on 2026-03-04)
- Wave 0 / Team A evidence:
  - Row2 closeout guards are present and executable in
    `test/PipelineSpec.hs` and `test/ElaborationSpec.hs` under matcher
    `row2 closeout guard`.
- Gate A evidence:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
    -> PASS (`3 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    -> PASS (`8 examples, 0 failures`).
- Wave 1 / Team B evidence:
  - `src/` no longer contains `rtcSolvedCompat` or `rtcSolveLike`
    (`rg` shows occurrences only in row2 guard tests).
- Wave 1 / Team C evidence:
  - `src/` no longer contains `ecSolved`
    (`rg` shows occurrences only in row2 guard tests).
- Gate B evidence:
  - `row2 closeout guard` includes both result-type and Phase 6 Elaborate
    migration guards, all PASS.
- Wave 2 / Teams C/D evidence:
  - Pipeline and result-type call-path behavior remains
    checked-authoritative with dual-path guards green.
- Gate C evidence:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    -> PASS (`4 examples, 0 failures`).
- Final gate evidence:
  - `cabal build all && cabal test` -> PASS (`929 examples, 0 failures`).

## Errors Encountered
| Time | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-04 | None | 1 | N/A |

## Notes
- Primary source of truth remains `papers/these-finale-english.txt`.
- Preserve checked-authoritative output policy as hard invariant through all waves.
- Gate commands should use non-empty matcher evidence; if a matcher yields zero examples, run the fallback commands listed in the plan.
