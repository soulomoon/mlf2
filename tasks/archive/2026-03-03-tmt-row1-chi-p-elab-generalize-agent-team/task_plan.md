# Task Plan: TMT row1 chi-p elab/generalize closeout

## Goal
Execute `/Volumes/src/mlf4/docs/plans/2026-03-03-tmt-row1-chi-p-elab-generalize-agent-team-implementation-plan.md` with agent teams to remove `eeSolvedCompat` and entry-time `Solved.rebuildWithConstraint` while preserving checked-authoritative output.

## Phases
- [x] Wave 0 / Team A: add row1 closeout guard tests and parity characterization.
- [x] Gate A: chi-first + row1 guard + representative checked-authoritative slice.
- [x] Wave 1 / Team B: introduce view-first generalization entry points.
- [x] Wave 1 / Team C: migrate Elaborate/Scope helper reads to chi-first queries.
- [x] Gate B: Elaborate + ResultType chi-first slices.
- [x] Wave 2 / Teams C+D: remove `eeSolvedCompat` and entry rebuild.
- [x] Wave 2 / Team D: migrate remaining row1-relevant generalization callsites.
- [x] Gate C: Pipeline + Dual-path verification + checked-authoritative slices.
- [x] Wave 3 / Team E: docs/changelog/todo/bugs closeout.
- [x] Final gate: `cabal build all && cabal test`.

## Team Ownership
- Team A (`contracts`): `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`
- Team B (`generalize-api`): `src/MLF/Elab/Run/Generalize.hs`, `src/MLF/Elab/Run/ResultType/Util.hs`
- Team C (`elab-core`): `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run/Scope.hs`, `src/MLF/Elab/Run/ChiQuery.hs`
- Team D (`pipeline-wire`): `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType/*`
- Team E (`verifier`): verification + docs

## Errors Encountered
| Time | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-03 | None | 1 | N/A |
| 2026-03-03 | Gate A matcher returned `0 examples` with the plan's literal pattern | 1 | Kept exact gate command for compliance; used Task 1 red matcher to verify new guards fail as intended (`2 examples, 2 failures`). |
| 2026-03-03 | Gate B / Gate C matcher strings returned `0 examples` with literal `|` patterns | 1 | Ran exact gate commands for compliance, then verified equivalent narrow matchers (`generalizeWithPlan`, `chi-first Elaborate|Phase 6`, `ResultType`, `Pipeline (Phases 1-5)`, `Dual-path verification`, `checked-authoritative`). |
| 2026-03-03 | Parallel Cabal invocations created transient build-cache lock/race failures | 1 | Re-ran failed commands sequentially; all verification commands passed. |

## Notes
- Primary source of truth remains `papers/these-finale-english.txt`.
- Preserve checked-authoritative output policy as hard invariant through all waves.
- Wave 0 commit: `bf9f6a49ca770512ee618a5bc51d94d5b8923a4d` (`test: add row1 closeout guards for eeSolvedCompat and entry rebuild`).
- Wave 1 commits:
  - `54be25f` (`refactor: migrate elaborate/scope helper reads to chi-first queries`)
  - `0f737d6` (`refactor: add view-first generalization helper entry points`)
- Wave 2 commits:
  - `84b0b84` (`refactor: remove eeSolvedCompat and elaborate entry-time solved rebuild`)
  - `817b803` (`refactor: migrate row1-relevant generalization callsites to chi-first contracts`)
- Wave 3 commit:
  - `5b1c154` (`docs: record row1 chi-first closeout and verification evidence`)
