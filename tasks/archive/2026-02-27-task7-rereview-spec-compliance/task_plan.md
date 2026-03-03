# Task Plan - Task 7 Re-review Spec Compliance

## Goal
Re-verify Task 7 compliance in `/Volumes/src/mlf4-strict-replay-cutover` after coverage commit `6c00c8c42dc65b216ce6b2cf8a5cd3573bb8dda4`, focused on OpRaise fail-fast behavior and matcher-slice status.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Gather context (commit diff + relevant modules/tests) | completed | `6c00c8c4` changes only `test/ElaborationSpec.hs`; `8fad0bb` carries `Omega` fail-fast branch |
| 2. Validate behavior requirements (trace-source fail-fast, non-trace-source no-op) | completed | `Omega.hs` still has trace-source unresolved-target fail-fast + non-trace-source no-op fallback |
| 3. Validate new regression test assertions | completed | New ElaborationSpec test passes and asserts `PhiInvariantError` with expected diagnostic substring |
| 4. Check broader matcher subset failures causality | completed | A6/BUG-002/BUG-003 failures reproduce identically on `HEAD` and parent `8fad0bb`; `MissingEdgeTrace` passes on both |
| 5. Report findings | completed | Ready to report compliance verdict with causality notes |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| Hspec OR matcher string (`OpRaise|A6|...`) matched 0 tests | 1 | Ran separate matcher buckets (`OpRaise`, `A6`, `BUG-002`, `BUG-003`, `MissingEdgeTrace`) and compared against parent commit |
