# Task Plan: BUG-2026-02-17-002 A6 applied-coercion let mismatch

## Objective
Track and reintroduce the A6 expected-pass applied bounded/coercion case as an open bug reproducer with regression scaffold coverage.

## Scope
- Add open bug entry to `Bugs.md`.
- Add a non-flaky regression scaffold test that reproduces the current failure class.
- Update `TODO.md` task rollup to reflect the open follow-up.

## Phases
1. Confirm failing candidate shape and failure class. (in progress)
2. Add regression scaffold in test suite. (pending)
3. Add/format bug tracker entry. (pending)
4. Run focused verification and sync task logs. (pending)

## Decisions
- Keep suite green by asserting current failure class in sentinel form (expected-pass bug repro).
- Track target expected behavior (`Int`) in bug metadata rather than forcing a red suite.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## 2026-02-17 execution update
- Phase 1: completed (confirmed candidate failure class remains `TCLetTypeMismatch` on unchecked path).
- Phase 2: completed (added test scaffold sentinel in `test/PipelineSpec.hs`).
- Phase 3: completed (added open bug record in `Bugs.md` and TODO task tracking entry).
- Phase 4: completed (focused and full verification green).

### Verification
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002 sentinel"'` -> PASS
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "A6"'` -> PASS
- `cabal build all && cabal test` -> PASS

## 2026-02-17 closure update
- Tracking + regression-scaffold task completed and archived.
- Underlying bug remains open in `/Volumes/src/mlf4/Bugs.md` as `BUG-2026-02-17-002`.
