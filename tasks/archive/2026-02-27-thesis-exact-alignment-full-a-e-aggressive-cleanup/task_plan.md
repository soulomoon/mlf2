# Task Plan: Thesis-Exact Alignment (Full A-E, Aggressive Cleanup)

## Goal
Implement `docs/plans/2026-02-27-thesis-exact-alignment-plan-full-a-e-aggressive-cleanup.md` end-to-end with thesis-exact ordering, strict presolution artifact contracts, projection-first solved usage, strict witness-first Phi translation, and legacy path cleanup.

## Scope
- Execute Phases A-E from the plan.
- Add/adjust tests to enforce new invariants.
- Update docs and trackers (`implementation_notes.md`, `docs/thesis-deviations.yaml`, `TODO.md`, `CHANGELOG.md`, `Bugs.md`) as needed.

## Phases
| Phase | Status | Notes |
|---|---|---|
| A. Thesis-exact ordering + seeded closure | completed | Seeded closure API + presolution boundary assertions implemented and tested |
| B. Presolution artifact contract | completed | Producer-boundary invariants + explicit PresolutionError contract checks implemented |
| C. Solved projection-first cutover | completed | Removed canonical-domain Solved queries; migrated fallback/reify consumers |
| D. Phi strict witness-first semantics | completed | Strict witness-domain default with explicit class-fallback lane + telemetry |
| E. Elaboration boundary cleanup + legacy removal | completed | Removed runtime projection-first entrypoint; retained test-only parity harness |
| Verification + docs + tracker sync | completed | `cabal build all && cabal test` passing; notes/changelog/TODO updated |

## Decisions
- Follow thesis anchors as normative source (`papers/these-finale-english.txt`).
- Favor minimal diffs that preserve behavior except where plan intentionally changes semantics.
- Keep failures deterministic with explicit error constructors/messages.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| Parallel `cabal test` invocations raced on build artifacts (`rename ... Solved.o.tmp ... does not exist`) | 1 | Re-ran all verification commands sequentially; avoided parallel Cabal runs for the session |
