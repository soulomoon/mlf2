# Progress: 2026-03-06 Row6 Replay-Contract Recovery

## Session log
- 2026-03-05T19:02:17Z initialized task folder and planning files.
- 2026-03-06T03:11:00Z verified clean baseline in isolated worktree: `cabal build all && cabal test` -> PASS (`951 examples, 0 failures`).
- 2026-03-06T03:29:00Z identified Wave A producer gaps in clean baseline: `WitnessNorm` still derives replay lane from post-pruning op shape and still drops non-root no-replay graft/weaken ops instead of rejecting them; `Driver` boundary validation also lacks replay-codomain injectivity checks.
- 2026-03-06T04:05:00Z compared clean baseline vs recovery branch artifacts for `\\y. let id = (\\x. x) in id y`, bug-002, A6, and BUG-2026-02-08-004 using isolated baseline/current worktrees; confirmed the regression came from rewritten-space no-replay weakening decisions, not consumer drift.
- 2026-03-06T04:27:00Z rewrote no-replay projection in `WitnessNorm` to operate on restored/source identities, preserved baseline graft-target weakening behavior, pruned wrapper `OpRaise` under non-type-tree parents, and added narrowed rogue-graft fail-fast.
- 2026-03-06T04:34:00Z recovered targeted gates:
  - `R-RAISE-INVALID-11`,
  - no-replay source-key / rogue-graft / wrapper-raise witness obligations,
  - `checked-authoritative`,
  - `Dual-path verification`,
  - bug-002 strict fail-fast,
  - `\\y. let id = (\\x. x) in id y`,
  - BUG-2026-02-08-004,
  - A6 parity,
  - BUG-2026-02-17-002,
  - explicit forall round-trip.
- 2026-03-06T04:39:00Z final verification `cabal build all && cabal test` -> PASS (`954 examples, 0 failures`).
