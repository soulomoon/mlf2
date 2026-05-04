# Retry Subloop

Roadmap family: `2026-05-05-00-type-level-safety-singletons-roadmap`
Revision: `rev-001`

## Rules

- If a round fails the build/test gate, fix the issue in the same round
  before requesting approval.
- If a round introduces a type-level regression (new `error`/`undefined`
  for node-kind or phase discrimination), revert and redesign.
- Maximum 3 retry attempts per round. After 3 failures, return to
  `select-task` and reassess the milestone approach.
- Type-level refactors are inherently trial-and-error. GHC error messages
  for GADTs and type families can be misleading. Prefer small incremental
  changes over large rewrites.
