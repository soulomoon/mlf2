# Retry Subloop Contract

This file defines retry behavior for the codebase quality and coverage
improvements loop.

The active roadmap bundle is resolved from `orchestrator/state.json`:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`

## Scope

- **item-1** (test coverage) retries for: incomplete spec wiring, tests that don't exercise meaningful behavior, missing `mlf2.cabal` entries.
- **item-2** (InstBot fix) retries for: fix that doesn't address the root cause, missing regression test, `Bugs.md` not updated.
- **item-3** (QuickCheck) retries for: trivial properties that don't test real invariants, missing `Arbitrary` instances, properties that don't run enough cases.
- **item-4** (module decomposition) retries for: behavioral changes during split, broken downstream imports, facade modules that are too large (>200 lines), missing `mlf2.cabal` entries.
- **item-5** (research hygiene) retries for: main library still depending on research modules, broken research module compilation, incomplete Cabal stanza separation.
- **item-6** (parameter bundling) retries for: record types that don't follow existing naming patterns, behavioral changes during refactor, insufficient arity reduction.
- **item-7** (golden tests) retries for: golden files not checked in, missing accept workflow documentation, tests that don't cover canonical examples.
- **item-8** (public API) retries for: missing Haddock comments, missing tests for new exports, leaking internal types through public API.
- **item-9** (Note audit) retries for: dangling function references still present, missing Notes on new submodules, stale `implementation_notes.md` content.
- Review may reject and return the same round to `plan`.
- Maximum 3 retry attempts per round before escalation.

## Machine State

`orchestrator/state.json` must carry:

- `contract_version: 2`
- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`
- `retry: null` when idle, or a retry object when the same round is looping

## Review Output

Every review must record:

- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`

Allowed combinations:

- `accepted + finalize`
- `rejected + retry`

Forbidden combinations:

- `rejected + finalize`

## Transition Rules

After review:

- `accepted + finalize`
  - controller clears `retry`
  - controller advances to `merge`
- `rejected + retry`
  - controller records the attempt in `attempt-log.jsonl`
  - controller increments `retry.attempt`
  - controller returns the same round to `plan`

## Escalation

After 3 consecutive rejected attempts on the same round:

- controller sets `controller_stage: "blocked"`
- controller records the failure summary in `resume_error`
- the orchestrator must not proceed until the controller receives human input
  or a new recovery recommendation
