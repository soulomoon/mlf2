# Round 020 Plan (Roadmap Item 1: `D1` Replay-Failure Reproduction Contract)

## Objective

Execute only `D1` for `URI-R2-C1` and produce one reviewer-auditable artifact plus machine-readable evidence that answers a narrow question:

- can the authoritative `P2-W` replay failure be reproduced from the inherited authoritative `P1` subject token under the new shared root-cause entrypoint; or
- must this stage record a bounded non-pass or inconclusive result without widening?

`D1` must preserve the inherited controlling boundary:

- authoritative subject source: `round-016` `P1` attempt `2`
- accepted replay-failure boundary: `round-017` `P2` attempt `2`
- accepted mismatch signature: `partial-replay` with `InstBot expects ⊥, got: t9 -> t9`

This round must not localize ownership, probe repairs, or reopen implementation work. It only establishes whether the real inherited failure can be reproduced faithfully enough for `D2` to proceed.

## Locked Round Context

- Round id: `round-020`
- Branch: `codex/round-020`
- Worktree: `.worktrees/round-020`
- Roadmap item: `D1`
- Active attempt: `1`
- Active subject boundary: `URI-R2-C1`
- Scenario boundary: `uri-r2-c1-only-v1`
- Research entrypoint id: `uri-r2-c1-p2-replay-root-cause-v1`
- Stage selector to add or execute: `D1-replay-reproduction`
- Retry contract: `contract_version: 2`, so `D1` must accept bounded retry attempts `1..100`; this round runs `attempt-1`
- Authoritative inherited inputs:
  - `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`
  - `orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-W.json`
  - `orchestrator/rounds/round-017/evidence/P2/attempt-2/stage-verdict.json`
  - `orchestrator/rounds/round-017/evidence/P2/attempt-2/trace-bundle.json`
  - `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`
  - `orchestrator/rounds/round-017/review-record.json`

## Scope

1. Add bounded `D1` routing through the existing CLI research interface using only:
   - `--research-entrypoint uri-r2-c1-p2-replay-root-cause-v1`
   - `--stage-selector D1-replay-reproduction`
   - `--scenario-id uri-r2-c1-only-v1`
   - `--attempt-id <1..100>`
2. Reuse the real bounded replay lane from the accepted `P2` path rather than fabricating the old diagnostic from files.
3. Consume only the inherited authoritative `P1` subject token plus the accepted `P2` replay-failure evidence needed to verify continuity and compare signatures.
4. Emit canonical `D1` artifact at:
   - `docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md`
5. Emit attempt-local machine-readable outputs under:
   - `orchestrator/rounds/round-020/evidence/D1/attempt-1/`
6. Keep default `cabal run mlf2` behavior unchanged.

## Non-Goals

- No `D2`, `D3`, or `D4` logic.
- No ownership localization beyond confirming inherited input continuity and mismatch-signature comparison.
- No production-path behavior change.
- No second executable interface.
- No widened subject search, alternate scenario, surrogate subject, or cross-scenario comparison.
- No rewriting of any accepted `P1` through `P4` artifact, review record, or evidence file.
- No edits to controller-owned files such as `orchestrator/rounds/round-020/state-snapshot.json` or `orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/roadmap.md`.

## Implementation Slice

Primary files expected in scope:

1. `src/MLF/Research/URI/R2/C1/Prototype/Types.hs`
2. `src/MLF/Research/URI/R2/C1/Prototype/Entrypoint.hs`
3. `src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs`
4. `src/MLF/Research/URI/R2/C1/Prototype/D1.hs` (new)
5. `src/MLF/Research/URI/R2/C1/Prototype/P2.hs` only if strictly required to factor out the accepted bounded replay lane without changing `P2` semantics
6. `test/Research/UriR2C1PrototypeP1Spec.hs` or one new focused root-cause spec module
7. `mlf2.cabal` (module registration only, if required)
8. `docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md`
9. `orchestrator/rounds/round-020/evidence/D1/attempt-1/`
10. `orchestrator/rounds/round-020/implementation-notes.md`

Files expected untouched:

- `orchestrator/rounds/round-020/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/roadmap.md`
- all files under `orchestrator/rounds/round-016/`
- all files under `orchestrator/rounds/round-017/`
- all files under `orchestrator/rounds/round-018/`
- all files under `orchestrator/rounds/round-019/`
- `src-public/`

## Sequential Tasks

### Task 1 - Add `D1` routing, ids, and bounded attempt policy

- Add the new root-cause research entrypoint id `uri-r2-c1-p2-replay-root-cause-v1`.
- Add the new stage selector `D1-replay-reproduction`.
- Accept only scenario `uri-r2-c1-only-v1`.
- Enforce retry-compatible attempt ids `1..100` for `D1`.
- Keep rejection behavior strict:
  - wrong entrypoint, wrong stage, wrong scenario, or out-of-range attempt must fail before writing attempt-local evidence.
- Preserve the existing zero-argument demo path and the older `P1` through `P4` prototype path.

### Task 2 - Implement inherited-input continuity (`D1-I`)

- Read only the authoritative inherited subject token from:
  - `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`
- Read only the accepted `P2` replay-failure boundary from:
  - `orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-W.json`
  - `orchestrator/rounds/round-017/evidence/P2/attempt-2/stage-verdict.json`
  - `orchestrator/rounds/round-017/evidence/P2/attempt-2/trace-bundle.json`
- Prove continuity facts explicitly:
  - subject id remains `uri-r2-c1/cluster-1`
  - inherited scenario remains `uri-r2-c1-only-v1`
  - inherited accepted `P2-W` trigger remains `partial-replay`
  - inherited accepted mismatch string remains `InstBot expects ⊥, got: t9 -> t9`
- Emit `check-D1-I.json` as the machine-readable continuity verdict.

### Task 3 - Re-run the bounded replay lane under `D1` (`D1-R`)

- Execute the real bounded lane, not a file replay:
  - `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback -> witness replay`
- Drive it through the new shared entrypoint only:
  - `{ research_entrypoint_id: uri-r2-c1-p2-replay-root-cause-v1, stage_selector: D1-replay-reproduction, scenario_id: uri-r2-c1-only-v1, attempt_id: 1 }`
- Use the inherited authoritative `P1` subject token as the only admissible subject input.
- Record a new D1 correlation id; use a deterministic form such as:
  - `uri-r2-c1-only-v1-d1-attempt-1`
- Emit:
  - `trace-bundle.json`
  - `check-D1-R.json`
- `check-D1-R` must distinguish:
  - exact bounded replay-failure reproduction;
  - bounded diagnostic drift inside the same lane;
  - bounded inability to reproduce without widening.

### Task 4 - Compare mismatch signature and finalize D1 outputs (`D1-M`)

- Compare the new D1 replay outcome against the accepted `P2-W` boundary.
- `D1-M` should treat the following as the authoritative target signature:
  - rejection trigger `partial-replay`
  - mismatch class `InstBot expects ⊥, got: t9 -> t9`
- Emit:
  - `check-D1-M.json`
  - `stage-verdict.json`
- Emit no handoff token for `D1`.
- Write the canonical artifact at:
  - `docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md`
- The artifact must record:
  - inherited authoritative inputs
  - exact entrypoint/scenario/attempt tuple
  - `D1-I`, `D1-R`, `D1-M` outcomes
  - whether the authoritative `P2-W` failure was reproduced exactly enough for `D2`
  - any bounded non-pass or inconclusive result without widened diagnosis

### Task 5 - Add focused tests for the D1 contract

- Cover accepted execution through the new root-cause entrypoint.
- Cover wrong-scenario, wrong-stage, wrong-entrypoint, and out-of-range attempt rejection without evidence mutation.
- Cover attempt-local evidence layout for `attempt-1`:
  - `check-D1-I.json`
  - `check-D1-R.json`
  - `check-D1-M.json`
  - `stage-verdict.json`
  - `trace-bundle.json`
- Cover the inherited-boundary assertions:
  - D1 uses only the authoritative `P1` subject token and accepted `P2` replay evidence
  - D1 does not fabricate the old mismatch from static files without rerunning the bounded lane
  - D1 does not emit `subject-token.json`
- Preserve the default no-argument app path.

## Stage Result Rules

- `pass` only if:
  - `D1-I` proves inherited continuity,
  - `D1-R` re-executes the bounded replay lane successfully, and
  - `D1-M` confirms the accepted `partial-replay` / `InstBot expects ⊥, got: t9 -> t9` boundary closely enough for bounded diagnosis to continue.
- `semantic-negative` if the run stays bounded but cannot reproduce the authoritative failure signature closely enough.
- `inconclusive` only if the attempt cannot stabilize within the bounded lane without widening or guessing.

## Acceptance Targets

1. `D1` executes only via `uri-r2-c1-p2-replay-root-cause-v1` with selector `D1-replay-reproduction` and scenario `uri-r2-c1-only-v1`.
2. `D1` consumes only the inherited authoritative `P1` subject token and accepted `P2` replay-failure evidence needed for continuity/signature comparison.
3. `D1` re-runs the actual bounded replay lane instead of fabricating the prior diagnostic from artifact text.
4. `D1` emits the canonical artifact plus exactly the planned attempt-local machine-readable outputs for `attempt-1`.
5. `D1` either reproduces the authoritative failure closely enough for bounded diagnosis to continue or records a bounded non-pass/inconclusive result without widening.
6. No second executable interface and no default-path drift.

## Reviewer Checks

Baseline checks from `orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-020/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \\{' orchestrator/rounds/round-020/state-snapshot.json`
- `rg -n '^\\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`
- `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`
- `test -f orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/retry-subloop.md`
- `cabal build all && cabal test`

Round-specific checks:

- Run `D1`:
  - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-p2-replay-root-cause-v1 --stage-selector D1-replay-reproduction --scenario-id uri-r2-c1-only-v1 --attempt-id 1`
- Verify attempt outputs exist and parse as JSON.
- Verify `check-D1-I.json` proves continuity to:
  - `round-016` `P1` attempt `2`
  - `round-017` `P2` attempt `2`
- Verify `check-D1-R.json` is backed by a real bounded replay execution trace, not static string copying.
- Verify `check-D1-M.json` compares against the accepted `partial-replay` / `InstBot expects ⊥, got: t9 -> t9` boundary explicitly.
- Verify `stage-verdict.json` records only `pass|semantic-negative|inconclusive`.
- Verify no `subject-token.json` exists under `orchestrator/rounds/round-020/evidence/D1/attempt-1/`.
- Verify wrong-scenario and wrong-stage rejection do not mutate `attempt-1` evidence.
- Verify default path:
  - `cabal run mlf2`
- Verify no forbidden widening in diff:
  - no edits to `orchestrator/rounds/round-020/state-snapshot.json`
  - no edits to `orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/roadmap.md`
  - no edits to historical round artifacts under `round-016` through `round-019`

## Reviewer Decision Rule

- Approve only if `D1` is anchored on the authoritative inherited subject and accepted replay-failure boundary, re-runs the actual bounded replay lane, and records a bounded result that is honest enough for `D2` to consume.
- Reject if `D1` fabricates reproduction from old artifact text, widens subject/scenario/scope, mutates historical authority, or claims successful reproduction without evidence tied to the new shared root-cause entrypoint.
