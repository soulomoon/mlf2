# Round 019 Plan (Roadmap Item 4: `P4` Prototype Decision Gate)

## Objective

Execute only `P4` for `URI-R2-C1` and produce one reviewer-auditable terminal decision artifact and machine-readable evidence under the bounded prototype lane.

`P4` must consume the authoritative inherited stage results:

- `P1` attempt `2` = `pass`
- `P2` attempt `2` = `semantic-negative`
- `P3` attempt `2` = `semantic-negative`

and emit exactly one terminal decision enum:

- `reopen-handoff-track`, or
- `hard-stop`.

Given the inherited negatives, this round must not assume reopen; it must evaluate against the design threshold and classify honestly.

## Locked Round Context

- Round id: `round-019`
- Branch: `codex/round-019`
- Worktree: `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019`
- Active subject boundary: `URI-R2-C1`
- Scenario boundary: `uri-r2-c1-only-v1`
- Research entrypoint id: `uri-r2-c1-prototype-entrypoint-v1`
- Stage selector to add or execute: `P4-prototype-decision-gate`
- Attempt policy: bounded to `1..3`; first run is `attempt-1`
- Authoritative inherited records:
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-016/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-017/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-018/review-record.json`

## Scope

1. Add bounded `P4` routing through the existing shared research entrypoint only.
2. Implement `P4` decision computation from authoritative `P1` through `P3` records.
3. Emit canonical `P4` artifact at:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/docs/plans/2026-03-15-uri-r2-c1-p4-prototype-decision-gate.md`
4. Emit attempt-local `P4` machine-readable outputs under:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/orchestrator/rounds/round-019/evidence/P4/attempt-1/`
5. Keep default `cabal run mlf2` behavior unchanged.

## Non-Goals

- No changes to `P1`, `P2`, or `P3` semantics or authoritative records.
- No production-path behavior change.
- No second executable interface.
- No widening beyond `URI-R2-C1` or `uri-r2-c1-only-v1`.
- No reopening or rewriting accepted `RE1` through `RE5`.
- No surrogate positive handoff when inherited evidence does not meet reopen criteria.

## Implementation Slice

Primary files expected in scope:

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/src/MLF/Research/URI/R2/C1/Prototype/Types.hs`
2. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/src/MLF/Research/URI/R2/C1/Prototype/Entrypoint.hs`
3. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs`
4. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/src/MLF/Research/URI/R2/C1/Prototype/P4.hs` (new)
5. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/test/Research/UriR2C1PrototypeP1Spec.hs`
6. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/mlf2.cabal` (module registration only, if required)
7. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/docs/plans/2026-03-15-uri-r2-c1-p4-prototype-decision-gate.md` (new canonical artifact)
8. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/orchestrator/rounds/round-019/evidence/P4/attempt-1/`
9. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/orchestrator/rounds/round-019/implementation-notes.md`

Files expected untouched:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/orchestrator/roadmap.md`
- any files under `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/orchestrator/rounds/round-016/`
- any files under `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/orchestrator/rounds/round-017/`
- any files under `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/orchestrator/rounds/round-018/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/app/Main.hs`
- all `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/src-public/*`

## Sequential Tasks

### Task 1 - Add `P4` bounded routing and types

- Add `P4` selector and path constants under existing prototype path helpers.
- Ensure `runResearchPrototype` accepts `P4-prototype-decision-gate` only through `uri-r2-c1-prototype-entrypoint-v1`.
- Keep unsupported selectors and scenarios rejecting as bounded errors.

### Task 2 - Implement `P4` terminal decision computation

- Add a dedicated `executeP4` unit; do not embed ad hoc decision logic in `Entrypoint`.
- Consume inherited authoritative review-record data only for `P1` through `P3`.
- Apply design threshold exactly:
- `reopen-handoff-track` only if `P1`, `P2`, and `P3` are all `pass` with no unresolved caveat.
- `hard-stop` required if any stage is `semantic-negative`, any blocking-stop-condition exists, or budget-exhausted inconclusive exists.
- For current inherited state (`P2` and `P3` both `semantic-negative`), expected decision is `hard-stop`.

### Task 3 - Emit `P4` machine-readable outputs and canonical artifact

- Create attempt directory:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/orchestrator/rounds/round-019/evidence/P4/attempt-1/`
- Emit machine-readable decision outputs sufficient for reviewer replay:
- one stage-consumption summary (authoritative attempts and results for `P1` through `P3`)
- one final decision object with terminal enum and decision rationale
- one trace bundle with shared invocation metadata (`research_entrypoint_id`, `stage_selector`, `scenario_id`, `attempt_id`)
- Write canonical artifact at:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-019/docs/plans/2026-03-15-uri-r2-c1-p4-prototype-decision-gate.md`
- Artifact must record:
- inherited authoritative stage vector
- explicit gate logic and triggered stop condition
- final decision enum
- no fabricated reopen narrative

### Task 4 - Add targeted tests for `P4` gate contract

- Extend prototype tests to cover:
- successful bounded `P4` execution through shared entrypoint
- wrong-scenario and wrong-stage rejection without evidence mutation
- inherited negatives forcing `hard-stop`
- decision enum restricted to `reopen-handoff-track|hard-stop`
- unchanged default `cabal run mlf2` path

## Acceptance Targets

1. `P4` executes only via `uri-r2-c1-prototype-entrypoint-v1` with selector `P4-prototype-decision-gate`.
2. `P4` consumes only authoritative `P1` through `P3` results; no stage rewrite and no widened evidence inputs.
3. `P4` emits exactly one terminal decision enum and, for current inherited vector, that decision is `hard-stop`.
4. Canonical `P4` artifact and attempt evidence are machine-readable and reviewer-replayable.
5. No second executable interface and no default-path drift.

## Reviewer Checks

Baseline checks from `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
- `cabal build all && cabal test`

Round-specific checks:

- Run `P4`:
- `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P4-prototype-decision-gate --scenario-id uri-r2-c1-only-v1 --attempt-id 1`
- Verify attempt outputs exist and parse as JSON.
- Verify inherited authoritative consumption points to `P1` attempt `2` pass, `P2` attempt `2` semantic-negative, `P3` attempt `2` semantic-negative.
- Verify final decision enum is one of allowed terminal enums and equals `hard-stop` for current inherited vector.
- Verify wrong-scenario and wrong-stage rejection do not mutate attempt outputs.
- Verify default path:
- `cabal run mlf2`
- Verify no forbidden diff widening:
- `git diff --name-only`
- no edits to `orchestrator/state.json`, `orchestrator/roadmap.md`, `app/Main.hs`, or `src-public/*`

## Reviewer Decision Rule

- Approve only if `P4` emits a terminal decision that is consistent with actual authoritative `P1` through `P3` results and design thresholds.
- Reject if `P4` reopens despite inherited negatives, rewrites inherited authority, widens scope, or emits non-terminal or fabricated decision semantics.
