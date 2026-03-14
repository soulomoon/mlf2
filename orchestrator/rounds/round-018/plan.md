# Round 018 Plan Delta (`P3` Repair After Rejected Review)

## Why This Delta Exists

Round `018` review rejected `P3` for one blocking reason: the implementation consumed the round-016 `P1` token directly even though authoritative `P2` (round `017`, attempt `2`) is `semantic-negative` and emits no `P2 -> P3` token. This violates the approved interface rule that `P3` may consume only the exact token emitted or reaffirmed by `P2`.

This delta keeps the same round/branch/worktree and narrows repair strictly to that contract violation.

## Locked Round Context (Unchanged)

- Round id: `round-018`
- Branch: `codex/round-018`
- Worktree: `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-018`
- Roadmap item: `3` (`P3` safety-validation prototype)
- Research entrypoint: `uri-r2-c1-prototype-entrypoint-v1`
- Scenario: `uri-r2-c1-only-v1`
- Stage selector: `P3-safety-validation`
- Attempt policy: preserve rejected `attempt-1`; rerun only as `attempt-2`
- Authoritative inherited records:
  - `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-016/review-record.json` (`P1` pass)
  - `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-017/review-record.json` (`P2` semantic-negative, no `P3` token handoff)

## Repair Scope

Only repair the `P3` stage-input contract and resulting artifact/evidence statements:

1. Remove direct `P1` token consumption from `P3` execution.
2. Derive `P3` non-pass solely from `P2` authoritative non-pass and absent `P2` token.
3. Regenerate `P3` outputs as `attempt-2` only.
4. Keep all existing bounds intact (shared entrypoint, fixed scenario, no second interface, default path unchanged).

## Non-Goals

- No new `P3` features, heuristics, or scope widening.
- No `P4` logic.
- No changes to controller files (`orchestrator/state.json`, `orchestrator/roadmap.md`).
- No changes to rounds other than `round-018`.
- No rewriting or deleting rejected `attempt-1` evidence.

## Minimal Implementation Changes

### Task 1 - Fix Stage-Input Source of Truth

- In `P3` execution code, stop reading any `P1` token artifact path as `P3` input.
- Use only `P2` authoritative review/evidence as the stage-input contract gate:
  - if `P2` is non-pass (here: `semantic-negative`) and no reaffirmed token exists, classify `P3` as bounded non-pass directly;
  - do not synthesize or backfill a token from `P1`.

### Task 2 - Keep Bounded `P3` Non-Pass Semantics

- Preserve current bounded result class (`semantic-negative`) and explicit trigger (`blocking-stop-condition`) for this inherited precondition.
- Ensure all `P3` checker outputs and `stage-verdict.json` remain machine-readable and include repeated invocation metadata.
- Keep `subject_token_ref = null` and do not emit `subject-token.json` for the non-pass run.
- If `subject_id` cannot be truthfully bound without a `P2` token, use `null` rather than borrowing `P1` identity.

### Task 3 - Repair Reviewer Artifact Language

- Update the canonical `P3` artifact so stage inputs no longer claim direct `P1` token consumption.
- Artifact must state that authoritative `P2` non-pass produced no handoff token and therefore `P3` records bounded non-pass under the interface contract.
- Keep artifact focused on `P3`; do not alter `P1`/`P2` records.

### Task 4 - Targeted Test Adjustments

- Add/adjust tests to lock the repaired contract:
  - `P3` with authoritative non-pass `P2` must not consume `P1` token as fallback;
  - non-pass `P3` emits no `subject-token.json`;
  - stage verdict/check outputs remain valid JSON with expected bounded rejection classification.
- Preserve existing isolation/default-path tests.

## Allowed File Slice (Narrow)

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-018/src/MLF/Research/URI/R2/C1/Prototype/P3.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-018/src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-018/test/Research/UriR2C1PrototypeP1Spec.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-018/docs/plans/2026-03-15-uri-r2-c1-p3-safety-validation-prototype.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-018/orchestrator/rounds/round-018/implementation-notes.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-018/orchestrator/rounds/round-018/evidence/P3/attempt-2/`

`mlf2.cabal`, `Types.hs`, and `Entrypoint.hs` should remain untouched unless a strictly compile-blocking fix is proven necessary.

## Attempt and Evidence Policy

- `attempt-1` remains immutable rejected history.
- Rerun exactly once as:
  - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P3-safety-validation --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
- Expected `attempt-2` outputs:
  - `check-P3-S.json`
  - `check-P3-A.json`
  - `check-P3-B.json`
  - `check-P3-C.json`
  - `stage-verdict.json`
  - `trace-bundle.json`
- `subject-token.json` must not exist for `attempt-2`.

## Acceptance Targets for This Repair

1. `P3` no longer reads or references the round-016 `P1` token as a stage input.
2. `P3` stage-input reasoning is derived only from authoritative `P2` handoff state (including no-token cases).
3. `attempt-1` remains unchanged; `attempt-2` contains the repaired bounded non-pass evidence.
4. Canonical `P3` artifact no longer claims direct `P1` token consumption.
5. Shared-entrypoint isolation and default-path behavior remain unchanged.

## Reviewer Checks for This Delta

Baseline (unchanged):

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
- `cabal build all && cabal test`

Delta-specific checks:

- `rg -n 'p1AuthoritativeSubjectTokenRelativePath|round-016/evidence/P1/attempt-2/subject-token.json' src/MLF/Research/URI/R2/C1/Prototype/P3.hs src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs`
  - expected: no contract-violating `P3` input use.
- Rerun `P3` as `attempt-2` using bounded entrypoint/scenario.
- `find orchestrator/rounds/round-018/evidence/P3/attempt-1 -maxdepth 1 -type f | sort`
- `find orchestrator/rounds/round-018/evidence/P3/attempt-2 -maxdepth 1 -type f | sort`
  - expected: `attempt-1` preserved; `attempt-2` regenerated.
- `python3 -m json.tool` over all `attempt-2` JSON outputs.
- `test ! -f orchestrator/rounds/round-018/evidence/P3/attempt-2/subject-token.json`
- Wrong-scenario and wrong-stage rejection checks (no mutation of `attempt-2` evidence).
- `cabal run mlf2` default-path check.

## Reviewer Decision Rule (This Retry)

- Approve only if `P3` fully removes the direct `P1` token handoff and records bounded non-pass from `P2` no-token continuity only.
- Reject if any `P3` logic or artifact still treats `P1` token material as direct stage input when `P2` did not emit/reaffirm a token.
