# Contract Amendment: `URI-R2-C1` Prototype-Evidence Retry Subloop

Date: 2026-03-16
Status: Approved amendment

## Purpose

Amend the live `URI-R2-C1` prototype-evidence control plane so future rounds may retry a fixable stage inside the same round instead of treating every accepted non-pass as immediate finalization.

This amendment does not reopen or rewrite completed rounds `round-016` through `round-019`. Those rounds remain valid historical evidence under the earlier single-shot contract.

## Relationship To The 2026-03-15 Roadmap Design

`docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md` remains the roadmap and subject-boundary source of truth.

This amendment supersedes only the attempt-budget and control-plane portions of that design for future rounds:

- the three-attempt cap;
- the assumption that review distinguishes only `approve` vs `reject`;
- the earlier reviewer-record shape that lacked retry summaries; and
- the implicit rule that any accepted non-pass must finalize immediately.

Future rounds under this control plane therefore run as `contract_version: 2`.

## Scope

The retry subloop applies only to `P1`, `P2`, and `P3`.

The outer round machine remains unchanged:

1. `select-task`
2. `plan`
3. `implement`
4. `review`
5. `merge`
6. `update-roadmap`
7. `done`

The change is internal to a stage:

- review may now send the same round back to `plan` after either a rejected attempt or an accepted-but-retryable attempt;
- the same round id, branch, and worktree remain in use while the retry subloop is active;
- `P4` remains a terminal aggregate gate and may not emit semantic `accepted + retry`.

## Review Decision Model

Every `P1` through `P3` review must record five fields:

- `attempt_verdict`: `accepted | rejected`
- `stage_result`: `pass | semantic-negative | inconclusive`
- `stage_action`: `finalize | retry`
- `retry_reason`: normalized short trigger
- `fix_hypothesis`: one-sentence target for the next attempt, or `none` when finalizing

Allowed combinations:

- `accepted + finalize`
  - the stage becomes authoritative and may advance to merge;
- `accepted + retry`
  - the attempt is valid evidence but not yet authoritative carry-forward;
- `rejected + retry`
  - the attempt is not valid carry-forward evidence and the same round must retry;
- `rejected + finalize`
  - forbidden.

For `P4`:

- `accepted + retry` is forbidden;
- `accepted + finalize` is required for any approved `P4`;
- `rejected + retry` remains allowed for ordinary implementation defects only.

## Retry Budget

Each of `P1`, `P2`, and `P3` receives an independent retry budget of `100`.

Stop retrying when any of the following becomes true:

- the reviewer records `accepted + finalize`;
- the reviewer classifies the latest non-pass as terminal and therefore records `accepted + finalize`;
- the retry budget is exhausted.

Budget-exhaustion rule:

- if at least one prior attempt was `accepted`, finalize the latest accepted attempt as the authoritative carried result with a budget-exhaustion terminal reason;
- if no prior attempt was ever `accepted`, stop with a controller-level blockage in `orchestrator/state.json` instead of fabricating an authoritative stage result.

## Machine State

`orchestrator/state.json` gains a forward-only version marker and retry block:

```json
{
  "contract_version": 2,
  "retry": {
    "stage_id": "P2",
    "attempt": 4,
    "max_attempts": 100,
    "latest_accepted_attempt": 3,
    "latest_accepted_result": "semantic-negative",
    "latest_attempt_verdict": "rejected",
    "latest_stage_action": "retry",
    "retry_reason": "partial-replay",
    "fix_hypothesis": "repair applyInstantiation replay mismatch"
  }
}
```

State rules:

- `retry = null` when no retry subloop is active;
- `retry.attempt` is the next attempt number to run;
- `latest_accepted_*` names the most recent accepted evidence that could be finalized on budget exhaustion;
- the outer `stage` values stay unchanged.

## Artifact Contract

Per-attempt historical evidence must stay immutable.

For future `contract_version: 2` rounds:

- `review.md` remains the latest live review for the active round;
- `reviews/attempt-<n>.md` stores an immutable snapshot of each attempt review;
- `attempt-log.jsonl` stores one controller-owned machine-readable record per attempt transition;
- `review-record.json` is written only when a stage becomes authoritative.

Minimum `attempt-log.jsonl` row shape:

```json
{
  "round": "round-020",
  "stage": "P2",
  "attempt": 2,
  "attempt_verdict": "accepted",
  "stage_result": "semantic-negative",
  "stage_action": "retry",
  "retry_reason": "partial-replay",
  "fix_hypothesis": "repair witness replay bottom-instantiation mismatch",
  "review_path": "orchestrator/rounds/round-020/reviews/attempt-2.md",
  "evidence_dir": "orchestrator/rounds/round-020/evidence/P2/attempt-2/"
}
```

`review-record.json` gains a retry summary for finalized `contract_version: 2` stages:

```json
"retry_summary": {
  "attempts_run": 7,
  "max_attempts": 100,
  "latest_accepted_attempt": 7,
  "finalization_mode": "accepted-final|budget-exhausted-latest-accepted",
  "latest_retry_reason": "partial-replay"
}
```

## Resume Rules

Resume from `orchestrator/state.json`.

- If `retry` is active and `stage` is `plan`, `implement`, or `review`, resume the exact same round and current attempt.
- If review completed with `stage_action = retry`, increment `retry.attempt`, preserve the same round id, branch, and worktree, and return to `plan`.
- Retrying a stage must not allocate a new round, new branch, or new worktree.

## Historical Compatibility

Historical rounds `round-016` through `round-019` remain valid `contract_version: 1` evidence.

Migration rules:

- do not rewrite their artifacts into the new schema;
- future rounds must record `contract_version: 2` in live machine state before use;
- if a future in-flight round is resumed from older machine state without `contract_version`, normalize only the live machine state by adding `contract_version: 2` and `retry: null` before continuing.

## Invariants

- Only `accepted + finalize` creates authoritative stage output.
- Downstream stages may consume only authoritative upstream output.
- `accepted + retry` is valid evidence but not authoritative carry-forward.
- `rejected + retry` is not downstream authority.
- `rejected + finalize` is invalid.
- `P4` may not emit `accepted + retry`.
- Retry budget does not authorize scope widening.
- Retry attempts must preserve earlier attempt artifacts byte-for-byte.
