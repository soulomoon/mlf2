# Run-Orchestrator-Loop Delegation Recovery Design

Date: 2026-03-24
Status: draft for review
Scope: shared `run-orchestrator-loop` skill only

## Goal

Make the shared `run-orchestrator-loop` skill resilient to incidental
delegation failures so the controller keeps working toward roadmap completion
instead of stopping at the first non-observable handoff.

The target change is process-only. It does not widen any repo-local roadmap,
role authority, or implementation boundary. It changes how the shared
controller skill reacts when a real-subagent stage fails to leave trustworthy
evidence.

## Triggering Failure

The motivating red case is the live `round-078` `select-task` blockage in
`/Users/ares/.codex/worktrees/d432/mlf4`:

- the controller opened `round-078` correctly;
- builtin guider delegation was dispatched as required;
- the expected stage artifact
  `orchestrator/rounds/round-078/selection.md` never appeared; and
- the shared skill treated that incidental delegation failure as a terminal
  controller blockage.

That stop behavior is too brittle. The stage result was not “guider selected no
task”; the real problem was “the controller could not verify that the guider
stage completed.”

## Problem Statement

Today the shared skill has a gap between two states:

1. normal delegated stage completion; and
2. terminal controller blockage.

There is no explicit shared recovery phase for cases where:

- the expected stage artifact is missing;
- a stage artifact is empty, malformed, stale, or inconsistent with the
  current machine state;
- the delegation mechanism itself is non-observable;
- the round worktree or state snapshot is out of sync with the controller root;
  or
- the controller has enough evidence to know the role did not finish, but not
  enough evidence to know what substantive stage result would have been.

In those cases the controller should recover and re-dispatch, not stop.

## Design Decision

Adopt broad controller recovery discretion, but constrain that discretion with
one dedicated real-subagent role: `recovery-investigator`.

The controller may use whatever recovery actions are needed to keep the loop
moving, provided all of the following remain true:

- guider, planner, implementer, reviewer, and merger substantive work still
  belongs to real subagents;
- the controller never fabricates `selection.md`, `plan.md`, implementation
  output, `review.md`, `review-record.json`, or `merge.md`;
- recovery is about restoring lawful execution conditions, not replacing role
  work; and
- terminal blockage is the last resort after recovery options are exhausted.

## Scope

- Update only the shared skill at
  `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/`.
- Keep repo-local orchestrator contracts unchanged.
- Preserve the existing stage order and role ownership.
- Preserve the existing requirement that substantive stage work be delegated to
  real subagents.

## Non-Goals

- No change to repo-local `orchestrator/roles/*.md` contracts.
- No change to repo-local retry semantics.
- No change to merge rules, stage order, or roadmap semantics.
- No controller-authored substitute for missing role outputs.
- No silent downgrade from “real subagent” to “controller does the work.”

## Controller Recovery Contract

The shared skill should add a controller-owned recovery subloop with these
rules.

### 1. Detect incidental delegation failure

Recovery begins when a delegated stage’s expected artifact is missing, empty,
malformed, stale, contradictory, or otherwise insufficient to prove the stage
completed.

Examples:

- `select-task` completed but `selection.md` is absent;
- `plan.md` exists but clearly targets the wrong round or stage;
- `review.md` exists without the required verdict/action fields; or
- a delegation mechanism returns without leaving any observable stage artifact.

This is not a lawful stage outcome. It is a controller-side execution failure.

### 2. Freeze the active round

During recovery the controller must keep:

- the same `active_round_id`;
- the same branch;
- the same worktree;
- the same stage;
- the same `current_task` value unless the missing stage itself would have set
  it; and
- the same retry attempt number unless repo-local retry state explicitly says
  otherwise.

Recovery must not silently advance roadmap state, merge state, or retry state.

### 3. Dispatch `recovery-investigator`

When incidental failure is detected, the controller dispatches a dedicated real
subagent whose job is diagnosis and recovery recommendation only.

Inputs:

- current `orchestrator/state.json`;
- current round directory contents;
- worktree status and branch state;
- role definitions and shared skill rules;
- prior wait/retry observations; and
- any controller-owned evidence explaining why the stage is non-observable.

Outputs:

- diagnosis of the failure mode;
- recommendation for the next lawful recovery action; and
- optional controller-owned recovery note if the shared skill chooses to keep
  one.

The recovery investigator must not perform guider/planner/implementer/
reviewer/merger substantive work.

### 4. Permit broad controller-owned recovery actions

After reading the recovery investigator’s output, the controller may take any
controller-owned action needed to restore lawful execution conditions,
including:

- retry the same stage with a fresh real subagent;
- switch to another available real-subagent mechanism;
- recreate a missing round worktree;
- resync controller-owned machine state into the round worktree;
- repair controller-owned artifact directories;
- clear incidental controller-side damage that prevents lawful dispatch;
- keep or add a controller-owned recovery note; and
- reopen the same stage on the same round and re-dispatch.

The controller still may not author the blocked role’s substantive artifact.

### 5. Re-check observability before advancing

After recovery and re-dispatch, the controller advances only when the expected
stage artifact exists and is consistent with:

- the current round id;
- the current stage;
- the current retry attempt; and
- the repo-local contract for that stage.

If the artifact is still not trustworthy, recovery continues.

### 6. Record terminal blockage only after exhaustion

The shared skill should stop only when the recovery investigator concludes that
no lawful real-subagent path remains.

Examples:

- every available delegation mechanism for the required role has failed;
- required role sources are missing;
- machine state or worktree metadata is corrupt beyond safe recovery; or
- the environment cannot run any qualifying real subagent for that stage.

Only then should the controller record a precise blockage or `resume_error`.

## Recovery-Investigator Boundary

The new recovery role exists to preserve the controller/worker split, not to
erase it.

It may:

- diagnose execution failures;
- compare machine state against artifact expectations;
- recommend recovery actions;
- recommend whether the stage should be retried with the same or a different
  subagent mechanism; and
- explain why the controller may or may not continue safely.

It may not:

- write `selection.md`, `plan.md`, implementation output, `review.md`,
  `review-record.json`, or `merge.md`;
- make roadmap decisions;
- review substantive code or docs as the stage reviewer; or
- merge or finalize a round.

## Expected Shared-Skill Changes

The implementation plan for this spec should update the shared skill and its
references so the recovery rules are explicit and discoverable.

Minimum change set:

- `SKILL.md`
  - introduce the controller-owned recovery subloop;
  - define incidental delegation failure vs terminal blockage;
  - define when the recovery investigator is required; and
  - clarify that the controller may switch delegation mechanisms without
    stopping, so long as the replacement is still a real subagent path.
- `references/delegation-boundaries.md`
  - add the recovery-investigator role and allowed controller recovery actions.
- `references/resume-rules.md`
  - define how an active round resumes after a non-observable delegated stage.

Optional change:

- add one new reference document if the recovery algorithm would otherwise make
  `SKILL.md` or the existing references too dense.

## Acceptance Criteria

Planning should treat this spec as satisfied only if the shared skill makes all
of the following unambiguous:

- a missing or non-trustworthy stage artifact is an incidental delegation
  failure, not automatically a terminal blockage;
- the controller keeps the same round, branch, worktree, and stage while
  recovering;
- the controller uses a dedicated real-subagent recovery investigator before
  concluding it is blocked;
- the controller may switch delegation mechanisms when necessary without
  violating the “real subagent only” rule;
- the controller may perform broad controller-owned repair actions but still
  may not impersonate the blocked role; and
- terminal blockage is recorded only after available lawful recovery paths are
  exhausted.

## Recommendation

Implement the recovery model in the shared `run-orchestrator-loop` skill now.

This change is narrow, directly justified by the observed `round-078`
blockage, and should let future orchestrator runs continue through incidental
subagent transport failures without weakening the delegated-stage boundary.
