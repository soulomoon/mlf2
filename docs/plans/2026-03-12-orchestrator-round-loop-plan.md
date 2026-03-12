# Recursive Types Orchestrator Round-Loop Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Run a strict orchestrator-only campaign for up to 10 rounds where each round freshly plans the next smallest roadmap slice, delegates all repo work to subagents, and advances only from delegated review/QA/verifier evidence.

**Architecture:** The orchestrator owns only packet state, role dispatch, and transition decisions. Every round begins from current `master`, runs a delegated authority check before planning, asks a fresh planner what this round should do, and then runs a delegated implement → review → QA → integrate → verify loop with cleanup, planner-delta retries, and quarantine-and-retry authority recovery inside the same round as needed. The authoritative machine-readable state lives in the orchestration packet, especially `orchestrator-log.jsonl`.

**Tech Stack:** Codex orchestration packet, fresh subagents per role, Git worktrees/branches, Cabal build/test verification, roadmap-driven milestone gating.

---

## Context and Non-Negotiable Rules

- The orchestrator is **not allowed** to perform repo work directly.
- The orchestrator may only:
  - read source-of-truth docs and packet files
  - update orchestration artifacts
  - spawn, wait for, close, and redirect subagents
  - decide transitions from delegated evidence
- Every repo action must be delegated to a fresh subagent:
  - repo-state inspection
  - planning
  - implementation
  - review
  - QA
  - integration
  - verification
  - cleanup
- The orchestrator must **not** decide all ten rounds up front.
- At the start of each round, a **fresh planner** chooses the current round’s smallest shippable slice based on current `master`, the roadmap, and the packet.
- The ordered roadmap milestone anchor is always the first row still marked `NO` in the mechanism table unless verifier evidence says the milestone is already `YES` on current `master`.
- Gate vocabulary is exact:
  - review/QA/milestone gates: `YES` / `NO`
  - terminal statuses: `COMPLETED` / `FAILED` / `MAXIMUMRETRY`

## Authoritative Files

**Primary packet files**
- `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl`
- `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/mechanism_table.md`
- `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/task_plan.md`
- `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/findings.md`
- `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/progress.md`
- `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator_prompt.md`

**Roadmap / evidence**
- `/Volumes/src/mlf4/docs/plans/2026-03-11-recursive-types-roadmap.md`
- `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-design/`

**Packet authority rule**
- `orchestrator-log.jsonl` is the machine-readable source of truth.
- `mechanism_table.md` is the milestone snapshot humans scan first.
- `task_plan.md`, `findings.md`, and `progress.md` summarize and explain the log.
- If prose and log diverge, fix the prose to match the log.
- Unlogged live worktree/branch/task-folder state is never adoptable evidence; recover it only through the delegated authority recovery lane.

## Roles and Ownership

### Role 1: Orchestrator

**Allowed**
- Read packet and source-of-truth docs
- Update packet artifacts
- Dispatch fresh subagents
- Wait for results
- Decide transitions strictly from delegated evidence

**Forbidden**
- Git inspection
- Diff inspection
- Coding
- Testing
- Commit/merge
- Cleanup
- Branch/worktree creation

### Role 2: Researcher

**Purpose:** Read-only repo-state facts for the current round.

**Typical outputs**
- current branch and `HEAD`
- current `master` SHA
- dirty path summary
- lingering branch/worktree state
- other factual repo-state blockers needed for safe branching

### Role 3: Planner

**Purpose:** Choose exactly one smallest shippable slice for the current round only.

**Rules**
- Must not speculate about future rounds
- Must anchor to the first unresolved roadmap milestone
- Must return decision-complete acceptance criteria
- On rejection, must return a `PlannerDelta` for the same round

### Role 4: Implementer

**Purpose:** Execute the planner’s slice on a fresh branch/worktree.

**Rules**
- Must start from the round’s `master_sha_before`
- Must not commit
- Must run focused validation before handoff
- Must report changed files and commands run

### Role 5: Reviewer

**Purpose:** Compare the actual diff against the planner’s slice.

**Rules**
- Returns `YES` or `NO`
- Must cite concrete scope violations or missing acceptance criteria if `NO`
- Must not be the same agent as the implementer

### Role 6: QA

**Purpose:** Run the round’s required validation.

**Rules**
- Returns `YES` or `NO`
- Runs focused checks first, then full gate when required
- Must not be the same agent as implementer or reviewer

### Role 7: Integrator

**Purpose:** Commit, merge to `master`, verify merged result, and clean branch/worktree state.

**Rules**
- Must use `--no-ff`
- Must preserve intentionally dirty root docs
- Must report commit SHA and merge SHA
- Must clean attempt worktrees/branches only

### Role 8: Verifier

**Purpose:** Decide milestone completion, full-campaign completion, and blockage.

**Rules**
- Must decide milestone gate for the current roadmap row on merged `master`
- Must decide completion gate for the whole campaign
- Must decide blockage gate if work should stop
- If campaign continues, must identify the next milestone anchor

## Round Loop Overview

Each round follows the same pattern. The orchestrator does not pre-assign the round’s task. The planner determines that task after a fresh repo-state snapshot.

```text
Read packet → choose next NO milestone → delegate authority check → if authority NO: recovery snapshot + quarantine + recovery resume
→ delegate repo snapshot → delegate planner
→ delegate implementer → delegate reviewer
→ if review NO: delegate authority check → if authority NO: recovery lane else cleanup + planner delta + retry same round
→ else delegate QA
→ if QA NO: delegate authority check → if authority NO: recovery lane else cleanup + planner delta + retry same round
→ else delegate integrator
→ delegate verifier on merged master
→ if milestone YES and completion NO: advance to next round
→ if completion YES: COMPLETED
→ before FAILED: delegate authority check → if recoverable packet fault: recovery lane else FAILED
→ if round exhausts retries: MAXIMUMRETRY (or FAILED only after recovery audit proves a real blockage)
```

## Retry Budget and Terminal Rules

- Maximum rounds: `10`
- Maximum attempts per round: `20`
- Retry policy is intra-round only:
  - review `NO` → retry same round
  - QA `NO` → retry same round
  - authority recovery success → retry same round on the next attempt number
- Authority recovery audits are mandatory before a fresh planner starts, before retry cleanup runs, and before any `FAILED` terminal status is emitted.
- Terminal status selection:
  - `COMPLETED` when verifier says all roadmap milestones are satisfied
  - `FAILED` only when delegated evidence proves a real blockage, recovery snapshot failure, quarantine/archive failure, cleanup-verification failure, `master` contamination, merged-history contamination, or irreconcilable packet truth
  - `MAXIMUMRETRY` when the current round exhausts its allowed attempts without verifier-backed campaign completion or stronger blockage after the required recovery audits

## Packet Update Rules

After every meaningful transition, update the packet immediately.

### `orchestrator-log.jsonl`
Append one event per transition, for example:
- `authority_check`
- `research_result`
- `planner_round_plan`
- `planner_delta`
- `round_started`
- `implementation_result`
- `review_gate`
- `qa_gate`
- `integration_result`
- `round_complete`
- `cleanup_result`
- `recovery_snapshot`
- `recovery_quarantine`
- `recovery_resume`
- `terminal_status`

Every event should include when relevant:
- `round`
- `attempt`
- `selected_milestone`
- `selected_slice`
- `agent_role`
- `review_gate`
- `qa_gate`
- `authority_gate`
- `reason_for_no`
- `branch_name`
- `commit_sha`
- `merge_sha`
- `master_sha_before`
- `master_sha_after`
- `recovery_policy`
- `orphan_attempt_detected`
- `orphan_worktree_path`
- `orphan_branch_name`
- `attempt_number_inferred`
- `quarantine_artifact_path`
- `cleanup_verification`
- `resume_from_attempt`
- `timestamp_utc`
- `run_id`

### `mechanism_table.md`
Keep milestone rows synchronized with the latest verifier-owned truth.

For each row maintain:
- current behavior
- target behavior
- closure gap
- evidence
- gate (`YES`/`NO`)
- next action

### `task_plan.md`
Track the campaign’s stable phases and any resumed-run synchronization notes.

### `findings.md`
Record durable discoveries, rejections, scope boundaries, and policy decisions.

### `progress.md`
Append timestamped narrative updates for each transition.

## Authority Recovery Lane

Use this delegated recovery path whenever an authority audit reports live attempt state that lacks authoritative handoff in `orchestrator-log.jsonl`.

**Recovery policy**
- Policy is fixed to `Quarantine + Retry`.
- Orphan worktree/branch/task-folder state is never reviewed, merged, or adopted into the packet as if it were a logged attempt.
- Recovery artifacts are forensic only and do not count as implementation handoff.

**Delegated recovery sequence**
1. Dispatch a fresh authority-audit agent and log `authority_check` with `authority_gate = YES|NO`.
2. If `authority_gate = YES`, continue the normal round flow unchanged.
3. If `authority_gate = NO`, dispatch a fresh recovery-snapshot agent to capture only facts: branch, `HEAD`, base SHA, status, diffstat, changed files, task-folder presence, and whether `master` is untouched.
4. Derive the orphan attempt number in this order: task-folder suffix, worktree suffix `-aN`, else `last_authoritative_attempt + 1`; set `attempt_number_inferred = YES` only when fallback inference is used.
5. Log `recovery_snapshot` for that round/attempt without synthesizing missing `planner_delta`, `round_started`, or `implementation_result` events.
6. Dispatch a fresh quarantine agent to write a non-authoritative forensic bundle under the active packet folder (summary + patch archive), then remove the orphan worktree/branch/task folder while preserving intentionally dirty root docs.
7. Log `recovery_quarantine` with the artifact path and cleanup outcome.
8. Dispatch a fresh recovery verifier to confirm the orphan state is gone, `master` remains uncontaminated, and packet authority is restored; log `recovery_resume`.
9. Resume the same round on the next attempt number with a fresh `planner_delta`, the same milestone anchor, and the same `master_sha_before`.

## Task 1: Create the Round Controller Skeleton

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator_prompt.md`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/task_plan.md`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/findings.md`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/progress.md`

**Step 1: Re-read the packet before any round decision**

Read the authoritative log tail and current mechanism rows.

Run:
```bash
tail -n 40 /Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl
rg -n '^\| M[0-7] ' /Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/mechanism_table.md
```

Expected: clear visibility into the latest `YES`/`NO` state and current round anchor.

**Step 2: Determine the next unresolved milestone from the mechanism table**

Use the first milestone row still marked `NO`.

Expected: exactly one current anchor milestone for the round.

**Step 3: Record the round-start intention in `progress.md`**

Add a timestamped note that the next round is starting from current `master` and that the planner will choose the smallest slice.

**Step 4: Record any packet drift in `findings.md`**

If the log and table disagree, note that the table must be corrected from the authoritative log before round execution proceeds.

**Step 5: Commit**

Do not actually commit from the orchestrator. This step exists only so implementers/integrators know packet-only changes are grouped separately from product changes.

## Task 2: Delegate the Round-Start Repo Snapshot

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/progress.md`

**Step 1: Spawn a fresh researcher**

Prompt requirements:
- report branch
- report `HEAD`
- report dirty state
- report lingering attempt worktrees/branches
- do not modify the repo

**Step 2: Wait for the researcher result**

Expected: machine-usable repo facts.

**Step 3: Log `research_result`**

Append a `research_result` event to `orchestrator-log.jsonl`.

**Step 4: Summarize in `progress.md`**

Add a one-line timestamped summary of the repo state.

## Task 3: Delegate a Fresh Planner for the Current Round

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/progress.md`

**Step 1: Spawn a fresh planner**

Planner input must include:
- current milestone anchor
- current `master` SHA
- roadmap path
- mechanism table path
- authoritative log path
- explicit instruction: choose current-round slice only

**Step 2: Wait for planner result**

Expected: one smallest shippable slice, acceptance criteria, test plan, out-of-scope list, branch/worktree naming.

**Step 3: Log `planner_round_plan` or `planner_delta`**

Expected: exact acceptance criteria recorded in the log.

**Step 4: Log `round_started`**

Include round number, attempt number, branch name, and worktree path.

## Task 4: Delegate Implementation for the Planned Slice

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/progress.md`

**Step 1: Spawn a fresh implementer**

Implementer input must include:
- exact `master_sha_before`
- exact branch name
- exact worktree path
- acceptance criteria
- test commands to run
- out-of-scope constraints
- instruction not to commit

**Step 2: Wait for implementation result**

Expected: changed files, focused checks, no commit.

**Step 3: Log `implementation_result`**

Record files changed and focused results.

## Task 5: Delegate Review

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/progress.md`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/findings.md`

**Step 1: Spawn a fresh reviewer**

Reviewer input must include:
- actual worktree path
- base SHA
- planner acceptance criteria
- out-of-scope list

**Step 2: Wait for review result**

Expected: `YES` or `NO`.

**Step 3A: If review is `YES`**

Log `review_gate` with `YES` and continue to QA.

**Step 3B: If review is `NO`**

- Log `review_gate` with `NO`
- Append rejection summary to `findings.md`
- Go to Task 8 (cleanup + planner delta retry)

## Task 6: Delegate QA

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/progress.md`

**Step 1: Spawn a fresh QA agent**

QA input must include:
- exact focused checks from planner/reviewer
- full verification requirement when behavior changes
- scope-drift checks relevant to the milestone

**Step 2: Wait for QA result**

Expected: `YES` or `NO` with command evidence.

**Step 3A: If QA is `YES`**

Log `qa_gate` with `YES` and continue to integration.

**Step 3B: If QA is `NO`**

Log `qa_gate` with `NO` and go to Task 8.

## Task 7: Delegate Integration and Verification

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/mechanism_table.md`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/findings.md`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/progress.md`

**Step 1: Spawn a fresh integrator**

Integrator input must include:
- reviewed/QA-green attempt worktree
- branch name
- `master_sha_before`
- requirement to merge with `--no-ff`
- requirement to preserve root dirty docs

**Step 2: Wait for integration result**

Expected: `commit_sha`, `merge_sha`, merged verification result, cleanup result.

**Step 3: Log `integration_result`**

Record both SHAs and merged verification.

**Step 4: Spawn a fresh verifier**

Verifier input must include:
- merged `master` SHA
- current milestone
- roadmap path
- mechanism table path

**Step 5: Wait for verifier result**

Expected:
- `milestone_gate`
- `completion_gate`
- `blockage_gate`
- `next_anchor_milestone`

**Step 6A: If milestone is `YES` and completion is `NO`**

- Log `round_complete`
- Update current mechanism row to `YES`
- Update next anchor row/action as needed
- Advance to next round

**Step 6B: If completion is `YES`**

- Log `terminal_status = COMPLETED`
- Mark all rows `YES`
- Stop

**Step 6C: If blockage is `YES`**

- Delegate a fresh authority audit before emitting `FAILED`.
- If the authority audit returns `NO` because of orphan attempt state, run the Authority Recovery Lane instead of stopping.
- Log `terminal_status = FAILED` only if delegated evidence still proves a genuine unrecoverable blockage after that audit.
- Record the verified blockage reason in `findings.md`.
- Stop.

## Task 8: Retry Handling Inside the Same Round

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/findings.md`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/progress.md`

**Step 1: Spawn a fresh authority-audit agent**

Authority-audit input must include:
- current round and last authoritative attempt
- expected branch/worktree naming for the round
- instruction to report whether any live worktree/branch/task folder lacks authoritative handoff in `orchestrator-log.jsonl`
- instruction not to modify the repo

**Step 2: Wait for the authority audit**

Expected: `authority_gate = YES|NO`, any orphan paths, and whether packet authority is intact.

**Step 3: Log `authority_check`**

**Step 4A: If `authority_gate = YES`**

- Spawn a fresh cleanup agent only for the logged failed attempt.
- Wait for cleanup result and log `cleanup_result`.
- Spawn a fresh planner delta agent with the latest rejection reason, previous slice, same milestone/round, and same `master_sha_before`.
- Wait for `PlannerDelta`, log `planner_delta`, increment the attempt counter, and retry the same round.

**Step 4B: If `authority_gate = NO`**

- Run the Authority Recovery Lane before any normal cleanup.
- Dispatch a fresh recovery-snapshot agent to capture facts only: branch, `HEAD`, base SHA, status, diffstat, changed files, task-folder presence, and whether `master` is untouched.
- Derive the orphan attempt number from task-folder suffix, else worktree suffix `-aN`, else `last_authoritative_attempt + 1`; set `attempt_number_inferred = YES` only on fallback.
- Log `recovery_snapshot` without inventing missing attempt handoff events.
- Dispatch a fresh quarantine agent to write a non-authoritative forensic bundle under the active packet folder (summary + patch archive), then remove the orphan worktree/branch/task folder while preserving intentionally dirty root docs.
- Log `recovery_quarantine` with `recovery_policy = Quarantine + Retry`, the artifact path, and cleanup outcome.
- Dispatch a fresh recovery verifier to confirm the orphan state is gone, packet authority is restored, and `master`/merged history remain uncontaminated.
- Log `recovery_resume`, increment the attempt counter to the next attempt number, dispatch a fresh planner delta agent, log `planner_delta`, and retry the same round from the same `master_sha_before`.

**Step 5: If attempts exhausted**

- Log `terminal_status = MAXIMUMRETRY` unless delegated recovery or verifier evidence proves a stronger `FAILED` condition after the required authority audit.
- Stop.

## Task 9: Ten-Round Control Logic

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/task_plan.md`
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator_prompt.md`

**Step 1: Encode the invariant**

Document clearly that the orchestrator does **not** plan all ten rounds in advance.

**Step 2: Encode the per-round planner requirement**

Document clearly that every round begins with a fresh planner decision based on current `master` and the first unresolved roadmap milestone.

**Step 3: Encode the role-separation rule**

Document clearly that the orchestrator never performs repo work itself.

**Step 4: Encode retry and terminal rules**

Document exactly when to continue, retry, or stop.

## Task 10: Final Reporting Format

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator_prompt.md`

**Step 1: Standardize the end-of-run report**

When the campaign reaches terminal state, report exactly:
- terminal status
- rounds used
- merged branches and SHAs
- milestones still `NO`
- authoritative log path

**Step 2: Keep this report derived from the packet**

Never derive terminal reporting from memory alone.

---

## Execution Notes

- The orchestrator should assume packet drift can happen and should repair prose artifacts whenever the log proves a newer truth.
- The planner’s main job is **not** roadmap decomposition for all future work; it is choosing one current round slice that can actually ship.
- Cleanup must happen before retries so failed-attempt state never bleeds into the next attempt.
- The verifier, not the planner, decides whether a milestone is actually complete on merged `master`.
- If the round fails only because of orchestration or packet hygiene, fix it through the delegated Authority Recovery Lane and then continue the same round; do not emit `FAILED` until delegated recovery evidence says the packet fault is unrecoverable.

Plan complete and saved to `/Volumes/src/mlf4/docs/plans/2026-03-12-orchestrator-round-loop-plan.md`.

Two execution options:

1. Subagent-Driven (this session) — dispatch fresh subagent per task, review between tasks, fast iteration
2. Parallel Session (separate) — open a new session with executing-plans and run the protocol there

Which approach?"}
