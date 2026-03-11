# Findings

## Live Snapshot
- `docs/plans/2026-03-11-recursive-types-roadmap.md` is the source-of-truth roadmap for recursive types, and its task breakdown spans `M0` through `M7`.
- `tasks/todo/2026-03-11-recursive-types-design/` already contains the design evidence for the semantic freeze: iso-recursive `μ`, explicit `roll`/`unroll`, no implicit unfolding in equality, contractiveness in v1, and no cyclic graph representation in v1.
- The live tree still contains no recursive-type implementation forms such as `TMu`, `TyMu`, `ERoll`, or `EUnroll`; the new campaign therefore starts from documentation-only evidence plus the existing roadmap.
- `tasks/archive/2026-03-10-agent-team-refactor-loop/` provides the closest local template for mechanism-table, prompt, and JSONL log structure, but the recursive-types campaign needs live per-round planner selection instead of a fixed loop queue.

## Orchestrator Decisions
- Completion scope is the full roadmap: `M0`, `M1`, `M2`, `M3`, `M4`, `M5`, `M6`, and `M7`.
- The orchestrator is coordination-only and never implements, reviews, runs QA, commits, merges, inspects git state, inspects diffs, creates branches/worktrees, or runs commands itself; those jobs are always delegated to fresh subagents.
- Every planner, researcher, implementer, reviewer, QA, verifier, and integrator action uses a fresh agent instance; reviewer, QA, verifier, and integrator must remain distinct from the implementer for that attempt.
- The planner chooses exactly one next slice from the lowest unfinished milestone, prefers the smallest mergeable unit, prioritizes unblockers before polish, and may request researchers only when evidence is missing.
- Retries stay inside the same round and restart from the same captured `master_sha_before`; three failed attempts in one round terminate the campaign as `FAILED` once the verifier confirms blockage or attempt exhaustion.

## Packet Contracts
- `mechanism_table.md` is the human-readable milestone ledger for `M0`..`M7`, with `YES`/`NO` gates only.
- `orchestrator_prompt.md` is the executable workflow contract for the orchestrator and delegated roles, including branch/worktree naming, event vocabulary, retry rules, and terminal states.
- `orchestrator-log.jsonl` is initialized with a `run_header` record capturing baseline commit, branch, scope, round/attempt limits, and artifact paths; all future machine state appends there.

## Validation Findings
- The roadmap overview now matches the milestone/task breakdown and the campaign packet uses the same `M0`..`M7` order.
- The prompt, table, and log use the same gate vocabulary (`YES`/`NO`) and terminal statuses (`COMPLETED`, `FAILED`, `MAXIMUMRETRY`).
- The packet includes six explicit dry-run scenarios: successful round, review rejection retry, QA rejection retry, early completion, blocked execution, and max-round exhaustion.

## Resources
- `docs/plans/2026-03-11-recursive-types-roadmap.md`
- `tasks/todo/2026-03-11-recursive-types-design/task_plan.md`
- `tasks/todo/2026-03-11-recursive-types-design/findings.md`
- `tasks/archive/2026-03-10-agent-team-refactor-loop/mechanism_table.md`
- `tasks/archive/2026-03-10-agent-team-refactor-loop/orchestrator_prompt.md`
- `tasks/archive/2026-03-10-agent-team-refactor-loop/orchestrator-log.jsonl`
