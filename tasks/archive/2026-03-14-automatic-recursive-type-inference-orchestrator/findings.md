# Findings & Decisions

## Requirements
- Scaffold a new top-level `orchestrator/` directory for this repo.
- Tailor the scaffold to the goal `automatic recursive-type inference`.
- Prepare the repo for round worktrees and stop after the initial checkpoint commit.
- Do not start implementation rounds.

## Research Findings
- The repo already contains substantial recursive-types planning/history, including `docs/plans/2026-03-11-recursive-types-roadmap.md`, `docs/plans/2026-03-13-m6-pipeline-feasibility-spike.md`, and `docs/plans/2026-03-13-m7-tymu-design-resolution.md`.
- The current branch is `codex/automatic-recursive-type-inference` and the worktree is already dirty in `CHANGELOG.md`, `TODO.md`, and `tasks/todo/2026-03-11-recursive-types-orchestration/*`.
- There is no existing top-level `orchestrator/` directory in this checkout.
- `.gitignore` does not currently include `.worktrees/`, which the scaffold contract requires.
- The scaffold template provides `orchestrator/roadmap.md`, `state.json`, `verification.md`, role prompts, and `rounds/`.
- The recursive-types roadmap treats automatic recursive-type inference as an optional high-risk milestone after explicit recursive-type support and annotation transport work.
- The user selected a research-first orchestrator, so the scaffold should prefer documentation, acceptance gates, and bounded spikes over direct solver edits.
- The older campaign under `tasks/todo/2026-03-11-recursive-types-orchestration/` no longer represents an open roadmap. Its mechanism table, JSONL log, and terminal status record the recursive-types campaign as `COMPLETED`, with `M0`..`M7` all `YES`.
- The old campaign’s `M7` completion is explicitly not automatic recursive-type inference. Its terminal slice is an explicit-only acyclic `TyMu` annotation path that preserves the non-equi-recursive, non-inferred boundary.
- Because of that, “taking over” the older history should mean inheriting it as prerequisite evidence and baseline state for a new research target, not reopening the old milestone ledger as if it were unfinished.
- The new top-level `orchestrator/` is now scaffolded as that successor control plane. Its roadmap starts with the inherited-baseline/acceptance-contract slice for automatic recursive-type inference, and its `state.json` targets base branch `codex/automatic-recursive-type-inference`.
- Repo guidance was synchronized for the new control plane: `.gitignore` now ignores `.worktrees/`, and both `AGENTS.md` and `tasks/readme` now distinguish the top-level `orchestrator/` from task-folder orchestration packets.
- The lightweight scaffold checks all passed: `python3 -m json.tool orchestrator/state.json`, `rg -n '^\\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmap.md`, and `git diff --check`.

## Technical Decisions
| Decision | Rationale |
|----------|-----------|
| Keep the new scaffold isolated from existing recursive-types task files | The repository already has active modified task artifacts; isolated scaffolding reduces accidental overlap. |
| Use the existing recursive-types roadmap/docs as source material for the orchestrator roadmap | The goal already has repo-specific context and milestone history that should inform task selection. |
| Treat automatic recursive-type inference as a research-track target, not the immediate implementation objective | This matches both the current repo state and the user's selected scope. |
| Interpret takeover as historical succession, not milestone reset | The older campaign is already complete; the new orchestrator must start from that merged-master baseline and target the next unsolved problem beyond explicit-only support. |
| Set `orchestrator/state.json` `base_branch` to `codex/automatic-recursive-type-inference` | The successor control plane is being scaffolded and checkpointed on that branch, so future accepted rounds should merge there unless a later round explicitly changes the base. |

## Issues Encountered
| Issue | Resolution |
|-------|------------|
| `test -d .git` reported false in the worktree survey because `.git` is a worktree file, not a directory | Verified Git status directly with `git status --short --branch`; no repo initialization is needed. |
| The initial `git diff --stat` did not show the new `orchestrator/` files because they were still untracked | Followed with `git status --short` and `find orchestrator -type f` to capture the full scaffold set before staging. |

## Resources
- `/Users/ares/.codex/worktrees/d432/mlf4/AGENTS.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-11-recursive-types-roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-13-m7-tymu-design-resolution.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/`
- `/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/mechanism_table.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator_prompt.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl`
- `/Users/ares/.codex/skills/scaffold-orchestrator-loop/references/roadmap-generation.md`
- `/Users/ares/.codex/skills/scaffold-orchestrator-loop/references/repo-contract.md`
- `/Users/ares/.codex/skills/scaffold-orchestrator-loop/references/verification-contract.md`
- `/Users/ares/.codex/skills/scaffold-orchestrator-loop/assets/orchestrator/`

## Visual/Browser Findings
- None.
