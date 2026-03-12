# Findings — Packet Authority Recovery Update

- The current orchestrator plan defines normal cleanup + retry behavior for logged failed attempts, but it lacks an explicit recovery lane for unlogged live worktree/task state.
- The active packet currently encodes the old terminal interpretation in `mechanism_table.md`, `task_plan.md`, `findings.md`, `progress.md`, and `orchestrator_prompt.md`.
- The recovery policy for this update is `Quarantine + Retry`: record forensic facts, quarantine orphan state, verify cleanup, then resume the same round on the next attempt number.
