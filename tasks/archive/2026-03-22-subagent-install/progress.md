# Progress — Project Subagent Install

- 2026-03-22: reviewed `AGENTS.md`, `tasks/readme`, `README.md`, `mlf2.cabal`,
  the existing `agents/` directory, and current worktree diff to establish
  constraints and the likely install target.
- 2026-03-22: created the active task packet
  `tasks/todo/2026-03-22-subagent-install/`.
- 2026-03-22: inspected `VoltAgent/awesome-codex-subagents` via GitHub and a
  temporary clone, then shortlisted the agent set against this repository's
  Haskell, Cabal, review, documentation, research, and refactoring-heavy
  workflow.
- 2026-03-22: installed eight project-local agents under `.codex/agents/` and
  added `.codex/agents/README.md` documenting the selection and the local
  `gpt-5.4` / `xhigh` normalization.
- 2026-03-22: verified the installed agent filenames, confirmed each copied
  `.toml` now points at `gpt-5.4` with `xhigh` reasoning, and checked `git
  status` to confirm the new `.codex/agents/` tree and task packet are present
  as untracked additions.
