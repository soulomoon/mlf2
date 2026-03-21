# Findings — Project Subagent Install

## Durable findings

- This repository already has a top-level `agents/` directory, but the
  referenced external catalog targets Codex project-local installs under
  `.codex/agents/`.
- The codebase is a Haskell implementation/research project with substantial
  emphasis on paper-faithful behavior, review, debugging, architecture, and
  orchestrated multi-step execution rather than frontend or product work.
- The worktree already contains unrelated edits in `TODO.md`, `CHANGELOG.md`,
  and the active 2026-03-21 task packet; those changes must be preserved.
- The upstream catalog does not provide a Haskell-specific agent, so the best
  fit is a curated set of language-agnostic agents for mapping, review,
  debugging, testing, build tooling, documentation, research, and safe
  refactors.
- The repository already has stronger repo-local orchestration/process skills
  than the generic upstream orchestration agents, so utility agents add more
  value than installing orchestration duplicates.
- This repo's `AGENTS.md` requires `gpt-5.4` with `xhigh` reasoning for
  subagents by default, so the copied upstream agents were normalized to those
  settings locally.
