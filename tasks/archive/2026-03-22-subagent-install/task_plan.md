# Task Plan — Project Subagent Install

Date: 2026-03-22
Goal: inspect `mlf4`, evaluate candidate subagents from
`VoltAgent/awesome-codex-subagents`, and install a project-specific set that is
useful for this repository.

## Phases

1. Inspect repository context, existing agent assets, and install target —
   completed
2. Review the external subagent catalog and shortlist project-relevant agents —
   completed
3. Install the selected agents into `.codex/agents/` with minimal local
   project notes — completed
4. Verify installed assets and summarize rationale — completed

## Constraints

- Preserve unrelated local changes already present in the worktree.
- Use project-local subagents (`.codex/agents/`) rather than global agent
  installs.
- Favor agents that fit this repository's Haskell, research, review,
  orchestration, and documentation-heavy workflow.
- Avoid installing a large undifferentiated batch; keep the set curated and
  defensible.

## Errors / Recovery

- Initial attempt to clone the upstream repository with a chained cleanup +
  clone shell command was rejected by command policy; recovered by switching to
  a fresh `mktemp` directory and separate non-destructive commands.
