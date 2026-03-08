# Findings

## Scope

- Audit target: non-archival documentation only.
- Excluded: `tasks/archive/` and `docs/plans/archive/` historical records.

## Live stale references found

- `docs/plans/2026-03-03-chi-p-query-first-elab-resulttype-agent-team-implementation-plan.md` still referenced the retired `codex-tmux-team` skill path in its execution-discipline line.
- No other live non-archival Markdown docs referenced `codex-tmux-team`, `tmux-team`, `@superpowers:` handles, or `/skills/public/` stale skill paths.

## Update decision

- Replaced the stale `codex-tmux-team` reference with the current `@dispatching-parallel-agents` + `@tmux` pairing because the plan still describes a parallel/team execution model and tmux-oriented coordination.
- Left archival docs untouched so historical task records remain accurate.
