# Findings

## Scope

- Audit target: live non-archival Markdown docs.
- Excluded: `tasks/archive/`, `docs/plans/archive/`, and transient audit-task notes.

## Live stale workflow terminology found

- `docs/plans/*.md` still contained a widespread platform-specific execution note that referred to an older runner/workflow shape.
- A subset of live plan docs still used older `Agent Team` / `Agent-Team` wording in headings and titles even though the current workflow language is better expressed as parallel work.
- The remaining live `tmux` wording was not inherently stale where the plan still explicitly uses the `@tmux` skill.

## Update decision

- Standardized the stale execution-note line to `Execution Note` plus `@executing-plans`.
- Renamed older `Agent Team` terminology to `Parallel Work` in live plan titles/headings, without renaming files.
- Left active `tmux` references intact where they still describe actual coordination details rather than retired workflow branding.
