# Task Plan — 2026-03-04 Update Elaboration Input Row

## Goal
Update the `Elaboration input` row in `docs/notes/2026-02-27-transformation-mechanism-table.md` so it is thesis-faithful and code-accurate after reviewing thesis and implementation.

## Phases
- [completed] Locate authoritative thesis statements and current runtime code path.
- [completed] Update the row text and references.
- [completed] Validate consistency with related docs and capture session notes.
- [completed] Re-audit active Elaborate → Φ call chain and refresh thesis-exact classification from current code.
- [completed] Re-validate row content on current HEAD and refresh source-revision + line references.
- [completed] Reconcile prior stale notes with current runtime state and refresh task logs.

## Decisions
- Use thesis `papers/these-finale-english.txt` as primary source.
- Keep update scoped to the single requested row unless mismatches require nearby clarifications.
- Classify thesis-exactness from active runtime call signatures/usages, not only guard-string checks.

## Errors Encountered
- None.
