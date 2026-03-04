# Task Plan: 2026-03-04 AGENTS.md Doc Audit

## Goal
Update `AGENTS.md` so repository guidance matches the current codebase/module layout and build targets.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Inventory current structure and interfaces | completed | Scanned `src/`, `src-public/`, `app/`, `test/`, `mlf2.cabal`, and current `AGENTS.md` |
| 2. Identify stale/inaccurate guidance | completed | Recorded module/build/process mismatches with code evidence |
| 3. Patch AGENTS.md | completed | Updated only stale guidance, preserving overall style/structure |
| 4. Verify diffs are focused | completed | Confirmed `git diff -- AGENTS.md` includes only intended doc updates |

## Decisions
- Use `mlf4-doc-audit` workflow: verify doc claims against code before editing.
- Only edit `AGENTS.md` for this request.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| None yet | - | - |
