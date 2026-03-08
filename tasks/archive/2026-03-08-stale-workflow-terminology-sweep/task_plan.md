# Task Plan: Stale Workflow Terminology Sweep

## Metadata
- Date: 2026-03-08
- Execution mode: docs-maintenance
- Skills in use: using-superpowers, planning-with-files, verification-before-completion

## Goal
Audit live non-archival docs for stale workflow terminology beyond skill names/paths, update active guidance where needed, and record the results.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Initialize context | completed | Reviewed catchup output, workspace status, and created a dedicated task folder |
| 2. Inventory stale workflow terminology | completed | Identified broad live-doc patterns around stale execution-note and older parallel-work wording |
| 3. Patch live stale wording | completed | Updated live `docs/plans/*.md` guidance and added a `CHANGELOG.md` note |
| 4. Verify and archive task | completed | Verified the stale wording is gone from `CHANGELOG.md` and live `docs/plans/*.md`, then archived the task |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Initial verification still found hyphenated `Agent-Team` leftovers in several live plan titles/headings plus the changelog summary | 1 | Patched the remaining live-doc headings/titles and reworded the changelog note before rerunning verification |

## Conclusion
- Live plan docs no longer use the stale platform-specific execution-note wording.
- Older `Agent Team`/`Agent-Team` terminology in live plan titles and headings has been normalized to `Parallel Work`.
- Still-accurate `tmux` references remain only where the plan explicitly uses the `@tmux` skill.
