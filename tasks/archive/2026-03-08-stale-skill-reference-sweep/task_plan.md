# Task Plan: Stale Skill Reference Sweep

## Metadata
- Date: 2026-03-08
- Execution mode: docs-maintenance
- Skills in use: using-superpowers, planning-with-files, verification-before-completion

## Goal
Audit non-archival documentation for stale skill references, update live references that are no longer valid, and record the results.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Initialize context and scan scope | completed | Reviewed current guidance, workspace state, and planning-skill catchup |
| 2. Inventory live skill references | completed | Scanned live Markdown docs excluding `tasks/archive/` and `docs/plans/archive/` |
| 3. Patch stale references | completed | Replaced the lone live retired team-orchestration skill reference and updated `CHANGELOG.md` |
| 4. Verify and archive task | completed | Verified diff hygiene, confirmed no stale skill-reference patterns remain in live docs within scope, and archived the task |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Initial file inventory command used an invalid `rg --files` negated-path form | 1 | Re-ran the inventory with `--glob '!…'` exclusions instead |
| Final stale-reference grep matched the changelog note because it repeated the retired skill name verbatim | 1 | Reworded the changelog note to describe the retired reference without reintroducing the stale literal name |

## Conclusion
- The sweep found one live stale skill reference in a non-archival implementation plan.
- That reference now points to the current `@dispatching-parallel-agents` + `@tmux` pairing.
- No stale skill-reference patterns remain in live docs within the verification scope (`tasks/archive/`, `docs/plans/archive/`, and this audit task folder excluded).
