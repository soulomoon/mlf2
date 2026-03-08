# Task Plan: Worktree cleanup

## Goal
Remove every auxiliary git worktree for `/Volumes/src/mlf4` without deleting the primary worktree, then prune and verify the repository's worktree metadata.

## Current Phase
Phase 5

## Phases
### Phase 1: Requirements & Discovery
- [x] Understand user intent
- [x] Identify constraints and requirements
- [x] Document findings in findings.md
- **Status:** complete

### Phase 2: Planning & Structure
- [x] Define technical approach
- [x] Create task tracking structure
- [x] Document decisions with rationale
- **Status:** complete

### Phase 3: Implementation
- [x] Remove each auxiliary worktree
- [x] Use force removal for dirty worktrees as requested
- [x] Record outcomes in progress.md
- **Status:** complete

### Phase 4: Testing & Verification
- [x] Verify only the primary worktree remains
- [x] Prune worktree metadata
- [x] Confirm repository status from the primary worktree
- **Status:** complete

### Phase 5: Delivery
- [x] Summarize removals and any remaining branches/state
- [x] Confirm cleanup is complete
- [x] Deliver results to user
- **Status:** complete

## Key Questions
1. Which registered worktrees exist for this repository?
2. Are any auxiliary worktrees dirty and therefore in need of forced removal?
3. After removal, does `git worktree list` show only the primary checkout?

## Decisions Made
| Decision | Rationale |
|----------|-----------|
| Keep `/Volumes/src/mlf4` as the primary worktree | It is the current checkout on `master` and should not be removed while executing this request |
| Remove auxiliary worktrees with `git worktree remove --force` when needed | Several auxiliary worktrees contained uncommitted changes, and the user explicitly asked to clean up and delete all worktrees |
| Do not delete branches unless separately requested | Removing a worktree does not inherently require deleting its branch |
| Remove the now-empty project worktree directory under `~/.config/superpowers/worktrees/mlf4` | The user asked for cleanup, and only residue remained after worktree removal |

## Decisions Outcome
- 17 auxiliary worktrees were removed.
- `git worktree list --porcelain` now shows only `/Volumes/src/mlf4`.
- Residual empty directories and `.DS_Store` files under `/Users/ares/.config/superpowers/worktrees/mlf4` were deleted.

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| `zsh: command not found: mapfile` | 1 | Switched the removal loop to `/bin/bash` with a `while read` pipeline |

## Notes
- Branches that previously backed removed worktrees still exist unless separately deleted.
- Task notes are ready to archive under `tasks/archive/2026-03-09-worktree-cleanup/`.
