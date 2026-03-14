# Task Plan

Task: Scaffold successor orchestrator for the unannotated iso-recursive research track
Created: 2026-03-14
Status: COMPLETE

## Objective

- Refresh the repo-local top-level `orchestrator/` control plane so it succeeds the completed automatic-recursive-inference roadmap with a new staged research track rooted in the approved unannotated iso-recursive roadmap design.
- Keep prior orchestrator rounds and predecessor packet evidence intact while resetting machine state for the next pending round.
- Stop after the initial scaffold checkpoint commit; do not start runtime orchestration rounds.

## Current Context

- The existing top-level `orchestrator/` is complete through `round-005` on branch `codex/automatic-recursive-type-inference`.
- The approved successor design is documented in `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md`.
- `.worktrees/` is already gitignored and available for future round worktrees.

## Phases

| Phase | Status | Notes |
| --- | --- | --- |
| 1. Survey current control plane and scaffold references | complete | Reviewed current `orchestrator/`, scaffold-skill references, approved successor spec, git status, and `.worktrees/` ignore state |
| 2. Initialize scaffold task packet | complete | Created dedicated task folder for this control-plane refresh |
| 3. Rewrite successor orchestrator contract | complete | Replaced roadmap, state, verification, and role prompts with successor-track versions rooted in the approved design spec while preserving historical rounds |
| 4. Sync adjacent planning surfaces | complete | Updated `TODO.md` and `CHANGELOG.md`; left other adjacent workflow docs unchanged because the general contract did not move |
| 5. Verify and checkpoint commit | complete | `python3 -m json.tool orchestrator/state.json`, roadmap status-line check, and `git diff --check` all passed; this checkpoint commit closes the scaffold task before any runtime rounds |

## Open Questions

- None. The scaffold task ends at the checkpoint commit.

## Errors Encountered

- None yet.
