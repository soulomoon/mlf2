# Task Plan: Automatic Recursive-Type Inference Orchestrator Scaffold

## Goal
Scaffold a repo-local `orchestrator/` control plane tailored to the goal of automatic recursive-type inference, without starting runtime rounds.

## Current Phase
Phase 5

## Phases
### Phase 1: Requirements & Discovery
- [x] Understand user intent
- [x] Identify constraints and requirements
- [x] Document findings in `findings.md`
- **Status:** complete

### Phase 2: Design Checkpoint
- [x] Define the orchestration target and scope boundary
- [x] Propose scaffold approaches and recommend one
- [x] Get user approval before implementation edits
- **Status:** complete

### Phase 3: Scaffold Repo Contract
- [x] Create `orchestrator/` from the scaffold template
- [x] Tailor roadmap, verification contract, and role prompts
- [x] Ensure `.worktrees/` is ignored
- **Status:** complete

### Phase 4: Checkpoint Commit
- [x] Stage scaffold files
- [x] Create the initial checkpoint commit
- [x] Stop without starting runtime rounds
- **Status:** complete

### Phase 5: Delivery
- [x] Summarize scaffolded files and assumptions
- [x] Call out any residual risks or follow-ups
- [x] Deliver the checkpoint details to the user
- **Status:** complete

## Key Questions
1. Should the scaffold supersede the existing recursive-types task documents, or treat them as background inputs and keep the new orchestrator isolated?
2. What should the first concrete round optimize for: acceptance criteria/specification, or a bounded feasibility spike?

## Decisions Made
| Decision | Rationale |
|----------|-----------|
| Use a new task folder under `tasks/todo/2026-03-14-automatic-recursive-type-inference-orchestrator/` | The branch already has dirty recursive-types task files; isolating this scaffold avoids overwriting or conflating active work. |
| Hold repo edits until the design checkpoint | The `brainstorming` skill requires a short design/approval step before implementation/scaffolding actions. |
| Optimize the orchestrator for a research-first loop | The user explicitly chose proving the shape of automatic recursive-type inference before any implementation rounds. |
| Treat the old recursive-types packet as inherited history, not as an unfinished ledger | That packet is already authoritative and complete through explicit-only `TyMu`; the new top-level orchestrator should succeed it, not rewrite it. |
| Leave the pre-existing dirty `tasks/todo/2026-03-11-recursive-types-orchestration/*` files unstaged | Those edits predated this scaffold effort and must not be overwritten or accidentally folded into the checkpoint commit. |

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| None so far | 1 | N/A |

## Notes
- Existing branch: `codex/automatic-recursive-type-inference`.
- Existing repo state is dirty in `CHANGELOG.md`, `TODO.md`, and `tasks/todo/2026-03-11-recursive-types-orchestration/*`.
- The scaffold contract required `.worktrees/` to be gitignored; this task added that ignore entry.
- Scaffold verification currently passes for `orchestrator/state.json` JSON validity, roadmap status-item structure, and `git diff --check`.
