# Findings

## Survey

- The repo already has a mature top-level `orchestrator/` tree, so the named
  `scaffold-orchestrator-loop` skill cannot be applied literally as a blank
  bootstrap. The correct adaptation is a controlled refresh / re-scaffold of
  the existing control plane.
- `orchestrator/state.json` currently points at an unrelated completed roadmap
  family:
  `2026-03-30-01-codebase-quality-and-coverage-improvements`.
- The top-level pointer stub `orchestrator/roadmap.md` is stale: it points at
  `2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap`, which no
  longer matches `state.json`.
- Current state is idle and serial:
  `stage: "done"`, `controller_stage: "done"`, `max_parallel_rounds: 1`,
  no active rounds, no pending merges.
- `.gitignore` already ignores `orchestrator/worktrees/`, so no new ignore
  rule is required unless the user wants a different worktree location.

## Scaffold Contract Read

- The scaffold skill expects a new roadmap family id
  `YYYY-MM-DD-NN-<slug>`, `rev-001`, and a repo-specific active roadmap bundle
  with tailored `verification.md`, `retry-subloop.md`, and role prompts.
- The scaffold references support explicit parallel metadata, but default to
  serial execution unless the repo has strong evidence for safe parallel
  rounds.

## Repo-Goal Tension

- The user goal is to push toward general automatic iso-recursive inference.
- The currently active orchestrator family is unrelated codebase-quality work,
  already marked complete.
- Because the repo already has accepted roadmap history for automatic
  iso-recursive inference, the main design choice is whether to replace the
  active control plane with a fresh successor family for that goal, or preserve
  the current family as live and scaffold only a sidecar documentation bundle.

## Approved Design

- The user approved the recommended design:
  create a fresh successor roadmap family and make it live immediately.
- The written design spec is
  `docs/superpowers/specs/2026-04-01-general-automatic-iso-recursive-orchestrator-refresh-design.md`.
- The approved roadmap-family identifier in the spec is
  `2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap`.
- The first concrete roadmap item is docs-only and freezes the inherited
  blocker lane and writable slice before any new implementation round starts.

## Implemented Refresh

- Added the new live roadmap bundle under
  `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/`.
- Retargeted `orchestrator/state.json` to that family and set the controller to
  idle `select-task` with serial defaults.
- Repaired the top-level pointer stubs
  `orchestrator/roadmap.md`, `orchestrator/verification.md`, and
  `orchestrator/retry-subloop.md` so they match `state.json`.
- Retuned the repo-local role prompts away from the completed code-quality
  roadmap and toward the inherited automatic-iso-recursive successor lane.

## Verification And Commit

- Verified:
  - `python3 -m json.tool orchestrator/state.json`
  - presence of the new active roadmap bundle files
  - pointer-stub consistency against `state.json`
  - stale code-quality wording removed from the new active family surfaces
  - `git diff --check -- orchestrator`
- Created the checkpoint commit:
  `3990ccc Retarget orchestrator to auto iso-recursive successor`
