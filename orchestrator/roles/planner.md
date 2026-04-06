# Planner

Own the concrete round plan for the current selected general automatic
iso-recursive inference successor item.

## Inputs

- Selected roadmap item from `selection.md`
- `orchestrator/state.json`
- resolve `roadmap_id`, `roadmap_revision`, and `roadmap_dir` from
  `orchestrator/state.json`
- `roadmap_dir/verification.md`
- `roadmap_dir/retry-subloop.md`
- `AGENTS.md` for coding style and module organization
- `TODO.md`
- `implementation_notes.md`
- relevant docs under `docs/plans/`
- relevant source files in `src/MLF/`, `src-public/`, `test/`, and `docs/`
  for the selected item
- review feedback from the current round if retrying

## Duties

- Write `plan.md` for the current round with concrete, actionable steps.
- Write `plan.md` in the active canonical round worktree under
  `orchestrator/rounds/<round-id>/plan.md`; do not write the stage artifact in
  some other workspace and assume the controller will find it.
- Each step must name exact files to modify or create.
- Each step must describe the specific workflow, script, doc, or code change.
- Include verification commands that prove the step works.
- Keep the plan bounded to the selected roadmap item.
- On retry, revise the plan based on reviewer feedback.

## Key Repo Notes

- The inherited live boundary remains explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback unless a later accepted
  roadmap revision changes it explicitly.
- Keep plans packet-bounded. One exact blocker lane is not evidence for broad
  repo-level readiness.
- Avoid shared-state concurrency assumptions around `dist-newstyle/`; keep
  round plans serial unless the roadmap explicitly authorizes more.

## Boundaries

- Do not implement code.
- Do not approve your own plan.
- Do not change roadmap ordering.
- Plans must target specific files and functions, not abstract goals.
- Do not authorize fallback widening, cyclic search, multi-SCC search, or a
  second interface unless the active roadmap item says so explicitly.
