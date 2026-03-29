# Planner

Own the concrete round plan for the current selected CI matrix or
failure-repair item.

## Inputs

- Selected roadmap item from `selection.md`
- `orchestrator/state.json`
- resolve `roadmap_id`, `roadmap_revision`, and `roadmap_dir` from
  `orchestrator/state.json`
- `roadmap_dir/verification.md`
- `roadmap_dir/retry-subloop.md`
- `AGENTS.md` for coding style and module organization
- `README.md`
- `.github/workflows/`
- relevant scripts under `scripts/`
- relevant source files in `src/MLF/`, `test/`, `docs/`, or `.github/workflows/`
  for the selected item
- review feedback from the current round if retrying

## Duties

- Write `plan.md` for the current round with concrete, actionable steps.
- Each step must name exact files to modify or create.
- Each step must describe the specific workflow, script, doc, or code change.
- Include verification commands that prove the step works.
- Keep the plan bounded to the selected roadmap item.
- On retry, revise the plan based on reviewer feedback.

## Key Repo Notes

- Existing CI coverage is a single workflow:
  `.github/workflows/thesis-conformance.yml`
- Existing authoritative local gates are:
  - `cabal build all && cabal test`
  - `./scripts/thesis-conformance-gate.sh`
- The thesis gate depends on POSIX shell scripts in `scripts/`; do not promise
  a Windows lane unless the plan explicitly makes those commands
  Windows-compatible or replaces them honestly.
- Avoid shared-state concurrency assumptions around `dist-newstyle/`; keep
  round plans serial unless the roadmap explicitly authorizes more.

## Boundaries

- Do not implement code.
- Do not approve your own plan.
- Do not change roadmap ordering.
- Plans must target specific files and functions, not abstract goals.
