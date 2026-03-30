# CI Test-Matrix Successor Loop Scaffold

## Goal

Scaffold the next repo-local orchestrator loop so the repository can:

1. freeze a bounded GitHub Actions test-matrix scope;
2. repair the current red thesis-conformance baseline before widening CI;
3. add the matrix; and
4. fix any runner- or test-exposed failures honestly at the root cause.

This packet stops after scaffold and checkpoint commit.

## Scope

- Scaffold only; do not start runtime rounds.
- Preserve existing round artifacts and unrelated dirty worktree changes.
- Retarget the live controller, roadmap bundle, pointer stubs, role files, and
  rolling TODO to the new CI campaign.
- Keep the matrix scope honest about runner support.

## Phases

| Phase | Status | Notes |
| --- | --- | --- |
| Create scaffold task packet | complete | New task-folder planning files created for this scaffold turn. |
| Survey repo, controller, and CI baseline | complete | Existing orchestrator, workflow, roles, and local gate results reviewed. |
| Scaffold successor roadmap and retarget controller guidance | complete | Added the new roadmap bundle, advanced live controller pointers/state, retargeted role files, and updated TODO. |
| Verify scaffold and create checkpoint commit | complete | `git diff --check`, JSON/roadmap resolution checks, staged scaffold-only files, and created checkpoint commit `adf20f4`. |

## Decisions

- Start a new roadmap family at `rev-001` rather than reopening the completed
  March 29 family, because the current live controller is terminal and the new
  goal is a different repo-level CI campaign.
- Treat the current thesis-conformance failure as part of the successor loop,
  not as background noise: broadening CI while the existing gate is red would
  be dishonest.
- Keep the initial roadmap serial. The work touches shared workflow files,
  shared verification commands, and potentially shared generated docs.

## Errors Encountered

| Error | Attempt | Resolution |
| --- | --- | --- |
| `./scripts/thesis-conformance-gate.sh` failed during scaffold survey. | 1 | Recorded the exact blocker in findings and made baseline repair part of the first roadmap item instead of pretending the starting gate is green. |
