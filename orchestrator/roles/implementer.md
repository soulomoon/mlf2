# Implementer

Own round changes for the current bounded `C1` / `P2`
authoritative-surface item.

## Inputs

- `plan.md`
- `orchestrator/state.json`
- resolve `roadmap_id`, `roadmap_revision`, and `roadmap_dir` from
  `orchestrator/state.json`
- `roadmap_dir/verification.md`
- `roadmap_dir/retry-subloop.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
- `docs/plans/2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision.md`
- `test/Research/C1AuthoritativeSurfaceSpec.hs`
- active round worktree
- `Bugs.md`

## Duties

- Implement the approved round plan in the round worktree.
- Preserve prior retry attempts byte-for-byte when retry is active.
- Use docs, production code, focused tests, and orchestrator artifacts only as
  required by the selected stage.
- When the plan explicitly authorizes parallel sidecars, use parallel
  subagents only for bounded independent sub-slices and keep write scopes
  disjoint.
- Consolidate any parallel-sidecar results back into one authoritative round
  artifact set and record the consolidation in `implementation-notes.md`.
- Record a concise change summary in `implementation-notes.md`.

## Boundaries

- Do not reopen the settled same-lane `C2` / `C5` / `C7` pocket as live debt
  unless the accepted roadmap item explicitly does so.
- Do not promote `P5` into a second live lane.
- Do not silently treat local planning drafts or unrepublished harnesses as
  authoritative round evidence.
- Do not widen one exact `C1` packet into broader `P2` or repo-level success
  claims inside implementation artifacts.
- Do not touch cyclic search, multi-SCC search, a second interface,
  convenience fallback, or default-path widening unless the accepted roadmap
  item explicitly authorizes it.
- Do not assign overlapping write scopes to parallel subagents without
  replanning.
- Do not run concurrent Cabal commands against shared `dist-newstyle`; use an
  isolated build dir or serialize the reruns.
- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
