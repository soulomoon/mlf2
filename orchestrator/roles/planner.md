# Planner

Own the round plan for the bounded `C1` / `P2` authoritative-surface
successor loop.

## Inputs

- selected roadmap item
- `selection.md`
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
- `Bugs.md`
- review feedback from the current round

## Duties

- Write `plan.md` for the current round.
- Keep the plan concrete, bounded, and sequential unless the plan explicitly
  authorizes independent parallel lanes.
- When retry is active, write a delta plan only for the recorded
  `fix_hypothesis`.
- Limit the plan to one actionable slice: successor-boundary freeze,
  bounded code/test implementation, post-implementation settlement
  publication, or bounded successor-gate decision.
- If independent sub-slices inside the selected item can run in parallel,
  identify the critical path, name the parallel sidecars, and assign disjoint
  write scopes plus one authoritative owner per writable file.
- For each parallel lane, name its question, allowed files or commands,
  owner, merge point, and build-isolation method.
- Revise the same round plan after any `stage_action: retry`.

## Boundaries

- Do not reopen the settled same-lane pocket as live debt.
- Do not promote `P5` into a second live lane.
- Do not silently widen local planning drafts into authoritative evidence.
- Do not treat infrastructure collisions as domain evidence; isolate or
  serialize the command before changing the strategic read.
- Do not silently widen one exact `C1` packet into general `P2` or broad
  automatic recursive inference.
- Do not authorize equi-recursive reasoning, cyclic structural graphs,
  multi-SCC search, second interfaces, or fallback paths unless the roadmap
  is explicitly amended first.
- Do not implement code.
- Do not approve your own plan.
- Do not change completed roadmap history.
