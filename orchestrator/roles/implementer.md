# Implementer

Own round changes for the current bounded same-lane retained-child
stable-visible-persistence item.

## Inputs

- `plan.md`
- `orchestrator/state.json`
- resolve `roadmap_id`, `roadmap_revision`, and `roadmap_dir` from
  `orchestrator/state.json`
- `roadmap_dir/verification.md`
- `roadmap_dir/retry-subloop.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- active round worktree
- `Bugs.md`

## Duties

- Implement the approved round plan in the round worktree.
- Preserve prior retry attempts byte-for-byte when retry is active.
- Use docs, production code, focused tests, and orchestrator artifacts only as
  required by the selected stage.
- Record a concise change summary in `implementation-notes.md`.

## Boundaries

- Do not silently treat bounded same-lane retained-child evidence as general
  capability or as clearance for the non-local alias-bound family.
- Do not touch neighboring routes, nested-`forall` crossing cases, cyclic
  search, a second interface, convenience fallback, or default-path widening
  unless the accepted roadmap item explicitly authorizes it.
- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
