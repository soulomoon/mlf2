# Round 223 - Task Selection

**Selected milestone**: `milestone-2`
**Selected direction**: `direction-2a-pin-eager-runtime-contract`
**Selected extraction**: absent in `rev-001`

**Round metadata**:
- round_id: `round-223`
- branch: `orchestrator/round-223-pin-eager-runtime-contract`
- worktree_path: `orchestrator/worktrees/round-223`
- active_round_dir: `orchestrator/rounds/round-223`

**Roadmap identity**:
- roadmap_id: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- milestone_id: `milestone-2`
- direction_id: `direction-2a-pin-eager-runtime-contract`
- extracted_item_id: absent in the active `rev-001` roadmap lineage

## Why This Should Run Now

`orchestrator/state.json` points this round at the active backend-IR boundary
family `2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`,
and `retry` is `null`, so there is no same-round retry state that can lawfully
displace normal forward selection.

The active roadmap bundle and
`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
both fix the mechanism order and require each round to anchor on the first
remaining `NO` row. Accepted `round-222`, merged as `5365d975`, completed
`milestone-1`, flipped only row 1 to `YES`, and left rows 2 through 7 as
`NO`. That makes row 2, `Eager runtime lowering contract`, the first lawful
remaining mechanism and therefore the next required round anchor. The roadmap
states this directly by naming `milestone-2` as the next lawful anchor after
the accepted row-1 contract freeze.

This is also the most concrete honest next move on the current `master`
baseline. The repository already has a typed `MLF.Backend.IR`, backend
conversion, LLVM lowering, backend-native pipeline guidance, and selected
backend tests, but the eager-runtime boundary is still partly implicit across
those surfaces. Before later work on callable shapes, ADT/layout ownership,
primitive/evaluation-order assumptions, polymorphism lowerability, or final
cross-surface synchronization can proceed honestly, the family must first make
explicit which runtime/codegen concerns belong in `MLF.Backend.IR`, which
belong in LLVM lowering or native emission, and which lazy STG-like runtime
mechanisms are intentionally out of scope.

## Scope Boundaries

- Keep this round anchored strictly to mechanism-table row 2 and to
  `milestone-2` / `direction-2a-pin-eager-runtime-contract`.
- Limit the round to making the eager-runtime lowering contract explicit
  across the relevant backend-owned docs, module notes, and tests, including
  the excluded lazy-runtime machinery set.
- Keep the round honest to the current backend-native baseline on `master`;
  do not pretend the later row-3 through row-7 questions are already settled.
- Do not widen into callable-shape refinement, ADT/layout ownership,
  primitive/evaluation-order design, polymorphism lowerability, or row-7
  closeout beyond the minimum context needed to define the row-2 contract.
- Do not authorize or introduce lazy STG machinery, graph reduction, thunks,
  update frames, CAF update semantics, a second public backend IR, or a
  broader public lowering boundary not explicitly allowed by `rev-001`.
- Preserve the already accepted row-1 one-backend-IR contract and treat it as
  predecessor truth, not as live debt to reopen inside this round.
- Do not edit `orchestrator/state.json` as part of this round selection, and
  do not treat dirty controller-owned orchestrator files outside this round
  artifact as implementation scope.
