# Round 225 - Task Selection

**Selected milestone**: `milestone-4`
**Selected direction**: `direction-4a-freeze-adt-layout-ownership`
**Selected extraction**: absent in `rev-001`

**Round metadata**:
- round_id: `round-225`
- branch: `orchestrator/round-225-freeze-adt-layout-ownership`
- worktree_path: `orchestrator/worktrees/round-225`
- active_round_dir: `orchestrator/rounds/round-225`

**Roadmap identity**:
- roadmap_id: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- milestone_id: `milestone-4`
- direction_id: `direction-4a-freeze-adt-layout-ownership`
- extracted_item_id: absent in the active `rev-001` roadmap lineage

## Why This Should Run Now

`orchestrator/state.json` points the live round at the backend-IR
executable-boundary family
`2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`, and
`retry` is `null`, so there is no same-round retry state that can lawfully
displace normal forward selection.

The active roadmap bundle and
`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
fix the mechanism order and require each round to anchor on the first
remaining `NO` row. Accepted `round-224`, merged as `2c1661b3`, completed
`milestone-3`, kept rows 1 through 3 at `YES`, and left rows 4 through 7 at
`NO`. The roadmap states directly that `milestone-4` is now the next lawful
round anchor because `ADT/case semantics versus layout` is the first
remaining `NO` mechanism.

This is also the most concrete honest next move on the current `master`
baseline. `MLF.Backend.IR` already preserves semantic ADT and case structure
through nodes such as `BackendData`, `BackendConstructor`,
`BackendConstruct`, and `BackendCase`, while the lowering path still owns most
of the concrete tag, field-layout, boxing, and nullary-constructor policy as
behavior rather than as an explicit frozen contract. Before this family can
lawfully move into primitive/evaluation-order assumptions, polymorphism
lowerability, or final closeout synchronization, it first needs to lock where
semantic backend ownership stops and lowerer-owned layout policy begins.

## Scope Boundaries

- Keep this round anchored strictly to mechanism-table row 4 and to
  `milestone-4` /
  `direction-4a-freeze-adt-layout-ownership`.
- Limit the round to clarifying or locking semantic ADT/case ownership versus
  lowering-owned tag values, field layout, boxing policy,
  nullary-constructor strategy, and comparable runtime representation details.
- Keep the round honest to the current backend baseline; do not claim the
  semantic/layout split is already explicit before evidence is added.
- Preserve the accepted milestone-1 through milestone-3 freezes:
  xMLF remains the typed elaboration IR,
  `MLF.Backend.IR` remains the single executable eager backend IR,
  no public `LowerableBackend.IR` is authorized,
  lazy STG machinery remains out of scope, and
  the direct-vs-closure callable-shape contract stays fixed.
- Do not widen into milestone-5 primitive/evaluation-order work,
  milestone-6 polymorphism-lowerability work, or milestone-7 closeout
  synchronization beyond the minimum context needed to preserve the fixed row
  ordering.
- Do not introduce or authorize a second public backend IR, a broader public
  lowering boundary, public or lazy backend surfaces, lazy STG machinery,
  fallback runtime paths, or any backend-boundary revision not explicitly
  authorized by `rev-001`.
- Do not edit `orchestrator/state.json` as part of this selection, and do not
  treat dirty controller-owned roadmap pointer files outside this round
  artifact as implementation scope.
