# Round 222 - Task Selection

**Selected milestone**: `milestone-1`
**Selected direction**: `direction-1a-freeze-one-backend-ir-contract`
**Selected extraction**: absent in `rev-001`

**Round metadata**:
- round_id: `round-222`
- branch: `orchestrator/round-222-freeze-one-backend-ir-contract`
- worktree_path: `orchestrator/worktrees/round-222`
- active_round_dir: `orchestrator/rounds/round-222`

**Roadmap identity**:
- roadmap_id: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- milestone_id: `milestone-1`
- direction_id: `direction-1a-freeze-one-backend-ir-contract`
- extracted_item_id: absent in the active `rev-001` roadmap lineage

## Why This Should Run Now

`orchestrator/state.json` points this round at the fresh backend-IR boundary
family `2026-05-02-00-backend-ir-executable-boundary-roadmap` /
`rev-001`, and `retry` is `null`, so there is no same-round retry state that
can lawfully displace normal forward selection.

The active roadmap bundle fixes the mechanism order to the seven rows in
`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md` and
requires every round to anchor on the first `NO` row. The mechanism table still
shows every row as `NO`, so the first lawful anchor is row 1,
`IR role separation and non-duplication`, which maps directly to
`milestone-1` / `direction-1a-freeze-one-backend-ir-contract`.

This choice is also the most concrete honest next move on the current `master`
baseline named by `rev-001`: the repository already has xMLF,
`MLF.Backend.IR`, backend conversion, LLVM lowering, architecture notes, and
selected backend tests, but the one-backend-IR contract is still described as
understood in discussion rather than encoded as a durable invariant. Before any
later runtime, callable-shape, layout, primitive-order, or polymorphism
boundary work can advance, the family must first freeze the authoritative claim
that xMLF remains the typed elaboration IR and `MLF.Backend.IR` is the single
executable eager backend IR unless later accepted evidence earns a new public
boundary.

## Scope Boundaries

- Keep this round anchored strictly to mechanism-table row 1 and to
  `milestone-1` / `direction-1a-freeze-one-backend-ir-contract`.
- Limit the round to making the one-backend-IR role-separation contract
  durable, reviewable, and hard to regress, including the criteria that would
  justify any future lower IR.
- Keep the round honest to the current backend-native baseline on `master`;
  do not pretend later backend-boundary questions are already settled.
- Do not widen into the row-2-through-row-7 subjects: eager-runtime ownership,
  callable-shape refinement, ADT/layout ownership, primitive/evaluation-order
  assumptions, polymorphism lowerability, or final cross-surface closeout.
- Do not authorize or introduce a second public backend IR,
  `LowerableBackend.IR`, lazy STG machinery, or any broader public boundary not
  explicitly allowed by `rev-001`.
- Do not edit `orchestrator/state.json` as part of this round selection, and do
  not treat dirty controller-owned orchestrator files outside this round
  artifact as implementation scope.
