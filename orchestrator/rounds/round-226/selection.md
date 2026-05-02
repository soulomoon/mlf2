# Round 226 - Task Selection

**Selected milestone**: `milestone-5`
**Selected direction**: `direction-5a-lock-primitive-and-evaluation-order-contract`
**Selected extraction**: absent in `rev-001`

**Round metadata**:
- round_id: `round-226`
- branch: `orchestrator/round-226-lock-primitive-evaluation-order-contract`
- worktree_path: `orchestrator/worktrees/round-226`
- active_round_dir: `orchestrator/rounds/round-226`

**Roadmap identity**:
- roadmap_id: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- milestone_id: `milestone-5`
- direction_id: `direction-5a-lock-primitive-and-evaluation-order-contract`
- extracted_item_id: absent in the active `rev-001` roadmap lineage

## Why This Should Run Now

`orchestrator/state.json` points the live round at the backend-IR
executable-boundary family
`2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`, and
`retry` is `null`, so there is no same-round retry state that can lawfully
displace normal forward selection.

The active roadmap bundle, its bundle-local `selection.md` note, and
`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
all fix the mechanism order and require each round to anchor on the first
remaining `NO` row. Accepted `round-225`, merged as `5adb3702`, completed
`milestone-4`, kept rows 1 through 4 at `YES`, and left rows 5 through 7 at
`NO`. The roadmap states directly that `milestone-5` is now the next lawful
round anchor because `Primitive operations and eager evaluation order` is the
first remaining `NO` mechanism.

This is also the most concrete honest next move on the current `master`
baseline. The backend already has executable primitive-support behavior and
eager lowering behavior, but the roadmap and mechanism table both record that
much of the primitive-operation surface and sequencing contract still lives as
lowerer assumptions, expression-shaped convention, or unsupported-path
behavior rather than as an explicitly frozen backend boundary. Before this
family can lawfully move into polymorphism lowerability or closeout
synchronization, it first needs to lock which primitive/evaluation-order
assumptions belong to the backend contract and which ordering-sensitive or
unsupported shapes must still fail with explicit backend diagnostics.

## Scope Boundaries

- Keep this round anchored strictly to mechanism-table row 5 and to
  `milestone-5` /
  `direction-5a-lock-primitive-and-evaluation-order-contract`.
- Limit the round to primitive-operation support, eager evaluation-order
  assumptions, sequencing-sensitive backend behavior, and explicit diagnostics
  for unsupported primitive or ordering-sensitive shapes.
- Keep the round honest to the current backend baseline; do not claim the
  primitive/evaluation-order contract is already explicit before evidence is
  added.
- Preserve the accepted milestone-1 through milestone-4 freezes:
  xMLF remains the typed elaboration IR,
  `MLF.Backend.IR` remains the single executable eager backend IR,
  no public `LowerableBackend.IR` is authorized,
  lazy STG machinery remains out of scope, and
  the callable-shape and ADT/case semantic-versus-layout contracts stay fixed.
- Do not widen into milestone-6 polymorphism-lowerability work or
  milestone-7 closeout synchronization beyond the minimum context needed to
  preserve the fixed row ordering.
- Do not introduce or authorize a second public backend IR, a broader public
  lowering boundary, public or lazy backend surfaces, lazy STG machinery,
  fallback runtime paths, FFI-surface expansion beyond what row 5 requires,
  or any backend-boundary revision not explicitly authorized by `rev-001`.
- Do not edit `orchestrator/state.json` as part of this selection, and do not
  treat dirty controller-owned roadmap pointer files outside this round
  artifact as implementation scope.
