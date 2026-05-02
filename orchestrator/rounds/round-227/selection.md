# Round 227 - Task Selection

**Selected milestone**: `milestone-6`
**Selected direction**: `direction-6a-freeze-polymorphism-lowerability-contract`
**Selected extraction**: absent/null in `rev-001`

**Round metadata**:
- round_id: `round-227`
- branch: `orchestrator/round-227-freeze-polymorphism-lowerability-contract`
- worktree_path: `orchestrator/worktrees/round-227`
- active_round_dir: `orchestrator/rounds/round-227`

**Roadmap identity**:
- roadmap_id: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- milestone_id: `milestone-6`
- direction_id: `direction-6a-freeze-polymorphism-lowerability-contract`
- extracted_item_id: absent/null in the active `rev-001` roadmap lineage

## Why This Should Run Now

`orchestrator/state.json` points the live round at the backend-IR
executable-boundary family
`2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`, and
`retry` is `null`, so there is no same-round retry state that can lawfully
displace normal forward selection.

The active roadmap bundle, its bundle-local `selection.md` note, and
`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
all fix the mechanism order and require each round to anchor on the first
remaining `NO` row. Accepted `round-226`, merged as `b4e239c5`, completed
`milestone-5`, kept rows 1 through 5 at `YES`, and left rows 6 through 7 at
`NO`. The roadmap states directly that `milestone-6` is now the next lawful
round anchor because `Polymorphism erasure and lowerability` is the first
remaining `NO` mechanism.

This is also the most concrete honest next move on the current `master`
baseline. `MLF.Backend.IR` already permits polymorphic nodes such as
`BackendTyAbs` and `BackendTyApp`, but the roadmap and mechanism table both
record that LLVM lowerability is still only partly an implementation property
and unsupported-node behavior rather than a fully frozen backend contract.
Before this family can lawfully move into row-7 closeout synchronization, it
first needs to define which polymorphic backend shapes may remain executable
at the backend boundary, which must be erased or specialized before LLVM
emission, and which must still fail with explicit backend diagnostics.

## Scope Boundaries

- Keep this round anchored strictly to mechanism-table row 6 and to
  `milestone-6` /
  `direction-6a-freeze-polymorphism-lowerability-contract`.
- Limit the round to defining and testing the boundary between permissive
  checked `Backend.IR` shapes and the LLVM-lowerable executable subset,
  including explicit unsupported diagnostics where lowerability remains
  intentionally closed.
- Keep the round honest to the current backend baseline; do not claim the
  polymorphism-erasure/lowerability contract is already explicit before
  evidence is added.
- Preserve the accepted milestone-1 through milestone-5 freezes:
  xMLF remains the typed elaboration IR,
  `MLF.Backend.IR` remains the single executable eager backend IR,
  no public `LowerableBackend.IR` is authorized,
  lazy STG machinery remains out of scope, and
  the eager-runtime, callable-shape, ADT/case semantic-versus-layout, and
  primitive/eager sequencing contracts stay fixed.
- Do not widen into milestone-7 closeout synchronization beyond the minimum
  context needed to preserve the fixed row ordering.
- Do not introduce or authorize a second backend IR, any broader public
  lowering surface, public or lazy backend surfaces, fallback runtime paths,
  lazy STG machinery, or any unearned polymorphism-runtime implementation that
  goes beyond the row-6 lowerability contract earned by the evidence.
- Do not edit `orchestrator/state.json` as part of this selection, and do not
  treat dirty controller-owned roadmap pointer files or other pre-existing
  non-round artifacts outside this selection note as implementation scope.
