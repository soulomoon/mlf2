# Round 228 - Task Selection

**Selected milestone**: `milestone-7`
**Selected direction**: `direction-7a-close-the-mechanism-table-and-guidance-ledger`
**Selected extraction**: absent/null in `rev-001`

**Round metadata**:
- round_id: `round-228`
- branch: `orchestrator/round-228-close-backend-boundary-guidance-ledger`
- worktree_path: `orchestrator/worktrees/round-228`
- active_round_dir: `orchestrator/rounds/round-228`

**Roadmap identity**:
- roadmap_id: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- milestone_id: `milestone-7`
- direction_id: `direction-7a-close-the-mechanism-table-and-guidance-ledger`
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
remaining `NO` row. Accepted `round-227`, merged as `710c92eb`, completed
`milestone-6`, kept rows 1 through 6 at `YES`, and left row 7 as the only
remaining `NO`. The roadmap states directly that `milestone-7` is now the
next lawful round anchor because `Validation, evidence, and guidance
synchronization` is the first remaining gap in the fixed-order mechanism
table.

This is also the most concrete honest next move on the current `master`
baseline. The backend-boundary contract itself is already frozen through
accepted milestones 1 through 6; what remains is family closeout work that
refreshes the mechanism table, backend docs, and guidance ledger so every
cited evidence owner and contract statement agrees on the final one-backend-IR
/ eager-runtime / no-lazy-STG boundary without silently widening into new
backend architecture or implementation scope.

## Scope Boundaries

- Keep this round anchored strictly to mechanism-table row 7 and to
  `milestone-7` /
  `direction-7a-close-the-mechanism-table-and-guidance-ledger`.
- Limit the round to closeout synchronization: mechanism-table honesty,
  evidence-owner refresh, and durable guidance alignment across the
  family-owned backend surfaces.
- Preserve the accepted milestone-1 through milestone-6 freezes:
  xMLF remains the typed elaboration IR,
  `MLF.Backend.IR` remains the single executable eager backend IR,
  no public `LowerableBackend.IR` or second backend IR is authorized,
  lazy STG machinery remains out of scope, and
  the eager-runtime, callable-shape, ADT/case semantic-versus-layout,
  primitive/eager sequencing, and polymorphism-lowerability contracts stay
  fixed.
- Do not reopen rows 1 through 6, do not widen into public or lazy backend
  surfaces, do not authorize fallback lowering or runtime-rescue paths, and
  do not introduce any new backend mechanism beyond row-7 closeout
  synchronization.
- Do not edit `orchestrator/state.json` as part of this selection, and do not
  treat dirty controller-owned roadmap pointer files or other pre-existing
  non-round artifacts outside this selection note as implementation scope.
