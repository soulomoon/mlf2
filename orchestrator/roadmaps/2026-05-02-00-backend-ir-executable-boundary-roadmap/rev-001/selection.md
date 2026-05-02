# Post-Round-227 Selection

This bundle-local note records the next lawful roadmap item after accepted
`round-227`. It is guidance for the next `select-task` handoff, not
machine-control state.

- `roadmap_id`: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- `milestone_id`: `milestone-7`
- `direction_id`: `direction-7a-close-the-mechanism-table-and-guidance-ledger`
- `extracted_item_id`: `absent`

## Why This Should Run Now

Accepted `round-227`, merged as `710c92eb`, completed `milestone-6` and froze
the backend polymorphism-lowerability contract without changing the family
boundary. The fixed mechanism order in
`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
therefore now leaves row 7 as the only remaining `NO`.

`milestone-7` / `direction-7a-close-the-mechanism-table-and-guidance-ledger` is
the lowest-numbered unfinished roadmap item and the only lawful next round
anchor. The accepted review record now fixes mechanism-table rows 1 through 6
at `YES` and leaves row 7 at `NO`, so row 7 is the first remaining gap.
Because the earlier backend-boundary mechanisms are now frozen, the remaining
lawful work is family closeout: refresh the mechanism table, backend docs, and
guidance ledger so every cited evidence owner, contract statement, and row-7
claim stays synchronized with the accepted backend boundary on `master`
without reopening the earlier milestone freezes.

## Scope Boundaries

- Stay inside `milestone-7` /
  `direction-7a-close-the-mechanism-table-and-guidance-ledger`.
- Keep the closeout bounded to validation, evidence ownership, mechanism-table
  honesty, and guidance synchronization across the family-owned backend
  surfaces.
- Preserve the accepted milestone-1 through milestone-6 boundaries:
  xMLF remains the typed elaboration IR,
  `MLF.Backend.IR` remains the single executable eager backend IR, and
  no public `LowerableBackend.IR` is authorized;
  lazy STG machinery remains out of scope; and
  direct-vs-closure callable-shape semantics, the ADT/case
  semantic-versus-layout ownership split, and the primitive/eager sequencing
  contract plus the row-6 specialization-versus-rejection lowerability
  contract stay fixed.
- Do not reopen milestone-1 through milestone-6, and do not widen the family
  into new backend architecture, runtime polymorphism, or broader
  implementation work beyond row-7 closeout synchronization.
- Do not authorize lazy STG machinery, a second public backend IR, any
  broader public lowering surface, fallback lowering/runtime rescue, or
  broader primitive/FFI expansion.
