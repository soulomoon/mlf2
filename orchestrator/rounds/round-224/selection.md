# Round 224 - Task Selection

**Selected milestone**: `milestone-3`
**Selected direction**: `direction-3a-clarify-direct-vs-closure-callable-shapes`
**Selected extraction**: absent in `rev-001`

**Round metadata**:
- round_id: `round-224`
- branch: `orchestrator/round-224-clarify-callable-shapes`
- worktree_path: `orchestrator/worktrees/round-224`
- active_round_dir: `orchestrator/rounds/round-224`

**Roadmap identity**:
- roadmap_id: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- milestone_id: `milestone-3`
- direction_id: `direction-3a-clarify-direct-vs-closure-callable-shapes`
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
remaining `NO` row. Accepted `round-223`, merged as `006eb569`, completed
`milestone-2`, kept rows 1 and 2 at `YES`, and left rows 3 through 7 at
`NO`. The roadmap states directly that `milestone-3` is now the next lawful
round anchor because `Direct calls, closure values, and callable shapes` is
the first remaining `NO` mechanism.

This is also the most concrete honest next move on the current `master`
baseline. The backend already distinguishes `BackendApp` from
`BackendClosureCall`, and validation already rejects some confused shapes, but
the mechanism table and roadmap both record that the current callable split
still depends too much on validator/lowerer convention rather than an
explicit callable-shape contract with clear diagnostics. Before the family can
move on to ADT/layout ownership, primitive/evaluation-order assumptions,
polymorphism lowerability, or final guidance synchronization, it first needs
to settle how direct calls, local first-order calls, and closure calls are
represented and diagnosed at the `MLF.Backend.IR` boundary.

## Scope Boundaries

- Keep this round anchored strictly to mechanism-table row 3 and to
  `milestone-3` /
  `direction-3a-clarify-direct-vs-closure-callable-shapes`.
- Limit the round to callable-shape semantics for direct calls, closure
  values, confused-shape diagnostics, and the alias/captured-call cases named
  in the roadmap direction.
- Keep the round honest to the current backend baseline; do not claim the
  callable contract is already explicit before evidence is added.
- Do not widen into milestone-4 ADT/layout ownership, milestone-5
  primitive/evaluation-order work, milestone-6 polymorphism-lowerability
  work, or milestone-7 guidance closeout beyond the minimum context needed to
  preserve the row ordering.
- Do not introduce a second public backend IR, a public
  `LowerableBackend.IR`, lazy STG machinery, fallback runtime paths, or any
  broader backend-boundary revision not explicitly authorized by `rev-001`.
- Preserve the accepted milestone-1 and milestone-2 contract freezes as
  predecessor truth and do not reopen them as live debt inside this round.
- Do not edit `orchestrator/state.json` as part of this selection, and do not
  treat dirty controller-owned orchestrator files outside this round artifact
  as implementation scope.
