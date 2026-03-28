# Round 139 Task Selection

- **roadmap_id**: `2026-03-29-00-automatic-iso-recursive-type-inference-implementation`
- **roadmap_revision**: `rev-001`
- **roadmap_dir**: `orchestrator/roadmaps/2026-03-29-00-automatic-iso-recursive-type-inference-implementation/rev-001`

## Selected item

- **Item**: `item-1`
- **Title**: Implement cycle detection and automatic μ-introduction in the constraint solver
- **Status in roadmap**: `[pending]` (lowest-numbered unfinished item)

## Why this runs now

`item-1` is the first unfinished roadmap item and has no dependencies. All later items (`item-2`..`item-6`) explicitly depend on earlier implementation milestones, so execution must start here. This round is the implementation kickoff for the new roadmap family, and the key blocking gap is the missing automatic path from detected type cycles to `TyMu` introduction.

## Concrete scope for this round

Implementation scope is limited to the solver-side cycle-handling foundation:

1. Extend `MLF.Constraint.Acyclicity` or add a dedicated post-acyclicity pass to detect type-level cycles in the constraint graph.
2. Introduce `TyMu` nodes automatically to break those cycles and convert cyclic type structures into iso-recursive `μ` types.
3. Preserve behavior for non-recursive programs (no semantic drift outside recursive-cycle handling).

No pipeline wiring, elaboration-term roll/unroll shaping, or documentation closeout beyond this item’s boundary are included in this selection.

## Completion criteria reference

This selection targets roadmap `item-1` completion notes in `roadmap.md`:

- Detect type-level cycles during acyclicity/post-acyclicity processing.
- Automatically introduce `TyMu` to break cycles.
- Keep non-recursive program behavior identical.
- Pass full verification gate: `cabal build all && cabal test` with zero regressions.
