# Round 207 - Task Selection

**Selected milestone**: `milestone-2`
**Selected direction**: `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`
**Selected extraction**: `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`
**Current task/title**: `Implement the bounded first same-wrapper nested-forall internal mechanism slice in target selection and term-closure preservation before any pipeline threading`

**Roadmap identity**:
- roadmap_id: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-001`
- milestone_id: `milestone-2`
- direction_id: `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`
- extracted_item_id: `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`

## Why now

`retry` is `null`, the active family is explicitly serial, and
`milestone-1` is already complete in the just-updated `rev-001` roadmap, so
the lawful next move is the lowest-numbered unfinished milestone:
`milestone-2`.

The current roadmap's `milestone-1` completion notes already route the family
forward to exactly one next step:
`milestone-2` /
`direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`.
Accepted `round-205` opened exactly one later enactment / implementation
family for the still-live broader-positive `P5 polymorphism-nested-forall`
pressure, and accepted `round-206` consumed that handoff by freezing the
exact broader-positive frontier, expected behavior shift, representative
corpus, authoritative success surfaces, and writable slice. Nothing in that
accepted lineage authorizes skipping directly to `milestone-3`, `milestone-4`,
or a pipeline-threading-first round.

Inside `milestone-2`, `direction-2a` is the lawful first direction because it
owns the core mechanism change. `direction-2b` is downstream threading work
for the selected semantics once that mechanism exists. The roadmap extraction
notes are explicit that the first serial extraction should start with the
internal mechanism seams in
`src/MLF/Elab/Run/ResultType/Fallback.hs`,
`src/MLF/Elab/Run/ResultType/Fallback/Core.hs`, and
`src/MLF/Elab/TermClosure.hs`, with only the smallest focused updates in
`test/PipelineSpec.hs` and `test/Research/P5ClearBoundarySpec.hs`, while
deferring
`src/MLF/Elab/Run/Pipeline.hs`,
`src/MLF/Elab/Pipeline.hs`, and
`src-public/MLF/Pipeline.hs`
to `direction-2b` unless leaving them untouched would make the slice dishonest
or non-reviewable.

The current baseline makes that bounded first extraction concrete rather than
abstract. `implementation_notes.md` still records
`Nested-forall-mediated recursive types` as absorption-driven under
polymorphic mediation. The frozen representative targets still show the old
controlling read:
`nestedForallContrastExpr` remains fail-closed with
`PhiTranslatabilityError` on both authoritative entrypoints, and the
same-wrapper nested-`forall` fallback baseline in `test/PipelineSpec.hs`
still reports no recursive wrapper. The honest next round is therefore the
first internal mechanism extraction that changes that selected read at its
actual seam cluster before any later pipeline-threading round claims broader
surface closure.

## Current baseline

- Round-207 branch/worktree `HEAD`: `3ce69d5`
- Controller-prepared round branch:
  `orchestrator/round-207-implement-core-polymorphic-mediation-recursive-structure-preservation`
- Controller-prepared round worktree: `orchestrator/worktrees/round-207`
- Active roadmap family and revision remain
  `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
  at `rev-001`; `milestone-1` is done, and `milestone-2`,
  `milestone-3`, and `milestone-4` remain pending
- No retry state is active (`retry: null`, no `resume_error`)
- Accepted `round-205` remains the binding family-entry handoff: it selected
  exactly one later enactment / implementation family and did not itself
  freeze a code-bearing slice, a concrete mechanism diff, or a broader claim
- Accepted `round-206` now freezes the exact writable slice and records the
  relevant internal mechanism seams honestly: the current repo anchors expose
  `boundHasForallFrom`,
  `sameLaneLocalRetainedChildTarget`,
  `keepTargetFinal`,
  `targetC`, and
  `preserveRetainedChildAuthoritativeResult`
  as the live seam cluster, while the three pipeline files remain the later
  authoritative continuity seams rather than the first extraction target
- `implementation_notes.md` still records the live production baseline under
  `Known correct behavior under polymorphic mediation`:
  nested-forall-mediated recursive types still absorb `mu` through
  polymorphic mediation today
- `test/Research/P5ClearBoundarySpec.hs` still keeps the clear-boundary
  controls recursive while `nestedForallContrastExpr` remains fail-closed on
  `runPipelineElab` and `runPipelineElabChecked`
- `test/PipelineSpec.hs` still keeps the same-wrapper nested-`forall`
  fallback baseline non-recursive (`containsMu False`) and still guards the
  existing `P5` seam cluster through
  `boundHasForallFrom` and
  `preserveRetainedChildAuthoritativeResult`
- `TODO.md` and the cited March 25 / 27 / 28 / 29 plan documents still
  provide predecessor capability-contract and settlement context only; they do
  not override the accepted round-206 freeze or authorize skipping the
  bounded `direction-2a` extraction
- Repository status in the canonical round worktree before writing this
  selection was one pre-existing controller-owned modification:
  `M orchestrator/state.json`

## Scope

- Keep this round faithful to exactly one
  `milestone-2` / `direction-2a` extraction:
  `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`
- Limit the selected code-bearing slice to the bounded internal mechanism
  seams in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`, and
  `src/MLF/Elab/TermClosure.hs`,
  with only the smallest focused evidence updates in
  `test/PipelineSpec.hs` and
  `test/Research/P5ClearBoundarySpec.hs`
- Keep the extraction concrete: adjust the selected same-wrapper nested-`forall`
  target-selection / recursive-structure-preservation seam so the broader
  positive quantified-crossing lane stops being controlled only by the old
  polymorphic-mediation `mu`-absorption read
- Defer
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`, and
  `src-public/MLF/Pipeline.hs`
  to `direction-2b` unless the implementer can show that leaving them
  untouched would make this bounded slice dishonest or non-reviewable
- Preserve the settled clear-boundary controls, keep `P2` unopened, keep
  `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
  `N6 termination-pressure` fail-closed, and do not widen into cyclic search,
  multi-SCC behavior, equi-recursive reasoning, fallback rescue, or a second
  interface
- Do not overclaim milestone-2 completion, full broader-positive closure, or
  milestone-3 authoritative-entrypoint success from this first extraction
