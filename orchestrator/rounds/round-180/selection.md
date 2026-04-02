# Round 180 — Task Selection

**Selected item**: item-4
**Item title**: Define the reconstruction-visible readiness contract and authoritative evaluation surfaces
**Roadmap identity**:
- roadmap_id: 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001
- roadmap_item_id: item-4

## Why now

`item-4` is the lowest-numbered unfinished roadmap item, its dependencies
`item-1`, `item-2`, and `item-3` are now done, and no live retry state forces
a same-round retry.

Accepted `round-177` froze this family's predecessor authority chain,
unresolved semantic matrix, family success bar, and first concrete
deliverable. Accepted `round-178` then published the current-architecture
mechanism map and made the current baseline honest: recursive shape is
represented through `TyMu` / `TMu`, current packet-facing success still runs
through the named route arms `rootNonLocalSchemeAliasBaseLike` and
`sameLaneLocalRetainedChildTarget`, and the authoritative entrypoints remain
`runPipelineElab`, `runPipelineElabChecked`, and the matching internal/public
pipeline facades. Accepted `round-179` then fixed the fail-closed search
contract for those named route arms only, including ambiguity rejection and
bounded termination around `boundHasForallFrom`, `keepTargetFinal`, and
`targetC`.

The next honest move is therefore the roadmap's item-4 docs-first readiness
contract, not item-5 implementation and not any repo-level readiness claim.
This family still lacks an accepted contract for what counts as
reconstruction-visible success on the authoritative surfaces, when `TyMu` /
`TMu` plus `ERoll` / `EUnroll` continuity is sufficient evidence for positive
`P6`, which representative positive and negative corpus slices must be rerun
before any broader claim is honest, and why solver-only success remains
insufficient by contract. Until item-4 freezes those terms, later
implementation or evidence work would be under-specified and too easy to
overclaim.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `bad637b`
  (`Advance full-inference roadmap after round-179`)
- Active roadmap revision: `rev-001`, with items `1` through `3` done and
  items `4` through `7` pending
- Live retry state: none recorded for this family
- Accepted item-1 freeze artifact:
  `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze.md`
- Accepted item-2 mechanism-map artifact:
  `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
- Accepted item-3 search-contract artifact:
  `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`
- Mechanism/search baseline carried forward: current behavior is explained by
  the named non-local arm `rootNonLocalSchemeAliasBaseLike`, the named
  same-lane retained-child arm `sameLaneLocalRetainedChildTarget`, and the
  fail-closed retained-child guard cluster centered on `boundHasForallFrom`,
  `keepTargetFinal`, and `targetC`; ambiguity remains reject-side and search
  remains serial and bounded for those named arms only
- Authoritative output surfaces carried forward from the accepted family docs:
  `runPipelineElab`, `runPipelineElabChecked`,
  `src/MLF/Elab/Pipeline.hs`, and `src-public/MLF/Pipeline.hs`
- Named bounded predecessor truth carried forward:
  `sameLaneAliasFrameClearBoundaryExpr` and
  `sameLaneDoubleAliasFrameClearBoundaryExpr` remain accepted `narrow
  success` packets only on `runPipelineElab` and `runPipelineElabChecked`
- Still-live obligations carried forward from items `1` through `3`: positive
  `P2` through `P6` plus fail-closed pressure `N1`, `N2`, and `N6`
- Out-of-scope negative families remain unchanged: `N3`
  equi-recursive-required, `N4` cyclic-or-multi-SCC-required, and `N5`
  second-interface-or-fallback-required
- The inherited boundary remains explicit-only / iso-recursive /
  non-equi-recursive / `non-cyclic-graph = unknown` / no-fallback, with no
  second interface and no cyclic or multi-SCC widening authorized
- Controller-prepared round branch:
  `orchestrator/round-180-define-reconstruction-visible-readiness`
- Controller-prepared round worktree: `orchestrator/worktrees/round-180`
- The round worktree already contains a pre-existing `orchestrator/state.json`
  modification that remains out of scope for this round
- No accepted artifact in this family yet defines the reconstruction-visible
  readiness contract, freezes the authoritative evaluation surfaces and
  corpus obligations for positive `P6`, or states what evidence is sufficient
  versus insufficient for a later repo-level readiness claim

## Scope

- Author one docs-only item-4 readiness-contract artifact at
  `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`
- Start from the accepted item-2 mechanism map and accepted item-3
  fail-closed search contract rather than re-freezing the family baseline
- Define what counts as reconstruction-visible recursive success on
  `runPipelineElab`, `runPipelineElabChecked`, and the matching public
  pipeline facade, including when `TyMu` / `TMu` and `ERoll` / `EUnroll`
  continuity is sufficient evidence for positive `P6`
- Bind the representative positive and negative corpus slices that must be
  rerun before a broader readiness claim is honest, and state what evidence
  is sufficient versus insufficient for repo-level generality
- Keep the round docs-only and non-widening: solver-only success remains
  insufficient, no production/test/Cabal edits, no roadmap or controller-state
  edits, no implementation campaign, and no repo-level readiness or
  boundary-revision claim
