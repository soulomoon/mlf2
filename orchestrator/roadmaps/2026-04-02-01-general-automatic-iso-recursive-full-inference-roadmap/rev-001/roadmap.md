# General Automatic Iso-Recursive Full-Inference Roadmap

## Context

- This roadmap family succeeds the completed
  `2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap`
  family.
- That completed family settled one additional bounded packet only:
  `sameLaneDoubleAliasFrameClearBoundaryExpr` is accepted predecessor truth as
  a `narrow success` packet on both `runPipelineElab` and
  `runPipelineElabChecked`.
- The accepted handoff from that family is still
  `continue-bounded` plus
  `open one bounded current-architecture family`;
  it does not establish repo-level readiness for general automatic
  iso-recursive inference.
- The March 25 capability contract and architectural audit remain the
  authoritative repo-level semantic target and boundary read:
  explicit recursive annotations remain the production baseline,
  recursive meaning remains iso-recursive only,
  `non-equi-recursive = keep`,
  `non-cyclic-graph = unknown`,
  `no-fallback = keep`,
  no second interface is authorized,
  and no cyclic or multi-SCC widening is authorized unless a later accepted
  decision changes that boundary explicitly.
- The honest current repo position is therefore still:
  bounded positive evidence exists across several pockets,
  but the repo does not yet have a general account of non-local propagation,
  owner/binder-sensitive placement, reconstruction-visible success, bounded
  fail-closed search, or a repo-level readiness claim for full automatic
  iso-recursive inference.
- The live goal for this family is:
  push from packet-bounded predecessor truth toward an honest repo-level answer
  on full automatic iso-recursive type inference, while preserving the
  inherited current-architecture boundary unless later evidence explicitly
  earns revision.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Freeze predecessor authority, unresolved semantic matrix, family success bar, and first concrete deliverable
   Item id: `item-1`
   Depends on: none
   Parallel safe: no
   Parallel group: none
   Merge after: none
   Completion notes: accepted `round-177` finalized this item through
   `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze.md`
   with authoritative review in
   `orchestrator/rounds/round-177/review-record.json`. That docs-only
   artifact bound the predecessor authority chain from the March baseline
   contract, the March 25 capability contract, the March 25 architectural
   audit, the March 25 reconstruction contract, the accepted March/April
   bounded-settlement chain, and the April 2 `continue-bounded` handoff;
   preserved `sameLaneAliasFrameClearBoundaryExpr` and
   `sameLaneDoubleAliasFrameClearBoundaryExpr` as bounded predecessor truth
   only; froze the exact repo-level full-inference readiness question under
   the inherited explicit-only / iso-recursive / non-equi-recursive /
   non-cyclic-graph / no-fallback current architecture; froze still-live
   `P2`-`P6` plus `N1` / `N2` / `N6` while keeping `N3`-`N5` out of scope;
   and fixed the first concrete deliverable plus fail-closed docs-only
   writable slice without widening semantics, interfaces, search shape, or
   the repo-level readiness claim.

2. [done] Publish the current-architecture semantic mechanism map for automatic iso-recursive inference
   Item id: `item-2`
   Depends on: `item-1`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-1`
   Completion notes: accepted `round-178` finalized this item through
   `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
   with authoritative review in
   `orchestrator/rounds/round-178/review-record.json`. That docs-only
   artifact distinguished settled predecessor packets from still-missing
   general rules across recursive-shape discovery, non-local propagation,
   owner-sensitive placement, binder-sensitive placement, polymorphism /
   nested-`forall` interaction, and reconstruction visibility; carried
   forward `sameLaneAliasFrameClearBoundaryExpr` and
   `sameLaneDoubleAliasFrameClearBoundaryExpr` as bounded `narrow success`
   packets only plus the March 25 named-route vocabulary as predecessor
   fragments only; named the smallest lawful read-only code/test seams headed
   by `src/MLF/Constraint/Acyclicity.hs`,
   `src/MLF/Reify/Type.hs`,
   `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
   `src/MLF/Elab/TermClosure.hs`,
   `src/MLF/Elab/Run/Pipeline.hs`,
   `src/MLF/Elab/Pipeline.hs`, and
   `src-public/MLF/Pipeline.hs`; and kept the inherited explicit-only /
   iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback
   boundary plus the repo-level readiness question explicitly unresolved.

3. [pending] Define fail-closed candidate generation, ambiguity rejection, and bounded termination discipline
   Item id: `item-3`
   Depends on: `item-1`, `item-2`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-2`
   Completion notes: starting from the accepted item-2 mechanism map, produce
   one docs-first artifact that defines fail-closed candidate generation,
   ambiguity rejection, and bounded termination for the currently named route
   arms only: the non-local `rootNonLocalSchemeAliasBaseLike` arm, the
   same-lane `sameLaneLocalRetainedChildTarget` arm, and the retained-child
   guards centered on `boundHasForallFrom`, `keepTargetFinal`, and `targetC`.
   The artifact must state when those routes may introduce or preserve
   recursive candidates, how competing anchors / owners / binder-side
   placements are compared or rejected, and why representative `N1`, `N2`,
   and `N6` pressure remains bounded without fallback, heuristic guessing,
   cyclic search, multi-SCC handling, or equi-recursive equality. It may
   include bounded pseudo-algorithm or harness planning, but it must keep
   runtime semantics unchanged unless a later roadmap revision authorizes
   code.

4. [pending] Define the reconstruction-visible readiness contract and authoritative evaluation surfaces
   Item id: `item-4`
   Depends on: `item-1`, `item-2`, `item-3`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-3`
   Completion notes: starting from the accepted item-2 mechanism map and the
   item-3 search contract, produce one docs-first artifact that states what it
   means for inferred recursive structure to stay review-visible on
   `runPipelineElab`, `runPipelineElabChecked`, and the matching public
   pipeline facade, including when `TyMu` / `TMu` and `ERoll` / `EUnroll`
   continuity counts as sufficient evidence for positive `P6`. The artifact
   must bind the authoritative repo entrypoints and output surfaces, name the
   representative positive and negative corpus slices that must be rerun for a
   readiness claim, and record what evidence is sufficient or insufficient for
   repo-level generality. Solver-only success must remain insufficient by
   contract.

5. [pending] Run the bounded positive-family implementation and evidence campaign
   Item id: `item-5`
   Depends on: `item-1`, `item-2`, `item-3`, `item-4`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-4`
   Completion notes: execute the minimum bounded implementation and verification
   rounds needed to push the unresolved positive families toward an honest
   current-architecture answer. The campaign must target representative
   `P2`-`P6` cases one bounded slice at a time, land only the minimum code/test
   changes justified by the frozen contracts, and end in one aggregate artifact
   that states which positive families now have credible general support,
   which still reduce to packet-specific folklore, and which remain current-
   architecture blockers.

6. [pending] Run the fail-closed negative-family and termination-pressure campaign
   Item id: `item-6`
   Depends on: `item-1`, `item-3`, `item-4`, `item-5`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-5`
   Completion notes: execute the minimum bounded evidence campaign needed to
   validate ambiguity rejection, soundness guards, and bounded termination for
   representative `N1`, `N2`, and `N6` cases under the same current
   architecture. The resulting aggregate read must show fail-closed or bounded
   behavior honestly, preserve `N3`-`N5` as out of scope unless explicitly
   revised later, and avoid counting any ambiguous or unsafe case as positive
   success.

7. [pending] Record the repo-level readiness and architecture decision for full automatic iso-recursive inference
   Item id: `item-7`
   Depends on: `item-5`, `item-6`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-6`
   Completion notes: record exactly one end-state decision for this family:
   repo-level readiness reached inside the current architecture,
   `continue-bounded` with named unresolved semantic families,
   or an explicit boundary-revision candidate. If readiness is reached, bind
   the final enablement or hardening handoff concretely. If readiness is not
   reached, record the exact semantic reason the broader claim is still not
   honest and the precise next lawful successor move.
