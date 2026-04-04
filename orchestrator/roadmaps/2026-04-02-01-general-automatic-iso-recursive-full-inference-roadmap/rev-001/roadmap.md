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

3. [done] Define fail-closed candidate generation, ambiguity rejection, and bounded termination discipline
   Item id: `item-3`
   Depends on: `item-1`, `item-2`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-2`
   Completion notes: accepted `round-179` finalized this item through
   `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`
   with authoritative review in
   `orchestrator/rounds/round-179/review-record.json`. That docs-only
   artifact froze fail-closed candidate generation to the currently named
   route arms `rootNonLocalSchemeAliasBaseLike` and
   `sameLaneLocalRetainedChildTarget` plus the retained-child guard cluster
   centered on `boundHasForallFrom`, `keepTargetFinal`, and `targetC`;
   rejected competing anchors / owners / binder-side placements under `N1`
   and `N2` instead of ranking or guessing among them; explained why `N6`
   stays bounded through a finite serial search over those named families and
   guards only; and preserved the inherited explicit-only / iso-recursive /
   non-equi-recursive / non-cyclic-graph / no-fallback boundary while
   leaving runtime semantics unchanged and deferring reconstruction-visible
   readiness, implementation, and repo-level decision work to later items.

4. [done] Define the reconstruction-visible readiness contract and authoritative evaluation surfaces
   Item id: `item-4`
   Depends on: `item-1`, `item-2`, `item-3`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-3`
   Completion notes: accepted `round-180` finalized this item through
   `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`
   with authoritative review in
   `orchestrator/rounds/round-180/review-record.json`. That docs-only
   artifact named `runPipelineElab`, `runPipelineElabChecked`,
   `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Pipeline.hs`, and
   `src-public/MLF/Pipeline.hs` as the authoritative current surfaces; froze
   positive `P6` to reconstruction-visible `TyMu` to `TMu` plus
   `ERoll` / `EUnroll` continuity on those surfaces instead of solver-only,
   helper-only, witness-only, fallback-only, internal-only, or
   packet-history-only success; bound representative reruns for `P2`-`P6`
   plus `N1`, `N2`, and `N6` to
   `test/Research/C1AuthoritativeSurfaceSpec.hs`,
   `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
   `test/Research/P5ClearBoundarySpec.hs`, and `test/PipelineSpec.hs`; and
   preserved the inherited explicit-only / iso-recursive / non-equi-recursive
   / non-cyclic-graph / no-fallback boundary while leaving implementation,
   aggregate evidence, and the repo-level readiness decision to later items.

5. [pending] Run the bounded positive-family implementation and evidence campaign
   Item id: `item-5`
   Depends on: `item-1`, `item-2`, `item-3`, `item-4`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-4`
   Completion notes: execute the minimum bounded implementation and verification
   rounds needed to push the unresolved positive families toward an honest
   current-architecture answer. Start with one bounded slice at a time on the
   item-4 authoritative surfaces `runPipelineElab`,
   `runPipelineElabChecked`, `src/MLF/Elab/Pipeline.hs`, and
   `src-public/MLF/Pipeline.hs`, reusing the representative positive corpus
   anchors `test/Research/C1AuthoritativeSurfaceSpec.hs`,
   `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`, the
   positive-control row in `test/Research/P5ClearBoundarySpec.hs`, and
   `test/PipelineSpec.hs` while keeping the item-3-admitted route families
   and guard cluster unchanged. The campaign must target representative
   `P2`-`P6` cases one bounded slice at a time, land only the minimum
   code/test changes justified by the frozen contracts, and end in one
   aggregate artifact that states which positive families now have credible
   general support, which still reduce to packet-specific folklore, and which
   remain current-architecture blockers. Accepted `round-181` settles the
   first bounded `P2` slice only: on the exact `C1` non-local
   scheme-alias/base-like packet from
   `test/Research/C1AuthoritativeSurfaceSpec.hs` and `test/PipelineSpec.hs`,
   removing `preserveC1AuthoritativeRecursiveAlias` /
   `isBlockedC1AliasScheme` from `src/MLF/Elab/Run/Pipeline.hs` leaves the
   fallback `baseTarget -> baseC` lane honestly non-recursive while
   `runPipelineElab` and `runPipelineElabChecked` remain recursively
   reconstruction-visible, so that packet no longer depends on a packet-local
   `Run/Pipeline` rescue. Item-5 remains pending because this is still one
   representative `P2` packet only. Accepted `round-182` settles one bounded
   same-lane packet only: `sameLaneAliasFrameClearBoundaryExpr` remains honest
   on `runPipelineElab` and `runPipelineElabChecked` only via a narrowed
   shared `src/MLF/Elab/TermClosure.hs` clear-boundary retained-child rule,
   with the item-3 route / guard cluster and pipeline facades unchanged.
   Accepted `round-183` settles one adjacent same-lane packet only:
   `sameLaneDoubleAliasFrameClearBoundaryExpr` remains honest on the
   authoritative entrypoints only via the existing bounded one-extra-alias-shell
   `TermClosure` rule, and the current round needed only exact
   authoritative-output / mechanism-guard test tightening rather than a new
   production edit. Accepted `round-184` settles one further adjacent
   same-lane packet only:
   `sameLaneTripleAliasFrameClearBoundaryExpr` remains honest on
   `runPipelineElab` and `runPipelineElabChecked` only via an exact depth-2 /
   two-extra-alias-shell `TermClosure` preservation rule on the authoritative
   entrypoints, with the retained-child route / guard cluster and pipeline
   facades unchanged. Accepted `round-185` settles one further adjacent
   same-lane packet only:
   `sameLaneQuadrupleAliasFrameClearBoundaryExpr` remains honest on
   `runPipelineElab` and `runPipelineElabChecked` only via extending the
   shared `src/MLF/Elab/TermClosure.hs` retained-child alias-boundary entry
   budget from `1` to `2`, while keeping `hasRetainedChildClearBoundary` as
   the terminal bounded rule and leaving the retained-child route / guard
   cluster, pipeline facades, and fallback seams unchanged. Accepted
   `round-186` settles one further adjacent same-lane packet only:
   `sameLaneQuintupleAliasFrameClearBoundaryExpr` remains honest on
   `runPipelineElab` and `runPipelineElabChecked` only via keeping the outer
   `src/MLF/Elab/TermClosure.hs` retained-child alias-boundary entry budget
   fixed at `2` while adding a bounded one-step alias budget inside the
   terminal clear-boundary helper, with the retained-child route / guard
   cluster, pipeline facades, and fallback seams unchanged. Fresh sextuple
   probes still fail closed on both authoritative entrypoints before any
   further widening. Accepted `round-187` settles one further adjacent
   same-lane packet only:
   `sameLaneSextupleAliasFrameClearBoundaryExpr` remains honest on
   `runPipelineElab` and `runPipelineElabChecked` only via raising the
   terminal `src/MLF/Elab/TermClosure.hs`
   `hasRetainedChildClearBoundaryWithAliasBudget source term` entry budget
   from `1` to `2` while keeping the outer
   `hasRetainedChildAliasBoundary v body 2 =` seam fixed, with the
   retained-child route / guard cluster, pipeline facades, and fallback seams
   unchanged. Accepted `round-188` settles one further adjacent same-lane
   packet only: `sameLaneSeptupleAliasFrameClearBoundaryExpr` remains honest
   on `runPipelineElab` and `runPipelineElabChecked` only via raising the
   terminal `src/MLF/Elab/TermClosure.hs`
   `hasRetainedChildClearBoundaryWithAliasBudget source term` entry budget
   from `2` to `3` while keeping the outer
   `hasRetainedChildAliasBoundary v body 2 =` seam fixed, with the
   retained-child route / guard cluster, pipeline facades, and fallback seams
   unchanged. Fresh octuple probes still fail closed on both authoritative
   entrypoints. Accepted `round-189` settles one further adjacent same-lane
   packet only: `sameLaneOctupleAliasFrameClearBoundaryExpr` remains honest
   on `runPipelineElab` and `runPipelineElabChecked` only via raising the
   terminal `src/MLF/Elab/TermClosure.hs`
   `hasRetainedChildClearBoundaryWithAliasBudget source term` entry budget
   from `3` to `4` while keeping the outer
   `hasRetainedChildAliasBoundary v body 2 =` seam fixed, with the
   retained-child route / guard cluster, pipeline facades, and fallback seams
   unchanged. Fresh nonuple probes still fail closed on both authoritative
   entrypoints. Accepted `round-190` settles one further adjacent same-lane
   packet only: `sameLaneNonupleAliasFrameClearBoundaryExpr` remains honest
   on `runPipelineElab` and `runPipelineElabChecked` only via raising the
   terminal `src/MLF/Elab/TermClosure.hs`
   `hasRetainedChildClearBoundaryWithAliasBudget source term` entry budget
   from `4` to `5` while keeping the outer
   `hasRetainedChildAliasBoundary v body 2 =` seam fixed, with the
   retained-child route / guard cluster, pipeline facades, and fallback seams
   unchanged. Fresh decuple probes still fail closed on both authoritative
   entrypoints. Item-5 remains pending because these are still ten
   representative packets only, not a general positive-family closure; the
   next unfinished work should now stay concrete by producing the required
   aggregate item-5 artifact from the accepted `C1` packet, the
   alias-through-nonuple same-lane chain, and the fresh decuple fail-closed
   frontier instead of extending the adjacent same-lane packet ladder by one
   more step.

6. [pending] Run the fail-closed negative-family and termination-pressure campaign
   Item id: `item-6`
   Depends on: `item-1`, `item-3`, `item-4`, `item-5`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-5`
   Completion notes: execute the minimum bounded evidence campaign needed to
   validate ambiguity rejection, soundness guards, and bounded termination for
   representative `N1`, `N2`, and `N6` cases under the same current
   architecture. Use the item-4 authoritative surfaces and the already-frozen
   negative / pressure rows, including the nested-`forall` contrast in
   `test/Research/P5ClearBoundarySpec.hs` plus the disagreement, ambiguity,
   and widened-search pressure checks in `test/PipelineSpec.hs`, without
   reopening the item-3 route-family or guard contract. The resulting
   aggregate read must show fail-closed or bounded behavior honestly,
   preserve `N3`-`N5` as out of scope unless explicitly revised later, and
   avoid counting any ambiguous or unsafe case as positive success.

7. [pending] Record the repo-level readiness and architecture decision for full automatic iso-recursive inference
   Item id: `item-7`
   Depends on: `item-5`, `item-6`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-6`
   Completion notes: record exactly one end-state decision for this family
   after the item-5 and item-6 aggregate evidence is compared against the
   item-4 readiness classifications `stable visible persistence`,
   `admitted but not reconstruction-visible / blocker debt`, and
   `fail-closed rejection` on the authoritative current surfaces:
   repo-level readiness reached inside the current architecture,
   `continue-bounded` with named unresolved semantic families,
   or an explicit boundary-revision candidate. If readiness is reached, bind
   the final enablement or hardening handoff concretely. If readiness is not
   reached, record the exact semantic reason the broader claim is still not
   honest and the precise next lawful successor move.
