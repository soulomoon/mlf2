# General Automatic Iso-Recursive Inference Successor Roadmap

## Context

- This roadmap family succeeds the completed March 29 automatic
  iso-recursive implementation and gap-fix families:
  `2026-03-29-01-automatic-iso-recursive-type-inference-completion` and
  `2026-03-29-02-iso-recursive-inference-gap-fixes`.
- Those families establish the bounded supported mechanism as predecessor
  truth only:
  automatic `TyMu` introduction exists,
  reification produces `TMu`,
  elaboration emits `ERoll` / `EUnroll`,
  and the gap-fix campaign repaired witness normalization, alias-bound
  handling, recursive-let reduction, and bounded non-local fallback opening.
- The repo still does not have an honest broad repo-level readiness claim for
  general automatic iso-recursive inference. The post-rev-004 repo-scope
  successor gate and the same-lane representative-gap rounds preserve a
  narrower unresolved current-architecture blocker lane.
- The inherited production boundary remains binding unless a later accepted
  roadmap revision changes it explicitly:
  explicit recursive annotations remain the production baseline,
  recursive meaning remains iso-recursive only,
  `non-equi-recursive = keep`,
  the inherited structural model remains non-cyclic,
  `no-fallback = keep`,
  no second interface is authorized,
  and no multi-SCC search is authorized.
- The settled exact `C1` / `P2`, `P1`, `P5`, and first same-lane retained-child
  pockets remain predecessor truth only.
- The live question for this family is:
  can one bounded current-architecture successor lane move the repo from
  bounded mechanism support toward a stronger general automatic
  iso-recursive capability claim without silently widening semantics,
  representation, or interfaces?

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Freeze successor authority, exact inherited blocker lane, and current writable slice
   Item id: `item-1`
   Depends on: none
   Parallel safe: no
   Parallel group: none
   Merge after: none
   Completion notes: complete when an accepted docs-only artifact freezes the
   predecessor authority chain from the March baseline, March 25 capability
   contract, March 27 repo-scope narrowed-successor decision, March 28 exact
   representative-gap freeze, and March 29 exact blocker settlement; binds the
   inherited live blocker lane anchored by
   `sameLaneAliasFrameClearBoundaryExpr`; records the current exact blocker read
   for that lane as whatever the live focused harness currently proves
   (`PhiTranslatabilityError` or a later exact-packet
   type-check/pipeline blocker); freezes the exact item-2 success bar; and
   freezes the writable slice for one bounded current-architecture attempt.
   This item must not authorize cyclic search, multi-SCC search,
   equi-recursive reasoning, fallback widening, second-interface work, or a
   repo-level readiness claim.

2. [done] Implement and validate one bounded current-architecture slice on the frozen blocker lane
   Item id: `item-2`
   Depends on: `item-1`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-1`
   Completion notes: accepted in `round-170`, merged as commit `45d765b`
   (`Preserve recursive output for frozen alias-frame packet`). The accepted
   bounded slice stayed inside the frozen writable slice, changed only
   `src/MLF/Elab/TermClosure.hs`,
   `test/PipelineSpec.hs`, and
   `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`, and
   extended authoritative retained-child preservation just enough to descend
   through top-level `ETyAbs` wrappers and one same-lane alias-frame boundary.
   The exact frozen packet `sameLaneAliasFrameClearBoundaryExpr` now reaches
   the bounded item-2 outcome `narrow success`: recursive output is preserved
   on both `runPipelineElab` and `runPipelineElabChecked` for that packet.
   Focused reruns and the required full gate `cabal build all && cabal test`
   passed in review, and the accepted result remains packet-bounded only: it
   does not settle general `P3` / `P4` / `P6` readiness or reopen any settled
   March predecessor packet.

3. [done] Publish one post-item-2 narrow-success settlement surface and exact repo-impact read for the frozen lane
   Item id: `item-3`
   Depends on: `item-1`, `item-2`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-2`
   Completion notes: accepted in `round-171`, merged as commit `34f3e50`
   (`Publish post-item-2 settlement surface for frozen alias-frame packet`)
   via
   `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md`.
   The accepted aggregate artifact republishes the exact post-item-2
   narrow-success read for `sameLaneAliasFrameClearBoundaryExpr`: the frozen
   alias-frame packet preserves recursive output on both `runPipelineElab`
   and `runPipelineElabChecked` within the inherited current architecture.
   It binds supporting focused reruns and full-gate provenance to accepted
   round-170 artifacts plus the active item-2 completion notes recording
   merged commit `45d765b`, records the exact repo-impact read as one settled
   packet only, and keeps the broader `P3` / `P4` / `P6`
   representative-gap, repo-level readiness, and item-4 decision / handoff
   questions unresolved without reopening any settled March predecessor
   packet.

4. [done] Record one successor decision and immediate handoff after the bounded lane
   Item id: `item-4`
   Depends on: `item-1`, `item-2`, `item-3`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-3`
   Completion notes: accepted in `round-172`, merged as commit `53996e3`
   (`Record bounded successor decision and handoff after item-3 settlement`)
   via
   `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md`.
   The accepted aggregate artifact records exactly one item-4 outcome token
   `continue-bounded` and exactly one immediate handoff token
   `open one bounded current-architecture family`, grounded in the accepted
   item-3 baseline that `sameLaneAliasFrameClearBoundaryExpr` is one settled
   `narrow success` packet on both `runPipelineElab` and
   `runPipelineElabChecked` within the inherited explicit-only /
   iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback
   architecture while broader `P3` / `P4` / `P6` and repo-level readiness
   remain unresolved. This closes `rev-001` honestly: no unfinished items
   remain in this roadmap family, the family stops after recording this
   decision/handoff, and any continuation must begin as a fresh bounded
   current-architecture successor family rather than another item inside this
   roadmap or an implicit boundary revision.
