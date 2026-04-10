# P5 Polymorphism-Nested-Forall Broader-Positive Explicit Boundary-Enactment Roadmap

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-025`
Base branch: `codex/automatic-recursive-type-inference`
Created: 2026-04-10

## Goal

Continue the same enactment family after accepted `round-219` was
squash-merged onto `codex/automatic-recursive-type-inference` as commit
`7616109` with title
`Promote sameLaneOctupleAliasFrameClearBoundaryExpr to the next milestone-3 anchor`,
on top of accepted `round-218` milestone-3 anchor commit `7a127e2`,
accepted `round-217` milestone-3 anchor commit `f405079`,
accepted `round-216` milestone-3 anchor commit `21fddba`,
accepted `round-215` milestone-3 anchor commit `1b62ad5`,
accepted `round-214` milestone-3 anchor commit `ed66291`,
accepted `round-213` milestone-3 anchor commit `2091c39`,
accepted `round-212` milestone-3 anchor commit `9bb2229`, and accepted
`round-211` milestone-2 baseline commit `5b775b2`.

`rev-025` preserves the merged baseline exactly:

- preserve the merged `round-211` production/test payload in
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`,
  plus the merged `round-212`, `round-213`, `round-214`, `round-215`,
  `round-216`, `round-217`, `round-218`, and `round-219`
  evidence-surface promotions in the three test files and the accepted
  round-local notes under `orchestrator/rounds/round-212/` through
  `orchestrator/rounds/round-219/`;
- preserve the green merged behavior and gates now visible on base-branch
  `HEAD = 7616109`: the selected same-wrapper nested-`forall` packet stays
  recursive on both authoritative entrypoints,
  `sameLaneAliasFrameClearBoundaryExpr` remains preserved predecessor truth
  on both authoritative entrypoints,
  `sameLaneClearBoundaryExpr` is the first explicit milestone-3
  clear-boundary anchor on the research / pipeline / elaboration surfaces,
  `sameLaneDoubleAliasFrameClearBoundaryExpr`,
  `sameLaneTripleAliasFrameClearBoundaryExpr`,
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr`,
  `sameLaneQuintupleAliasFrameClearBoundaryExpr`,
  `sameLaneSextupleAliasFrameClearBoundaryExpr`,
  `sameLaneSeptupleAliasFrameClearBoundaryExpr`, and
  `sameLaneOctupleAliasFrameClearBoundaryExpr` are the successive next
  explicit milestone-3 clear-boundary anchors on those same surfaces,
  checked-authoritative representative parity,
  `BUG-2026-02-06-002`,
  the retained-child exact packet,
  `BUG-2026-02-17-002`,
  the correct semantic `g g` failure,
  the A6 / nested-let / representative let-polymorphism cluster,
  `./scripts/thesis-conformance-gate.sh`, and
  `cabal build all && cabal test` with `1362 examples, 0 failures`;
- treat accepted `rev-015` as the exhausted first milestone-3 publication
  epoch that produced and merged the `sameLaneClearBoundaryExpr` anchor,
  accepted `rev-016` as the exhausted second milestone-3 publication epoch
  that produced and merged the
  `sameLaneDoubleAliasFrameClearBoundaryExpr` anchor, accepted `rev-017` as
  the exhausted third milestone-3 publication epoch that produced and merged
  the `sameLaneTripleAliasFrameClearBoundaryExpr` anchor, accepted
  `rev-018` as the exhausted fourth milestone-3 publication epoch that
  produced and merged the
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr` anchor, accepted `rev-019`
  as the exhausted fifth milestone-3 publication epoch that produced and
  merged the `sameLaneQuintupleAliasFrameClearBoundaryExpr` anchor, accepted
  `rev-020` as the exhausted sixth milestone-3 publication epoch that
  produced and merged the
  `sameLaneSextupleAliasFrameClearBoundaryExpr` anchor, and accepted
  `rev-021` as the exhausted seventh milestone-3 publication epoch that
  produced and merged the
  `sameLaneSeptupleAliasFrameClearBoundaryExpr` anchor, and accepted
  `rev-024` as the exhausted eighth milestone-3 publication epoch that
  produced and merged the
  `sameLaneOctupleAliasFrameClearBoundaryExpr` anchor; treat `rev-022` and
  `rev-023` as stale unusable recovery publications that do not participate
  in the accepted lineage and must remain untouched and unactivated; and
- keep the family inside the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback / no-second-interface
  boundary unless later accepted evidence explicitly changes it.

Accepted `round-219` merged four controlling facts:

- the eighth milestone-3 clear-boundary publication step is now landed on
  the base branch rather than preserved only in a round worktree;
- no production edit was needed to earn that step, so the merged milestone-2
  mechanism remains the active semantic baseline;
- the current base branch still carries the already-green nonuple-alias
  packet in inherited bounded guard/control surfaces, but that packet is not
  yet an explicit milestone-3 `P5` corpus anchor in
  `test/Research/P5ClearBoundarySpec.hs` plus matching
  `test/PipelineSpec.hs` / `test/ElaborationSpec.hs` rows; and
- the honest next control-plane move is still milestone-3 corpus broadening,
  not milestone-4 closeout.

`rev-025` therefore keeps milestone-3 live with a concrete post-round-219
contract:

- start from merged base-branch `HEAD = 7616109`; do not relitigate the
  accepted `round-211` / `round-212` / `round-213` / `round-214` /
  `round-215` / `round-216` / `round-217` / `round-218` / `round-219`
  merge bookkeeping as live blocker debt;
- keep the writable production/test slice unchanged:
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`;
- keep
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as read-only continuity anchors unless a later accepted revision proves
  another widening is required;
- treat `sameLaneAliasFrameClearBoundaryExpr` as preserved predecessor truth
  only, and treat the merged `sameLaneClearBoundaryExpr` first anchor, the
  merged `sameLaneDoubleAliasFrameClearBoundaryExpr` next anchor, the merged
  `sameLaneTripleAliasFrameClearBoundaryExpr` next anchor after that, the
  merged `sameLaneQuadrupleAliasFrameClearBoundaryExpr` next anchor after
  that, the merged `sameLaneQuintupleAliasFrameClearBoundaryExpr` next anchor
  after that, the merged `sameLaneSextupleAliasFrameClearBoundaryExpr` next
  anchor after that, the merged
  `sameLaneSeptupleAliasFrameClearBoundaryExpr` next anchor after that, the
  merged `sameLaneOctupleAliasFrameClearBoundaryExpr` next anchor after
  that, and the merged selected same-wrapper nested-`forall` packet as
  baseline evidence, not as whole-frontier closure; and
- require the next milestone-3 extraction to widen from the merged first
  eight clear-boundary anchors to the smallest fresh packet still missing
  explicit `P5` corpus status on this baseline:
  `sameLaneNonupleAliasFrameClearBoundaryExpr`, already green in inherited
  bounded guard/control surfaces but not yet promoted into
  `test/Research/P5ClearBoundarySpec.hs` and the matching authoritative
  pipeline / elaboration guards as the next explicit milestone-3
  representative anchor.

The family goal itself is unchanged:

- preserve recursive structure across the broader positive quantified-crossing
  frontier the accepted ledger authorizes,
- make representative broader-positive support honest on both
  `runPipelineElab` and `runPipelineElabChecked`,
- preserve the settled retained-child lane as predecessor truth rather than
  whole-frontier closure, and
- record the new controlling behavior only when the code and evidence
  actually earn it.

## Outcome Boundaries

- Source of truth remains `papers/these-finale-english.txt`.
- Success remains authoritative only when the enacted broader-positive
  frontier is visible on both `runPipelineElab` and `runPipelineElabChecked`.
- Preserve accepted `round-197` as predecessor truth only:
  `sameLaneAliasFrameClearBoundaryExpr` is one settled retained-child lane,
  not full-family closure.
- Preserve accepted `round-203`, `round-204`, and `round-205` as the planning
  ledger; accepted `round-206` as the milestone-1 freeze; accepted
  `round-207` as the first core mechanism slice; accepted `round-211`,
  merged as `5b775b2`, as the milestone-2 completion result; and accepted
  `round-212`, merged as `9bb2229`, accepted `round-213`, merged as
  `2091c39`, accepted `round-214`, merged as `ed66291`, accepted
  `round-215`, merged as `1b62ad5`, accepted `round-216`, merged as
  `21fddba`, accepted `round-217`, merged as `f405079`, accepted
  `round-218`, merged as `7a127e2`, and accepted `round-219`, merged as
  `7616109`, as successive explicit milestone-3 clear-boundary anchors.
- Preserve blocked `round-208`, `round-209`, and `round-210` as immutable
  predecessor evidence only.
- Record `rev-012`, `rev-013`, and `rev-014` honestly as exhausted
  milestone-2 coordination epochs, `rev-015` as the exhausted first
  milestone-3 post-merge publication epoch, `rev-016` as the exhausted
  second milestone-3 post-merge publication epoch, `rev-017` as the
  exhausted third milestone-3 post-merge publication epoch, `rev-018` as the
  exhausted fourth milestone-3 post-merge publication epoch, `rev-019` as
  the exhausted fifth milestone-3 post-merge publication epoch, `rev-020` as
  the exhausted sixth milestone-3 post-merge publication epoch, and
  `rev-021` as the exhausted seventh milestone-3 post-merge publication
  epoch, and `rev-024` as the exhausted eighth milestone-3 post-merge
  publication epoch. `rev-022` and `rev-023` remain stale unusable recovery
  publications outside the accepted coordination lineage. `rev-025`
  supersedes `rev-024` only for post-round-219 coordination; it does not
  rewrite accepted evidence.
- `rev-025` does not authorize milestone-4 closeout work.
- No cyclic search, multi-SCC widening, equi-recursive reasoning, fallback
  rescue, or second interface is authorized.
- Backwards compatibility shims remain out of scope.

## Global Sequencing Rules

- `milestone-1` remains complete before any code-bearing enactment round.
- `milestone-2` is complete on merged commit `5b775b2`.
- `milestone-3` may broaden representative success only on top of merged
  `7616109` and may not silently reopen milestone-2 as live debt or collapse
  the merged first eight clear-boundary anchors into whole-frontier closure.
- `milestone-4` closes the family only after milestone-3 actually earns the
  broader-positive claim.
- Keep predecessor roadmap families and revisions immutable. Any later change
  to this family's coordination or semantic contract must publish a new
  revision under the same `roadmap_id`.
- This family remains intentionally serial.

## Parallel Lanes

- `lane-main`: default serial lane for the full enactment family.

## Milestones

### 1. [done] Freeze the enactment contract, authoritative frontier, writable slice, and representative corpus

- Milestone id: `milestone-1`
- Depends on: none
- Intent: publish the authoritative code-bearing contract that converts the
  accepted planning handoff into an exact enactment family scope.
- Completion signal: an accepted artifact names the exact broader positive
  frontier beyond the settled retained-child lane, the authoritative success
  bar on `runPipelineElab` and `runPipelineElabChecked`, the preserved closed
  guardrails, and the writable slice for later code-bearing rounds.

### 2. [done] Implement the broader-positive recursive-structure-preservation mechanism

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: land the core production semantics that stop treating
  polymorphic-mediation `mu` absorption as the controlling broader-positive
  read and instead preserve recursive structure across the milestone-1
  frontier within the accepted explicit-only / iso-recursive boundary.
- Completion signal: accepted code-bearing rounds land the core mechanism
  inside the milestone-1 writable slice, representative broader-positive
  quantified-crossing cases no longer fail for the old controlling reason,
  the full verification gate is green, focused tests capture the new
  behavior, and the retained-child lane plus preserved negative guardrails
  remain honest.

Accepted direction lineage:

- Direction id: `direction-2a-align-same-wrapper-nested-forall-fallback-target-and-scope`
  Status: accepted in `round-207`, merged as `fb85bd4`.
  Outcome: completed the first internal slice while keeping authoritative
  seams for later milestone-2 work.

- Direction id: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: accepted in `round-211`, merged as `5b775b2`.
  Outcome: completed the core post-merge `AAppF` / `ALetF` handoff repair and
  moved the family onto milestone-3.

### 3. [pending] Broaden representative broader-positive success on the merged baseline

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: broaden evidence from the merged milestone-2 mechanism to the
  representative broader-positive `P5 polymorphism-nested-forall` frontier
  named by milestone-1 until success is honest on both
  `runPipelineElab` and `runPipelineElabChecked`.
- Completion signal: accepted rounds expand the representative broader-
  positive corpus beyond the merged selected same-wrapper nested-`forall`
  packet and the first eight explicit clear-boundary anchors, the enacted
  frontier passes honestly on both authoritative entrypoints, focused
  regression coverage records that success, and preserved `P2` / `N1` /
  `N2` / `N6` guardrails remain fail-closed.
- Parallel lane: `lane-main`
- Coordination notes:
  - start from merged commit `7616109`, not from a live same-round baseline;
  - do not collapse the merged selected same-wrapper nested-`forall` packet,
    the merged `sameLaneClearBoundaryExpr` first anchor, the merged
    `sameLaneDoubleAliasFrameClearBoundaryExpr` next anchor, the merged
    `sameLaneTripleAliasFrameClearBoundaryExpr` next anchor after the merged
    double-alias anchor, the merged
    `sameLaneQuadrupleAliasFrameClearBoundaryExpr` next anchor after the
    merged triple-alias anchor, the merged
    `sameLaneQuintupleAliasFrameClearBoundaryExpr` next anchor after the
    merged quadruple-alias anchor, the merged
    `sameLaneSextupleAliasFrameClearBoundaryExpr` next anchor after the
    merged quintuple-alias anchor, the merged
    `sameLaneSeptupleAliasFrameClearBoundaryExpr` next anchor after the
    merged sextuple-alias anchor, the merged
    `sameLaneOctupleAliasFrameClearBoundaryExpr` next anchor after the
    merged septuple-alias anchor, and the preserved
    `sameLaneAliasFrameClearBoundaryExpr` predecessor lane into
    whole-frontier closure;
  - keep the deeper alias-budget ladder as inherited guard/control evidence
    only until a later accepted round explicitly promotes the next packet;
  - keep the current negative quantified-crossing contrasts honest unless the
    evidence really changes them; and
  - keep milestone-3 inside the existing writable slice and out of closeout
    work.

Candidate directions:

- Direction id: `direction-3a-expand-the-broader-positive-representative-corpus`
  Summary: widen from the merged selected same-wrapper nested-`forall` repair
  and the merged first eight clear-boundary anchors to the remaining
  representative broader-positive `P5` corpus already visible in inherited
  bounded guard/control surfaces.
  Why it matters now: the family goal is not one repaired packet plus seven
  explicit anchors; it is representative broader-positive support beyond the
  one settled retained-child lane.
  Preconditions: `milestone-2`; accepted `round-219`.
  Extraction notes: the next unfinished item is
  `promote-same-lane-nonuple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`,
  meaning:
  promote `sameLaneNonupleAliasFrameClearBoundaryExpr` from inherited
  bounded guard/control evidence to the next explicit milestone-3
  representative anchor in `test/Research/P5ClearBoundarySpec.hs`, the
  matching `test/PipelineSpec.hs` rows, and an exact-edge authoritative-
  instantiation guard in `test/ElaborationSpec.hs`, while preserving
  `sameLaneClearBoundaryExpr`,
  `sameLaneDoubleAliasFrameClearBoundaryExpr`,
  `sameLaneTripleAliasFrameClearBoundaryExpr`,
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr`,
  `sameLaneQuintupleAliasFrameClearBoundaryExpr`,
  `sameLaneSextupleAliasFrameClearBoundaryExpr`,
  `sameLaneSeptupleAliasFrameClearBoundaryExpr`,
  `sameLaneOctupleAliasFrameClearBoundaryExpr`,
  the merged selected same-wrapper nested-`forall` packet,
  `sameLaneAliasFrameClearBoundaryExpr` predecessor truth,
  checked-authoritative parity, and the bounded rule that deeper alias
  shells stay outside this exact extraction.

- Direction id: `direction-3b-tighten-public-internal-parity-and-regression-guards`
  Summary: close any remaining parity or regression holes so milestone-3 ends
  with stable broader-positive support on both authoritative entrypoints.
  Why it matters now: representative support is not honest if it survives
  only on one entrypoint or only through helper-local behavior.
  Preconditions: `milestone-2`; likely depends on accepted progress from
  `direction-3a`.

Accepted extraction lineage:

- Extraction id: `promote-same-lane-clear-boundary-packet-to-first-explicit-milestone-3-representative-corpus-anchor`
  Direction: `direction-3a-expand-the-broader-positive-representative-corpus`
  Status: accepted in `round-212`, merged as `9bb2229`.
  Outcome: made `sameLaneClearBoundaryExpr` explicit as the first
  milestone-3 clear-boundary anchor across the research, pipeline, and
  elaboration surfaces without reopening production code.

- Extraction id: `promote-same-lane-double-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`
  Direction: `direction-3a-expand-the-broader-positive-representative-corpus`
  Status: accepted in `round-213`, merged as `2091c39`.
  Outcome: made `sameLaneDoubleAliasFrameClearBoundaryExpr` explicit as the
  next milestone-3 clear-boundary anchor across the research, pipeline, and
  elaboration surfaces without reopening production code.

- Extraction id: `promote-same-lane-triple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`
  Direction: `direction-3a-expand-the-broader-positive-representative-corpus`
  Status: accepted in `round-214`, merged as `ed66291`.
  Outcome: made `sameLaneTripleAliasFrameClearBoundaryExpr` explicit as the
  next milestone-3 clear-boundary anchor across the research, pipeline, and
  elaboration surfaces without reopening production code.

- Extraction id: `promote-same-lane-quadruple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`
  Direction: `direction-3a-expand-the-broader-positive-representative-corpus`
  Status: accepted in `round-215`, merged as `1b62ad5`.
  Outcome: made `sameLaneQuadrupleAliasFrameClearBoundaryExpr` explicit as
  the next milestone-3 clear-boundary anchor across the research, pipeline,
  and elaboration surfaces without reopening production code.

- Extraction id: `promote-same-lane-quintuple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`
  Direction: `direction-3a-expand-the-broader-positive-representative-corpus`
  Status: accepted in `round-216`, merged as `21fddba`.
  Outcome: made `sameLaneQuintupleAliasFrameClearBoundaryExpr` explicit as
  the next milestone-3 clear-boundary anchor across the research, pipeline,
  and elaboration surfaces without reopening production code.

- Extraction id: `promote-same-lane-sextuple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`
  Direction: `direction-3a-expand-the-broader-positive-representative-corpus`
  Status: accepted in `round-217`, merged as `f405079`.
  Outcome: made `sameLaneSextupleAliasFrameClearBoundaryExpr` explicit as
  the next milestone-3 clear-boundary anchor across the research, pipeline,
  and elaboration surfaces without reopening production code.

- Extraction id: `promote-same-lane-septuple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`
  Direction: `direction-3a-expand-the-broader-positive-representative-corpus`
  Status: accepted in `round-218`, merged as `7a127e2`.
  Outcome: made `sameLaneSeptupleAliasFrameClearBoundaryExpr` explicit as
  the next milestone-3 clear-boundary anchor across the research, pipeline,
  and elaboration surfaces without reopening production code.

- Extraction id: `promote-same-lane-octuple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`
  Direction: `direction-3a-expand-the-broader-positive-representative-corpus`
  Status: accepted in `round-219`, merged as `7616109`.
  Outcome: made `sameLaneOctupleAliasFrameClearBoundaryExpr` explicit as
  the next milestone-3 clear-boundary anchor across the research, pipeline,
  and elaboration surfaces without reopening production code.

### 4. [pending] Close out the family honestly

- Milestone id: `milestone-4`
- Depends on: `milestone-3`
- Intent: publish the authoritative closeout record that states the enacted
  broader-positive behavior, records the evidence surface, and explains which
  guardrails and excluded families remain closed.
- Completion signal: an accepted closeout artifact records that the broader
  positive frontier succeeds on both `runPipelineElab` and
  `runPipelineElabChecked`, documents preserved closed boundaries honestly,
  and updates repo-facing notes only when the evidence requires it.
- Parallel lane: `lane-main`
- Coordination notes: do not publish closeout until milestone-3 has actually
  earned the claim.
