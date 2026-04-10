# P5 Polymorphism-Nested-Forall Broader-Positive Explicit Boundary-Enactment Roadmap

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-026`
Base branch: `codex/automatic-recursive-type-inference`
Created: 2026-04-10

## Goal

Continue the same enactment family after accepted `round-220` was
squash-merged onto `codex/automatic-recursive-type-inference` as commit
`ea8db76` with title
`Promote sameLaneNonupleAliasFrameClearBoundaryExpr to the next milestone-3 anchor`,
on top of accepted `round-219` milestone-3 anchor commit `7616109`,
accepted `round-218` milestone-3 anchor commit `7a127e2`,
accepted `round-217` milestone-3 anchor commit `f405079`,
accepted `round-216` milestone-3 anchor commit `21fddba`,
accepted `round-215` milestone-3 anchor commit `1b62ad5`,
accepted `round-214` milestone-3 anchor commit `ed66291`,
accepted `round-213` milestone-3 anchor commit `2091c39`,
accepted `round-212` milestone-3 anchor commit `9bb2229`, and accepted
`round-211` milestone-2 baseline commit `5b775b2`.

`rev-026` preserves the merged baseline exactly:

- preserve the merged `round-211` production/test payload in
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`,
  plus the merged `round-212`, `round-213`, `round-214`, `round-215`,
  `round-216`, `round-217`, `round-218`, `round-219`, and `round-220`
  evidence-surface promotions in the three test files and the accepted
  round-local notes under `orchestrator/rounds/round-212/` through
  `orchestrator/rounds/round-220/`;
- preserve the green merged behavior and gates now visible on base-branch
  `HEAD = ea8db76`: the selected same-wrapper nested-`forall` packet stays
  recursive on both authoritative entrypoints,
  `sameLaneAliasFrameClearBoundaryExpr` remains preserved predecessor truth
  on both authoritative entrypoints,
  `sameLaneClearBoundaryExpr` remains the first explicit milestone-3
  clear-boundary anchor on the research / pipeline / elaboration surfaces,
  `sameLaneDoubleAliasFrameClearBoundaryExpr`,
  `sameLaneTripleAliasFrameClearBoundaryExpr`,
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr`,
  `sameLaneQuintupleAliasFrameClearBoundaryExpr`,
  `sameLaneSextupleAliasFrameClearBoundaryExpr`,
  `sameLaneSeptupleAliasFrameClearBoundaryExpr`,
  `sameLaneOctupleAliasFrameClearBoundaryExpr`, and
  `sameLaneNonupleAliasFrameClearBoundaryExpr` are the successive next
  explicit milestone-3 clear-boundary anchors on those same surfaces,
  checked-authoritative representative parity,
  `BUG-2026-02-06-002`,
  the retained-child exact packet,
  `BUG-2026-02-17-002`,
  the correct semantic `g g` failure,
  the A6 / nested-let / representative let-polymorphism cluster,
  `./scripts/thesis-conformance-gate.sh`, and
  `cabal build all && cabal test` with `1365 examples, 0 failures`;
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
  `sameLaneSextupleAliasFrameClearBoundaryExpr` anchor, accepted `rev-021`
  as the exhausted seventh milestone-3 publication epoch that produced and
  merged the `sameLaneSeptupleAliasFrameClearBoundaryExpr` anchor, accepted
  `rev-024` as the exhausted eighth milestone-3 publication epoch that
  produced and merged the
  `sameLaneOctupleAliasFrameClearBoundaryExpr` anchor, and accepted
  `rev-025` as the exhausted ninth milestone-3 publication epoch that
  produced and merged the
  `sameLaneNonupleAliasFrameClearBoundaryExpr` anchor; treat `rev-022` and
  `rev-023` as stale unusable recovery publications that do not participate
  in the accepted lineage and must remain untouched and unactivated; and
- keep the family inside the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback / no-second-interface
  boundary unless later accepted evidence explicitly changes it.

Accepted `round-220` merged four controlling facts:

- the ninth milestone-3 clear-boundary publication step is now landed on the
  base branch rather than preserved only in a round worktree;
- no production edit was needed to earn that step, so the merged milestone-2
  mechanism remains the active semantic baseline;
- the merged research / pipeline / elaboration surfaces now make
  `sameLaneNonupleAliasFrameClearBoundaryExpr` explicit on both
  authoritative entrypoints and on the exact-edge authoritative-
  instantiation guard; and
- accepted predecessor evidence from `round-190` and `round-191` still
  records the same-lane decuple frontier as fail-closed on both
  authoritative entrypoints, so deeper alias shells remain outside the live
  positive extraction and do not create leftover milestone-3 debt inside this
  family.

`rev-026` therefore closes milestone-3 and opens milestone-4 with a concrete
post-round-220 contract:

- start from merged base-branch `HEAD = ea8db76`; do not relitigate the
  accepted `round-211` / `round-212` / `round-213` / `round-214` /
  `round-215` / `round-216` / `round-217` / `round-218` / `round-219` /
  `round-220` merge bookkeeping as live blocker debt;
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
  only, and treat the merged selected same-wrapper nested-`forall` packet
  plus the merged explicit clear-boundary anchors from
  `sameLaneClearBoundaryExpr` through
  `sameLaneNonupleAliasFrameClearBoundaryExpr` as the exact broader-positive
  frontier this family has now earned, not as authorization to reopen
  decuple/deeper alias shells as positive support; and
- require the next round to be docs-only milestone-4 closeout, not another
  milestone-3 corpus-promotion round.

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
  `round-218`, merged as `7a127e2`, accepted `round-219`, merged as
  `7616109`, and accepted `round-220`, merged as `ea8db76`, as successive
  explicit milestone-3 clear-boundary anchors.
- Preserve blocked `round-208`, `round-209`, and `round-210` as immutable
  predecessor evidence only.
- Record `rev-012`, `rev-013`, and `rev-014` honestly as exhausted
  milestone-2 coordination epochs, `rev-015` as the exhausted first
  milestone-3 post-merge publication epoch, `rev-016` as the exhausted
  second milestone-3 post-merge publication epoch, `rev-017` as the
  exhausted third milestone-3 post-merge publication epoch, `rev-018` as the
  exhausted fourth milestone-3 post-merge publication epoch, `rev-019` as
  the exhausted fifth milestone-3 post-merge publication epoch, `rev-020` as
  the exhausted sixth milestone-3 post-merge publication epoch, `rev-021` as
  the exhausted seventh milestone-3 post-merge publication epoch, `rev-024`
  as the exhausted eighth milestone-3 post-merge publication epoch, and
  `rev-025` as the exhausted ninth milestone-3 post-merge publication epoch.
  `rev-022` and `rev-023` remain stale unusable recovery publications outside
  the accepted coordination lineage. `rev-026` supersedes `rev-025` only for
  post-round-220 coordination; it does not rewrite accepted evidence.
- `rev-026` authorizes milestone-4 closeout only. It does not reopen
  milestone-3 widening, deeper alias shells, or substitute positive-support
  claims beyond the merged nonuple frontier.
- No cyclic search, multi-SCC widening, equi-recursive reasoning, fallback
  rescue, or second interface is authorized.
- Backwards compatibility shims remain out of scope.

## Global Sequencing Rules

- `milestone-1` remains complete before any code-bearing enactment round.
- `milestone-2` is complete on merged commit `5b775b2`.
- `milestone-3` is complete on merged commit `ea8db76` and may not be
  reopened as live debt unless a later accepted roadmap revision explicitly
  changes the family boundary or evidence bar.
- `milestone-4` closes the family on top of merged `ea8db76` and may not
  overclaim beyond the merged evidence or reopen the decuple/deeper alias
  frontier as positive support.
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

### 3. [done] Broaden representative broader-positive success on the merged baseline

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
  - preserve accepted `round-212` through `round-220` as the full merged
    milestone-3 lineage for this family;
  - do not collapse the merged selected same-wrapper nested-`forall` packet,
    the merged `sameLaneClearBoundaryExpr` first anchor, the merged
    `sameLaneDoubleAliasFrameClearBoundaryExpr` next anchor, the merged
    `sameLaneTripleAliasFrameClearBoundaryExpr` next anchor after that, the
    merged `sameLaneQuadrupleAliasFrameClearBoundaryExpr` next anchor after
    that, the merged `sameLaneQuintupleAliasFrameClearBoundaryExpr` next
    anchor after that, the merged
    `sameLaneSextupleAliasFrameClearBoundaryExpr` next anchor after that, the
    merged `sameLaneSeptupleAliasFrameClearBoundaryExpr` next anchor after
    that, the merged `sameLaneOctupleAliasFrameClearBoundaryExpr` next anchor
    after that, the merged
    `sameLaneNonupleAliasFrameClearBoundaryExpr` next anchor after that, and
    the preserved `sameLaneAliasFrameClearBoundaryExpr` predecessor lane into
    whole-frontier closure;
  - keep the deeper alias-budget ladder as inherited guard/control evidence
    only, with the accepted decuple fail-closed frontier remaining outside
    live positive support; and
  - no further milestone-3 extraction remains live in `rev-026`; the next
    lawful work is milestone-4 closeout.

Accepted direction lineage:

- Direction id: `direction-3a-expand-the-broader-positive-representative-corpus`
  Status: completed through accepted `round-212` through `round-220`, merged
  as `9bb2229`, `2091c39`, `ed66291`, `1b62ad5`, `21fddba`, `f405079`,
  `7a127e2`, `7616109`, and `ea8db76`.
  Outcome: made `sameLaneClearBoundaryExpr` through
  `sameLaneNonupleAliasFrameClearBoundaryExpr` explicit across the research,
  pipeline, and elaboration surfaces without reopening production code, and
  exhausted the positive same-lane alias budget this family presently
  authorizes.

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

- Extraction id: `promote-same-lane-nonuple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`
  Direction: `direction-3a-expand-the-broader-positive-representative-corpus`
  Status: accepted in `round-220`, merged as `ea8db76`.
  Outcome: made `sameLaneNonupleAliasFrameClearBoundaryExpr` explicit as
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
  earned the claim. On `rev-026`, that precondition is now satisfied by the
  merged `ea8db76` baseline.

Candidate directions:

- Direction id: `direction-4a-publish-broader-positive-enactment-closeout`
  Summary: publish the canonical final artifact and repo-facing record of the
  broader-positive enactment family now that the merged evidence reaches the
  nonuple frontier and stops before the accepted decuple fail-closed edge.
  Why it matters now: the family is not complete until the new controlling
  behavior is recorded honestly for future work.
  Preconditions: `milestone-3`; accepted `round-220`.
  Parallel hints: serial finalization only.
  Boundary notes: closeout may update docs such as `implementation_notes.md`,
  `TODO.md`, `CHANGELOG.md`, and thesis-facing notes when the evidence
  requires, but it must not overclaim beyond the accepted test surface or
  reopen decuple/deeper alias shells as positive support.
  Extraction notes: the next unfinished item is
  `publish-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier`,
  meaning:
  publish one docs-only closeout artifact under `docs/plans/` that names the
  exact broader-positive frontier now earned on both `runPipelineElab` and
  `runPipelineElabChecked` (the merged selected same-wrapper nested-`forall`
  packet plus explicit clear-boundary anchors from
  `sameLaneClearBoundaryExpr` through
  `sameLaneNonupleAliasFrameClearBoundaryExpr`), records
  `sameLaneAliasFrameClearBoundaryExpr` as predecessor truth only, records the
  accepted decuple fail-closed frontier and deeper alias shells as still
  closed outside the live extraction, and updates repo-facing notes only if
  the accepted evidence requires it.
