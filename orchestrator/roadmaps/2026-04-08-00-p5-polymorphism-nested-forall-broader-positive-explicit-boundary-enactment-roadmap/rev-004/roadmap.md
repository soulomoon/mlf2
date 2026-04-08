# P5 Polymorphism-Nested-Forall Broader-Positive Explicit Boundary-Enactment Roadmap

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-004`
Base branch: `codex/automatic-recursive-type-inference`
Created: 2026-04-08

## Goal

Continue the same code-bearing enactment family after blocked `round-210`
proved that `rev-003` stopped one seam too early in the opposite direction:
the handoff-only continuation is not independently reachable because the
selected same-wrapper nested-`forall` packet still dies in Phase 6 before
`Run/Pipeline` reaches the post-annotation authoritative-result handoff.

This revision keeps the family goal unchanged:

- preserve recursive structure across the broader positive quantified-
  crossing frontier where the accepted revised ledger now requires that read,
- make the representative broader positive frontier pass on both
  `runPipelineElab` and `runPipelineElabChecked`,
- preserve the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback / no-second-interface
  guardrails unless later accepted evidence explicitly revises them, and
- record the new controlling behavior honestly once the code and evidence
  actually earn it.

What changes in `rev-004` is only the milestone-2 continuation contract:
blocked `round-208` exhausted the pipeline-only continuation, blocked
`round-209` exhausted the annotation-only continuation, and blocked
`round-210` exhausted the handoff-only continuation by proving that the same
Phase 6 authoritative annotation failure still fires before any
`Run/Pipeline` handoff logic executes on a fresh branch. The next fresh
follow-on round is therefore authorized exactly across both still-live
boundaries for the same selected packet: reopen the authoritative annotation
translation seam in `src/MLF/Elab/Elaborate/Annotation.hs` with exact
companion room in `src/MLF/Elab/Legacy.hs`, and keep the downstream
post-annotation authoritative-result handoff centered on
`src/MLF/Elab/Run/Pipeline.hs` with exact companion room in
`src/MLF/Elab/TermClosure.hs`, plus only the exact focused test surfaces
`test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, and
`test/Research/P5ClearBoundarySpec.hs`.

## Outcome Boundaries

- Source of truth remains `papers/these-finale-english.txt`. This family may
  revise the controlling broader-positive `P5` behavior only where the
  accepted `round-203` / `round-204` / `round-205` planning ledger now
  authorizes that enactment.
- The family remains code-bearing. `src/`, `src-public/`, `test/`, docs, and
  control-plane artifacts may change when the selected milestone authorizes
  them, and tests/docs must move with behavior changes.
- Success is authoritative only when the enacted broader-positive frontier is
  visible on both `runPipelineElab` and `runPipelineElabChecked`. Internal-only
  success, helper-only success, or packet-history-only success is not enough.
- Preserve accepted `round-197` as predecessor truth only:
  `sameLaneAliasFrameClearBoundaryExpr` remains one settled retained-child
  clear-boundary lane, not broad family closure.
- Preserve accepted `round-203` as the controlling revised freeze,
  accepted `round-204` as the broader-positive planning ledger, accepted
  `round-205` as the one exact downstream bind, accepted `round-206` as the
  milestone-1 freeze, and accepted `round-207` as the first internal
  milestone-2 slice. Those artifacts remain immutable predecessor authority
  and must not be rewritten in place.
- Preserve `round-208` as blocked predecessor evidence only. It proves the
  remaining selected seam sits in the authoritative annotation translation
  path, but it did not accept or merge a milestone-2 code-bearing result.
- Preserve `round-209` as blocked predecessor evidence only. It proves the
  annotation seam can be reopened far enough to move the selected packet
  beyond the original Phase 6 `reifyInst` failure, but not far enough to
  settle the downstream post-annotation authoritative-result handoff.
- Preserve `round-210` as blocked predecessor evidence only. It proves the
  handoff-only continuation is not independently reachable because the
  selected packet still dies in Phase 6 before
  `Run/Pipeline.termSubst` / `termClosed0` / `termClosed` /
  `checkedAuthoritative` can run on the fresh branch.
- Keep `P2` unopened on this family's live ledger unless a later accepted
  roadmap revision explicitly changes that scope.
- Keep `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
  `N6 termination-pressure` fail-closed unless later accepted evidence under
  this family explicitly reclassifies them.
- No cyclic search, multi-SCC widening, equi-recursive reasoning, fallback
  rescue, or second interface is authorized.
- `rev-004` does not authorize milestone-3 corpus widening, milestone-4
  closeout work, or edits outside the exact combined annotation-plus-handoff
  continuation named below.
- Backwards compatibility shims are not a goal. The goal is a clean, honest,
  thesis-faithful enactment of the accepted revised ledger.

## Global Sequencing Rules

- `milestone-1` remains complete before any code-bearing enactment round.
- `milestone-2` may change production semantics only inside the writable slice
  frozen by accepted `milestone-1`, as superseded narrowly by `rev-004`.
- `milestone-3` may broaden representative success only after the core
  milestone-2 mechanism is accepted. Do not claim full broader-positive
  support before the representative frontier passes on both authoritative
  entrypoints.
- `milestone-4` closes the family only after the code-bearing evidence is in
  place and the new controlling behavior can be recorded honestly.
- Keep predecessor roadmap families and revisions immutable. Any later change
  to this family's coordination or semantic contract must publish a new
  revision under the same `roadmap_id`.
- This family remains intentionally serial unless a later accepted revision
  explicitly authorizes safe parallel extraction.
- Treat blocked `round-208`, `round-209`, and `round-210` as predecessor
  evidence only. Continue from a fresh follow-on round rather than mutating
  any blocked round's artifacts in place.

## Parallel Lanes

- `lane-main`: default serial lane for the full enactment family. The broader
  positive frontier should remain single-threaded until the enacted semantics
  and evidence surface are stable enough for lawful concurrency.

## Milestones

### 1. [done] Freeze the enactment contract, authoritative frontier, writable slice, and representative corpus

- Milestone id: `milestone-1`
- Depends on: none
- Intent: publish the authoritative code-bearing contract that converts the
  accepted `round-205` handoff into an exact enactment family scope by naming
  the broader positive frontier, the expected recursive-structure-preserving
  behavior shift, the authoritative success surfaces, the representative
  positive and negative corpus obligations, and the exact writable slice for
  later code-bearing rounds.
- Completion signal: an accepted artifact names the exact broader positive
  `P5 polymorphism-nested-forall` frontier beyond the one settled retained-
  child lane, the exact authoritative success bar on
  `runPipelineElab` and `runPipelineElabChecked`, the exact preserved closed
  families, and the exact writable production/test/docs surfaces for later
  enactment rounds.
- Completion notes: accepted `round-206`, merged as base commit `12fd9dc`
  (`Freeze P5 broader-positive enactment contract, corpus, and writable
  slice`), finalized
  `direction-1a-freeze-broader-positive-enactment-contract` through
  `docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`
  with authoritative review in
  `orchestrator/rounds/round-206/review-record.json`. That accepted
  docs/control-plane-only freeze consumes accepted `round-205` as the binding
  family-entry authority, keeps accepted `round-204` and `round-203` as the
  controlling broader-positive ledger beneath it, freezes the exact broader-
  positive frontier beyond the one settled retained-child lane, freezes the
  expected shift away from treating polymorphic-mediation `mu` absorption as
  the controlling broader-positive read, freezes the representative corpus and
  exact writable slice, and preserves `P2`, `N1 ambiguity-reject`,
  `N2 unsoundness-guard`, and `N6 termination-pressure` as closed predecessor
  truth or guardrails.
- `rev-002` supersession note: blocked `round-208` proved that the selected
  same-wrapper nested-`forall` packet still hits
  `Phase 6 (elaboration): PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation for edge 2", ...]`
  after the rev-001-authorized pipeline/fallback/term-closure experiments.
  The active contract therefore expands the frozen writable slice only enough
  to include:
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`, and
  `test/ElaborationSpec.hs`,
  in addition to the rev-001 slice. No other part of the milestone-1 freeze
  changes.
- `rev-003` supersession note: blocked `round-209` re-applied the fresh
  authoritative expectations on a new branch and exhausted that exact
  annotation-seam continuation. `Annotation.hs`-only repairs could move the
  packet past the original Phase 6 translation error, but only into
  downstream post-annotation type-check mismatches after
  `runPipelineElabWith` closes or preserves the elaborated term and re-runs
  the authoritative `typeCheck`. The active continuation therefore recenters
  the next serial extraction on the already-frozen milestone-1 handoff slice:
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/TermClosure.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`.
  No other part of the milestone-1 freeze changes, and
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`, and
  `test/ElaborationSpec.hs`
  return to predecessor-evidence status for that revision.
- `rev-004` supersession note: blocked `round-210` re-applied the fresh
  authoritative expectations on a handoff-only branch and proved that the
  selected packet still fails in Phase 6 before `Run/Pipeline` reaches
  `termSubst`,
  `termClosed0`,
  `termClosed`,
  or the final authoritative `typeCheck`. The active contract therefore
  reopens exactly enough of the rev-002 annotation slice together with the
  rev-003 handoff slice for the same selected packet:
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/TermClosure.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`.
  No other part of the milestone-1 freeze changes, and
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  remain read-only continuity anchors for this revision.
- Parallel lane: `lane-main`
- Coordination notes: consume accepted `round-203`, `round-204`, `round-205`,
  and `round-206` directly; preserve the retained-child lane as predecessor
  truth only; keep `P2` and the representative negative-family rows closed;
  and do not start production edits before the contract freeze is accepted.

Accepted direction lineage:

- Direction id: `direction-1a-freeze-broader-positive-enactment-contract`
  Status: accepted in `round-206`, merged as `12fd9dc`.
  Outcome: published the canonical milestone-1 enactment-contract freeze,
  preserved predecessor history unchanged, froze the exact broader-positive
  frontier, expected behavior shift, authoritative entrypoints,
  representative corpus, and writable slice, and routed the family forward
  only to `milestone-2` /
  `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`.

### 2. [pending] Implement the broader-positive recursive-structure-preservation mechanism

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: land the core production semantics that stop treating
  polymorphic-mediation `mu` absorption as the controlling broader-positive
  read and instead preserve recursive structure across the milestone-1
  frontier within the accepted explicit-only / iso-recursive boundary.
- Completion signal: accepted code-bearing rounds land the core mechanism
  inside the milestone-1 writable slice, representative broader-positive
  quantified-crossing cases no longer fail for the old controlling
  absorption-driven reason, focused tests capture the new behavior, and the
  retained-child lane plus preserved negative guardrails remain honest.
- Parallel lane: `lane-main`
- Coordination notes: consume accepted `round-206` and its milestone-1
  contract freeze directly, consume accepted `round-207` as the first bounded
  internal mechanism slice, consume blocked `round-208` as predecessor
  evidence that the annotation seam had to be opened, consume blocked
  `round-209` as predecessor evidence that annotation-only continuation is
  insufficient because the downstream handoff remains live, consume blocked
  `round-210` as predecessor evidence that the rev-003 handoff-only
  continuation is not independently reachable because Phase 6 still blocks
  before `Run/Pipeline` reaches the selected handoff code, stay inside the
  exact `rev-004` combined continuation slice, keep the family within the
  accepted explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary, and do not overclaim full
  broader-positive closure before the broader representative frontier has
  passed on both authoritative entrypoints.
- Progress notes: accepted `round-207`, merged as base commit `fb85bd4`
  (`Align same-wrapper nested-forall fallback target and scope`), completed
  the first serial `direction-2a` extraction
  `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`
  with authoritative review in
  `orchestrator/rounds/round-207/review-record.json`. That accepted bounded
  code-bearing slice stayed inside
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`, aligned the selected same-wrapper
  nested-`forall` fallback target with the matching generalization scope so
  the internal mechanism now preserves recursive structure for that packet,
  kept the settled retained-child lane and preserved negative/control
  guardrails honest, passed focused checks plus `cabal build all && cabal test`,
  and left `src/MLF/Elab/Run/ResultType/Fallback.hs`,
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`, and
  `src-public/MLF/Pipeline.hs` untouched.
- Blocked predecessor notes: blocked `round-208` preserved fresh evidence in
  its own worktree only. It strengthened the selected authoritative
  expectations in `test/Research/P5ClearBoundarySpec.hs` and
  `test/PipelineSpec.hs`, then proved that both `runPipelineElab` and
  `runPipelineElabChecked` still fail on the selected packet with
  `Phase 6 (elaboration): PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation for edge 2","expansion args=[NodeId {getNodeId = 48}]]`.
  That proved the remaining seam sat in
  `src/MLF/Elab/Elaborate/Annotation.hs`, outside the old `rev-001` writable
  slice. Blocked `round-209` then re-applied those authoritative
  expectations on a fresh branch, explored `Annotation.hs`-only repairs, and
  reverted them after they merely moved the packet into downstream Phase 7
  type-check mismatches. Blocked `round-210` then re-applied the handoff-only
  continuation on another fresh branch and proved that the selected packet
  still dies in the same Phase 6 `reifyInst` path before
  `Run/Pipeline.termSubst`,
  `termClosed0`,
  `termClosed`, or
  `checkedAuthoritative` can run. The combined evidence proves that the next
  exact move is a fresh follow-on round for
  `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  with the extracted task
  `repair-same-wrapper-nested-forall-across-authoritative-annotation-and-post-annotation-handoff-seams`.

Candidate directions:

- Direction id: `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`
  Summary: update the core elaboration / inference / recursive-structure
  machinery so broader-positive quantified-crossing cases preserve recursive
  structure instead of being controlled by polymorphic-mediation `mu`
  absorption.
  Why it matters now: accepted `round-206` froze the exact frontier,
  representative corpus, authoritative entrypoints, and writable slice. The
  first lawful move was the code-bearing round that changed only the core
  recursive-structure machinery needed to stop the old absorption-driven read.
  Preconditions: `milestone-1`.
  Parallel hints: serial extraction only.
  Boundary notes: no fallback rescue, no second interface, no equi-recursive
  or cyclic widening, and no reopening of `P2` or the representative negative
  families.
  Extraction notes: completed by accepted `round-207`; preserve that internal
  mechanism slice as predecessor truth only while milestone-2 continues.

- Direction id: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Summary: carry the milestone-2 mechanism through the authoritative pipeline
  and public/internal entrypoints so the selected behavior is visible where
  the family measures success, beginning with the exact same-packet combined
  seam across authoritative annotation translation and the downstream
  post-annotation authoritative-result handoff now known to remain jointly
  live.
  Why it matters now: accepted `round-207` completed the first serial
  `direction-2a` internal mechanism slice and proved the selected same-wrapper
  nested-`forall` packet can preserve recursive structure inside fallback
  without widening into pipeline/public seams, but blocked `round-208`
  showed that pipeline-only threading is insufficient because the selected
  packet still fails in Phase 6 at the authoritative annotation translation
  path, blocked `round-209` showed that annotation-only repair is still
  insufficient because the packet next fails after annotation when the
  authoritative path closes or preserves the elaborated term and re-typechecks
  it, and blocked `round-210` showed that handoff-only continuation is not
  independently reachable because the packet still dies in the same Phase 6
  path before `Run/Pipeline` reaches that downstream handoff.
  Preconditions: `milestone-1`; accepted progress from
  `direction-2a` via `round-207`; blocked predecessor evidence from
  `round-208`, `round-209`, and `round-210`.
  Parallel hints: keep serial unless a later accepted revision proves an
  independent split is safe.
  Boundary notes: do not drift into milestone-3 corpus work, milestone-4
  closeout, or broad architectural reopen here; this direction is for one
  exact same-wrapper nested-`forall` packet, the authoritative annotation
  translation seam centered on `src/MLF/Elab/Elaborate/Annotation.hs` with
  exact companion room in `src/MLF/Elab/Legacy.hs`, the downstream
  post-annotation authoritative-result handoff centered on
  `src/MLF/Elab/Run/Pipeline.hs` with exact companion room in
  `src/MLF/Elab/TermClosure.hs`, and narrowly justified focused tests only.
  Extraction notes: the first fresh round after `rev-004` should own
  `test/Research/P5ClearBoundarySpec.hs`,
  `test/PipelineSpec.hs`,
  `test/ElaborationSpec.hs`,
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`, and
  only the exact companion surface the downstream handoff requires:
  `src/MLF/Elab/TermClosure.hs`.
  Keep
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as read-only continuity and predecessor anchors. Reapply any needed
  authoritative-expectation changes on a fresh branch rather than mutating
  `round-208`, `round-209`, or `round-210` artifacts, and stop again if
  another seam outside this bounded combined slice is required.

Accepted extraction lineage:

- Extraction id: `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`
  Direction: `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`
  Status: accepted in `round-207`, merged as `fb85bd4`.
  Outcome: repaired the first bounded same-wrapper nested-`forall` internal
  mechanism slice by carrying the selected recursive target together with the
  matching generalization scope inside fallback core, proving recursive
  preservation on the selected internal packet and focused tests while keeping
  `Fallback.hs`, `TermClosure.hs`, and the authoritative pipeline/public
  seams untouched and still fail-closed. This extraction is complete.

Blocked predecessor lineage:

- Evidence id: `round-208-blocked-authoritative-annotation-translation-seam`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: blocked predecessor evidence only; not accepted, not merged.
  Outcome: proved that the selected same-wrapper nested-`forall` packet still
  fails on both authoritative entrypoints with the same Phase 6
  `PhiTranslatabilityError`, and that `Run/Pipeline`-only experiments do not
  clear the blocker. The next exact seam is therefore the authoritative
  annotation translation path in `src/MLF/Elab/Elaborate/Annotation.hs`,
  which `rev-002` admitted explicitly and `round-209` then exhausted.

- Evidence id: `round-209-blocked-post-annotation-authoritative-result-handoff-seam`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: blocked predecessor evidence only; not accepted, not merged.
  Outcome: re-applied the fresh authoritative expectations on a clean follow-on
  branch, proved that `Annotation.hs`-only repair attempts can move the
  selected packet beyond the original Phase 6 translation failure, and then
  reverted those edits after they only exposed downstream post-annotation
  type-check mismatches. That preserved the downstream authoritative-result
  handoff as the next still-live seam for the same selected packet, which
  `rev-003` then isolated.

- Evidence id: `round-210-blocked-handoff-only-continuation-not-independently-reachable`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: blocked predecessor evidence only; not accepted, not merged.
  Outcome: re-applied the fresh authoritative expectations on a handoff-only
  branch and proved that the selected packet still fails in Phase 6 before
  `Run/Pipeline` reaches `termSubst`, `termClosed0`, `termClosed`, or the
  final authoritative `typeCheck`. The next exact move is therefore not a
  broader reopen; it is the same selected packet across both still-live
  boundaries at once, which `rev-004` now admits explicitly.

### 3. [pending] Prove broader-positive frontier success on both authoritative entrypoints

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: broaden evidence from the milestone-2 core mechanism to the full
  broader positive `P5 polymorphism-nested-forall` frontier named by
  milestone-1 until representative success is honest on both
  `runPipelineElab` and `runPipelineElabChecked`.
- Completion signal: accepted rounds expand the representative broader-
  positive frontier corpus, the enacted frontier passes on both authoritative
  entrypoints, focused regression coverage records that success honestly, and
  preserved `P2` / `N1` / `N2` / `N6` guardrails remain fail-closed.
- Parallel lane: `lane-main`
- Coordination notes: treat milestone-2 as the core mechanism prerequisite;
  do not collapse one or two repaired packets into whole-frontier closure; and
  keep the representative retained-child lane as predecessor evidence only,
  not as the measure of broader-positive completion.

Candidate directions:

- Direction id: `direction-3a-expand-the-broader-positive-representative-corpus`
  Summary: widen from the initial repaired slices to the representative
  broader-positive frontier frozen by milestone-1 and prove that those cases
  now succeed honestly on both authoritative entrypoints.
  Why it matters now: the family goal is not one repaired packet. It is the
  broader positive frontier beyond the settled retained-child lane.
  Preconditions: `milestone-2`.
  Parallel hints: serial extraction only.
  Boundary notes: each extracted round must keep the corpus widening honest,
  bounded, and reviewable; no reopening of negative-family cases as positives.
  Extraction notes: representative cases should stay grounded in
  `test/Research/P5ClearBoundarySpec.hs`,
  `test/PipelineSpec.hs`, and any milestone-1-authorized companion specs.

- Direction id: `direction-3b-tighten-public-internal-parity-and-regression-guards`
  Summary: close any remaining parity gaps or regression holes so the broader
  positive frontier is stably supported on both authoritative entrypoints
  without reopening `P2` or the representative negative families.
  Why it matters now: milestone-3 must end with honest, reviewable support on
  both authoritative entrypoints, not a fragile internal-only success.
  Preconditions: `milestone-2`; likely depends on accepted progress from
  `direction-3a`.
  Parallel hints: serial unless a later accepted revision proves separation.
  Boundary notes: parity fixes must stay inside the milestone-1 writable slice
  and must not invent a second interface or compatibility shim.
  Extraction notes: regression guards should prove the selected support while
  preserving fail-closed behavior where the family says the boundary stays
  closed.

### 4. [pending] Publish closeout and record the new controlling broader-positive behavior

- Milestone id: `milestone-4`
- Depends on: `milestone-3`
- Intent: close the enactment family by publishing the authoritative closeout
  record that states the enacted broader-positive behavior, records the
  evidence surface, and explains which guardrails and excluded families remain
  closed.
- Completion signal: an accepted closeout artifact records that the broader
  positive frontier now succeeds on both `runPipelineElab` and
  `runPipelineElabChecked`, states that polymorphic-mediation `mu` absorption
  is no longer the controlling broader-positive read for this frontier,
  documents preserved closed boundaries honestly, and updates repo-facing notes
  needed to reflect the new state.
- Parallel lane: `lane-main`
- Coordination notes: do not publish closeout until milestone-3 has actually
  earned the claim. Closeout must reflect the enacted code and tests rather
  than restating the planning-family handoff.

Candidate directions:

- Direction id: `direction-4a-publish-broader-positive-enactment-closeout`
  Summary: publish the canonical final artifact and repo-facing record of the
  broader-positive enactment family once the code-bearing evidence is complete.
  Why it matters now: the family is not complete until the new controlling
  behavior is recorded honestly for future work.
  Preconditions: `milestone-3`.
  Parallel hints: serial finalization only.
  Boundary notes: closeout may update docs such as `implementation_notes.md`,
  `TODO.md`, `CHANGELOG.md`, and thesis-facing notes when the evidence
  requires, but it must not overclaim beyond the accepted test surface.
  Extraction notes: the final artifact must name the exact broader-positive
  frontier that now succeeds, the preserved closed guardrails, and any thesis
  deviation or non-deviation claim the enacted family actually earned.
