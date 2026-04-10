# P5 Polymorphism-Nested-Forall Broader-Positive Explicit Boundary-Enactment Roadmap

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-007`
Base branch: `codex/automatic-recursive-type-inference`
Created: 2026-04-08

## Goal

Continue the same code-bearing enactment family after live `round-211`
proved that `rev-006` still stopped one seam too early: the preserved
round-211 annotation-plus-handoff baseline still clears the old Phase 6
authoritative translation stop and the downstream
`PhiReorder: missing binder identity` detour, but the authorized
`Algebra.hs`-only repairs now show that the selected same-wrapper
nested-`forall` packet does not honestly finish inside the rev-006
`ALetF` scheme-selection / closure slice. The clean baseline still stops in
Phase 7 at the authoritative application with `TCArgumentMismatch`; the
minimal same-file `AAppF` recovery reaches the let boundary immediately as
`TCLetTypeMismatch`; collapsing that let scheme monomorphically only shifts
the blocker further downstream into the body consumer as
`TCArgumentMismatch (TVar "t25") ...`; and the bound-relaxation / closure
variant still fails at `TCLetTypeMismatch`.

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

What changes in `rev-007` is only the milestone-2 continuation contract:
live `round-211` remains the preserved same-branch / same-worktree baseline,
including its current diff in
`src/MLF/Elab/Elaborate/Annotation.hs`,
`src/MLF/Elab/Legacy.hs`,
`test/ElaborationSpec.hs`,
`test/PipelineSpec.hs`, and
`test/Research/P5ClearBoundarySpec.hs`.
The newly authorized addition is still exactly one production owner:
`src/MLF/Elab/Elaborate/Algebra.hs`,
but the writable seam is now broader than the rev-006
scheme-selection / `closeTermWithSchemeSubstIfNeeded` / `rhsFinal` slice and
still narrower than a general algebra reopen. It is limited to the exact
post-let consumer seam: the authoritative `ALetF` `bodyElab` / `env'`
handoff plus the immediate downstream body-side `AAppF` inference path, with
`ALamF` parameter recovery admitted only where that consumer still
elaborates against the old non-recursive lane.
`src/MLF/Elab/Run/Pipeline.hs`,
`src/MLF/Elab/TermClosure.hs`,
`src/MLF/Elab/Pipeline.hs`,
`src-public/MLF/Pipeline.hs`,
`src/MLF/Elab/Run/ResultType/Fallback.hs`, and
`src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
remain read-only continuity anchors for this continuation.

## Outcome Boundaries

- Source of truth remains `papers/these-finale-english.txt`. This family may
  revise the controlling broader-positive `P5` behavior only where the
  accepted `round-203` / `round-204` / `round-205` planning ledger now
  authorizes that enactment.
- The family remains code-bearing. `src/`, `src-public/`, `test/`, docs, and
  control-plane artifacts may change when the selected milestone authorizes
  them, and tests/docs must move with behavior changes.
- Success is authoritative only when the enacted broader-positive frontier is
  visible on both `runPipelineElab` and `runPipelineElabChecked`. Internal-
  only success, helper-only success, or packet-history-only success is not
  enough.
- Preserve accepted `round-197` as predecessor truth only:
  `sameLaneAliasFrameClearBoundaryExpr` remains one settled retained-child
  clear-boundary lane, not broad family closure.
- Preserve accepted `round-203` as the controlling revised freeze,
  accepted `round-204` as the broader-positive planning ledger, accepted
  `round-205` as the one exact downstream bind, accepted `round-206` as the
  milestone-1 freeze, and accepted `round-207` as the first internal
  milestone-2 slice. Those artifacts remain immutable predecessor authority
  and must not be rewritten in place.
- Preserve `round-208`, `round-209`, and `round-210` as blocked predecessor
  evidence only.
- Preserve live `round-211` as the same-round continuation baseline, not as
  blocked predecessor evidence and not as a discarded fresh-round draft. Its
  current diff already clears the old Phase 6 `reifyInst` stop and the
  downstream `PhiReorder: missing binder identity` detour for the selected
  packet.
- Preserve the `round-211` implementation-note proof honestly: the clean
  baseline still fails in Phase 7 `TCArgumentMismatch` at the authoritative
  application builder; the minimal same-file `AAppF` recovery reaches the
  immediate authoritative `ALetF` `TCLetTypeMismatch`; the ALetF
  monomorphic-collapse experiment only shifts the blocker into the
  downstream body consumer as `TCArgumentMismatch (TVar "t25") ...`; and the
  bound-relaxation / closure experiment still fails at
  `TCLetTypeMismatch`. `rev-007` treats that proof as the reason for
  widening the admitted seam only to the same-file post-let consumer path in
  `Algebra.hs`, not as accepted implementation success.
- Keep `P2` unopened on this family's live ledger unless a later accepted
  roadmap revision explicitly changes that scope.
- Keep `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
  `N6 termination-pressure` fail-closed unless later accepted evidence under
  this family explicitly reclassifies them.
- No cyclic search, multi-SCC widening, equi-recursive reasoning, fallback
  rescue, or second interface is authorized.
- `rev-007` does not authorize milestone-3 corpus widening, milestone-4
  closeout work, or edits outside the preserved round-211 baseline plus the
  exact `src/MLF/Elab/Elaborate/Algebra.hs` continuation named below.
- Backwards compatibility shims are not a goal. The goal is a clean, honest,
  thesis-faithful enactment of the accepted revised ledger.

## Global Sequencing Rules

- `milestone-1` remains complete before any code-bearing enactment round.
- `milestone-2` may change production semantics only inside the writable slice
  frozen by accepted `milestone-1`, as superseded narrowly by `rev-007`.
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
  evidence only. Continue from the same live `round-211` branch/worktree
  baseline rather than mutating any blocked round's artifacts in place or
  discarding the current round-211 diff on a fresh replacement round.

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
  slice`), finalized the controlling milestone-1 freeze and preserved the
  exact broader-positive frontier, the expected shift away from treating
  polymorphic-mediation `mu` absorption as the controlling broader-positive
  read, the representative corpus, the exact writable slice, and the closed
  guardrails.
- `rev-002` supersession note: blocked `round-208` proved that the selected
  same-wrapper nested-`forall` packet still hit the authoritative
  annotation-translation stop, so the active contract expanded the writable
  slice only enough to include
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`, and
  `test/ElaborationSpec.hs`,
  in addition to the rev-001 slice.
- `rev-003` supersession note: blocked `round-209` proved that
  `Annotation.hs`-only repair can move the packet beyond the original Phase 6
  failure only to expose downstream post-annotation type-check mismatches, so
  the active continuation re-centered on the already-frozen handoff slice in
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/TermClosure.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`.
- `rev-004` supersession note: blocked `round-210` proved that the handoff-
  only continuation was not independently reachable because the selected
  packet still died in Phase 6 before the preserved downstream handoff code
  ran, so the active continuation reopened exactly enough of the annotation
  slice together with the handoff slice.
- `rev-005` supersession note: live `round-211` materially advanced the
  rev-004 combined continuation on its existing branch/worktree by clearing
  the old Phase 6 authoritative translation stop and the downstream
  `PhiReorder: missing binder identity` detour. The active contract therefore
  preserved the current round-211 diff in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`
  as the live same-round baseline, and admitted
  `src/MLF/Elab/Elaborate/Algebra.hs`
  limited to the `AAppF` path and the exact helper logic around authoritative
  argument instantiation / `mu` coercion.
- `rev-006` supersession note: the round-211 `AAppF` experiment did not land,
  but it proved the remaining contradiction now sits later in
  `src/MLF/Elab/Elaborate/Algebra.hs`
  at the authoritative `ALetF` let-scheme finalization / closure boundary.
  The active contract therefore preserves the same live round-211 baseline
  and keeps the one newly authored production owner in `Algebra.hs`, but
  broadens that admitted seam from `AAppF`-only to the selected `ALetF`
  let-scheme finalization / closure logic around scheme selection,
  `closeTermWithSchemeSubstIfNeeded`, and `rhsFinal`, with same-file
  `AAppF` context allowed only if needed to keep the let-scheme repair
  honest.
- `rev-007` supersession note: the live round-211 rev-006 probes exhausted
  that selected `ALetF` let-scheme finalization / closure slice. The clean
  baseline still stops at Phase 7 `TCArgumentMismatch`; the minimal same-file
  `AAppF` recovery reaches immediate `TCLetTypeMismatch`; the ALetF
  monomorphic-collapse experiment only shifts the blocker into the
  downstream body consumer as `TCArgumentMismatch (TVar "t25") ...`; and the
  bound-relaxation / closure experiment still fails at `TCLetTypeMismatch`.
  The active contract therefore preserves the same live round-211 baseline
  and keeps the one newly authored production owner in `Algebra.hs`, but
  widens the admitted same-file seam only to the exact post-let consumer
  path: `ALetF` `bodyElab` / `env'` handoff plus the immediate downstream
  body-side `AAppF` inference path, with `ALamF` parameter recovery admitted
  only where that consumer still elaborates against the old non-recursive
  lane.
- Parallel lane: `lane-main`
- Coordination notes: consume accepted `round-203`, `round-204`, `round-205`,
  and `round-206` directly; preserve the retained-child lane as predecessor
  truth only; keep `P2` and the representative negative-family rows closed;
  and do not start production edits before the contract freeze is accepted.

Accepted direction lineage:

- Direction id: `direction-1a-freeze-broader-positive-enactment-contract`
  Status: accepted in `round-206`, merged as `12fd9dc`.
  Outcome: published the canonical milestone-1 enactment-contract freeze and
  routed the family forward only to `milestone-2` /
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
- Coordination notes: consume accepted `round-206` directly, consume accepted
  `round-207` as the first bounded internal mechanism slice, consume blocked
  `round-208`, `round-209`, and `round-210` as predecessor evidence, and
  consume live `round-211` as the continuing same-round baseline. Stay inside
  the exact `rev-007` continuation, keep the family within the accepted
  explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph /
  no-fallback boundary, and do not overclaim full broader-positive closure
  before the broader representative frontier has passed on both authoritative
  entrypoints.
- Progress notes: accepted `round-207`, merged as base commit `fb85bd4`
  (`Align same-wrapper nested-forall fallback target and scope`), completed
  the first serial `direction-2a` extraction on the internal mechanism slice.
  That accepted bounded code-bearing slice stayed inside fallback core and the
  focused pipeline/research tests, aligned the selected same-wrapper
  nested-`forall` fallback target with the matching generalization scope so
  the internal mechanism now preserves recursive structure for that packet,
  and left the authoritative pipeline/public seams untouched.
- Blocked predecessor notes: blocked `round-208` proved the remaining seam sat
  in the authoritative annotation path. Blocked `round-209` proved that
  annotation-only repair was insufficient because the packet next failed after
  annotation. Blocked `round-210` proved that the handoff-only continuation
  was not independently reachable because the packet still died before the
  preserved downstream handoff code ran.
- Live continuation notes: live `round-211` remains on its existing
  branch/worktree only. Its current diff in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`
  materially advanced the same selected packet: the old Phase 6
  `reifyInst` stop is gone and the post-annotation path now clears the
  earlier `PhiReorder: missing binder identity` detour. The clean baseline
  still fails later in Phase 7 with `TCArgumentMismatch` at the authoritative
  application term. The minimal same-file `AAppF` recovery reaches immediate
  `TCLetTypeMismatch` at the let boundary; the ALetF monomorphic-collapse
  probe only shifts the blocker to downstream body-side
  `TCArgumentMismatch (TVar "t25") ...`; and the bound-relaxation /
  closure probe still returns `TCLetTypeMismatch`. The next exact move is
  therefore not a fresh round and not a reopen of pipeline/handoff/public/
  fallback seams; it is a same-round continuation under
  `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  that preserves the current round-211 diff and admits only the selected
  `Algebra.hs` post-let consumer seam named below.

Candidate directions:

- Direction id: `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`
  Summary: update the core elaboration / inference / recursive-structure
  machinery so broader-positive quantified-crossing cases preserve recursive
  structure instead of being controlled by polymorphic-mediation `mu`
  absorption.
  Why it matters now: accepted `round-206` froze the exact frontier,
  representative corpus, authoritative entrypoints, and writable slice. The
  first lawful move was the code-bearing round that changed only the core
  recursive-structure machinery needed to stop the old absorption-driven
  read.
  Preconditions: `milestone-1`.
  Parallel hints: serial extraction only.
  Boundary notes: no fallback rescue, no second interface, no equi-recursive
  or cyclic widening, and no reopening of `P2` or the representative
  negative families.
  Extraction notes: completed by accepted `round-207`; preserve that internal
  mechanism slice as predecessor truth only while milestone-2 continues.

- Direction id: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Summary: carry the milestone-2 mechanism through the authoritative pipeline
  and post-let consumer so the selected behavior is visible where the family
  measures success, continuing the same round-211 packet from the preserved
  annotation-plus-handoff baseline into the exact same-file post-let
  consumer seam that still blocks honest completion.
  Why it matters now: accepted `round-207` completed the first serial
  `direction-2a` internal mechanism slice and proved the selected same-
  wrapper nested-`forall` packet can preserve recursive structure inside
  fallback without widening into pipeline/public seams; blocked `round-208`
  showed that pipeline-only threading was insufficient because the selected
  packet still failed in Phase 6 at the authoritative annotation translation
  path; blocked `round-209` showed that annotation-only repair was still
  insufficient because the packet next failed after annotation when the
  authoritative path closed or preserved the elaborated term and re-typed it;
  blocked `round-210` showed that handoff-only continuation was not
  independently reachable; and live `round-211` now clears those earlier
  stops on the same branch/worktree. The remaining honest proof is that the
  rev-006 `ALetF` scheme-selection / closure slice is exhausted:
  `AAppF`-only repair exposes the immediate `TCLetTypeMismatch`, the
  monomorphic let collapse only shifts the blocker to downstream body-side
  `TCArgumentMismatch (TVar "t25") ...`, and the bound-relaxation path still
  stops at `TCLetTypeMismatch`. The next move is therefore to preserve the
  current diff and repair only that admitted same-file post-let consumer
  seam.
  Preconditions: `milestone-1`; accepted progress from `direction-2a` via
  `round-207`; blocked predecessor evidence from `round-208`, `round-209`,
  and `round-210`; live round-211 baseline; and the round-211 implementation
  proof that the rev-006 let-scheme / closure slice is exhausted and the
  remaining honest blocker now sits in the immediate post-let consumer path.
  Parallel hints: keep serial unless a later accepted revision proves an
  independent split is safe.
  Boundary notes: do not drift into milestone-3 corpus work, milestone-4
  closeout, or broad architectural reopen here; this direction's current
  continuation is one exact same-wrapper nested-`forall` packet, the
  preserved current round-211 diff in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`,
  plus one additional same-file seam in
  `src/MLF/Elab/Elaborate/Algebra.hs`
  limited to the authoritative `ALetF` `bodyElab` / `env'` handoff plus the
  immediate downstream body-side `AAppF` inference path, with `ALamF`
  parameter recovery admitted only where that consumer still elaborates
  against the old non-recursive lane.
  Keep
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as read-only continuity and predecessor anchors.
  Extraction notes: the exact same-round continuation task is
  `continue-round-211-selected-same-wrapper-nested-forall-through-post-let-body-consumer-seam`.
  Current task/title:
  `Continue the selected same-wrapper nested-forall packet through the authoritative post-let body consumer seam while preserving the live round-211 annotation-plus-handoff baseline`.
  Preserve the existing round-211 diff in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`.
  Add only `src/MLF/Elab/Elaborate/Algebra.hs` as newly authored production
  logic, and touch the same focused tests only if the post-let consumer
  repair truly needs companion expectation updates. Do not discard the
  current round-211 branch/worktree, do not reopen blocked round artifacts,
  and stop again if another seam outside this tighter continuation is
  required.

Accepted extraction lineage:

- Extraction id: `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`
  Direction: `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`
  Status: accepted in `round-207`, merged as `fb85bd4`.
  Outcome: repaired the first bounded same-wrapper nested-`forall` internal
  mechanism slice by carrying the selected recursive target together with the
  matching generalization scope inside fallback core, proving recursive
  preservation on the selected internal packet and focused tests while
  keeping `Fallback.hs`, `TermClosure.hs`, and the authoritative
  pipeline/public seams untouched and still fail-closed. This extraction is
  complete.

Blocked predecessor lineage:

- Evidence id: `round-208-blocked-authoritative-annotation-translation-seam`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: blocked predecessor evidence only; not accepted, not merged.
  Outcome: proved that the selected same-wrapper nested-`forall` packet still
  failed on both authoritative entrypoints with the same Phase 6
  `PhiTranslatabilityError`, so the next exact seam was the authoritative
  annotation translation path.

- Evidence id: `round-209-blocked-post-annotation-authoritative-result-handoff-seam`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: blocked predecessor evidence only; not accepted, not merged.
  Outcome: proved that `Annotation.hs`-only repair can move the selected
  packet beyond the original Phase 6 translation failure only to expose
  downstream post-annotation type-check mismatches, preserving the
  authoritative-result handoff as the next still-live seam.

- Evidence id: `round-210-blocked-handoff-only-continuation-not-independently-reachable`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: blocked predecessor evidence only; not accepted, not merged.
  Outcome: proved that the selected packet still failed in Phase 6 before
  `Run/Pipeline` reached `termSubst`, `termClosed0`, `termClosed`, or the
  final authoritative `typeCheck`, so the next exact move was the combined
  same-round continuation later picked up by `round-211`.

Live continuation lineage:

- Continuation id: `round-211-live-post-let-body-consumer-seam-blocker`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: live same-round continuation baseline; not accepted, not merged.
  Outcome: preserved the current round-211 diff in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`,
  which clears the old Phase 6 authoritative translation stop and the
  downstream `PhiReorder: missing binder identity` detour for the selected
  packet. The clean baseline still fails with Phase 7
  `TCArgumentMismatch`; the minimal same-file `AAppF` recovery reaches
  immediate `TCLetTypeMismatch`; the ALetF monomorphic-collapse experiment
  only shifts the blocker to downstream body-side
  `TCArgumentMismatch (TVar "t25") ...`; and the bound-relaxation / closure
  experiment still fails at `TCLetTypeMismatch`. The next exact move is to
  continue on the same round-211 branch/worktree, preserve that current
  diff, and add only the selected `Algebra.hs` post-let consumer seam:
  `ALetF` `bodyElab` / `env'` handoff plus the immediate downstream
  body-side `AAppF` inference path, with `ALamF` parameter recovery admitted
  only where that consumer still elaborates against the old non-recursive
  lane.

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
  keep the representative retained-child lane as predecessor evidence only.

Candidate directions:

- Direction id: `direction-3a-expand-the-broader-positive-representative-corpus`
  Summary: widen from the initial repaired slices to the representative
  broader-positive frontier frozen by milestone-1 and prove that those cases
  now succeed honestly on both authoritative entrypoints.
  Why it matters now: the family goal is not one repaired packet; it is the
  broader positive frontier beyond the settled retained-child lane.
  Preconditions: `milestone-2`.
  Parallel hints: serial extraction only.
  Boundary notes: each extracted round must keep the corpus widening honest,
  bounded, and reviewable; no reopening of negative-family cases as
  positives.
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
  Boundary notes: parity fixes must stay inside the milestone-1 writable
  slice and must not invent a second interface or compatibility shim.
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
  documents preserved closed boundaries honestly, and updates repo-facing
  notes needed to reflect the new state.
- Parallel lane: `lane-main`
- Coordination notes: do not publish closeout until milestone-3 has actually
  earned the claim. Closeout must reflect the enacted code and tests rather
  than restating the planning-family handoff.

Candidate directions:

- Direction id: `direction-4a-publish-broader-positive-enactment-closeout`
  Summary: publish the canonical final artifact and repo-facing record of the
  broader-positive enactment family once the code-bearing evidence is
  complete.
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
