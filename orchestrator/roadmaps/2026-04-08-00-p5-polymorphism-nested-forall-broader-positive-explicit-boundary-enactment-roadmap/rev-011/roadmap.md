# P5 Polymorphism-Nested-Forall Broader-Positive Explicit Boundary-Enactment Roadmap

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-011`
Base branch: `codex/automatic-recursive-type-inference`
Created: 2026-04-08

## Goal

Continue the same code-bearing enactment family after live `round-211`
proved that `rev-010` also stopped one seam too early.

The same live `round-211` branch/worktree remains the authoritative
continuation baseline, and `rev-011` preserves its inherited truth exactly:

- preserve the existing round-owned diff in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`;
- preserve the already-cleared selected same-wrapper nested-`forall` packet on
  both authoritative entrypoints, checked-authoritative representative parity,
  the classic let-polymorphism / explicit-`forall` positives,
  `BUG-2026-02-06-002`, and the retained-child exact packet;
- preserve the same live `round-211` branch/worktree rather than restarting
  on a fresh round; and
- keep the family inside the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback / no-second-interface
  boundary unless later accepted evidence explicitly changes it.

`rev-010` is now exhausted after the bounded post-closure `ALetF`
continuation. The authoritative review and implementation notes preserve four
controlling facts:

- the kept diff already had to move outside the admitted `ALetF`
  post-closure locals in `src/MLF/Elab/Elaborate/Algebra.hs`;
- that broader live diff still leaves the thesis gate red on the A6
  dual-coercion `TCExpectedArrow` cluster;
- the full gate improved to `1341` examples / `12` failures, but the
  surviving red rows are now concentrated in downstream application-consumer
  recovery and one remaining authoritative-instantiation companion seam; and
- `src/MLF/Elab/TermClosure.hs`, pipeline/public surfaces, and fallback
  surfaces remain untouched, so the honest next move is downstream of the
  exhausted `ALetF` handoff slice rather than another closure retry.

The newly admitted seam in `rev-011` is therefore one step downstream of the
exhausted `rev-010` post-closure lane and no broader:

- keep `src/MLF/Elab/TermClosure.hs` closed again and treat both `rev-009`
  and `rev-010` as blocker evidence, not as active writable surfaces;
- preserve the settled `ALetF` handoff repair as inherited baseline rather
  than relitigating it;
- admit `src/MLF/Elab/Elaborate/Algebra.hs` only in the downstream
  application-consumer recovery surface under `ALamF` / `AAppF`, including the
  immediate helper/read sites around
  `collapseTrivialBoundAlias`,
  `singleAppInstArg`,
  `recoverSingleAppArg`,
  `identityLikeMuArgInst`,
  and the exact consumer branches that now stop at `TCExpectedArrow`;
- admit `src/MLF/Elab/Elaborate/Annotation.hs` only inside the authoritative
  witness / instantiation refinement helpers under `reifyInst`
  (`authoritativeTargetType`, `inferAuthoritativeInstArgs`,
  `reifyTraceBinderInstArgs`, `instNeedsAuthoritativeRefinement`, and
  `instSeqApps`) when reviewer-visible evidence proves that the downstream
  consumer repair still cannot clear `BUG-2026-02-17-002` / `Phi alignment C4`
  honestly without that immediate companion; and
- admit `src/MLF/Elab/Legacy.hs` only at the direct authoritative
  witness-translation helper
  `expInstantiateArgsToInstNoFallback` / `instAppsFromTypes` if the admitted
  `Annotation.hs` companion cannot be expressed honestly without it.

Keep
`src/MLF/Elab/Run/Pipeline.hs`,
`src/MLF/Elab/Pipeline.hs`,
`src-public/MLF/Pipeline.hs`,
`src/MLF/Elab/Run/ResultType/Fallback.hs`,
`src/MLF/Elab/Run/ResultType/Fallback/Core.hs`, and
`src/MLF/Elab/TermClosure.hs`
as read-only continuity anchors unless later accepted evidence proves another
revision is required.

The family goal itself is unchanged:

- preserve recursive structure across the broader positive quantified-crossing
  frontier the accepted ledger authorizes,
- make the representative broader positive frontier pass on both
  `runPipelineElab` and `runPipelineElabChecked`,
- preserve the settled retained-child lane as predecessor truth rather than
  whole-frontier closure, and
- record the new controlling behavior only when the code and evidence actually
  earn it.

## Outcome Boundaries

- Source of truth remains `papers/these-finale-english.txt`.
- Success remains authoritative only when the enacted broader-positive
  frontier is visible on both `runPipelineElab` and `runPipelineElabChecked`.
- Preserve accepted `round-197` as predecessor truth only:
  `sameLaneAliasFrameClearBoundaryExpr` is one settled retained-child lane,
  not full-family closure.
- Preserve accepted `round-203`, `round-204`, and `round-205` as the planning
  ledger; accepted `round-206` as the milestone-1 freeze; and accepted
  `round-207` as the first bounded internal milestone-2 slice.
- Preserve blocked `round-208`, `round-209`, and `round-210` as immutable
  predecessor evidence only.
- Preserve live `round-211` as the same-round continuation baseline, not as a
  discarded draft and not as blocked predecessor evidence.
- Preserve the honest `round-211` retry evidence:
  the current baseline keeps the selected packet, checked-authoritative
  representative parity, and classic let-polymorphism / explicit-`forall`
  positives green, but the required full gate still fails on the remaining
  A6 `TCExpectedArrow` cluster and nested-let strict-failure-shape gap.
- Record `rev-009` honestly as exhausted after the failed closure-only
  continuation: the alias-side mismatch disappears only when the nested-let
  probe turns into false success `forall a. a -> a`.
- Record `rev-010` honestly as exhausted after the bounded post-closure
  `ALetF` continuation: the kept diff had to cross outside that admitted seam,
  yet the thesis gate still fails on A6 `TCExpectedArrow` and the full gate
  still ends at `1341` examples / `12` failures, so the next lawful move is
  downstream application-consumer recovery plus only the immediate
  authoritative-instantiation companion seam, not another same-slice retry.
- `rev-010` does not authorize milestone-3 corpus widening or milestone-4
  closeout work.
- No cyclic search, multi-SCC widening, equi-recursive reasoning, fallback
  rescue, or second interface is authorized.
- Backwards compatibility shims remain out of scope.

## Global Sequencing Rules

- `milestone-1` remains complete before any code-bearing enactment round.
- `milestone-2` may change production semantics only inside the milestone-1
  writable slice as superseded narrowly by `rev-010`.
- `milestone-3` may broaden representative success only after the core
  milestone-2 mechanism is accepted.
- `milestone-4` closes the family only after the code-bearing evidence is in
  place and the new controlling behavior can be recorded honestly.
- Keep predecessor roadmap families and revisions immutable. Any later change
  to this family's coordination or semantic contract must publish a new
  revision under the same `roadmap_id`.
- This family remains intentionally serial.
- Treat blocked `round-208`, `round-209`, and `round-210` as predecessor
  evidence only. Continue from the same live `round-211` branch/worktree
  baseline rather than mutating blocked round artifacts or discarding the
  current `round-211` diff on a fresh round.

## Parallel Lanes

- `lane-main`: default serial lane for the full enactment family.

## Milestones

### 1. [done] Freeze the enactment contract, authoritative frontier, writable slice, and representative corpus

- Milestone id: `milestone-1`
- Depends on: none
- Intent: publish the authoritative code-bearing contract that converts the
  accepted `round-205` handoff into an exact enactment family scope.
- Completion signal: an accepted artifact names the exact broader positive
  frontier beyond the settled retained-child lane, the authoritative success
  bar on `runPipelineElab` and `runPipelineElabChecked`, the preserved closed
  guardrails, and the writable slice for later code-bearing rounds.
- Completion notes:
  - accepted `round-206`, merged as `12fd9dc`
    (`Freeze P5 broader-positive enactment contract, corpus, and writable slice`),
    finalized the controlling milestone-1 freeze;
  - accepted `round-207`, merged as `fb85bd4`
    (`Align same-wrapper nested-forall fallback target and scope`),
    completed the first serial `direction-2a` slice;
  - blocked `round-208`, `round-209`, and `round-210` successively proved that
    the remaining seam moved from authoritative annotation translation, to the
    post-annotation handoff, to the combined same-round continuation later
    preserved in `round-211`;
  - `rev-005` preserved the live `round-211` annotation-plus-handoff baseline
    and first admitted `src/MLF/Elab/Elaborate/Algebra.hs`;
  - `rev-006` widened that admitted `Algebra.hs` continuation to the selected
    `ALetF` let-scheme finalization / closure logic;
  - `rev-007` widened only to the post-let consumer path in the same file;
  - `rev-008` combined the earlier `ALetF` scheme / closure locals with the
    already-admitted post-let consumer path;
  - `rev-009` recorded that the `rev-008` Algebra repair was no longer the
    blocker and admitted a bounded `TermClosure.hs` continuation around
    `closeTermWithSchemeSubstIfNeeded`; and
  - `rev-010` recorded `rev-009` as exhausted after the failed closure-only
    continuation; and
  - `rev-011` now records `rev-010` as exhausted after the bounded
    post-closure `ALetF` continuation: the kept diff had to cross into
    downstream consumer recovery, the thesis gate still fails on A6
    `TCExpectedArrow`, and the full gate still ends at `1341` examples /
    `12` failures, so the active contract preserves the `ALetF` handoff as
    settled baseline and admits only the next downstream `ALamF` / `AAppF`
    consumer seam plus the immediate authoritative-instantiation companion
    already named by `BUG-2026-02-17-002` / `Phi alignment C4`.

Accepted direction lineage:

- Direction id: `direction-1a-freeze-broader-positive-enactment-contract`
  Status: accepted in `round-206`, merged as `12fd9dc`.
  Outcome: published the canonical milestone-1 contract freeze and routed the
  family forward to `milestone-2`.

### 2. [pending] Implement the broader-positive recursive-structure-preservation mechanism

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: land the core production semantics that stop treating
  polymorphic-mediation `mu` absorption as the controlling broader-positive
  read and instead preserve recursive structure across the milestone-1
  frontier within the accepted explicit-only / iso-recursive boundary.
- Completion signal: accepted code-bearing rounds land the core mechanism
  inside the milestone-1 writable slice, representative broader-positive
  quantified-crossing cases no longer fail for the old controlling reason, the
  full verification gate is green, focused tests capture the new behavior, and
  the retained-child lane plus preserved negative guardrails remain honest.
- Parallel lane: `lane-main`
- Coordination notes:
  - consume accepted `round-206` directly;
  - consume accepted `round-207` as the first internal mechanism slice;
  - consume blocked `round-208`, `round-209`, and `round-210` as predecessor
    evidence;
  - consume live `round-211` as the continuing same-round baseline; and
  - stay inside the exact `rev-011` continuation without overclaiming whole
    broader-positive closure before the representative frontier has passed on
    both authoritative entrypoints.
- Progress notes:
  - accepted `round-207` completed the first serial `direction-2a` extraction
    inside fallback core while keeping authoritative pipeline/public seams
    untouched;
  - live `round-211` then cleared the old Phase 6 `reifyInst` stop and the
    downstream `PhiReorder: missing binder identity` detour on the same
    branch/worktree;
  - the admitted `rev-008` combined `Algebra.hs` continuation repaired the
    selected same-wrapper nested-`forall` packet on both authoritative
    entrypoints, restored checked-authoritative representative parity, and
    restored classic let-polymorphism / explicit-`forall` positives;
  - `rev-009` is exhausted because the direct `TermClosure.hs`
    continuation removed the alias-side mismatch only by producing false
    success `forall a. a -> a`, then had to be reverted;
  - `rev-010` is exhausted because the bounded post-closure `ALetF`
    continuation still left the thesis gate red on A6 and the full gate red at
    `1341` examples / `12` failures, with the kept diff already reaching the
    downstream consumer surface; and
  - the next exact move is therefore downstream application-consumer recovery
    plus only the immediate authoritative-instantiation companion seam, not
    another closure-only retry, not another `ALetF`-only retry, and not a
    reopen of pipeline/public/fallback surfaces.

Candidate directions:

- Direction id: `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`
  Summary: update the core elaboration / inference / recursive-structure
  machinery so broader-positive quantified-crossing cases preserve recursive
  structure instead of being controlled by polymorphic-mediation `mu`
  absorption.
  Why it matters now: it supplied the accepted predecessor mechanism slice
  that later authoritative work must preserve.
  Preconditions: `milestone-1`.
  Parallel hints: serial extraction only.
  Boundary notes: no fallback rescue, no second interface, no equi-recursive
  or cyclic widening, and no reopening of `P2` or representative negative
  families.
  Extraction notes: completed by accepted `round-207`; preserve it as
  predecessor truth only while milestone-2 continues.

- Direction id: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Summary: carry the milestone-2 mechanism through the authoritative
  annotation, settled `ALetF` handoff, and now the downstream
  application-consumer recovery seam so the selected behavior is visible where
  the family measures success.
  Why it matters now: accepted `round-207` and live `round-211` already
  repaired the selected packet itself; `rev-009` proved that direct closure is
  not the remaining blocker, and `rev-010` proved that the bounded post-closure
  `ALetF` handoff is no longer sufficient either. The remaining blocker is now
  the downstream `ALamF` / `AAppF` consumer read, plus only the immediate
  authoritative-instantiation companion still required by
  `BUG-2026-02-17-002` / `Phi alignment C4`.
  Preconditions:
  - `milestone-1`;
  - accepted progress from `direction-2a` via `round-207`;
  - blocked predecessor evidence from `round-208`, `round-209`, and
    `round-210`;
  - live `round-211` baseline; and
  - the authoritative `round-211` review proof that the direct
    `TermClosure.hs` continuation and the bounded post-closure `ALetF`
    continuation are both exhausted, and the next lawful seam is downstream
    application-consumer recovery plus the immediate authoritative companion.
  Parallel hints: keep serial unless a later accepted revision proves an
  independent split is safe.
  Boundary notes:
  - this direction remains one exact same-wrapper nested-`forall` packet plus
    the repo-wide fail-fast cluster that the selected packet repair exposed;
  - preserve the current `round-211` diff in
    `src/MLF/Elab/Elaborate/Annotation.hs`,
    `src/MLF/Elab/Legacy.hs`,
    `src/MLF/Elab/Elaborate/Algebra.hs`,
    `test/ElaborationSpec.hs`,
    `test/PipelineSpec.hs`, and
    `test/Research/P5ClearBoundarySpec.hs`;
  - preserve the current `ALetF` handoff repair as inherited baseline rather
    than new writable scope;
  - admit `src/MLF/Elab/Elaborate/Algebra.hs` only around the exact
    downstream `ALamF` / `AAppF` consumer recovery locals and immediate helper
    reads that now stop at `TCExpectedArrow`, including
    `collapseTrivialBoundAlias`,
    `singleAppInstArg`,
    `recoverSingleAppArg`,
    `identityLikeMuArgInst`,
    and the exact consumer branches that use them;
  - admit `src/MLF/Elab/Elaborate/Annotation.hs` only inside the
    `reifyInst` authoritative refinement helpers
    `authoritativeTargetType`,
    `inferAuthoritativeInstArgs`,
    `reifyTraceBinderInstArgs`,
    `instNeedsAuthoritativeRefinement`, and
    `instSeqApps` when the `Algebra.hs` landing needs that direct witness
    companion;
  - admit `src/MLF/Elab/Legacy.hs` only at
    `expInstantiateArgsToInstNoFallback` / `instAppsFromTypes`
    when the admitted `Annotation.hs` witness refinement cannot be expressed
    honestly without that direct translator companion; and
  - keep
    `src/MLF/Elab/TermClosure.hs`,
    `src/MLF/Elab/Run/Pipeline.hs`,
    `src/MLF/Elab/Pipeline.hs`,
    `src-public/MLF/Pipeline.hs`,
    `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
    `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
    read-only continuity anchors unless later accepted evidence proves another
    revision is required.
  Extraction notes:
  - exact same-round continuation task:
    `continue-round-211-selected-same-wrapper-nested-forall-through-downstream-application-consumer-recovery-and-immediate-authoritative-instantiation-companion-seam`
  - current task/title:
    `Continue the selected same-wrapper nested-forall packet through the downstream application-consumer recovery / immediate authoritative-instantiation companion seam while preserving the live round-211 baseline`
  - preserve the current `round-211` diff rather than discarding it on a
    fresh round;
  - keep the inherited `Algebra.hs` packet and `ALetF` handoff repairs as
    baseline rather than relitigating them as unresolved work;
  - keep `src/MLF/Elab/TermClosure.hs` closed; and
  - stop again if another seam outside this tighter continuation is required.

Accepted extraction lineage:

- Extraction id: `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`
  Direction: `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`
  Status: accepted in `round-207`, merged as `fb85bd4`.
  Outcome: repaired the first bounded same-wrapper nested-`forall` internal
  mechanism slice while keeping authoritative pipeline/public seams untouched.

Blocked predecessor lineage:

- Evidence id: `round-208-blocked-authoritative-annotation-translation-seam`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: blocked predecessor evidence only.
  Outcome: proved that the selected packet still failed on both authoritative
  entrypoints with the same Phase 6 `PhiTranslatabilityError`.

- Evidence id: `round-209-blocked-post-annotation-authoritative-result-handoff-seam`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: blocked predecessor evidence only.
  Outcome: proved that `Annotation.hs`-only repair could move the selected
  packet beyond the original Phase 6 failure only to expose downstream
  post-annotation type-check mismatches.

- Evidence id: `round-210-blocked-handoff-only-continuation-not-independently-reachable`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: blocked predecessor evidence only.
  Outcome: proved that the selected packet still failed before the preserved
  downstream handoff code ran.

Live continuation lineage:

- Continuation id: `round-211-live-downstream-application-consumer-recovery-and-immediate-authoritative-instantiation-companion-continuation`
  Direction: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: live same-round continuation baseline; not accepted, not merged.
  Outcome:
  - preserves the current `round-211` diff in
    `src/MLF/Elab/Elaborate/Annotation.hs`,
    `src/MLF/Elab/Legacy.hs`,
    `src/MLF/Elab/Elaborate/Algebra.hs`,
    `test/ElaborationSpec.hs`,
    `test/PipelineSpec.hs`, and
    `test/Research/P5ClearBoundarySpec.hs`;
  - keeps the selected same-wrapper nested-`forall` packet green on both
    authoritative entrypoints;
  - keeps checked-authoritative representative parity, the classic
    let-polymorphism / explicit-`forall` positives, `BUG-2026-02-06-002`, and
    the retained-child exact packet green;
  - records `rev-010` honestly as exhausted because the bounded post-closure
    `ALetF` continuation still leaves the thesis gate red on A6
    `TCExpectedArrow`, the full gate red at `1341` examples / `12` failures,
    and the kept diff already reaches downstream consumer recovery; and
  - makes the next exact move a same-round continuation that preserves the
    current diff, keeps `TermClosure.hs` and pipeline/public/fallback surfaces
    closed, and minimally reopens only the downstream `ALamF` / `AAppF`
    consumer seam plus the immediate authoritative-instantiation companion
    already named by `BUG-2026-02-17-002` / `Phi alignment C4`.

### 3. [pending] Prove broader-positive frontier success on both authoritative entrypoints

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: broaden evidence from the milestone-2 core mechanism to the full
  broader positive `P5 polymorphism-nested-forall` frontier named by
  milestone-1 until representative success is honest on both
  `runPipelineElab` and `runPipelineElabChecked`.
- Completion signal: accepted rounds expand the representative broader-positive
  corpus, the enacted frontier passes on both authoritative entrypoints,
  focused regression coverage records that success honestly, and preserved
  `P2` / `N1` / `N2` / `N6` guardrails remain fail-closed.
- Parallel lane: `lane-main`
- Coordination notes: treat milestone-2 as the core mechanism prerequisite; do
  not collapse one or two repaired packets into whole-frontier closure; keep
  the retained-child lane as predecessor evidence only.

Candidate directions:

- Direction id: `direction-3a-expand-the-broader-positive-representative-corpus`
  Summary: widen from the initial repaired slices to the representative
  broader-positive frontier frozen by milestone-1 and prove that those cases
  now succeed honestly on both authoritative entrypoints.
  Why it matters now: the family goal is the broader positive frontier beyond
  the settled retained-child lane, not one repaired packet.
  Preconditions: `milestone-2`.

- Direction id: `direction-3b-tighten-public-internal-parity-and-regression-guards`
  Summary: close any remaining parity gaps or regression holes so the broader
  positive frontier is stably supported on both authoritative entrypoints.
  Why it matters now: milestone-3 must end with honest, reviewable support on
  both authoritative entrypoints, not fragile internal-only success.
  Preconditions: `milestone-2`; likely depends on accepted progress from
  `direction-3a`.

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
  earned the claim.

Candidate directions:

- Direction id: `direction-4a-publish-broader-positive-enactment-closeout`
  Summary: publish the canonical final artifact and repo-facing record of the
  broader-positive enactment family once the code-bearing evidence is complete.
  Why it matters now: the family is not complete until the new controlling
  behavior is recorded honestly for future work.
  Preconditions: `milestone-3`.
