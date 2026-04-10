# P5 Polymorphism-Nested-Forall Broader-Positive Explicit Boundary-Enactment Roadmap

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-012`
Base branch: `codex/automatic-recursive-type-inference`
Created: 2026-04-09

## Goal

Continue the same code-bearing enactment family after live `round-211`
proved that `rev-011` still stopped one seam too early.

The same live `round-211` branch/worktree remains the authoritative
continuation baseline, and `rev-012` preserves its inherited truth exactly:

- preserve the existing round-owned diff in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`;
- preserve the protected wins now visible on that baseline:
  the selected same-wrapper nested-`forall` packet on both authoritative
  entrypoints, checked-authoritative representative parity,
  `BUG-2026-02-06-002`,
  the retained-child exact packet,
  `BUG-2026-02-17-002`, and
  the non-local proxy-wrapper `g g` control;
- preserve the same live `round-211` branch/worktree rather than restarting
  on a fresh round; and
- keep the family inside the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback / no-second-interface
  boundary unless later accepted evidence explicitly changes it.

`rev-011` is now exhausted after the bounded downstream
`ALamF` / `AAppF` consumer recovery plus immediate authoritative-companion
continuation. The controlling review evidence preserves four facts:

- the admitted `Algebra.hs` / `Annotation.hs` / `Legacy.hs` seam is real
  enough to keep the selected packet green and to repair
  `BUG-2026-02-17-002`;
- the thesis gate still fails on the A6 dual-consumer lane, now at
  `TCArgumentMismatch` rather than the older `TCExpectedArrow`;
- both nested-let fail-fast rows are still red, now at
  `TCInstantiationError`; and
- the full gate ends at `1341` examples / `16` failures because the same
  working diff also regresses broader let-polymorphism /
  representative-corpus behavior (`runtime snapshot rebuild`, redirected
  let-use polymorphism, `elaborates dual instantiation in application`,
  `let id = (\x. x) in id id should have type ∀a. a -> a`, Phi/alignment
  let-poly rows, and frozen parity baseline).

The newly admitted seam in `rev-012` therefore widens only from the
`rev-011` downstream consumer lane to the next broader authoritative
application / let-polymorphism handoff, and no farther:

- keep `src/MLF/Elab/TermClosure.hs` closed again and treat the earlier
  closure-only and bounded post-closure retries as blocker evidence, not as
  active writable surfaces;
- preserve the `rev-011` downstream `ALamF` / `AAppF` consumer recovery as
  inherited baseline rather than relitigating it;
- admit `src/MLF/Elab/Elaborate/Algebra.hs` not only at the downstream
  consumer locals and helper reads already opened in `rev-011`
  (`collapseTrivialBoundAlias`,
  `singleAppInstArg`,
  `recoverSingleAppArg`,
  `identityLikeMuArgInst`,
  `funInstByFunType`,
  `funInstRecovered`,
  `argInstFromFun`,
  and `fAppRecovered`),
  but also at the adjacent authoritative application / let-polymorphism
  handoff that now controls scheme reuse and monomorphizing drift:
  `inferInstFromSchemeTarget`,
  `rhsAbs0`,
  `rhsAbsStripped`,
  `rhsAbs`,
  `rhsBodySchemeInfo`,
  `bodyEnv`, and
  `rhsFinal`;
- admit `src/MLF/Elab/Elaborate/Annotation.hs` only inside the same
  authoritative witness / instantiation refinement helpers under `reifyInst`
  (`authoritativeTargetType`, `inferAuthoritativeInstArgs`,
  `reifyTraceBinderInstArgs`, `instNeedsAuthoritativeRefinement`, and
  `instSeqApps`) when reviewer-visible evidence proves the widened
  `Algebra.hs` handoff still needs that immediate companion; and
- admit `src/MLF/Elab/Legacy.hs` only at
  `expInstantiateArgsToInstNoFallback` / `instAppsFromTypes`
  when the admitted `Annotation.hs` companion still cannot be expressed
  honestly without that direct translator support.

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
  ledger; accepted `round-206` as the milestone-1 freeze; and accepted
  `round-207` as the first bounded internal milestone-2 slice.
- Preserve blocked `round-208`, `round-209`, and `round-210` as immutable
  predecessor evidence only.
- Preserve live `round-211` as the same-round continuation baseline, not as a
  discarded draft and not as blocked predecessor evidence.
- Record `rev-011` honestly as exhausted: the admitted downstream consumer /
  immediate authoritative-companion seam can keep the selected packet green
  and repair `BUG-2026-02-17-002`, but it is still non-landable because A6
  remains red, nested-let remains red, and the same working diff regresses
  broader let-polymorphism / representative-corpus rows.
- `rev-012` does not authorize milestone-3 corpus widening or milestone-4
  closeout work.
- No cyclic search, multi-SCC widening, equi-recursive reasoning, fallback
  rescue, or second interface is authorized.
- Backwards compatibility shims remain out of scope.

## Global Sequencing Rules

- `milestone-1` remains complete before any code-bearing enactment round.
- `milestone-2` may change production semantics only inside the milestone-1
  writable slice as superseded narrowly by `rev-012`.
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
  - accepted `round-206`, merged as `12fd9dc`, finalized the controlling
    milestone-1 freeze;
  - accepted `round-207`, merged as `fb85bd4`, completed the first serial
    `direction-2a` slice;
  - blocked `round-208`, `round-209`, and `round-210` successively proved
    that the remaining seam moved from authoritative annotation translation,
    to post-annotation handoff, to the same-round continuation later
    preserved in `round-211`;
  - `rev-011` exhausted the bounded downstream consumer /
    immediate authoritative-companion continuation; and
  - `rev-012` now records that the honest next move is the broader
    authoritative application / let-polymorphism handoff in the same files,
    not another same-slice retry and not a reopen of closed surfaces.

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
  - stay inside the exact `rev-012` continuation without overclaiming whole
    broader-positive closure before the representative frontier has passed on
    both authoritative entrypoints.
- Progress notes:
  - accepted `round-207` completed the first serial `direction-2a` extraction
    inside fallback core while keeping authoritative pipeline/public seams
    untouched;
  - live `round-211` then cleared the old Phase 6 `reifyInst` stop and the
    downstream `PhiReorder: missing binder identity` detour on the same
    branch/worktree;
  - `rev-011` made the selected packet and `BUG-2026-02-17-002` green within
    the admitted downstream consumer / immediate companion seam; and
  - `rev-012` now reopens only the adjacent authoritative application /
    let-polymorphism handoff needed by the remaining A6, nested-let, and
    representative-corpus let-poly regressions.

Accepted direction lineage:

- Direction id: `direction-2a-align-same-wrapper-nested-forall-fallback-target-and-scope`
  Status: accepted in `round-207`, merged as `fb85bd4`.
  Outcome: completed the first internal slice while keeping authoritative
  seams for later milestone-2 work.

- Direction id: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: in progress.
  Outcome target: continue the same live `round-211` baseline through the
  authoritative application / let-polymorphism handoff without widening into
  closed surfaces.

### 3. [pending] Broaden representative broader-positive success once the core mechanism is accepted

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: prove representative broader-positive support on both authoritative
  entrypoints once the core mechanism is stable and landable.

### 4. [pending] Close out the family honestly

- Milestone id: `milestone-4`
- Depends on: `milestone-3`
- Intent: record the enacted behavior, repo-facing notes, and any required
  thesis-deviation accounting only after the code and evidence earn them.
