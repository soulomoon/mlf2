# P5 Polymorphism-Nested-Forall Broader-Positive Explicit Boundary-Enactment Roadmap

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-014`
Base branch: `codex/automatic-recursive-type-inference`
Created: 2026-04-09

## Goal

Continue the same code-bearing enactment family after live `round-211`
proved that `rev-013`'s helper-flattening structural audit is narrower than
the already-green baseline it tried to police.

The same live `round-211` branch/worktree remains the authoritative
continuation baseline, and `rev-014` preserves its inherited truth exactly:

- preserve the existing round-owned diff in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`;
- preserve the protected wins and green gates now visible on that baseline:
  the selected same-wrapper nested-`forall` packet on both authoritative
  entrypoints, checked-authoritative representative parity,
  `BUG-2026-02-06-002`,
  the retained-child exact packet,
  `BUG-2026-02-17-002`, and
  the non-local proxy-wrapper `g g` control, plus the now-green A6 /
  nested-let / representative let-polymorphism cluster,
  `./scripts/thesis-conformance-gate.sh`, and
  `cabal build all && cabal test` with `1341 examples, 0 failures`;
- preserve the same live `round-211` branch/worktree rather than restarting
  on a fresh round or discarding the current diff; and
- keep the family inside the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback / no-second-interface
  boundary unless later accepted evidence explicitly changes it.

`rev-013` is now exhausted as a structural-policing contract. The
attempt-13 review preserved five controlling facts:

- parent-workspace and canonical-worktree lineage checks, pointer stubs, the
  focused protected matrix, the thesis gate, and the full repo gate all pass
  from the preserved `round-211` baseline;
- the exact attempt-12 helper-name audit is now clean;
- the current diff still stays inside the same admitted
  `Annotation.hs` / `Legacy.hs` / `Algebra.hs` / round-owned-test surfaces,
  with no drift in `TermClosure.hs`, pipeline/public seams, or fallback
  surfaces;
- the only remaining review blocker is that `Algebra.hs` still carries nested
  helper-local scaffold under the already-admitted `AAppF` / `ALetF` locals;
  and
- demanding another flatten-only cleanup or a broader rewrite would now be
  contract churn rather than new behavioral evidence.

`rev-014` therefore does not broaden writable files or runtime behavior. It
keeps the admitted `rev-013` surfaces open and republishes the same live
baseline under a more honest helper-local contract:

- keep `src/MLF/Elab/TermClosure.hs` closed again and treat the earlier
  closure-only and bounded post-closure retries as blocker evidence, not as
  active writable surfaces;
- preserve the admitted downstream `AAppF` consumer / `ALetF` handoff seam in
  `src/MLF/Elab/Elaborate/Algebra.hs`, but restrict new repair work to the
  existing instantiation-recovery and let-scheme locals around
  `funInstByFunType`,
  `funInst'`,
  `normalizeFunInst`,
  `funInstNorm`,
  `funInstRecovered`,
  `fAppForArgInference`,
  `argInstFromFun`,
  `argInst'`,
  `argInstFinal`,
  `fApp`,
  `aApp`,
  `schemeBase`,
  `scheme`,
  `subst0`,
  `subst`,
  `schemeInfo`,
  `env'`,
  `rhsAbs0`,
  `rhsAbs`,
  `bodyElab`, and
  `rhsFinal`;
- newly admit the nested helper-local scaffold currently living directly under
  `funInstRecovered`,
  `argInstFromFun`,
  `fApp`,
  `scheme`,
  `rhsAbs0`, and
  `rhsAbs`, including
  `containsMuType`,
  `containsMuBound`,
  `isIdentityLikeSchemeType`,
  `shouldInlineParamTy`,
  `shouldInferArgInst`,
  `isInternalTyVar`,
  `isIdentityLambdaBody`,
  `muAnnotationTy`, and
  the current `schemeTy` / strip-candidate staging locals used by
  `rhsAbs0` / `rhsAbs`, provided that scaffold stays local to those
  already-open blocks and does not widen into new top-level helpers or new
  files;
- keep `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`, and the preserved round-owned tests as
  carry-forward baseline evidence only; `rev-014` does not authorize fresh
  edits there unless a later accepted review or roadmap revision proves the
  helper-local contract false; and
- require the next continuation to treat the current green runtime behavior as
  fixed and to judge the preserved `round-211` baseline against this
  republished structural contract, rather than demanding another flatten-only
  cleanup whose only effect is to erase already-local helper bindings.

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
- Record `rev-012` honestly as exhausted: the admitted broader authoritative
  application / let-polymorphism handoff is still the right seam, but the
  unchanged `rev-012` state is not fit for another direct same-revision retry.
- Record `rev-013` honestly as exhausted too: its "no fresh helper bindings
  under the same seam" audit is narrower than the already-green preserved
  `round-211` baseline.
- `rev-014` admits the existing helper-local scaffold inside the same
  `Algebra.hs` handoff while keeping `Annotation.hs`, `Legacy.hs`, and the
  round-owned tests as read-only carry-forward evidence.
- `rev-014` does not authorize milestone-3 corpus widening or milestone-4
  closeout work.
- No cyclic search, multi-SCC widening, equi-recursive reasoning, fallback
  rescue, or second interface is authorized.
- Backwards compatibility shims remain out of scope.

## Global Sequencing Rules

- `milestone-1` remains complete before any code-bearing enactment round.
- `milestone-2` may change production semantics only inside the milestone-1
  writable slice as superseded narrowly by `rev-014`.
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
  - `rev-012` proved that the broader authoritative application /
    let-polymorphism handoff is the surviving seam;
  - `rev-013` carried that same seam far enough to make the focused matrix,
    thesis gate, and full repo gate green on the preserved `round-211`
    baseline; and
  - `rev-014` now records that the honest next move is to keep that same
    baseline and admit its still-local helper scaffold explicitly, not to
    reopen broader cleanup or closed surfaces.

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
  - stay inside the exact `rev-014` continuation without overclaiming whole
    broader-positive closure before the representative frontier has passed on
    both authoritative entrypoints.
- Progress notes:
  - accepted `round-207` completed the first serial `direction-2a` extraction
    inside fallback core while keeping authoritative pipeline/public seams
    untouched;
  - live `round-211` then cleared the old Phase 6 `reifyInst` stop and the
    downstream `PhiReorder: missing binder identity` detour on the same
    branch/worktree;
  - `rev-012` proved that the surviving blocker lives in the broader
    authoritative application / let-polymorphism handoff rather than in
    `TermClosure.hs`, pipeline/public surfaces, or fallback surfaces;
  - `rev-013` carried that handoff far enough to make the focused matrix,
    thesis gate, and full repo gate green on the preserved `round-211`
    baseline; and
  - `rev-014` keeps that exact runtime baseline but republishes the
    structural contract so the surviving helper-local scaffold under
    `AAppF` / `ALetF` is admitted rather than treated as a retry-only
    blocker.

Accepted direction lineage:

- Direction id: `direction-2a-align-same-wrapper-nested-forall-fallback-target-and-scope`
  Status: accepted in `round-207`, merged as `fb85bd4`.
  Outcome: completed the first internal slice while keeping authoritative
  seams for later milestone-2 work.

- Direction id: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
  Status: in progress.
  Outcome target: carry the same admitted `round-211` handoff and helper-local
  scaffold to an honest landable milestone-2 result, without widening into
  `TermClosure.hs`, pipeline/public/fallback surfaces, cyclic widening,
  equi-recursive reasoning, or a second interface.

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
