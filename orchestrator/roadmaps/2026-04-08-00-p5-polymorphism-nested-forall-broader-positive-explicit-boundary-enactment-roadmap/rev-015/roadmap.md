# P5 Polymorphism-Nested-Forall Broader-Positive Explicit Boundary-Enactment Roadmap

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-015`
Base branch: `codex/automatic-recursive-type-inference`
Created: 2026-04-10

## Goal

Continue the same enactment family after accepted `round-211` was merged onto
`codex/automatic-recursive-type-inference` as commit `5b775b2`, completing the
core milestone-2 repair that `rev-014` existed to judge.

`rev-015` preserves the new merged baseline exactly:

- preserve the merged `round-211` code/test payload now on the base branch in
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`,
  plus the accepted round-local notes under `orchestrator/rounds/round-211/`;
- preserve the green behavior and gates that `round-211` proved and merged:
  the selected same-wrapper nested-`forall` packet on both authoritative
  entrypoints, checked-authoritative representative parity,
  `BUG-2026-02-06-002`,
  the retained-child exact packet,
  `BUG-2026-02-17-002`,
  the correct semantic `g g` failure,
  the A6 / nested-let / representative let-polymorphism cluster,
  `./scripts/thesis-conformance-gate.sh`, and
  `cabal build all && cabal test` with `1341 examples, 0 failures`;
- treat accepted `rev-014` as complete milestone-2 predecessor authority, not
  as a live same-round continuation contract; and
- keep the family inside the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback / no-second-interface
  boundary unless later accepted evidence explicitly changes it.

Accepted `round-211` merged five controlling facts:

- the broader authoritative `AAppF` / `ALetF` handoff repair is now landed,
  not merely staged on a preserved round branch;
- the helper-local scaffold admitted by `rev-014` was validated by accepted
  review and no longer counts as an open structural blocker by itself;
- the current base-branch implementation now keeps the selected broader
  positive `P5` packet recursive on both `runPipelineElab` and
  `runPipelineElabChecked`;
- the focused regression cluster, thesis gate, and full repo gate all passed
  on the merged result; and
- the honest next control-plane move is no longer milestone-2 retry logic,
  but milestone-3 representative broadening on top of the merged baseline.

`rev-015` therefore activates milestone-3 with a concrete post-merge contract:

- keep the merged `round-211` baseline as the starting point for any further
  work; do not reopen the old same-round freshness, helper-flattening, or
  merge-scope arguments as if milestone-2 were still pending;
- keep the writable production/test slice unchanged from the accepted family
  baseline:
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
- treat the settled retained-child clear-boundary lane as predecessor truth
  only, not as whole-frontier closure;
- require milestone-3 to widen from the now-merged selected same-wrapper
  nested-`forall` success to the remaining representative broader-positive
  corpus already anchored in `test/Research/P5ClearBoundarySpec.hs` and the
  corresponding `test/PipelineSpec.hs` / `test/ElaborationSpec.hs` guards; and
- preserve the current fail-closed quantified-crossing contrasts and the
  inherited `P2` / `N1 ambiguity-reject` / `N2 unsoundness-guard` /
  `N6 termination-pressure` obligations unless a later accepted round
  reclassifies them honestly.

The family goal itself is unchanged:

- preserve recursive structure across the broader positive quantified-crossing
  frontier the accepted ledger authorizes,
- make representative broader-positive support honest on both
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
  ledger; accepted `round-206` as the milestone-1 freeze; accepted
  `round-207` as the first core mechanism slice; and accepted `round-211`,
  merged as `5b775b2`, as the milestone-2 completion result.
- Preserve blocked `round-208`, `round-209`, and `round-210` as immutable
  predecessor evidence only.
- Record `rev-012`, `rev-013`, and `rev-014` honestly as exhausted milestone-2
  coordination epochs. `rev-015` supersedes them only for post-merge family
  coordination; it does not rewrite their accepted evidence.
- `rev-015` does not authorize milestone-4 closeout work.
- No cyclic search, multi-SCC widening, equi-recursive reasoning, fallback
  rescue, or second interface is authorized.
- Backwards compatibility shims remain out of scope.

## Global Sequencing Rules

- `milestone-1` remains complete before any code-bearing enactment round.
- `milestone-2` is now complete on the merged `5b775b2` baseline.
- `milestone-3` may broaden representative success only on top of that merged
  baseline and may not silently reopen milestone-2 as live debt.
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
- Completion notes:
  - accepted `round-206`, merged as `12fd9dc`, finalized the controlling
    milestone-1 freeze;
  - accepted `round-207`, merged as `fb85bd4`, completed the first serial
    `direction-2a` slice;
  - blocked `round-208`, `round-209`, and `round-210` successively proved
    that the remaining seam moved through downstream authoritative handoff
    work rather than through closed continuity anchors; and
  - accepted `round-211`, merged as `5b775b2`, confirms the freeze was fit for
    the completed milestone-2 repair.

### 2. [done] Implement the broader-positive recursive-structure-preservation mechanism

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
- Completion notes:
  - accepted `round-207`, merged as `fb85bd4`, completed
    `direction-2a-align-same-wrapper-nested-forall-fallback-target-and-scope`;
  - accepted `round-211`, merged as `5b775b2`, completed
    `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
    by repairing the broader authoritative `AAppF` / `ALetF` handoff on the
    selected same-wrapper nested-`forall` packet;
  - the merged result keeps the selected packet recursive on both
    authoritative entrypoints, preserves checked-authoritative representative
    parity and the accepted guard cluster, and passes both
    `./scripts/thesis-conformance-gate.sh` and
    `cabal build all && cabal test` (`1341 examples, 0 failures`); and
  - the helper-local scaffold admitted by `rev-014` is now accepted merged
    baseline evidence, not live blocker debt.

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
- Completion signal: accepted rounds expand the representative broader-positive
  corpus beyond the one selected packet, the enacted frontier passes honestly
  on both authoritative entrypoints, focused regression coverage records that
  success, and preserved `P2` / `N1` / `N2` / `N6` guardrails remain
  fail-closed.
- Parallel lane: `lane-main`
- Coordination notes:
  - start from merged commit `5b775b2`, not from a live same-round baseline;
  - do not collapse the selected same-wrapper nested-`forall` packet into
    whole-frontier closure;
  - keep the retained-child clear-boundary lane as predecessor evidence only;
  - keep the current negative quantified-crossing contrasts honest unless the
    evidence really changes them; and
  - keep milestone-3 inside the existing writable slice and out of closeout
    work.

Candidate directions:

- Direction id: `direction-3a-expand-the-broader-positive-representative-corpus`
  Summary: widen from the merged selected same-wrapper nested-`forall` repair
  to the remaining representative broader-positive `P5` corpus already anchored
  in `test/Research/P5ClearBoundarySpec.hs` and the matching
  `test/PipelineSpec.hs` / `test/ElaborationSpec.hs` rows.
  Why it matters now: the family goal is not one repaired packet; it is
  representative broader-positive support beyond the one settled retained-child
  lane.
  Preconditions: `milestone-2`.
  Extraction notes: the next unfinished item is to prove that the merged
  same-wrapper nested-`forall` success generalizes across the remaining
  representative broader-positive corpus on both authoritative entrypoints
  without reopening the fail-closed quantified contrasts or checked-authoritative
  parity guards.

- Direction id: `direction-3b-tighten-public-internal-parity-and-regression-guards`
  Summary: close any remaining parity or regression holes so milestone-3 ends
  with stable broader-positive support on both authoritative entrypoints.
  Why it matters now: representative support is not honest if it survives only
  on one entrypoint or only through helper-local behavior.
  Preconditions: `milestone-2`; likely depends on accepted progress from
  `direction-3a`.

### 4. [pending] Close out the family honestly

- Milestone id: `milestone-4`
- Depends on: `milestone-3`
- Intent: publish the authoritative closeout record that states the enacted
  broader-positive behavior, records the evidence surface, and explains which
  guardrails and excluded families remain closed.
- Completion signal: an accepted closeout artifact records that the broader
  positive frontier succeeds on both `runPipelineElab` and
  `runPipelineElabChecked`, documents preserved closed boundaries honestly, and
  updates repo-facing notes only when the evidence requires it.
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
