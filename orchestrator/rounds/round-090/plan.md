# Round 090 Plan (`item-2` Retry Delta Attempt-2)

## Objective

Execute only the same-round retry for roadmap item `2`.

This rewrite replaces the rejected `attempt-1` plan with the required
`attempt-2` retry delta only. The retry scope is exactly the recorded fix
hypothesis from `orchestrator/rounds/round-090/state-snapshot.json`, `review.md`, and
`reviews/attempt-1.md`: re-run the breakpoint audit against exact-pocket
evidence only; add reviewer-visible `runPipelineElab` /
`runPipelineElabChecked` evidence for the frozen
`let k = (\x : mu a. a -> Int. x) in let u = (\y. y) k in u` packet; then
rewrite the ledger so the first failing row matches that exact evidence. If
that exact-pocket replay still fails in Phase 6 elaboration with
`PhiTranslatabilityError`, the earliest breakpoint must move to elaboration
and every later row must stop receiving credit.

The canonical item-2 artifact path remains:

`docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`

The round remains docs-only, breakpoint-audit-only, and bounded to the frozen
same-lane retained-child tuple only:

- family: same-lane retained-child;
- recursive-shape anchor: `boundVarTargetRoot`;
- owner / binder frame: one owner-local retained-child frame;
- route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`;
- quantified-boundary state: clear-boundary only
  (`boundHasForallFrom` false and `not hasForall` true); and
- current bounded recursive fact: `containsMu True`.

This retry does not reopen item `3`, item `4`, item `5`, the alias-bound
family, neighboring routes, nested-`forall` success, architecture revision,
or any code/test/product surface.

## Locked Retry Context

- Round id: `round-090`
- Roadmap item: `item-2`
- Stage: `plan`
- Active attempt: `attempt-2`
- Latest attempt verdict: `rejected`
- Latest stage action: `retry`
- Retry reason:
  the prior audit replaced exact-pocket public-output evidence with the
  out-of-pocket unannotated `PipelineSpec.hs:1693-1698` variant, while direct
  replay of the exact frozen pocket already failed earlier in Phase 6
  elaboration with `PhiTranslatabilityError`.
- Fix hypothesis:
  re-run item `2` against exact-pocket evidence only; add reviewer-visible
  `runPipelineElab` / `runPipelineElabChecked` evidence for the frozen
  `let k ... let u ... in u` packet; if that exact-pocket public pipeline
  still fails at Phase 6 elaboration, move the earliest breakpoint
  accordingly and stop crediting later rows.

Carry forward without replanning:

- `selection.md` still fixes the round to roadmap `item-2` only and keeps the
  same frozen tuple / route / clear-boundary subject.
- `review.md`, `reviews/attempt-1.md`, and `attempt-log.jsonl` already record
  the rejected `attempt-1` blocker and must remain immutable.
- The same canonical breakpoint-audit artifact must be refreshed in place for
  `attempt-2`; this retry does not create a replacement artifact or a second
  packet.
- `orchestrator/rounds/round-090/implementation-notes.md` should be refreshed
  in place so its summary and verification notes match the retry outcome.

## File Map

### Modify

- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`
  - Responsibility: refresh the canonical item-2 audit artifact with
    `attempt-2` metadata, exact-pocket replay evidence, and the corrected
    earliest-break ledger.

- `orchestrator/rounds/round-090/implementation-notes.md`
  - Responsibility: refresh the round-local notes so they report the same
    exact-pocket evidence and the same earliest-break conclusion as the
    canonical artifact.

### Read-Only Evidence

- `orchestrator/rounds/round-090/selection.md`
- `orchestrator/rounds/round-090/review.md`
- `orchestrator/rounds/round-090/reviews/attempt-1.md`
- `orchestrator/rounds/round-090/attempt-log.jsonl`
- `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/verification.md`
- `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/retry-subloop.md`
- `orchestrator/rounds/round-090/state-snapshot.json`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`
- `Bugs.md`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `test/PipelineSpec.hs`

### Preserve Unchanged

- `orchestrator/rounds/round-090/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/verification.md`
- `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/retry-subloop.md`
- `orchestrator/rounds/round-090/selection.md`
- `orchestrator/rounds/round-090/review.md`
- `orchestrator/rounds/round-090/reviews/attempt-1.md`
- `orchestrator/rounds/round-090/attempt-log.jsonl`
- `Bugs.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`

## Sequential Retry Tasks

### Task 1 - Re-anchor `attempt-2` to exact-pocket evidence only

- Treat this document as a retry delta over rejected `attempt-1`, not as a
  fresh item-2 redesign.
- State explicitly in the canonical artifact that the rejected `attempt-1`
  public-output row was out-of-pocket because
  `test/PipelineSpec.hs:1693-1698` exercises only the unrelated unannotated
  variant `ELam "x" (EVar "x")`.
- Remove any credit path that depends on that out-of-pocket test as evidence
  for the frozen same-lane retained-child tuple.
- Keep the round docs-only and breakpoint-audit-only. This retry authorizes
  evidence repair and ledger correction only.

### Task 2 - Gather the exact-pocket replay evidence before crediting any later row

- Use `test/PipelineSpec.hs:1495-1499` as the exact frozen pocket anchor:
  `ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))`.
- Reuse `test/PipelineSpec.hs:1561-1570` only as the exact-pocket
  helper-visible recursive fact for that same expression:
  `computeResultTypeFallback` returns a type with `containsMu True`.
- Capture reviewer-visible `runPipelineElab` and `runPipelineElabChecked`
  evidence for that same exact expression, using the same `unsafeNormalizeExpr`
  entrypoint family that the live pipeline uses. The evidence must be copied
  into the canonical artifact and the round notes, not left implicit.
- Preferred evidence path: rerun the exact replay workflow already cited by
  the reviewer through `cabal repl mlf2-test`, so the retry artifact can quote
  the actual unchecked and checked outcomes for the same packet.
- The exact-pocket replay output must make one of two things review-visible:
  - both entrypoints succeed on that exact packet; or
  - one or both entrypoints fail before public output is produced, including
    the concrete Phase 6 error text if the reviewer-observed
    `PhiTranslatabilityError` still reproduces.
- Do not substitute a different expression unless it is explicitly proven to
  preserve the same family, anchor, owner-local frame, route, and
  clear-boundary status. The default and expected anchor is the frozen
  `let k ... let u ... in u` packet already named above.

### Task 3 - Rebuild the ledger in strict earliest-break order

- Preserve the six contractual rows from the frozen review ledger:
  solver admission state; elaboration handoff / result state;
  reification / reconstruction state; internal output surface;
  public output surface; reviewer-visible evidence trail.
- Keep only these row results:
  - `satisfied on current evidence`
  - `first actual continuity breakpoint`
  - `not credited after earlier breakpoint`
- Credit the solver-admission row only from exact-pocket anchors:
  - `Fallback.hs:558-735` for `boundVarTargetRoot`,
    `boundHasForallFrom`, `boundVarTarget`,
    `sameLaneLocalRetainedChildTarget`, `keepTargetFinal`, and `targetC`;
  - `PipelineSpec.hs:1495-1570` for the same exact-pocket setup.
- Audit the elaboration row next, before any reconstruction or output row,
  using:
  - `Pipeline.hs:127-144` for elaboration environment assembly and
    `elaborateWithEnv`;
  - the exact-pocket `runPipelineElab` / `runPipelineElabChecked` replay
    evidence gathered in Task 2.
- If the exact-pocket replay still fails in Phase 6 elaboration with
  `PhiTranslatabilityError`, record the elaboration handoff / result state as
  the first actual continuity breakpoint. In that case:
  - reification / reconstruction becomes `not credited after earlier breakpoint`;
  - internal output surface becomes `not credited after earlier breakpoint`;
  - public output surface becomes `not credited after earlier breakpoint`; and
  - reviewer-visible evidence trail becomes `not credited after earlier breakpoint`.
- Only if the exact-pocket replay actually survives elaboration may the audit
  continue to later rows. If that happens, later rows must still use the same
  exact packet only and may not fall back to the unannotated variant or any
  neighboring route.
- If a later row becomes the first break after exact-pocket elaboration
  succeeds, explain that break using the same tuple only and stop crediting
  every later row after it.

### Task 4 - Refresh the canonical artifact and round notes in place

- Update the canonical breakpoint-audit artifact to:
  - switch metadata from `attempt-1` / `retry: null` to `attempt-2` /
    active retry state;
  - restate the same frozen tuple unchanged;
  - include the exact-pocket `runPipelineElab` /
    `runPipelineElabChecked` replay evidence explicitly;
  - remove any earlier credit that depended on
    `PipelineSpec.hs:1693-1698`; and
  - rewrite the ledger so the first failing row matches the exact-pocket
    evidence honestly.
- If elaboration is the first break, keep the helper-visible
  `containsMu True` fact only as bounded exact-pocket context; do not use it
  to keep reconstruction, internal-output, or public-output rows marked
  `satisfied` after the earlier break.
- End the canonical artifact with one bounded summary only:
  - the frozen tuple under audit;
  - which rows remain satisfied on exact-pocket evidence;
  - the first actual continuity breakpoint;
  - the exact blocker phase / surface; and
  - the bounded handoff that item `3` remains later work only.
- Refresh `orchestrator/rounds/round-090/implementation-notes.md` so its
  change summary and verification note report the same retry facts and the
  same earliest breakpoint as the canonical artifact.
- Do not create a new canonical filename, a new notes filename, or a second
  round-local evidence packet.

### Task 5 - Run the docs-only verification needed for retry closure

Run the baseline docs/state checks required by
`orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-090/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-090/state-snapshot.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/roadmap.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `test -f docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`
- `test -f orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/retry-subloop.md`

Run retry-specific evidence checks:

- confirm the canonical artifact and notes both mention `attempt-2`,
  `runPipelineElab`, `runPipelineElabChecked`, and the exact frozen
  `let k ... let u ... in u` packet;
- confirm the canonical artifact no longer credits
  `PipelineSpec.hs:1693-1698` as public-output evidence for this pocket;
- if the replay still fails in elaboration, confirm the canonical artifact and
  notes both mention `Phase 6 (elaboration)` and `PhiTranslatabilityError`;
- confirm later ledger rows are marked `not credited after earlier breakpoint`
  whenever elaboration is the first break.

Reconfirm the diff stays docs-only:

- `git diff --name-only -- src test src-public app mlf2.cabal`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
- `git diff --name-only -- orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/roadmap.md Bugs.md orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/retry-subloop.md orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/verification.md orchestrator/rounds/round-090/state-snapshot.json`

Do not rerun `cabal build all && cabal test` as part of this retry plan. This
stage remains docs-only. The required runtime evidence here is the exact-pocket
unchecked / checked replay outcome, not a widened repo gate.

## Completion Criteria

This retry plan is complete only if all of the following become true:

1. `plan.md` is explicitly an `attempt-2` retry delta tied only to the
   recorded fix hypothesis.
2. The canonical breakpoint-audit artifact is refreshed in place and uses
   exact-pocket evidence only.
3. Reviewer-visible `runPipelineElab` / `runPipelineElabChecked` evidence for
   the frozen `let k ... let u ... in u` packet is recorded directly in the
   retry packet.
4. The first failing row in the ledger matches that exact-pocket evidence.
   If elaboration still fails first, later rows are no longer credited.
5. The round remains docs-only and item-2-only, with no code, test, roadmap,
   bug-tracker, retry-contract, or state edits.

## Non-Authorization

This retry delta does not authorize:

- any edit to `orchestrator/rounds/round-090/state-snapshot.json`;
- any edit to `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/roadmap.md`, `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/verification.md`, or
  `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-002/retry-subloop.md`;
- any edit to `Bugs.md`;
- any edit under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- any rewrite of `selection.md`, `review.md`, `reviews/attempt-1.md`, or
  `attempt-log.jsonl`;
- any implementation slice for item `3`, any end-to-end validation slice for
  item `4`, or any bounded successor decision work for item `5`;
- any widening into the alias-bound family, neighboring routes,
  nested-`forall` success, broad automatic recursive inference,
  equi-recursive reasoning, cyclic structural graphs, multi-SCC search,
  second interfaces, or fallback paths.

## Reviewer Checks For This Retry

1. `plan.md` explicitly names `attempt-2`, the retry reason, and the exact
   fix hypothesis as a delta only.
2. The retry packet uses the frozen `let k ... let u ... in u` case, not the
   out-of-pocket unannotated variant, as the public replay anchor.
3. The canonical artifact records reviewer-visible `runPipelineElab` and
   `runPipelineElabChecked` outcomes for that same exact packet.
4. If Phase 6 elaboration still fails, the canonical artifact places the
   earliest breakpoint there and stops crediting reconstruction, internal
   output, public output, and reviewer-visible evidence rows afterward.
5. The diff stays within the existing docs-only round surface and preserves
   prior retry history immutably.
