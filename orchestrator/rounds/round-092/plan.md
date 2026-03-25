# Round 092 Plan (`item-4` Retry Delta Attempt-2)

## Objective

Execute only the same-round retry for roadmap item `4`.

This rewrite replaces the rejected `attempt-1` plan with the required
`attempt-2` retry delta only. The retry scope is exactly the recorded fix
hypothesis from `orchestrator/state.json`, `orchestrator/rounds/round-092/review.md`,
`orchestrator/rounds/round-092/reviews/attempt-1.md`, and
`orchestrator/rounds/round-092/attempt-log.jsonl`:

- rewrite the canonical item-4 ledger so it uses only the exact approved
  row-result vocabulary;
- carry rows `1` and `2` explicitly as accepted predecessor evidence unless
  current exact-pocket reruns contradict that accepted record;
- extend the exact-pocket public-output freeze so one focused spec asserts the
  observed `forall identity` result for both `runPipelineElab` and
  `runPipelineElabChecked`; and
- refresh the round-local notes so they restate the round-local contract
  summary the reviewer asked for, while keeping the same bounded subject and
  the same item-4 outcome:
  `admitted but not reconstruction-visible / blocker debt`.

The canonical item-4 artifact path remains:

`docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-end-to-end-revalidation-and-classification.md`

The round remains bounded to the exact same frozen pocket only:

- family: same-lane retained-child;
- recursive-shape anchor: `boundVarTargetRoot`;
- owner / binder frame: one owner-local retained-child frame;
- route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`; and
- quantified-boundary status: clear-boundary only.

The exact frozen packet remains:

```haskell
ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
  (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
```

where
`recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`.

This retry does not reopen roadmap item `5`, any `src/` repair, the
alias-bound family, neighboring routes, nested-`forall` success, replay /
`InstBot`, architecture revision, or any broader automatic-recursive
inference claim.

## Locked Retry Context

- Round id: `round-092`
- Roadmap item: `item-4`
- Stage: `plan`
- Active attempt: `attempt-2`
- Latest attempt verdict: `rejected`
- Latest stage action: `retry`
- Retry reason:
  the attempt had the right exact-pocket runtime story, but the canonical
  ledger did not use the exact approved item-4 row-result vocabulary, and the
  new exact-pocket public-output freeze asserted the returned type only for
  `runPipelineElabChecked`, not for both public entrypoints.
- Fix hypothesis:
  keep the same bounded subject and the same blocker-debt outcome, rewrite
  the canonical ledger to use only the approved item-4 row-result shapes and
  carry rows `1` and `2` explicitly as accepted predecessor evidence unless
  contradicted, extend the exact-pocket public-output freeze so one focused
  spec asserts the observed `forall identity` result for both pipeline
  entrypoints directly, then refresh `implementation-notes.md` so it restates
  the round-local contract summary (`attempt-1`, `retry: null`, exact frozen
  tuple/packet continuity, and that item `5` remains later work only).

Carry forward without replanning:

- `orchestrator/rounds/round-092/selection.md` still fixes the round to
  roadmap item `4` only and preserves the exact frozen tuple and packet.
- `orchestrator/rounds/round-092/review.md`,
  `orchestrator/rounds/round-092/reviews/attempt-1.md`, and
  `orchestrator/rounds/round-092/attempt-log.jsonl` already record the
  rejected `attempt-1` findings and must remain immutable.
- The current exact-pocket runtime story is already the bounded factual
  baseline for this retry:
  helper-visible reconstruction remains recursive, while both public pipeline
  entrypoints return `TForall "a" Nothing (TVar "a")`.
- The retry does not search for a new outcome token. It repairs reviewer
  authority gaps around the same bounded classification:
  `admitted but not reconstruction-visible / blocker debt`.

## File Map

### Modify

- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-end-to-end-revalidation-and-classification.md`
  - Responsibility: refresh the canonical item-4 artifact in place so its
    ledger uses the exact approved row-result vocabulary and its summary keeps
    the same bounded blocker-debt read.

- `orchestrator/rounds/round-092/implementation-notes.md`
  - Responsibility: refresh the round-local notes so they restate the
    round-local contract summary the reviewer requested and mirror the same
    exact-pocket evidence and bounded outcome as the canonical artifact.

- `test/PipelineSpec.hs`
  - Responsibility: strengthen one exact-pocket public-output freeze so the
    returned type is asserted directly for both `runPipelineElab` and
    `runPipelineElabChecked`, without widening to any second packet or any
    code-path repair.

### Read-Only Evidence

- `orchestrator/rounds/round-092/selection.md`
- `orchestrator/state.json`
- `orchestrator/rounds/round-092/review.md`
- `orchestrator/rounds/round-092/reviews/attempt-1.md`
- `orchestrator/rounds/round-092/attempt-log.jsonl`
- `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-003/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-003/verification.md`
- `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-003/retry-subloop.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-phase-6-elaboration-resolution.md`
- `orchestrator/rounds/round-089/review-record.json`
- `orchestrator/rounds/round-090/review-record.json`
- `orchestrator/rounds/round-091/review-record.json`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `Bugs.md`

### Preserve Unchanged

- `orchestrator/state.json`
- `orchestrator/roadmap.md`
- `orchestrator/retry-subloop.md`
- `orchestrator/verification.md`
- `Bugs.md`
- `src/`
- `src-public/`
- `app/`
- `mlf2.cabal`
- reviewer-owned history under earlier round directories

## Sequential Retry Tasks

### Task 1 - Re-anchor `attempt-2` to the exact reviewer findings only

- Treat this plan as a retry delta over rejected `attempt-1`, not as a fresh
  item-4 redesign.
- Keep the same frozen tuple, the same frozen packet, and the same bounded
  output token:
  `admitted but not reconstruction-visible / blocker debt`.
- State explicitly in the canonical artifact and the round-local notes that
  item `4` still does not decide item `5`; item `5` remains later work only.
- Preserve the accepted predecessor continuity exactly:
  - item `1` froze the pocket and six-row ledger;
  - item `2` localized the first earlier breakpoint to `Phase 6
    (elaboration)`; and
  - item `3` cleared that exact `Phase 6 (elaboration)` breakpoint for the
    same pocket.
- Do not spend retry scope on any new runtime theory. This retry exists only
  to make the already-observed item-4 blocker-debt read authoritative under
  the approved review schema.

### Task 2 - Rewrite the six-row ledger with the exact approved item-4 row-result vocabulary

- Rebuild the canonical item-4 ledger using only these row-result shapes:
  - `satisfied on accepted predecessor evidence`
  - `satisfied on current exact-pocket evidence`
  - `first actual continuity breakpoint`
  - `not credited after earlier breakpoint`
- Remove the softened attempt-1 row-result phrases entirely:
  - `satisfied on current evidence`
  - `satisfied on current helper-visible evidence`
  - `first remaining continuity break`
  - `not satisfied after public-output break`
- Carry rows `1` and `2` explicitly as
  `satisfied on accepted predecessor evidence` unless the current exact-pocket
  reruns contradict the accepted item-2 or item-3 record. The expected rows
  are:
  - solver admission state:
    `satisfied on accepted predecessor evidence`
  - elaboration handoff / result state:
    `satisfied on accepted predecessor evidence`
- Re-state rows `3` and `4` using current exact-pocket evidence only. If the
  helper-visible replay still shows the same `TMu`-bearing reconstruction and
  `containsMu True`, the expected rows are:
  - reification / reconstruction state:
    `satisfied on current exact-pocket evidence`
  - internal output surface:
    `satisfied on current exact-pocket evidence`
- Use the public-output replay as the decisive remaining break. If both
  public entrypoints still return `TForall "a" Nothing (TVar "a")`, the
  expected later rows are:
  - public output surface:
    `first actual continuity breakpoint`
  - reviewer-visible evidence trail:
    `not credited after earlier breakpoint`
- Keep the same blocker-debt outcome tied to that exact ledger shape. Do not
  invent a different row order, a different row vocabulary, or a different
  outcome token inside this retry.

### Task 3 - Freeze the authoritative public-output result for both public pipeline entrypoints

- Modify only the focused exact-pocket public-output regression in
  `test/PipelineSpec.hs`.
- Keep one focused spec on the same frozen packet only. The retry should
  strengthen the existing public-output freeze rather than add a new packet or
  a broader regression cluster.
- The strengthened spec must assert the returned authoritative type directly
  for both entrypoints:
  - `runPipelineElab`
  - `runPipelineElabChecked`
- The expected authoritative public-output result to freeze is:

  ```haskell
  TForall "a" Nothing (TVar "a")
  ```

- Do not settle for pass/fail-only coverage. The spec must prove that both
  returned types collapse to the same `forall identity` result on the exact
  frozen packet.
- Do not modify `src/`, `test/ElaborationSpec.hs`, or any neighboring test
  packet for this retry. One focused public-output freeze is the full test
  surface authorized here.
- Copy the same two-entrypoint public-output fact into the canonical artifact
  and the round-local notes so review can see the same result on every
  surface.

### Task 4 - Refresh the round-local contract summary in place

- Refresh `orchestrator/rounds/round-092/implementation-notes.md` so it
  explicitly restates the round-local contract summary the reviewer requested.
- The notes must no longer omit the frozen packet / tuple continuity. They
  should state, in one place, all of the following:
  - the round is repairing authoritative item-4 evidence for the same pocket
    only;
  - the contract summary under repair includes `attempt-1` and `retry: null`;
  - the exact frozen packet and unchanged tuple;
  - the accepted item-1 / item-2 / item-3 continuity;
  - the helper-visible internal recursive fact
    (`TMu`-bearing reconstruction plus `containsMu True`);
  - the authoritative public-output result for both
    `runPipelineElab` and `runPipelineElabChecked`:
    `TForall "a" Nothing (TVar "a")`;
  - the bounded item-4 outcome token:
    `admitted but not reconstruction-visible / blocker debt`; and
  - the explicit note that item `5` remains later work only.
- Refresh the canonical artifact summary so it matches the same bounded
  subject and the same blocker-debt outcome, then make the notes mirror that
  summary exactly.
- Do not create a new artifact filename or a second round-local notes file.

### Task 5 - Run verification matched to the bounded retry diff

Run the baseline control-plane checks required by the live roadmap bundle:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`

Run the focused retry gates required by the actual diff:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- `cabal build all && cabal test`

Run reviewer-facing artifact checks for the exact findings:

- confirm the canonical artifact uses only the approved ledger row-result
  vocabulary:
  `rg -n 'satisfied on accepted predecessor evidence|satisfied on current exact-pocket evidence|first actual continuity breakpoint|not credited after earlier breakpoint' docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-end-to-end-revalidation-and-classification.md`
- confirm the softened attempt-1 row-result phrases are gone:
  `! rg -n 'satisfied on current evidence|satisfied on current helper-visible evidence|first remaining continuity break|not satisfied after public-output break' docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-end-to-end-revalidation-and-classification.md`
- confirm the round-local contract summary is present in the notes:
  `rg -n 'attempt-1|retry: null|sameLaneLocalRetainedChildTarget|keepTargetFinal|targetC|item `5` remains later work only|admitted but not reconstruction-visible / blocker debt|TForall "a" Nothing \\(TVar "a"\\)' orchestrator/rounds/round-092/implementation-notes.md`
- confirm the public-output spec now asserts both public entrypoints directly:
  `rg -n 'runPipelineElab Set.empty|runPipelineElabChecked Set.empty|TForall "a" Nothing \\(TVar "a"\\)' test/PipelineSpec.hs`

Reconfirm the bounded diff surface:

- `git diff --name-only -- src test src-public app mlf2.cabal`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
- `git diff --name-only -- orchestrator/state.json orchestrator/roadmap.md orchestrator/retry-subloop.md orchestrator/verification.md Bugs.md implementation_notes.md`

## Retry Limits

- If the exact-pocket rerun no longer reproduces the accepted item-3
  elaboration clearance or no longer reproduces the observed two-entrypoint
  public-output collapse to `TForall "a" Nothing (TVar "a")`, stop and report
  the contradiction rather than widening the subject or inventing a new
  outcome inside this retry.
- If the fix starts requiring a `src/` change, a second test packet, a new
  ledger vocabulary, or an item-5 decision, stop. Those are outside this
  retry delta.
- This retry is complete only when the canonical artifact, the focused public
  output spec, and the round-local notes all tell the same bounded story for
  the same pocket and the same blocker-debt outcome.
