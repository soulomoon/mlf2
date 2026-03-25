# Round 095 Plan (`item-2` Retry Delta Attempt-2)

## Objective

Execute only the same-round retry for roadmap item `2`.

This rewrite replaces the rejected `attempt-1` plan with the required
`attempt-2` retry delta only. The retry scope is exactly the recorded fix
hypothesis from `orchestrator/state.json`,
`orchestrator/rounds/round-095/review.md`,
`orchestrator/rounds/round-095/reviews/attempt-1.md`, and
`orchestrator/rounds/round-095/attempt-log.jsonl`:

- rewrite `orchestrator/rounds/round-095/implementation-notes.md` so it
  explicitly restates the exact frozen packet;
- restate the exact same-lane retained-child tuple
  (`boundVarTargetRoot`, one owner-local retained-child frame,
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  clear-boundary only);
- restate the exact helper-visible/internal versus authoritative public
  split (`TMu ...` plus `containsMu True` versus
  `TForall "a" Nothing (TVar "a")`); and
- restate the unchanged first exact owner-local continuity-loss-site
  conclusion:
  `checkedAuthoritative` remains the first break, with `termClosed` and
  `typeCheck termClosed` as the same-pocket dependencies that feed that
  authoritative result.

The canonical item-2 artifact remains:

`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`

The bounded item-2 conclusion also remains unchanged:

- helper-visible/internal reconstruction still preserves recursive structure
  as `TMu ...` plus `containsMu True`;
- both authoritative public entrypoints still return
  `TForall "a" Nothing (TVar "a")`; and
- the first exact owner-local continuity-loss site remains the
  `checkedAuthoritative` return choice.

This retry does not authorize a new audit theory, any code or test edit, any
roadmap or controller-state edit, any `Bugs.md` edit, or any widening beyond
the same exact item-2 docs-only audit.

## Locked Retry Context

- Round id: `round-095`
- Roadmap item: `item-2`
- Stage: `plan`
- Active attempt: `attempt-2`
- Latest attempt verdict: `rejected`
- Latest stage action: `retry`
- Retry reason:
  `implementation-notes.md` did not restate the exact frozen packet, the
  exact same-lane retained-child tuple, or the exact helper-visible/internal
  versus authoritative public split that the approved plan required both the
  canonical artifact and the round notes to carry.
- Fix hypothesis:
  keep the same bounded item-2 subject and the same unchanged-anchor
  conclusion, but rewrite `orchestrator/rounds/round-095/implementation-notes.md`
  so it mirrors the canonical artifact's exact packet, tuple, split, and
  unchanged `checkedAuthoritative` / `termClosed` / `typeCheck termClosed`
  continuity-loss-site conclusion without widening scope.

Carry forward without replanning:

- `orchestrator/rounds/round-095/selection.md` still fixes the round to
  roadmap item `2` only and preserves the exact same frozen pocket.
- `orchestrator/rounds/round-095/review.md`,
  `orchestrator/rounds/round-095/reviews/attempt-1.md`, and
  `orchestrator/rounds/round-095/attempt-log.jsonl` already record the
  rejected `attempt-1` findings and must remain immutable.
- The canonical item-2 artifact already satisfied the item-2 localization,
  predecessor-continuity, boundary-continuity, focused-gate, and docs-only
  checks in the rejected review attempt.
- The retry exists only to repair the round-local notes so they mirror that
  same bounded authoritative-path audit exactly.

## Frozen Subject That Remains Binding

The round remains bounded to the exact same frozen pocket only:

- family: same-lane retained-child;
- recursive-shape anchor: `boundVarTargetRoot`;
- owner / binder frame: one owner-local retained-child frame;
- route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`;
- quantified-boundary status: clear-boundary only; and
- exact packet:

  ```haskell
  ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
  ```

  where
  `recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`.

The inherited boundary also remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized;
- no second interface is authorized; and
- no compatibility, convenience, or fallback widening is authorized.

Accepted `N14`, accepted strategic items `2`, `5`, `6`, and `7`, plus
accepted rounds `089` through `094`, remain bounded predecessor evidence
only. This retry does not relitigate them.

## File Map

### Modify

- `orchestrator/rounds/round-095/implementation-notes.md`
  - Responsibility: repair the reviewer-identified notes-only contract miss
    so the round-local notes mirror the already-accepted canonical item-2
    audit facts exactly.

### Read-Only Evidence

- `orchestrator/rounds/round-095/selection.md`
- `orchestrator/state.json`
- `orchestrator/rounds/round-095/review.md`
- `orchestrator/rounds/round-095/reviews/attempt-1.md`
- `orchestrator/rounds/round-095/attempt-log.jsonl`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/verification.md`
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/retry-subloop.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-phase-6-elaboration-resolution.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-end-to-end-revalidation-and-classification.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-successor-decision-gate.md`
- `orchestrator/rounds/round-089/review-record.json`
- `orchestrator/rounds/round-090/review-record.json`
- `orchestrator/rounds/round-091/review-record.json`
- `orchestrator/rounds/round-092/review-record.json`
- `orchestrator/rounds/round-093/review-record.json`
- `orchestrator/rounds/round-094/review-record.json`

### Preserve Unchanged

- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
- `orchestrator/state.json`
- `orchestrator/roadmap.md`
- `orchestrator/retry-subloop.md`
- `orchestrator/verification.md`
- `Bugs.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- accepted predecessor docs and review records
- reviewer-owned history under earlier round directories

## Exact Retry Delta (Exactly One)

The only retry delta is:

repair the round-local notes so they restate the same exact item-2 audit facts
that the canonical artifact already carries.

This retry delta is allowed to:

- rewrite `orchestrator/rounds/round-095/implementation-notes.md`;
- copy the exact frozen packet, tuple, split, and unchanged-anchor
  conclusion from the canonical item-2 artifact into the round-local notes;
- restate that the round remains docs-only, item-2-only, and bounded to the
  same frozen pocket; and
- restate that roadmap item `3` remains later work only.

This retry delta is not allowed to:

- rewrite the canonical item-2 artifact unless a direct factual mismatch is
  discovered while mirroring the notes;
- change the bounded unchanged-anchor conclusion;
- introduce a new continuity-loss site;
- run or authorize a new implementation, proof slice, revalidation, or
  architecture decision;
- widen into alias-bound, neighboring-route, nested-`forall`, replay /
  `InstBot`, or broad capability work; or
- edit controller-owned state, roadmap contracts, retry contracts,
  verification contracts, `Bugs.md`, or code/test/public-surface files.

## Sequential Retry Tasks

### Task 1 - Re-anchor `attempt-2` to the exact review finding only

- Treat this plan as a retry delta over rejected `attempt-1`, not as a fresh
  item-2 redesign.
- Carry forward unchanged:
  - the canonical item-2 artifact path;
  - the exact frozen packet and tuple;
  - the exact `TMu ...` plus `containsMu True` versus
    `TForall "a" Nothing (TVar "a")` split; and
  - the unchanged conclusion that `checkedAuthoritative` remains the first
    exact owner-local continuity-loss site, with `termClosed` and
    `typeCheck termClosed` as same-pocket dependencies.
- State explicitly that this retry repairs reviewer authority gaps in the
  notes only. It does not reopen the audit itself.

### Task 2 - Rewrite `implementation-notes.md` so it mirrors the canonical artifact exactly

- Refresh `orchestrator/rounds/round-095/implementation-notes.md` in place.
- Make the notes explicitly mention, in one bounded summary:
  - the exact frozen packet:
    `ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))`;
  - the exact same-lane retained-child tuple:
    `boundVarTargetRoot`,
    one owner-local retained-child frame,
    `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
    clear-boundary only;
  - the exact split:
    helper-visible/internal `TMu ...` plus `containsMu True`
    versus authoritative public `TForall "a" Nothing (TVar "a")`;
  - the unchanged continuity-loss-site conclusion:
    `checkedAuthoritative` remains the first break, with `termClosed` and
    `typeCheck termClosed` as the same-pocket dependencies that feed that
    authoritative result; and
  - the bounded handoff:
    roadmap item `3` remains later work only.
- Keep the notes aligned with the canonical artifact's current factual
  conclusion. Do not invent any new wording that changes meaning or scope.

### Task 3 - Run notes-focused verification only

Run the minimal verification needed for this notes-only retry delta:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- confirm the revised notes now carry the exact required anchors:
  `rg -n 'ELet \"k\"|boundVarTargetRoot|sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC|TMu \\.\\.\\.|containsMu True|TForall \"a\" Nothing \\(TVar \"a\"\\)|checkedAuthoritative|termClosed|typeCheck termClosed|item `3` remains later work only' orchestrator/rounds/round-095/implementation-notes.md`
- confirm the notes mirror the canonical artifact rather than drift from it:
  `rg -n 'ELet \"k\"|boundVarTargetRoot|sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC|TMu \\.\\.\\.|containsMu True|TForall \"a\" Nothing \\(TVar \"a\"\\)|checkedAuthoritative|termClosed|typeCheck termClosed' docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md orchestrator/rounds/round-095/implementation-notes.md`
- confirm the retry diff stays notes-only from the implementer side:
  `git diff --name-only -- docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
  and
  `git diff --name-only -- src test src-public app mlf2.cabal Bugs.md orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/roadmap.md orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/retry-subloop.md orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/verification.md`

Do not rerun the focused `cabal test` commands in this retry delta unless the
diff escapes the authorized notes-only surface. Rejected `attempt-1` already
recorded those focused gates as passing, and this retry does not alter the
runtime-facing artifacts those tests exercise.

## Completion Criteria

This retry-delta plan is complete only if all of the following become true:

1. `plan.md` explicitly names `attempt-2` and describes a retry delta only
   for the recorded fix hypothesis.
2. `implementation-notes.md` restates the exact frozen packet, the exact
   same-lane retained-child tuple, the exact internal/public split, and the
   unchanged `checkedAuthoritative` / `termClosed` / `typeCheck termClosed`
   conclusion.
3. The canonical item-2 artifact remains unchanged as the bounded audit
   baseline unless a direct factual mismatch is discovered.
4. The retry diff stays limited to the round-local notes from the
   implementer-owned surface.
5. The retry does not widen beyond the same exact item-2 docs-only audit or
   authorize any repair, revalidation, or architecture-decision work.

## Non-Authorization

This retry-delta plan does not authorize:

- any edit to `orchestrator/state.json`;
- any edit to the active roadmap bundle files;
- any edit to `Bugs.md`;
- any edit under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- any rewrite of `selection.md`, `review.md`, `reviews/attempt-1.md`, or
  `attempt-log.jsonl`;
- any rewrite of the canonical audit artifact unless an exact factual
  mismatch is discovered while mirroring the notes;
- any new item-3 repair, item-4 revalidation, or item-5 architecture
  decision work; or
- any widening into the alias-bound family, neighboring routes,
  nested-`forall` success, broad automatic recursive inference,
  equi-recursive reasoning, cyclic structural graphs, multi-SCC search,
  second interfaces, or fallback paths.

## Reviewer Checks For This Retry

1. `plan.md` explicitly names `attempt-2`, the rejected `attempt-1` retry
   reason, and the exact notes-only fix hypothesis.
2. The file map modifies only `orchestrator/rounds/round-095/implementation-notes.md`
   on the implementer-owned surface.
3. The sequential tasks keep the exact frozen packet, tuple, split, and
   unchanged-anchor conclusion fixed while repairing only the notes.
4. The verification section checks the exact note anchors and confirms the
   diff stayed notes-only.
5. The plan preserves the same bounded item-2 subject and does not silently
   widen into repair or `non-cyclic-graph` reopen work.
