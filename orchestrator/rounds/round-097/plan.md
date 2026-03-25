# Round 097 Plan (`item-4` End-To-End Revalidation And Classification)

## Objective

Execute only roadmap item `4` and produce one refreshed canonical artifact at:
`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`.

This is the initial `item-4` plan for `attempt-1` with `retry: null`. The
round must rerun only the exact frozen same-lane retained-child pocket end
to end after accepted item `3` confirmed that the bounded
`Pipeline.hs` / `TermClosure.hs` root-handoff slice does not expose an
alternate recursive whole-packet authoritative result.

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

The round must record exactly one lawful item-4 outcome token for that same
exact pocket only:

- `stable visible persistence`;
- `admitted but not reconstruction-visible / blocker debt`; or
- `fail-closed rejection`.

Current planning read: because accepted item `3` just confirmed that the
bounded root-handoff slice still ends at
`TForall "a" Nothing (TVar "a")`, the expected strongest lawful item-4
outcome remains
`admitted but not reconstruction-visible / blocker debt`
unless fresh exact-pocket reruns contradict the accepted record. If the
current reruns do contradict the accepted record, fail closed inside this
same pocket and record the exact contradiction as item-4 evidence rather than
widening into repair work or architecture revision.

This round is docs-first and evidence-first. It may consume existing focused
tests and read-only exact-pocket replay harnesses as evidence, but it must
not repair runtime behavior, rewrite the roadmap, or reopen
`non-cyclic-graph`.

## Locked Round Context

- Round id: `round-097`
- Roadmap item: `item-4`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Active branch: `codex/round-097`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-097`
- Fixed live subject: one end-to-end revalidation / classification pass for
  the exact same-lane retained-child public-output continuity pocket only
- Current round review feedback: none yet; no `review.md`,
  `reviews/attempt-<n>.md`, or `review-record.json` exists for `round-097`,
  so this is a full first-attempt plan rather than a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `M orchestrator/state.json`
- `?? orchestrator/rounds/round-097/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-097/selection.md`
  fixes the round to roadmap item `4` only, keeps the subject on the exact
  frozen same-lane retained-child pocket, and forbids widening into a
  `non-cyclic-graph` decision during this round.
- `orchestrator/state.json`
  fixes the live controller state at `active_round_id = round-097`,
  `stage = plan`, `current_task = item-4`, `retry = null`,
  `roadmap_id = 2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap`,
  `roadmap_revision = rev-001`, and the authoritative
  `roadmap_dir = orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001`.
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/roadmap.md`
  makes item `4` the lowest-numbered unfinished dependency-satisfied item
  and defines its completion notes as one exact-pocket end-to-end
  revalidation / classification pass after the accepted item-3 confirm-only
  result.
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/retry-subloop.md`
  confirms that item `4` may retry in principle, but `retry: null` means
  this plan is the full first-attempt plan.
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/verification.md`
  requires item-4-specific checks proving that the round records solver,
  elaboration, reconstruction, internal output, public output, and
  reviewer-visible evidence for this same frozen pocket only, while using
  exactly one lawful item-4 outcome token.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  preserves the inherited boundary:
  explicit-only recursive behavior remains the production baseline;
  recursive meaning remains iso-recursive only; and non-equi-recursive,
  non-cyclic-graph, no-second-interface, and no-fallback boundaries remain
  binding.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
  freezes the refreshed exact same pocket, the exact helper-visible/internal
  versus authoritative public split, and the refreshed review ledger that
  this round must now revalidate.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
  keeps the accepted unchanged-anchor audit:
  helper-visible/internal reconstruction still carries `TMu ...` plus
  `containsMu True`, both authoritative public entrypoints still return
  `TForall "a" Nothing (TVar "a")`, and `checkedAuthoritative` remains the
  first exact owner-local break, with `termClosed` and
  `typeCheck termClosed` as the same-pocket dependencies.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
  confirms that the bounded `Pipeline.hs` / `TermClosure.hs` root-handoff
  slice contains no alternate recursive whole-packet authoritative result
  for this exact pocket, so the public-output collapse remains blocker debt
  within the unchanged current architecture.
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-end-to-end-revalidation-and-classification.md`
  and `orchestrator/rounds/round-092/review-record.json`
  remain the immediate predecessor item-4 classification evidence for the
  same exact pocket. This refreshed round must preserve that predecessor
  truth unless fresh exact-pocket reruns now contradict it.
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-successor-decision-gate.md`
  and `orchestrator/rounds/round-093/review-record.json`
  remain the immediate predecessor item-5 bounded successor decision:
  blocker debt remained within the current architecture on the prior exact
  accepted evidence, and this round must not relitigate that decision.
- Accepted `N14`, accepted strategic items `2`, `5`, `6`, and `7`, plus
  accepted rounds `089` through `096`, remain bounded predecessor evidence
  only. They do not authorize widening into the non-local alias-bound
  family, neighboring routes, nested-`forall`, replay / `InstBot` repair,
  broad automatic-recursive-inference claims, or a reopened
  `non-cyclic-graph` decision inside item `4`.
- `Bugs.md`
  still lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect
  remains predecessor context only. It does not authorize diverting this
  round away from the same exact-pocket revalidation gate.

## File Map

### Create

- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`
  - Responsibility: canonical refreshed item-4 artifact recording the
    exact-pocket six-row end-to-end ledger, the current exact-pocket replay
    evidence, and exactly one lawful item-4 outcome token after accepted
    item `3`.

- `orchestrator/rounds/round-097/implementation-notes.md`
  - Responsibility: concise round-local notes mirroring the same frozen
    tuple, exact packet, evidence sources, outcome token, and the explicit
    note that item `5` remains later work only.

### Read-Only Evidence

- `orchestrator/rounds/round-097/selection.md`
- `orchestrator/state.json`
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
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
- `orchestrator/rounds/round-092/review-record.json`
- `orchestrator/rounds/round-093/review-record.json`
- `orchestrator/rounds/round-094/review-record.json`
- `orchestrator/rounds/round-095/review-record.json`
- `orchestrator/rounds/round-096/review-record.json`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/TermClosure.hs`
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
- `test/`
- `mlf2.cabal`
- accepted predecessor docs and review records
- reviewer-owned history under earlier round directories

## Exact Selected Slice (Exactly One)

The only selected slice is:

refresh the exact-pocket end-to-end ledger and classify the current
public-output continuity result after the accepted item-3 confirm-only
artifact.

This slice is allowed to:

- create one refreshed canonical item-4 artifact for the 2026-03-26
  public-output continuity successor family;
- create one round-local `implementation-notes.md` that mirrors the same
  bounded subject and outcome;
- rerun the existing exact-pocket evidence through read-only commands,
  focused exact-pocket tests, and the already-frozen helper-visible/public
  harnesses; and
- record exactly one lawful exact-pocket item-4 outcome token with item `5`
  left as later work only.

This slice is not allowed to:

- repair runtime behavior, author a new implementation slice, or reopen item
  `3`;
- edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edit controller-owned state, roadmap contracts, retry contracts,
  verification contracts, or `Bugs.md`;
- widen into the non-local alias-bound family, neighboring consumer routes,
  nested-`forall`, replay / `InstBot`, broad automatic recursive inference,
  equi-recursive reasoning, cyclic structural graphs, multi-SCC search,
  second-interface work, or fallback widening; or
- reopen `non-cyclic-graph`; only roadmap item `5` may consume the accepted
  item-4 outcome and decide that bounded architecture-pressure question.

## Sequential Tasks

### Task 1 - Re-anchor refreshed item `4` to the accepted refreshed chain only

- Treat this as a fresh item-4 revalidation round for the refreshed
  2026-03-26 public-output continuity family, not as a repair round and not
  as an architecture-decision round.
- Carry forward unchanged:
  - the exact frozen tuple and packet from accepted items `1`, `2`, and `3`;
  - the exact continuity split:
    helper-visible/internal `TMu ...` plus `containsMu True` versus
    authoritative public `TForall "a" Nothing (TVar "a")`;
  - the exact unchanged blocker anchor:
    `checkedAuthoritative` remains the first exact owner-local break, with
    `termClosed` and `typeCheck termClosed` as the same-pocket dependencies;
    and
  - the accepted item-3 confirm-only result:
    the bounded `Pipeline.hs` / `TermClosure.hs` root-handoff slice contains
    no alternate recursive whole-packet authoritative result for this
    pocket.
- State explicitly in both artifact surfaces that item `4` records only the
  refreshed end-to-end classification for this pocket and that item `5`
  remains later work only.

### Task 2 - Rerun only the exact-pocket evidence with read-only harnesses

- Reuse the exact frozen same-lane retained-child harness already anchored in
  `test/PipelineSpec.hs`; do not create a new packet, new route, or new
  family.
- Gather fresh reviewer-visible evidence for the same exact pocket from:
  - a read-only exact-pocket replay through `cabal repl mlf2-test` or the
    equivalent existing harness already used in predecessor item-4 work, so
    the round records the helper-visible internal result, `containsMu`, and
    both authoritative public outputs for the exact frozen packet; and
  - the existing focused exact-pocket tests only:
    - `keeps retained-child fallback recursive through a same-lane local TypeRef root`
    - `same-lane retained-child exact packet clears Phase 6 elaboration`
    - `same-lane retained-child exact packet authoritative public output stays forall identity`
    - `same-lane retained-child exact edge 3 authoritative instantiation`
    - `ARI-C1 feasibility characterization (bounded prototype-only)`
- Treat the test and replay outputs as evidence only. Do not change the code
  or tests even if the rerun still shows blocker debt; the purpose here is
  classification, not repair.

### Task 3 - Rebuild the six-row end-to-end ledger for the refreshed family

- Write the refreshed canonical item-4 artifact so it records the same exact
  pocket across these six rows only:
  - solver admission state
  - elaboration handoff / result state
  - reification / reconstruction state
  - internal output surface
  - public output surface
  - reviewer-visible evidence trail
- Use the same approved row-result vocabulary already established by the
  predecessor item-4 artifact:
  - `satisfied on accepted predecessor evidence`
  - `satisfied on current exact-pocket evidence`
  - `first actual continuity breakpoint`
  - `not credited after earlier breakpoint`
- Apply that vocabulary narrowly:
  - rows `1` and `2` should stay
    `satisfied on accepted predecessor evidence`
    unless fresh exact-pocket reruns contradict accepted items `1` through
    `3`;
  - rows `3` and `4` should use
    `satisfied on current exact-pocket evidence`
    only if the fresh exact-pocket replay still shows retained-child
    reconstruction and helper-visible recursive structure for the same
    pocket;
  - row `5` should identify the first actual continuity breakpoint if the
    public output still collapses to
    `TForall "a" Nothing (TVar "a")`; and
  - row `6` should credit reviewer-visible continuity only if no earlier row
    already broke it.
- Keep every continuity note pinned to the same exact family, anchor, owner
  frame, route, and clear-boundary-only status. Do not substitute any
  neighboring route, witness-only explanation, replay-only rescue, or
  architecture theory.

### Task 4 - Record exactly one lawful item-4 outcome token

- Choose exactly one item-4 outcome token from the three roadmap-approved
  values only:
  - `stable visible persistence`
  - `admitted but not reconstruction-visible / blocker debt`
  - `fail-closed rejection`
- Use the following bounded decision rule:
  - if the fresh exact-pocket rerun still shows helper-visible recursive
    structure but both authoritative public entrypoints still return
    `TForall "a" Nothing (TVar "a")`, record
    `admitted but not reconstruction-visible / blocker debt`;
  - if the exact same pocket now preserves recursive structure on the
    authoritative public surface itself, record
    `stable visible persistence`;
  - if the fresh exact-pocket rerun now fails earlier than the public-output
    surface for this same pocket, record
    `fail-closed rejection`.
- Record the chosen outcome in both the canonical artifact and the round
  notes, with an explicit statement that item `5` remains later work only
  and that this round does not reopen `non-cyclic-graph`.

### Task 5 - Mirror the bounded result in round-local notes only

- Write `orchestrator/rounds/round-097/implementation-notes.md` as a concise
  round-local mirror of the canonical artifact.
- The notes must explicitly restate, in one place:
  - the exact frozen tuple and packet;
  - the accepted item-1 / item-2 / item-3 continuity;
  - the fresh evidence sources actually rerun in this round;
  - the helper-visible/internal versus authoritative public split observed in
    this round; and
  - the exact chosen item-4 outcome token.
- The notes must stay round-local only. Do not create additional side files,
  alternate ledgers, or extra decision documents in this round.

## Verification Handoff

The implementer should leave reviewer-visible evidence for all of the
following:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- predecessor-presence checks required by the active verification contract
- the exact-pocket replay command used for this round, with reviewer-visible
  output for the helper-visible internal result, `containsMu`, and both
  authoritative public outputs
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- `git diff --name-only -- src test src-public app mlf2.cabal`
  - expected result: no output; this round is docs-only unless the plan is
    violated

Because this plan preserves `src/`, `src-public/`, `app/`, `test/`, and
`mlf2.cabal` unchanged, `cabal build all && cabal test` is out of scope for
this round unless the diff escapes the authorized docs / orchestrator
artifact surface.

## Failure Discipline

If fresh exact-pocket evidence does not support the expected blocker-debt
classification, fail closed inside this exact same pocket:

- record the contradiction in the refreshed canonical artifact and the round
  notes;
- choose the lawful item-4 outcome token that the exact-pocket evidence now
  supports;
- keep item `5` as later work only; and
- do not improvise a repair round, new runtime theory, or architecture
  decision inside this packet.
