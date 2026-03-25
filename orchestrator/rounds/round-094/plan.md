# Round 094 Plan (`item-1` Public-Output Continuity Case Freeze And Review Ledger)

## Objective

Execute only roadmap item `1` and produce one docs-first artifact at:
`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`.

This is the initial `item-1` plan for `attempt-1` with `retry: null`. The
round must freeze exactly one same-lane retained-child pocket from the
accepted predecessor record:

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

The artifact must:

- freeze exactly that one pocket as the only live subject for this refreshed
  successor loop;
- freeze the exact continuity split now under review for that same pocket:
  helper-visible/internal recursive structure
  (`TMu ...`, `containsMu True`) versus authoritative public output
  `TForall "a" Nothing (TVar "a")`;
- freeze one review ledger for that same pocket only across solver admission,
  elaboration handoff / result, reification / reconstruction, internal
  output, public output, and reviewer-visible evidence;
- bind the accepted predecessor chain for this refreshed family:
  accepted `N14`, accepted strategic items `2`, `5`, `6`, and `7`, and
  accepted rounds `089` through `093`; and
- exclude neighboring routes, the non-local alias-bound family,
  nested-`forall`, replay repair, broad automatic-recursive-inference claims,
  and architecture revision.

This round is docs-only and contract-freeze-only. It must not edit production
code, tests, public surfaces, executables, `mlf2.cabal`, controller-owned
state, roadmap contracts, retry contracts, verification contracts, `Bugs.md`,
or any predecessor review / merge history.

This round must not silently widen into:

- the item-2 authoritative public-output path audit;
- the item-3 minimum bounded implementation / proof slice;
- the item-4 end-to-end revalidation / classification slice;
- the item-5 bounded architecture-pressure decision slice;
- the alias-bound family, neighboring consumer routes, or nested-`forall`
  success;
- equi-recursive reasoning, cyclic structural graphs, multi-SCC search,
  second interfaces, or fallback behavior; or
- a reopened `non-cyclic-graph` revision argument.

Current planning read: item `1` is a freeze-the-ledger round only. If the
accepted record does not justify a stronger statement, the artifact must fail
closed by freezing only predecessor-established truth and by naming anything
else as later-round work.

## Locked Round Context

- Round id: `round-094`
- Roadmap item: `item-1`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: one docs-only freeze of the exact same-lane
  retained-child public-output continuity case and review ledger
- Active branch: `codex/round-094`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-094`
- Current round review feedback: none yet; no `review.md`,
  `reviews/attempt-<n>.md`, or `review-record.json` exists for `round-094`,
  so this is a full `attempt-1` plan rather than a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `?? orchestrator/rounds/round-094/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-094/selection.md`
  is the stage authority for this round. It fixes the live subject to
  roadmap item `1` only, names the exact same pocket and the exact
  internal/public split, and limits the output to one refreshed case freeze
  plus one review ledger.
- `orchestrator/state.json`
  fixes the live controller state at `active_round_id = round-094`,
  `stage = plan`, `current_task = item-1`, `retry = null`,
  `roadmap_id = 2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap`,
  `roadmap_revision = rev-001`, and the authoritative
  `roadmap_dir = orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001`.
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/roadmap.md`
  makes item `1` the first unfinished item and defines its completion notes
  as one exact public-output continuity case freeze and review ledger only.
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/retry-subloop.md`
  confirms that item `1` may retry in principle, but `retry: null` means
  this plan is the full first-attempt plan.
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/verification.md`
  requires item-1-specific checks proving that the round freezes exactly one
  same-lane retained-child pocket, the exact helper-visible/public split, and
  the accepted predecessor chain without widening the subject.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  preserves the inherited boundary:
  explicit-only recursive behavior remains the production baseline;
  recursive meaning remains iso-recursive only; and non-equi-recursive,
  non-cyclic-graph, no-second-interface, and no-fallback boundaries remain
  binding.
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  remains the predecessor bounded packet. It preserves one exact same-lane
  retained-child route as bounded evidence only and does not authorize
  widening or architecture revision.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  keeps `non-cyclic-graph = unknown` as architecture-pressure context only.
  Item `1` must not reopen that question.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  supplies the controlling phase-and-surface ledger and outcome vocabulary.
  Item `1` must freeze the same kind of ledger, but now centered on the
  helper-visible/internal versus authoritative public-output continuity split
  for the selected pocket.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  still keeps the repo-level posture at bounded-subset-only feasibility and
  does not upgrade the same-lane retained-child family to broad accepted
  success.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  still records `continue within the current architecture` as the strongest
  lawful strategic read before this refreshed loop.
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`
  freezes the predecessor same pocket and six-row ledger; this round must
  treat it as bounded predecessor evidence and refresh the freeze around the
  narrower public-output continuity question, not discard it.
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`
  localizes the first earlier exact-pocket break to `Phase 6 (elaboration)`.
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-phase-6-elaboration-resolution.md`
  clears that exact elaboration breakpoint for the same frozen pocket.
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-end-to-end-revalidation-and-classification.md`
  freezes the exact current internal/public split:
  helper-visible/internal `TMu ...` plus `containsMu True`, versus
  authoritative public output
  `TForall "a" Nothing (TVar "a")`.
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-successor-decision-gate.md`
  records the accepted bounded successor decision
  `blocker debt remains within the current architecture`, explicitly
  refusing to reopen `non-cyclic-graph` on weaker evidence.
- `orchestrator/rounds/round-093/review-record.json`
  makes that round-093 item-5 result authoritative for predecessor
  continuity.
- `Bugs.md`
  still lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect
  remains predecessor context only. It does not authorize replay repair,
  subject widening, or architecture revision in item `1`.

## File Map

### Create

- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
  - Responsibility: canonical docs-only item-1 artifact freezing exactly one
    same-lane retained-child public-output continuity case, the exact
    helper-visible/internal versus authoritative public split, and the review
    ledger that later rounds must preserve.

### Read-Only Evidence

- `orchestrator/rounds/round-094/selection.md`
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
- `orchestrator/rounds/round-089/review-record.json`
- `orchestrator/rounds/round-090/review-record.json`
- `orchestrator/rounds/round-091/review-record.json`
- `orchestrator/rounds/round-092/review-record.json`
- `orchestrator/rounds/round-093/review-record.json`
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

freeze the exact same-lane retained-child public-output continuity case and
review ledger.

This slice is allowed to:

- restate the accepted predecessor evidence that defines the exact pocket;
- freeze the exact same pocket identity and exact packet;
- freeze the exact continuity split now under review:
  helper-visible/internal recursive structure versus authoritative public
  output;
- freeze one solver / elaboration / reconstruction / internal-output /
  public-output / reviewer-visible ledger for that same pocket only;
- record the honest inherited posture for that same pocket only:
  still blocker debt inside the current architecture, not broad success; and
- name the exact exclusions that later rounds must keep fail-closed.

This slice is not allowed to:

- audit the current public-output path;
- propose or land code or test changes;
- relitigate the already accepted item-2 / item-3 / item-4 / item-5 results;
- reopen `non-cyclic-graph`;
- widen into the alias-bound family, neighboring routes, nested-`forall`
  success, replay / `InstBot` repair, or general automatic-recursive-
  inference claims; or
- authorize a second interface, equi-recursive reasoning, cyclic structural
  graphs, or fallback widening.

If the artifact starts depending on fresh runtime evidence, code-path
inspection, or architecture-causality argument to justify a frozen field, it
must fail closed by preserving only the accepted predecessor fact or by
naming the missing proof as later-round work rather than widening item `1`.

## Sequential Tasks

### Task 1 - Freeze the docs-only item-1 frame

- Create the canonical item-1 artifact at the path above.
- State explicitly that item `1` is docs-only, contract-freeze-only, and
  `attempt-1` with `retry: null`.
- Reassert the inherited boundary unchanged:
  - explicit recursive annotations remain the production baseline;
  - recursive meaning remains iso-recursive only;
  - no equi-recursive equality or implicit unfolding is authorized;
  - no cyclic structural graph encoding or multi-SCC search is authorized;
  - no second interface is authorized; and
  - no compatibility / convenience / default-path fallback widening is
    authorized.
- State explicitly that accepted `N14`, accepted strategic items `2`, `5`,
  `6`, and `7`, plus accepted rounds `089` through `093`, contribute bounded
  predecessor evidence only.

### Task 2 - Freeze the exact pocket and continuity split

- Present one immutable case table for the selected pocket.
- Freeze these fields exactly:
  - family: same-lane retained-child;
  - recursive-shape anchor: `boundVarTargetRoot`;
  - owner / binder frame: one owner-local retained-child frame;
  - route:
    `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`;
  - quantified-boundary status: clear-boundary only; and
  - exact packet plus recursive annotation shape.
- Freeze the exact continuity split in the same artifact:
  - helper-visible/internal recursive structure remains
    `TMu ...` plus `containsMu True`; and
  - authoritative public output remains
    `TForall "a" Nothing (TVar "a")`.
- State explicit exclusions beside the table:
  - non-local alias-bound family excluded;
  - neighboring consumer routes excluded;
  - nested-`forall`, nested owner, and nested scheme-root crossings excluded;
  - any route that changes family, frame, route, or quantified status
    excluded.

### Task 3 - Freeze the refreshed review ledger and predecessor chain

- Reuse the established solver / elaboration / reconstruction /
  internal-output / public-output / reviewer-visible ledger vocabulary
  without alteration.
- Bind each row to the one selected pocket and the exact continuity split now
  under review.
- Record the accepted predecessor chain that later rounds must preserve:
  - accepted round `089` pocket freeze;
  - accepted round `090` breakpoint localization;
  - accepted round `091` Phase-6 clearance;
  - accepted round `092` internal/public split and blocker-debt
    classification; and
  - accepted round `093` successor decision keeping blocker debt within the
    current architecture.
- State that later rounds must not reinterpret predecessor truth into broad
  capability or architecture claims.

### Task 4 - Record the honest starting posture and handoff

- State the one honest starting posture for this refreshed family:
  the exact pocket remains blocker debt inside the current architecture, with
  the next missing work being an exact public-output path audit rather than a
  broad repair or architecture reopen.
- Add one explicit handoff section pointing only to roadmap item `2`:
  the exact authoritative public-output path audit for the same frozen
  pocket.
- State that item `1` itself does not reopen `non-cyclic-graph`.

## Completion Criteria

This plan is complete when the implementer can land one docs-only canonical
artifact that:

- freezes exactly one same-lane retained-child pocket and exact packet;
- freezes the exact helper-visible/internal versus authoritative public split
  now under review;
- freezes one review ledger for that same pocket only;
- preserves accepted predecessor continuity through rounds `089` through
  `093` plus the accepted strategic record;
- keeps the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary unchanged; and
- hands off only to roadmap item `2` without silently widening the subject.
