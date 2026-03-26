# Round 103 Plan (`item-5` Global `non-cyclic-graph` Settlement Gate)

## Objective

Execute only roadmap item `5` and produce one docs-only aggregate decision
artifact at:
`docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`.

This is the initial `item-5` plan for `attempt-1` with `retry: null`. The
round must consume only the accepted item-1 settlement freeze, the accepted
item-2 and item-3 bounded production-surface slices, the accepted item-4
representative family-matrix refresh, and the bound predecessor strategic
record needed to interpret them. It must then record exactly one lawful global
settlement outcome:

- `non-cyclic-graph = keep`; or
- `reopen the non-cyclic-graph revision question`.

Because item `5` is aggregate-only, this round owns accepted-evidence
synthesis and one canonical decision artifact only. It does not own new
evidence reruns, implementation notes, production work, hardening, capability
claims, or roadmap updates.

Current planning read: the accepted aggregate matrix now looks too weak for a
lawful `non-cyclic-graph = keep` on its face. The accepted item-4 refresh
still records zero `stable visible persistence` rows, five blocker-debt rows
(`P1-row`, `C1`, `C2`, `C5`, `C7`), three fail-closed rows (`C3`, `C4`, `C6`),
`P5` still reject-side only, and `P6` still below the full-pipeline visible
persistence bar. The decision artifact must test that posture strictly against
the accepted item-1 gate. It may keep the current architecture only if the
accepted evidence still makes representative family-matrix settlement credible
inside the inherited acyclic model without inventing fresh evidence. Otherwise
the lawful outcome is to reopen the `non-cyclic-graph` revision question and
leave item `6` blocked.

This round is docs-only, aggregate-only, and decision-only. It must not edit
production code, tests, `mlf2.cabal`, controller-owned machine state, roadmap
contracts, retry / verification contracts, or `Bugs.md`.

## Locked Round Context

- Round id: `round-103`
- Roadmap item: `item-5`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Active branch: `codex/round-103`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-103`
- Fixed live subject: one global aggregate `non-cyclic-graph` settlement gate
  over the accepted representative family matrix only
- Stage mode: one bounded decision slice only
- Current round review feedback: none yet; no `review.md`,
  `reviews/attempt-<n>.md`, or `review-record.json` exists for `round-103`, so
  this is a full `attempt-1` plan rather than a retry delta

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` (controller-owned; must remain untouched)
- `?? orchestrator/rounds/round-103/selection.md`

Item `5` is aggregate-only under the live retry contract. Review may reject
and send the same round back to `plan`, but review may not emit
`accepted + retry` for this item.

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-103/selection.md`
  fixes this round to roadmap item `5` only, binds the output to one global
  aggregate outcome, and forbids new evidence reruns, production
  implementation, hardening, capability-claim work, cyclic search, multi-SCC
  search, second interfaces, fallback paths, or silent widening beyond the
  accepted item-1 through item-4 chain.
- `orchestrator/state.json`
  fixes the live controller state at `active_round_id = round-103`,
  `stage = plan`, `current_task = item-5`, `retry = null`,
  `roadmap_id = 2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`,
  `roadmap_revision = rev-001`, and the authoritative
  `roadmap_dir = orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001`.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/roadmap.md`
  makes item `5` the lowest-numbered unfinished dependency-satisfied item and
  defines its completion notes as one aggregate decision that consumes only the
  accepted item-1 through item-4 evidence chain.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/retry-subloop.md`
  keeps item `5` aggregate-only and forbids `accepted + retry`.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/verification.md`
  requires global-settlement-gate checks proving that item `5` records exactly
  one outcome, that `keep` is backed by accepted production-surface evidence
  across the representative matrix, and that `reopen` is used if required
  positive-family evidence crosses the current architecture boundary.
- `docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`
  is the accepted item-1 freeze. It binds the exact later acceptance bar for
  `non-cyclic-graph = keep` versus
  `reopen the non-cyclic-graph revision question`, freezes the representative
  family ledger across `P1` through `P6` and `N1` through `N6`, and states
  explicitly which weaker evidence is insufficient for either branch.
- `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
  is the accepted item-2 slice. It fixes `C1`, `C2`, and `C5` as
  `admitted but not reconstruction-visible / blocker debt` and preserves their
  bounded family mapping without widening.
- `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`
  is the accepted item-3 slice. It fixes `C3` as `fail-closed rejection`,
  keeps `P5` reject-side only, and keeps `C7` below
  `stable visible persistence`.
- `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`
  is the accepted item-4 refresh. It is the binding aggregate matrix input:
  zero `stable visible persistence` rows; blocker-debt rows
  `P1-row`, `C1`, `C2`, `C5`, and `C7`; fail-closed rows `C3`, `C4`, and
  `C6`; `P5` reject-side only; `P6` below visible persistence; and `N4`
  pressure still context only unless this item-5 gate explicitly upgrades it.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  preserves the inherited boundary:
  explicit recursive annotations remain the production baseline;
  recursive meaning remains iso-recursive only; and non-equi-recursive,
  non-cyclic-graph, no-second-interface, and no-fallback constraints remain
  binding unless an accepted roadmap decision changes them.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  preserves the representative family matrix and the repo-level claim bar for
  `P1` through `P6` and `N1` through `N6`.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  keeps `non-cyclic-graph = unknown` at general scope before this roadmap
  family settles it.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  keeps the production-surface vocabulary binding, including the rule that
  positive `P6` success requires `stable visible persistence` on the accepted
  surfaces rather than helper-only recursion.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  remains the accepted predecessor family-matrix read of
  `bounded subset only`; it preserves the distinction between bounded admitted
  pockets and accepted general success.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  keeps `continue within the current architecture` as the strongest accepted
  predecessor strategic posture before the refreshed roadmap family, but does
  not itself settle the later global item-5 gate.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`,
  and
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
  remain bounded exact-pocket predecessor evidence only for the `C2` / `C5` /
  `C7` same-lane route. They may strengthen aggregate context, but they do not
  by themselves settle the global gate.
- Accepted rounds `round-094` through `round-102` remain predecessor evidence
  only. They do not authorize code work, test work, or a broader capability
  claim in this item-5 round.
- `Bugs.md`
  still lists `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
  predecessor context only. It does not authorize replay repair, fallback
  widening, roadmap reordering, or a different live subject here.

## File Map

### Create

- `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  - Responsibility: canonical docs-only item-5 decision artifact that
    assembles the accepted global decision-input ledger, evaluates only the
    two lawful item-5 outcomes, records exactly one authoritative outcome, and
    explains why the non-selected branch is weaker on the accepted evidence.

### Read-Only Evidence

- `orchestrator/rounds/round-103/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/verification.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
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

No round-local `implementation-notes.md` is authorized for this item-5 plan.
The decision artifact above is the only planned implementation output.

## Exact Selected Slice (Exactly One)

The only selected slice is:

assemble the accepted global evidence into one item-5 decision gate and record
exactly one lawful outcome for the inherited `non-cyclic-graph` boundary.

This slice is allowed to:

- restate the accepted settlement bars from item `1`;
- consolidate the accepted item-2, item-3, and item-4 matrix evidence into one
  decision-input ledger;
- use the accepted March 25 strategic documents and same-lane exact-pocket
  chain only as bounded predecessor context;
- evaluate only the two lawful item-5 outcomes
  `non-cyclic-graph = keep` and
  `reopen the non-cyclic-graph revision question`; and
- record exactly one authoritative outcome together with the reason the
  non-selected branch is weaker.

This slice is not allowed to:

- rerun tests, `cabal repl`, or any other new evidence commands;
- create implementation notes, side ledgers, alternate decision docs, or a
  shadow roadmap artifact;
- relitigate item `1`, item `2`, item `3`, or item `4`;
- plan or land item `6`, item `7`, or item `8`;
- edit `src/`, `src-public/`, `app/`, `test/`, `mlf2.cabal`,
  `orchestrator/state.json`, roadmap contracts, retry contracts, verification
  contracts, or `Bugs.md`;
- invent new packets, new representative rows, new families, or new outcome
  tokens;
- authorize equi-recursive reasoning, cyclic structural graphs, multi-SCC
  search, second interfaces, or fallback widening; or
- blur the difference between "current implementation still has blocker debt"
  and "the current architecture is globally sufficient."

## Sequential Tasks

### Task 1 - Freeze the docs-only global item-5 decision frame

- Create the canonical item-5 artifact at the path above.
- State explicitly that item `5` is docs-only, aggregate-only, and
  `attempt-1` with `retry: null`.
- Reassert the inherited boundary unchanged and say explicitly that this round
  does not authorize new evidence collection, code edits, test edits, or
  controller-owned file edits.
- Reassert the exact accepted item-1 settlement bar:
  - `non-cyclic-graph = keep` is lawful only if accepted production-surface
    evidence now makes representative family-matrix settlement credible inside
    the inherited acyclic model across `P1` through `P6` and `N1` through
    `N6`; and
  - `reopen the non-cyclic-graph revision question` is lawful only if accepted
    evidence now shows that at least one required positive family cannot be
    carried inside that model without crossing the current architecture
    boundary.
- State explicitly that weaker evidence remains insufficient on both sides:
  one bounded pocket, one blocker-debt pocket, one inherited `unknown` axis,
  or one predecessor exact-pocket decision cannot settle this gate by itself.

### Task 2 - Assemble one accepted global decision-input ledger

- Pull the accepted evidence into one compact ledger before choosing an
  outcome. At minimum it must include:
  - the inherited baseline boundary;
  - the item-1 frozen keep-vs-reopen bar;
  - the accepted March 25 strategic posture
    `continue within the current architecture`;
  - the March 25 audit classification
    `non-cyclic-graph = unknown`;
  - the full-pipeline `stable visible persistence` requirement for positive
    `P6`;
  - the accepted item-2 read for `C1`, `C2`, and `C5`;
  - the accepted item-3 read for `C3` and `C7`;
  - the accepted item-4 global matrix tally and positive / negative family
    ledgers; and
  - the same-lane exact-pocket decision chain only as bounded predecessor
    context, not as a global settlement result.
- Keep the ledger evidence-only. It must not choose the outcome yet.
- Keep `BUG-2026-03-16-001` as predecessor context only. It is not lawful
  decision evidence for flipping item `5`.

### Task 3 - Evaluate the two lawful item-5 branches explicitly

- Require the artifact to evaluate both candidate outcomes in one
  reviewer-visible schema such as:
  outcome token, supporting evidence, blocking evidence, and lawful-status
  read.
- For `non-cyclic-graph = keep`, require the artifact to prove from the
  accepted ledger only:
  - representative settlement remains credible inside the inherited acyclic
    model despite the current blocker debt;
  - the accepted matrix still leaves a lawful path to the representative
    positive families without crossing the current architecture boundary; and
  - the zero-`stable visible persistence` tally, `P5` reject-side posture, and
    `P6` visibility shortfall do not defeat that credibility claim.
- For `reopen the non-cyclic-graph revision question`, require the artifact to
  prove from the accepted ledger only:
  - at least one required positive family now fails the item-1 keep bar in a
    way that specifically crosses the current architecture boundary rather than
    remaining mere implementation debt;
  - that stronger read comes from the accepted aggregate matrix and family
    ledger, not from fresh reruns, speculative mechanisms, or unrelated bug
    context; and
  - the reopen stays bounded to the `non-cyclic-graph` question only, without
    silently authorizing equi-recursive semantics, cyclic search, multi-SCC
    search, second interfaces, or fallback behavior.
- Current preferred lawful read unless this analysis can still satisfy the
  keep bar without weakening it:
  `reopen the non-cyclic-graph revision question`.

### Task 4 - Record exactly one authoritative global settlement outcome

- Choose exactly one of the two item-5 outcomes and state it as the sole
  authoritative round result.
- Tie that choice directly to the decision ledger from Task 2 and the branch
  analysis from Task 3.
- Require the artifact to explain why the non-selected outcome is weaker on
  the accepted evidence.
- If the selected outcome is `non-cyclic-graph = keep`, require the artifact
  to say all of the following:
  - the accepted blocker-debt and fail-closed rows still add up to a credible
    representative settlement case inside the inherited acyclic model;
  - item `6` is therefore the next lawful move, still bounded by the existing
    architecture and accepted surfaces; and
  - the decision does not claim current implementation completeness or a final
    repo-level capability win.
- If the selected outcome is
  `reopen the non-cyclic-graph revision question`, require the artifact to say
  all of the following:
  - the accepted aggregate evidence now fails the keep bar and specifically
    identifies at least one required positive family whose representative
    burden cannot be carried credibly without revisiting the
    `non-cyclic-graph` boundary;
  - item `6` remains blocked and the next lawful move is a same-family roadmap
    revision rather than silent continuation into implementation; and
  - the reopen is bounded to the `non-cyclic-graph` question only and does not
    silently widen into a broader redesign claim.
- Do not let the artifact invent a third outcome, a mixed verdict, or a
  "still unknown" result. This gate must record exactly one lawful outcome.

### Task 5 - Verification and docs-only gate

Run the baseline docs-first checks:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `test -f docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`
- `test -f docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
- `test -f docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`
- `test -f docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`
- `test -f docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`

Confirm the round stayed docs-only and aggregate-only:

- `git diff --name-only -- src test src-public app mlf2.cabal`
  and confirm it is empty
- `git diff --name-only -- orchestrator/state.json orchestrator/roadmap.md orchestrator/retry-subloop.md orchestrator/verification.md Bugs.md`
  and confirm it reports only the pre-existing controller-owned
  `orchestrator/state.json` drift, with no round-owned edits to roadmap,
  retry contract, verification contract, or `Bugs.md`

Confirm the canonical item-5 artifact records the exact decision shape:

- `rg -n 'Stage Contract Freeze|Accepted Decision-Input Ledger|Outcome Evaluation Schema|One Authoritative Item-5 Outcome|non-cyclic-graph = keep|reopen the non-cyclic-graph revision question|stable visible persistence|bounded subset only|continue within the current architecture|item `6` remains blocked|item `6` is therefore the next lawful move' docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`

This round is expected to change only documentation. No focused or full
`cabal` gate is triggered unless the diff escapes that authorized docs-only
surface.

## Failure Discipline

If the accepted evidence does not support one branch under the exact item-1
bar, reject that branch explicitly in the artifact rather than weakening the
bar.

Do not compensate for missing proof by:

- rerunning tests or `cabal repl`;
- introducing new diagnostics, fresh source inspection claims, or repair
  hypotheses as if they were accepted evidence;
- promoting the same-lane exact-pocket predecessor chain into a global result;
- treating open bug context as aggregate settlement proof; or
- authorizing implementation simply because reopening would be inconvenient.

This round must end with one explicit lawful item-5 outcome grounded only in
the accepted evidence chain already named above.
