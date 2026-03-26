# Round 102 Selection

Date: 2026-03-26
Round: `round-102`
Role: guider
Active subject: global `non-cyclic-graph` settlement and automatic
iso-recursive inference roadmap family after the accepted item-3
`C3` / `C7` settlement-evidence slice in `round-101`

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only (`git status --short --branch`
  returned `## codex/round-102` and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `4`: run the representative end-to-end settlement campaign
across the family matrix.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state for this packet at
`active_round_id: "round-102"`, `stage: "select-task"`,
`current_task: null`, `last_completed_round: "round-101"`, and
`retry: null`. No live same-round retry object exists, so the retry contract
does not force a retry loop. The guider rule therefore remains: select the
lowest-numbered unfinished roadmap item whose dependencies are satisfied.

The active roadmap bundle already records items `1`, `2`, and `3` as done
from accepted `round-099`, `round-100`, and `round-101`. Item `4` is now the
lowest-numbered unfinished item, and its only dependencies are items `1`,
`2`, and `3`, all satisfied by the canonical item-1 freeze in
`docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`,
the canonical item-2 dossier in
`docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`,
and the canonical item-3 dossier in
`docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`.
Item `5` depends on item `4`, while items `6` through `8` remain blocked
until item `5` records the global settlement outcome. Selecting anything
later would skip roadmap order and violate the pre-item-5
settlement-evidence-only boundary.

Item `4` is also the exact aggregate proof step now missing from the frozen
ledger. The accepted item-1 contract froze the repo-level settlement bar and
kept the representative family matrix unresolved at repo scope. Accepted
item `2` then sharpened `C1`, `C2`, and `C5` only: `P2`, `P3`, and `P4`
still remain bounded blocker-debt rows rather than reconstruction-visible
success. Accepted item `3` then sharpened `C3` and `C7` only: `P5` remains
reject-side only through quantified-crossing fail-closed behavior, and `P6`
still has only one strongest same-lane blocker-debt route rather than
accepted `stable visible persistence`. That leaves item `4` as the first
lawful place to rerun the representative corpus across the full family
matrix and record the combined production-surface read without deciding the
item-5 architecture gate yet.

The inherited contracts point directly to this handoff. The capability
contract in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
still defines representative positive families `P1` through `P6` and
required negative or bounded families `N1` through `N6` as the minimum
repo-level claim bar. The architectural audit in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
still classifies `non-cyclic-graph = unknown`, not `keep` or `revise`. The
full-pipeline contract in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
still makes `stable visible persistence` the only positive `P6` success
token. The representative campaign artifact in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
already fixes the row schema and earlier bounded reads, so item `4` is where
the roadmap explicitly reruns that matrix on the current authoritative
production surfaces after the new item-2 and item-3 settlement slices.

The immediate predecessor exact-pocket chain also remains bounded input only.
The same-lane retained-child continuity artifacts from
`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
through
`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
still preserve one strongest bounded `C7` route only:
helper-visible/internal recursive structure survives, authoritative public
output still collapses, and the exact-pocket result remains blocker debt
within the current architecture rather than global settlement. Item `4` is
the lawful step that consumes that predecessor truth together with the
bounded non-local and quantified-crossing reads to decide whether the current
matrix remains only `bounded subset only`, whether any claimed `N4` pressure
is truly out of scope or actually required by the intended positive set, and
whether item `5` can later judge `keep` versus `reopen` on representative
evidence instead of pocket folklore.

The inherited baseline remains unchanged while item `4` runs.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still keeps the explicit-only / iso-recursive / non-equi-recursive /
non-cyclic-graph / no-fallback boundary live. The accepted architecture
decision in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
still keeps `continue within the current architecture` as the strongest
predecessor aggregate read, but not as a repo-level settlement result. Open
`BUG-2026-03-16-001` in `Bugs.md` remains predecessor replay / `InstBot`
context only and does not authorize roadmap reordering, production
implementation, cyclic search, multi-SCC search, second interfaces, or
fallback widening.

## Round Scope Guard

- This round is limited to roadmap item `4` only.
- The output must stay settlement-evidence-only and bounded to the
  representative family-matrix replay on the existing solver ->
  elaboration -> reconstruction -> internal/public output surfaces.
- The output must preserve the frozen pre-campaign truths unless the reruns
  themselves prove otherwise on those same surfaces:
  `P5` remains reject-side only through `C3`,
  `P6` still has only one bounded same-lane blocker-debt route through `C7`,
  and the current matrix still has zero `stable visible persistence` rows.
- The output must explicitly classify the representative rows using the
  frozen vocabulary:
  `stable visible persistence`,
  `admitted but not reconstruction-visible / blocker debt`, or
  `fail-closed rejection`.
- The output may clarify whether any asserted `N4` pressure is truly out of
  scope or actually required by the intended positive set, but it must not
  itself decide item `5`, revise the architecture, or widen into cyclic
  search, multi-SCC search, second interfaces, fallback paths, production
  implementation, hardening, or a broad capability claim.
- Do not edit `orchestrator/state.json` or production code in this round.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- repo-level `non-cyclic-graph` settlement is still unresolved;
- the accepted matrix still has zero `stable visible persistence` rows;
- `P2`, `P3`, `P4`, and `P6` remain bounded blocker-debt reads rather than
  accepted production-surface success, while `P5` remains reject-side only;
- any `N4` pressure is still unsettled at representative-campaign scope
  rather than already accepted as a reopen result; and
- `BUG-2026-03-16-001` remains open predecessor replay context only, not
  authority to skip the item-4 representative settlement campaign.
