# Round 101 Selection

Date: 2026-03-26
Round: `round-101`
Role: guider
Active subject: global `non-cyclic-graph` settlement and automatic
iso-recursive inference roadmap family after the accepted item-2
`C1` / `C2` / `C5` settlement-evidence slice in `round-100`

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only (`git status --short --branch`
  returned `## codex/round-101` and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `3`: produce the bounded production-surface settlement evidence
slice for `C3` nested-`forall` pressure and `C7` output-surface continuity.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state for this packet at
`active_round_id: "round-101"`, `stage: "select-task"`,
`current_task: null`, `last_completed_round: "round-100"`, and
`retry: null`. No live same-round retry object exists, so the retry contract
does not force a retry loop. The guider rule therefore remains: select the
lowest-numbered unfinished roadmap item whose dependencies are satisfied.

The active roadmap bundle already records items `1` and `2` as done from
accepted `round-099` and `round-100`. Item `3` is now the
lowest-numbered unfinished item, and its only dependencies are items `1`
and `2`, both satisfied by the canonical item-1 freeze in
`docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`
and the canonical item-2 dossier in
`docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`.
Items `4` and `5` both depend on item `3`, while items `6` through `8`
remain blocked until item `5` records the global settlement outcome.
Selecting anything later would skip roadmap order and violate the
pre-item-5 settlement-evidence boundary.

Item `3` is also the exact missing proof slice named by the accepted item-1
ledger. That ledger freezes `P5 polymorphism-nested-forall` as unresolved and
reject-side only, and freezes `P6 reconstruction-visible-output` as still
unresolved at repo scope. The accepted strategic predecessor artifacts keep
the same pressure points bounded and concrete: the representative campaign at
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
records `C3` as `fail-closed rejection` and `C7` as
`admitted but not reconstruction-visible / blocker debt`, while the
full-pipeline contract at
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
still requires review-visible persistence on the existing solver ->
elaboration -> reconstruction -> internal/public output surfaces before a
positive `P6` read is lawful.

The accepted item-2 slice in `round-100` sharpened only `C1`, `C2`, and `C5`
for `P2`, `P3`, and `P4`. It intentionally left `C3` and `C7` as the
remaining unresolved settlement-evidence rows before the representative
campaign can run honestly. That means item `3` is not optional follow-on
work; it is the next missing bounded settlement dossier needed to complete
the pre-campaign family evidence ledger without widening into item `4`
representative replay, item `5` global settlement, or production
implementation.

The immediate predecessor exact-pocket chain also points directly to this
selection. The accepted same-lane retained-child continuity family in
`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`,
`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`,
`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`,
`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`,
and
`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
already fixes the strongest same-lane `C7` route as one bounded retained-child
pressure point only: helper-visible recursive structure persists internally,
authoritative public output still collapses, and the exact-pocket result
remains blocker debt within the current architecture rather than a global
settlement result. Item `3` is where that bounded `C7` pressure is paired
with `C3` quantified-crossing pressure to finish the remaining unresolved
`P5` / `P6` settlement-evidence slice on the existing production surfaces.

The inherited baseline remains unchanged while item `3` runs.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still keeps the explicit-only / iso-recursive / non-equi-recursive /
non-cyclic-graph / no-fallback boundary live. The accepted architectural
audit in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
still classifies `non-cyclic-graph = unknown`, not `revise`, so this round
must stay settlement-evidence-only inside the inherited architecture. The
resolved `BUG-2026-03-16-001` entry in `Bugs.md` remains predecessor replay
context only and does not authorize roadmap reordering, production
implementation, cyclic search, multi-SCC search, second interfaces, or
fallback widening.

## Round Scope Guard

- This round is limited to roadmap item `3` only.
- The output must stay settlement-evidence-only and bounded to `C3`
  nested-`forall` pressure and `C7` reconstruction-heavy output-surface
  continuity on the existing production surfaces.
- The output must classify the selected slice honestly against the frozen
  ledger vocabulary:
  `stable visible persistence`,
  `admitted but not reconstruction-visible / blocker debt`, or
  `fail-closed rejection`.
- The output may consume accepted predecessor evidence, but it must preserve
  the truth that accepted rounds `round-094` through `round-098` remain
  exact-pocket predecessor evidence only and do not settle repo-level
  `non-cyclic-graph` by themselves.
- Do not widen into item `4` representative-campaign work, item `5` global
  settlement, production implementation, hardening, cyclic search,
  multi-SCC search, second interfaces, fallback paths, or a broad capability
  claim.
- Do not edit `orchestrator/state.json` or production code in this round.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- repo-level `non-cyclic-graph` settlement is still unresolved;
- the accepted matrix still has zero `stable visible persistence` rows;
- `P5` remains reject-side only on accepted evidence, while `P6` remains
  unresolved at repo scope;
- `C7` remains the strongest bounded same-lane public-output continuity
  pressure rather than an accepted production-surface success; and
- the resolved `BUG-2026-03-16-001` replay defect remains predecessor
  context only, not authority to skip the item-3 settlement slice.
