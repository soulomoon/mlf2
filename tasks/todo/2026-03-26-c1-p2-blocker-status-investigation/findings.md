# Findings

## Investigation Inputs

- Required docs:
  - `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  - `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  - `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
  - `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`
- Successor-doc check:
  - Read newer March 26 settlement/handoff docs only if they materially change `C1`.

## Notes

- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  fixes `P2 non-local-propagation` as a required positive family that `must
  succeed`; bounded packet evidence is not enough.
- `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
  keeps `C1` classified as `admitted but not reconstruction-visible / blocker
  debt` because the bounded non-local packet still yields visible
  non-recursive output `TBase (BaseTy "Int")` with `containsMu False`.
- `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  carries that `C1` state forward unchanged into the accepted aggregate gate:
  `P2` remains `admitted but visibly non-recursive`.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`
  is explicitly limited to the exact same-lane `C2` / `C5` / `C7` pocket and
  says `non-local C1` is retained only as bounded contrast context; it does
  not amend the `C1` classification.
- The reopened-revision chain keeps `C1` out of the live architecture lane:
  the candidate boundary, selected subject, rev-003 successor publication,
  rev-004 freeze, and rev-004 stop decision all remain exact-pocket-only for
  `C2` / `C5` / `C7`, with `C1` contrast-only rather than reopened.
- No later-dated plan doc exists beyond the March 26 chain, and no later
  March 26 successor doc reclassifies `C1`.
- Narrowest blocker-reduction task without scope widening: add one exact-packet
  `C1` authoritative-surface audit/harness over the existing production
  surfaces (`computeResultTypeFallback`, `runPipelineElab`,
  `runPipelineElabChecked`) for the `baseTarget -> baseC -> targetC` packet,
  because the accepted slice explicitly says no public-output-specific harness
  exists for `C1`.
