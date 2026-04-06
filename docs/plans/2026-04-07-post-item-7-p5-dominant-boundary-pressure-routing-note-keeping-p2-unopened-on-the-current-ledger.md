# Post-Item-7 P5 Dominant Boundary-Pressure Routing Note Keeping P2 Unopened On The Current Ledger

Date: 2026-04-07
Round: `round-199`
Milestone: `milestone-3`
Direction: `direction-3c-record-p5-dominant-boundary-pressure`
Extracted item: `record-p5-dominant-boundary-pressure`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one docs-only routing note only
Artifact kind: canonical milestone-3 routing note keeping `P2` unopened on the current ledger

## Stage Contract Freeze

This artifact implements only `round-199` / `milestone-3` /
`direction-3c-record-p5-dominant-boundary-pressure` for `attempt-1` with
`retry: null`.

This round is docs-only, serial, aggregate-only, routing-only, and
non-widening. Its job is to start from the accepted `round-198`
remaining-frontier ledger, record why that accepted ledger still keeps `P5`
above `P2`, and bind only the bounded routing consequence that `P2` stays
unopened on the current ledger.

This artifact does not:

- reopen the March 28 exact `P5` packet, the accepted `round-151`
  reclassification, prior same-lane settlements, or the accepted exact `C1`
  packet as live family debt;
- create a fresh `P2` freeze, implementation slice, test slice, or second
  routing artifact;
- claim general `P5` family closure;
- record an immediate boundary revision; or
- choose a milestone-4 readiness / architecture end-state early.

## Accepted Routing Ledger

Accepted `round-198` is the binding immediate predecessor for this note. The
earlier `round-197`, `round-193`, `round-191`, `round-181`, and `round-151`
materials are carried only as preserved authority beneath that accepted
milestone-3 ledger.

| Source | Binding read carried into this routing note |
| --- | --- |
| `docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md`; `orchestrator/rounds/round-198/review-record.json`; `orchestrator/rounds/round-198/merge.md` | Accepted `round-198` already concluded `P5 remains the stronger blocker / pressure source`, kept `direction-3b-freeze-one-bounded-p2-follow-on-lane` gated, and made `direction-3c-record-p5-dominant-boundary-pressure` the only lawful immediate milestone-3 move. |
| `docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`; `orchestrator/rounds/round-197/review-record.json` | Accepted `round-197` settled one retained-child clear-boundary `P5` lane only: `sameLaneAliasFrameClearBoundaryExpr` has bounded current-architecture support on `runPipelineElab` / `runPipelineElabChecked`, `nestedForallContrastExpr` remains fail-closed with `PhiTranslatabilityError`, and the merged payload stayed `test-only`. |
| `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`; `orchestrator/rounds/round-193/review-record.json` | Accepted `round-193` selected `continue-bounded`, kept `P5 polymorphism-nested-forall` sharper than `P2 non-local-propagation`, and did not earn repo-level readiness or an architecture revision. |
| `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`; `orchestrator/rounds/round-191/review-record.json` | Accepted `round-191` still governs the aggregate read that `P2` is `packet-specific folklore` while `P5` remains blocked at family scale. |
| `orchestrator/rounds/round-181/review-record.json`; `orchestrator/rounds/round-181/implementation-notes.md` | Accepted `round-181` keeps one exact `C1` non-local scheme-alias / base-like packet recursive on `runPipelineElab` / `runPipelineElabChecked`, while the fallback `baseTarget -> baseC` read remains the packet boundary rather than a family-wide non-local closure. |
| `implementation_notes.md`; `orchestrator/rounds/round-151/review.md`; `orchestrator/rounds/round-151/review-record.json` | Accepted `round-151` preserves nested-forall absorption under polymorphic mediation as known correct behavior. That reclassification remains closed predecessor truth only, not current live blocker debt. |

## Why P5 Pressure Still Dominates

The accepted ledger still leaves `P5` as the stronger unresolved pressure
source. Milestone-2 settled one retained-child clear-boundary lane only:
`sameLaneAliasFrameClearBoundaryExpr` now has bounded current-architecture
support on `runPipelineElab` / `runPipelineElabChecked`. That narrowed one
exact lane, but it did not produce general `P5` family closure or reusable
positive support across the broader `P5 polymorphism-nested-forall` family.

The remaining quantified-crossing pressure also stayed explicit in the same
accepted settlement surface: `nestedForallContrastExpr` still fails closed
with `PhiTranslatabilityError`. That preserved fail-closed contrast means the
broader nested-`forall` family still lacks an honest positive-family
resolution beyond the one retained-child clear-boundary lane.

Accepted `round-151` does not change that routing read. Its polymorphic-
mediation reclassification remains closed predecessor truth about known
correct behavior, not fresh blocker evidence and not a new reason to reopen
the settled exact packet. The accepted `round-198` ranking therefore still
stands exactly: `P5 remains the stronger blocker / pressure source`.

## Why P2 Stays Unopened On The Current Ledger

`P2 stays unopened on the current ledger` because the accepted ledger still
preserves `P2` as packet-specific `C1` folklore only. The current accepted
record still contains one exact non-local packet, not a second non-local
packet, not an adjacent family lane, and not new representative evidence that
would upgrade `P2` beyond that bounded packet.

The preserved `C1` result remains real but narrow: accepted `round-181` keeps
the packet recursively visible on `runPipelineElab` / `runPipelineElabChecked`
while the fallback `baseTarget -> baseC` read stays the packet boundary.
Accepted `round-191` therefore still classifies `P2` as `packet-specific
folklore`, not as a family-wide closure or the next strongest unresolved
front.

Opening `direction-3b-freeze-one-bounded-p2-follow-on-lane` now would
over-promote packet-bounded `P2` evidence, outrun the accepted `round-198`
ranking, and blur the still-unresolved `P5` family pressure that the current
ledger leaves above it.

## Routing Consequence

The bounded routing consequence is only this: `P2` stays unopened on the
current ledger because `P5` remains the stronger blocker / pressure source`,
so the only lawful downstream route is the later `milestone-4` decision
surface where refreshed readiness or architecture outcomes can be compared
honestly.

That route begins only with `direction-4a-publish-refreshed-readiness-decision`
and may bind a later consequence such as
`direction-4b-bind-final-enablement-or-next-family` only after that refreshed
decision exists. This note is not itself an immediate boundary revision and
not itself a milestone-4 readiness decision.

## Non-Claims

This artifact does not claim:

- a fresh `P2` freeze;
- new implementation evidence or new tests;
- general `P5` family closure;
- a repo-level readiness claim; or
- an immediate architecture revision.
