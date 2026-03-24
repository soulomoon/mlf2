# General Automatic Iso-Recursive Inference Full-Pipeline Reconstruction And Validation Contract

Date: 2026-03-25
Round: `round-086`
Roadmap item: `item-5`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one bounded full-pipeline reconstruction and validation
contract for admitted item-4 recursive candidate families
Artifact kind: canonical docs-only reconstruction / validation contract

## Stage Contract Freeze

This artifact implements only roadmap item `5` for `attempt-1` with
`retry: null`.

Item `5` is docs-only and reconstruction-contract-only. Its job is to define
the exact review-visible evidence trail required for one item-4-admitted
recursive candidate to remain the same lawful candidate through:

- solver admission state;
- elaboration handoff / result state;
- reification / reconstruction state;
- internal output surface;
- public output surface; and
- reviewer-visible evidence trail.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edits to `orchestrator/state.json`, `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, or `orchestrator/retry-subloop.md`;
- edits to `Bugs.md`;
- a second executable or interface;
- compatibility, convenience, or default-path fallback widening;
- equi-recursive equality or implicit unfolding;
- cyclic structural graph encoding or multi-SCC search;
- reopening item-4 candidate generation or search policy;
- a representative coverage campaign; or
- the final architecture decision.

The inherited boundary remains fixed and unchanged:

- explicit recursive annotations remain the current production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized; and
- no second interface or fallback widening is authorized.

Accepted item `3`, accepted item `4`, and accepted `N14` remain bounded
predecessor evidence only. They establish two admitted candidate families and
some bounded output facts, but they do not by themselves prove full-pipeline
recursive reconstruction.

## Inherited Continuity And Honest Starting Position

The controlling accepted record for item `5` is:

- item `1` defines `P6 reconstruction-visible-output` as requiring inferred
  recursive structure to survive into reviewable output surfaces, not merely
  solver admission;
- item `3` preserves exactly two bounded predecessor families and leaves
  reconstruction obligations unresolved;
- item `4` fixes the lawful candidate vocabulary to exactly those two
  families, with quantified crossing still reject-side only; and
- accepted `N14` preserves predecessor packets as bounded evidence only and
  does not authorize a repo-level generality claim.

The honest starting position for this round is therefore:

- solver-admitted recursive candidates exist only in the bounded item-4
  vocabulary;
- current accepted records do not yet prove that either admitted family
  remains review-visible as the same recursive result across every later
  phase; and
- wherever the accepted record does not justify a stronger read, item `5`
  must record blocker debt or fail-closed rejection rather than silently
  upgrading bounded predecessor evidence into full-pipeline success.

## Bounded Admitted Family Inventory

Item `5` inherits exactly two admitted families from item `4`.

| Family | Recursive-shape anchor | Owner / binder frame | Target / consumer route | Quantified-boundary state | Current accepted output fact | Item-5 starting read |
| --- | --- | --- | --- | --- | --- | --- |
| Non-local alias-bound / base-like | scheme-root alias-bound / base-like anchor reused by `baseTarget` | same scheme-root alias binder plus the owned rebound base-like bound | `baseTarget -> baseC -> targetC` | no positive nested-`forall` success; quantified local continuity remains separate context only | bounded helper-visible output still reads `TBase (BaseTy "Int")` with `containsMu False` | admissible family, but the accepted output fact is not reconstruction-visible recursive success |
| Same-lane retained-child | `boundVarTargetRoot` inside the owner-local retained-child frame | one owner-local retained-child frame carried through `boundVarTargetRoot`, `boundHasForallFrom`, and the retained child | `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` | clear boundary only: `boundHasForallFrom` is false and `not hasForall` holds | bounded positive example still keeps a recursive output fact (`containsMu True`) and the `keepTargetFinal -> targetC` route review-visible | strongest bounded output-side candidate, but still not full-pipeline proof unless the same family survives every later phase without reinterpretation |

These are the only family facts item `5` may reuse. No new family, no
neighboring route, and no packet-specific rescue story is admitted here.

## Persistence Tuple

For item `5`, a recursive candidate counts as the same lawful candidate across
phases only if the following tuple stays stable:

- admitted family identity;
- recursive-shape anchor interpretation;
- owner / binder frame interpretation;
- target / consumer route interpretation;
- quantified-boundary-clear status; and
- output-surface recursive-visibility obligation.

The first five fields are invariant across every listed phase. The sixth field
becomes mandatory once a phase exposes a reviewable output surface: from that
point forward, recursive structure must remain visibly present on both
internal and public output surfaces for a positive `P6` success read.

If a later phase changes any field in the tuple, the result is not a persisted
instance of the same admitted candidate. It must be classified as blocker debt
or fail-closed rejection.

## Phase And Surface Ledger

| Phase / surface | Facts that must persist unchanged | Acceptable evidence shape at this phase | Drift, erasure, or reinterpretation that invalidates the phase | Can this phase still contribute to lawful `P6` success? |
| --- | --- | --- | --- | --- |
| Solver admission state | family, anchor, owner / binder frame, target / consumer route, quantified-boundary-clear status | a single item-4-admitted family under the same rule family (`R2` or `R3`) with the same anchor and route already identified | family swap, anchor swap, owner / binder change, route switch, quantified crossing, ambiguity requiring ranking | `provisional only`; solver admission is necessary but never sufficient |
| Elaboration handoff / result state | the same persistence tuple carried into elaboration inputs and outputs | elaboration still refers to the same admitted family and does not replace the owner / binder or route story with a different mechanism | elaboration erases the family identity, changes owner / binder meaning, changes route, or preserves recursion only as a hidden historical note | `yes`, but only as a bridge; elaboration-only persistence is not enough |
| Reification / reconstruction state | the same persistence tuple, now expressed in reconstructed type terms rather than only solver terms | reification / reconstruction preserves the same family and route interpretation without hidden unfolding or witness-only rescue | reconstruction changes mechanism family, silently unfolds recursion, requires replay-specific reinterpretation, or can only be justified by witness-only inspection | `yes`, but only if it can feed review-visible output surfaces directly |
| Internal output surface | the same family, owner / binder, route meaning, and recursive-visibility obligation | the internal output surface still exposes recursive structure review-visibly for the same admitted family | recursion disappears, the route story changes, or the surface downgrades the candidate into a non-recursive read while the review still calls it recursive | `yes`; a positive `P6` read requires internal output visibility |
| Public output surface | the same family, owner / binder, route meaning, and recursive-visibility obligation | the public output surface exposes the same recursive structure as the internal output surface, without alternate-interface help | public output omits recursion, changes family / route meaning, or disagrees with the internal surface | `yes`; a positive `P6` read requires public output visibility too |
| Reviewer-visible evidence trail | one stable persistence tuple traced across all earlier rows | reviewers can point to phase-specific evidence for the same family without inventing a new route, a new interface, or a packet-local folklore story | manual reinterpretation, replay-only rescue, witness-only proof, fallback-like reasoning, or guesswork about which candidate survived | `yes`; without this row the result fails closed even if some earlier row looked promising |

## Output-Surface Honesty Rules

The output-surface read must stay honest to the bounded accepted facts.

### Non-local alias-bound / base-like family

The preserved `TBase (BaseTy "Int")` / `containsMu False` read is lawful
bounded predecessor evidence, but it is not positive `P6`
reconstruction-visible recursive success. Under this contract:

- it may count as `admitted but not reconstruction-visible / blocker debt`;
- it may not be silently relabeled as recursive persistence just because the
  solver-side family was admissible earlier; and
- it must fail closed if the only remaining reason to call it recursive is a
  packet-specific manual reading of the predecessor story.

### Same-lane retained-child family

The preserved recursive output fact (`containsMu True`) is the strongest
bounded positive-output signal currently in the record, but it still counts as
full-pipeline success only if:

- elaboration preserves the same owner / binder / route interpretation;
- reification / reconstruction preserves the same admitted family without a
  new mechanism family or silent unfolding;
- internal and public output surfaces both keep the recursive structure
  review-visible; and
- reviewers can verify that continuity directly, without route switching,
  replay-specific rescue, or witness-only inspection.

Solver-only success, witness-only success, packet-history-only success, or
internal-only success is not `P6`-credible success.

## Lawful Outcome Vocabulary

Item `5` uses exactly one bounded outcome vocabulary:

| Outcome | Meaning |
| --- | --- |
| `stable visible persistence` | the same admitted family and the same owner / binder / route interpretation survive every ledger row, and recursive structure remains review-visible on both internal and public output surfaces |
| `admitted but not reconstruction-visible / blocker debt` | solver admission or partial downstream continuity exists, but recursive structure is absent, incomplete, or not review-visible on one or more required output surfaces |
| `fail-closed rejection` | the candidate drifts, crosses a forbidden quantified boundary, becomes ambiguous, changes route / family / owner meaning, or survives only by manual reinterpretation, replay-specific reasoning, witness-only inspection, or fallback-like recovery |

Only `stable visible persistence` counts as positive item-5 / `P6`
success. The other two outcomes are intentionally non-success outcomes.

## Fail-Closed Drift And Invalidation Rules

A solver-admitted candidate is no longer a lawful full-pipeline recursive
result if any of the following occurs:

- recursion disappears at a later phase;
- the candidate changes mechanism family;
- the owner / binder frame changes;
- the target / consumer route changes or hops to a neighboring route;
- a nested `forall`, nested owner, or nested scheme-root crossing appears;
- internal and public output surfaces disagree about whether recursion
  survived;
- multiple candidate stories survive and would need ranking or guesswork; or
- the only remaining justification depends on manual interpretation,
  replay-specific reasoning, witness-only inspection, or fallback-like
  recovery.

These invalidation rules tie directly to the inherited family matrix:

- competing survivors remain `N1 ambiguity-reject`;
- owner / binder / scope violations remain `N2 unsoundness-guard`;
- quantified crossing remains reject-side only and does not upgrade into
  positive `P5 polymorphism-nested-forall` success;
- cyclic / multi-SCC or reopened-search pressure stays outside the lawful
  item-5 surface and remains later boundary debt; and
- fallback- or second-interface-dependent rescue remains `N5` and is not a
  lawful validation move.

If a case can be read as recursive only by reusing packet-local folklore from
predecessor artifacts, validation must fail closed.

## Reviewer-Facing Validation Procedure

Reviewers should apply the following checklist to any candidate from the two
admitted families:

1. Identify the admitted family and freeze the persistence tuple from item `4`:
   anchor, owner / binder frame, route, and quantified-boundary-clear status.
2. Confirm that solver admission names exactly one surviving family and does
   not depend on ambiguity resolution, route ranking, or cross-family search.
3. Confirm that elaboration and reification / reconstruction preserve the same
   persistence tuple rather than introducing a different family, owner frame,
   route, or recursive story.
4. Confirm that internal and public output surfaces both expose the same
   recursive structure review-visibly. A non-recursive surface read, including
   the bounded `TBase (BaseTy "Int")` / `containsMu False` fact, cannot be
   promoted to positive `P6` success.
5. Confirm that the evidence trail is review-visible end to end and does not
   require witness-only inspection, replay-specific rescue, a second
   interface, or packet-specific manual reinterpretation.
6. Record exactly one outcome from the bounded vocabulary above:
   `stable visible persistence`, `admitted but not reconstruction-visible /
   blocker debt`, or `fail-closed rejection`.

This procedure is docs-only and review-visible. It does not require a new
executable, an alternate interface, or a code-path experiment to make the
contract intelligible.

## Bounded Item-5 Outcome And Later-Item Debt

Bounded item-5 result: one full-pipeline reconstruction and validation
contract is now stated for the inherited boundary and the two admitted
item-4 families.

This contract defines:

- the stable persistence tuple;
- the phase-and-surface ledger;
- the only lawful positive `P6` success read; and
- the fail-closed invalidation rules for drift, erasure, quantified crossing,
  manual-only interpretation, or fallback-like rescue.

The current accepted record still leaves explicit blocker debt:

- the non-local alias-bound / base-like family currently preserves a
  non-recursive visible output fact and therefore remains
  `admitted but not reconstruction-visible / blocker debt` unless later
  accepted evidence changes that read;
- the same-lane retained-child family remains the strongest bounded positive
  output-side candidate, but it still requires explicit solver ->
  elaboration -> reconstruction -> internal/public output continuity evidence
  under this contract before reviewers may classify it as
  `stable visible persistence`;
- representative coverage across the broader family matrix remains item `6`
  work;
- any architecture revision or boundary change remains item `7` work; and
- positive nested-`forall` / `P5` success remains unresolved unless later
  accepted evidence changes that read.

This round is docs-only. Because the diff stays out of `src/`, `src-public/`,
`app/`, `test/`, and `mlf2.cabal`, no full `cabal build all && cabal test`
gate is triggered unless the work escapes that authorized docs-only surface.
