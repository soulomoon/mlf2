# Representative Coverage And Feasibility Campaign For General Automatic Iso-Recursive Inference

Date: 2026-03-25
Round: `round-087`
Roadmap item: `item-6`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one bounded representative coverage and feasibility campaign for
general automatic iso-recursive inference inside the inherited acyclic
architecture
Artifact kind: canonical docs-only representative coverage / feasibility
campaign

## Stage Contract Freeze

This artifact implements only roadmap item `6` for `attempt-1` with
`retry: null`.

Item `6` is docs-only and coverage-campaign-only. Its job is to classify one
representative row set from the accepted item-1 family matrix by using only:

- the accepted item-2 architectural audit;
- the accepted item-3 mechanism vocabulary;
- the accepted item-4 search / ambiguity / termination model; and
- the accepted item-5 persistence tuple, phase-and-surface ledger, and
  outcome vocabulary.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edits to `orchestrator/state.json`, `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, or `orchestrator/retry-subloop.md`;
- edits to `Bugs.md`;
- a new mechanism map, search model, or reconstruction contract;
- code-path experiments, implementation slices, or test expansion;
- equi-recursive equality or implicit unfolding;
- cyclic structural graph encoding or multi-SCC search;
- a second executable interface or fallback widening; or
- the item-7 architecture decision.

The inherited boundary remains fixed and unchanged:

- explicit recursive annotations remain the current production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized;
- no second interface is authorized; and
- no compatibility, convenience, or default-path fallback widening is
  authorized.

Accepted items `1` through `5` plus accepted `N14` contribute bounded evidence
only. They define the lawful family matrix, audit posture, mechanism
vocabulary, search discipline, and persistence rubric, but they do not by
themselves prove broad generality and they do not pre-clear the item-7
architecture choice.

## Campaign Inputs And Row Schema

This campaign is representative rather than exhaustive. It covers the minimum
review-visible pressure set required by `orchestrator/verification.md`:

- non-local alias-bound / base-like admitted family;
- same-lane retained-child admitted family;
- nested-`forall` / quantified-crossing pressure;
- ambiguity / competing-candidate pressure;
- binder-sensitive / owner-sensitive placement pressure;
- termination-pressure under the inherited acyclic model; and
- reconstruction-heavy output-surface pressure.

Where one accepted family represents multiple pressures, the row says so
explicitly rather than pretending that a new family was discovered.

Every row below records the same schema:

- representative family or pressure label;
- recursive-shape anchor;
- owner / binder frame;
- target / consumer route;
- quantified-boundary state;
- controlling evidence source:
  item-1 family label, item-3 mechanism family, item-4 search rule or reject
  boundary, and item-5 ledger expectation;
- accepted visible-output fact, if any;
- lawful item-5 outcome classification; and
- the bounded reason the current accepted record supports that classification.

## Representative Coverage Matrix Summary

| Row | Representative label | Controlling item-1 families | Outcome classification | Bounded read |
| --- | --- | --- | --- | --- |
| `C1` | Non-local alias-bound / base-like admitted family | `P2`, `P3`, `P6` | `admitted but not reconstruction-visible / blocker debt` | Admitted non-local family, but the accepted visible output stays `TBase (BaseTy "Int")` with `containsMu False`. |
| `C2` | Same-lane retained-child admitted family | `P3`, `P4`, `P6` | `admitted but not reconstruction-visible / blocker debt` | Strongest bounded positive-output candidate, but full solver -> elaboration -> reconstruction -> internal/public continuity is still not accepted. |
| `C3` | Nested-`forall` / quantified-crossing pressure | `P5`, `N2` | `fail-closed rejection` | Quantified crossing remains reject-side only under the accepted `R4` rule. |
| `C4` | Ambiguity / competing-candidate pressure | `N1` | `fail-closed rejection` | The accepted search model rejects multiple surviving candidates instead of ranking them. |
| `C5` | Binder-sensitive / owner-sensitive placement pressure | `P3`, `P4`, `N2` | `admitted but not reconstruction-visible / blocker debt` | One owner-local retained-child frame is admitted, but broader binder-sensitive persistence is not yet proven. |
| `C6` | Termination-pressure under the inherited acyclic model | `N6`, `N4`, `N5` | `fail-closed rejection` | Bounded rejection exists for forbidden growth, but that does not upgrade high-pressure positive cases into success. |
| `C7` | Reconstruction-heavy output-surface pressure | `P6` | `admitted but not reconstruction-visible / blocker debt` | The strongest bounded recursive output fact still lacks accepted internal/public end-to-end persistence proof. |

## Per-Row Classification Records

### `C1` Non-local alias-bound / base-like admitted family

- Representative family / pressure:
  `P2 non-local-propagation`, `P3 retained-child-owner-sensitive`, and
  `P6 reconstruction-visible-output`.
- Representative relation:
  this is the one accepted non-local admitted family from item `3`, not a
  general non-local clearance.
- Recursive-shape anchor:
  scheme-root alias-bound / base-like anchor reused by `baseTarget`.
- Owner / binder frame:
  the same scheme-root alias binder plus the owned rebound base-like bound.
- Target / consumer route:
  `baseTarget -> baseC -> targetC`.
- Quantified-boundary state:
  no positive nested-`forall` success is accepted here; quantified local
  continuity remains separate context only.
- Controlling evidence:
  item-1 families `P2`, `P3`, `P6`; item-3 mechanism families
  `local versus non-local propagation`, `binder / owner placement`,
  `target / consumer alignment`, and `reconstruction obligations`; item-4
  rules `R2`, `R5`, `R6`, and `R7`; item-5 non-local family inventory plus
  output-surface honesty rules.
- Item-5 ledger read:
  this family stays lawful only if the same admitted family, anchor, owner /
  binder frame, route, quantified-boundary-clear state, and recursive
  visibility survive the full ledger. The accepted record already says the
  current visible output read is non-recursive, so the ledger cannot end in
  positive persistence on present evidence.
- Accepted visible-output fact:
  `TBase (BaseTy "Int")` with `containsMu False`.
- Lawful item-5 outcome classification:
  `admitted but not reconstruction-visible / blocker debt`.
- Why this classification is bounded:
  item `5` explicitly keeps this family as admitted but non-success because
  recursive structure is not review-visible on the accepted output surface.
  Any stronger read would require manual reinterpretation or fresh evidence
  that this round does not own.

### `C2` Same-lane retained-child admitted family

- Representative family / pressure:
  `P3 retained-child-owner-sensitive`,
  `P4 binder-sensitive-placement`, and
  `P6 reconstruction-visible-output`.
- Representative relation:
  this is the one accepted same-lane admitted family. Rows `C5` and `C7`
  reuse this family as representative binder-sensitive and
  reconstruction-heavy pressure rather than as separate families.
- Recursive-shape anchor:
  `boundVarTargetRoot` inside the owner-local retained-child frame.
- Owner / binder frame:
  one owner-local retained-child frame carried through
  `boundVarTargetRoot`, `boundHasForallFrom`, and the retained child.
- Target / consumer route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`.
- Quantified-boundary state:
  clear-boundary only; `boundHasForallFrom` is false and `not hasForall`
  holds.
- Controlling evidence:
  item-1 families `P3`, `P4`, `P6`; item-3 mechanism families
  `recursive-shape discovery`, `binder / owner placement`,
  `target / consumer alignment`, and `local versus non-local propagation`;
  item-4 rules `R3`, `R4`, and `R5`; item-5 admitted-family inventory plus
  stable-persistence tuple.
- Item-5 ledger read:
  this is the strongest bounded positive-output candidate, but
  `stable visible persistence` is lawful only if the same family, anchor,
  owner / binder frame, route, quantified-boundary-clear state, and recursive
  visibility survive solver, elaboration, reconstruction, internal output,
  and public output without reinterpretation. The accepted record does not yet
  supply that full ledger continuity.
- Accepted visible-output fact:
  the bounded positive example still keeps a recursive output fact
  (`containsMu True`) and keeps the `keepTargetFinal -> targetC` route
  review-visible.
- Lawful item-5 outcome classification:
  `admitted but not reconstruction-visible / blocker debt`.
- Why this classification is bounded:
  accepted docs preserve one promising bounded packet, but item `5`
  expressly withholds `stable visible persistence` until the full pipeline
  continuity proof is accepted end to end.

### `C3` Nested-`forall` / quantified-crossing pressure

- Representative family / pressure:
  `P5 polymorphism-nested-forall` pressure plus
  `N2 unsoundness-guard`.
- Representative relation:
  this is reject-side pressure only. It is not a third admitted family and it
  does not upgrade the accepted negative-only nested-`forall` record into a
  positive `P5` success.
- Recursive-shape anchor:
  the same owner-local retained-child search is the bounded place where the
  quantified-crossing pressure was observed.
- Owner / binder frame:
  the candidate starts in one owner-local retained-child frame but then
  touches a nested `TyForall`, nested owner, or nested scheme-root boundary.
- Target / consumer route:
  no lawful recursive route survives; the candidate cannot stay on
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` once the
  quantified crossing appears.
- Quantified-boundary state:
  crossing detected; `boundHasForallFrom` and `not hasForall` fail the
  clear-boundary precondition.
- Controlling evidence:
  item-1 families `P5` and `N2`; item-3 mechanism family
  `interaction with polymorphism and instantiation` marked
  `negative-only / unresolved`; item-4 reject boundary `R4`; item-5
  invalidation rule for quantified crossing.
- Item-5 ledger read:
  quantified crossing invalidates the persistence tuple before a lawful
  recursive output-surface read exists.
- Accepted visible-output fact:
  the accepted nested-`forall` contrast remains non-recursive
  (`containsMu False`) and reject-side only.
- Lawful item-5 outcome classification:
  `fail-closed rejection`.
- Why this classification is bounded:
  the accepted record lifts quantified crossing only into a reject boundary.
  No accepted item-5 evidence permits positive `P5` success.

### `C4` Ambiguity / competing-candidate pressure

- Representative family / pressure:
  `N1 ambiguity-reject`.
- Representative relation:
  this row is a representative reject-side pressure across the admitted
  family vocabulary, not a third positive family.
- Recursive-shape anchor:
  any otherwise-lawful anchor that still leaves more than one surviving
  recursive candidate after admission and route filtering.
- Owner / binder frame:
  ambiguous by definition because more than one owner / binder story or more
  than one surviving recursive route remains live.
- Target / consumer route:
  no unique route survives; candidate overlap remains after admissibility
  filtering.
- Quantified-boundary state:
  not the deciding axis here; ambiguity alone is enough to fail closed.
- Controlling evidence:
  item-1 family `N1`; item-3 mechanism family
  `fail-closed ambiguity / unsafe-case handling`; item-4 rules `R5` and
  `R6`; item-5 invalidation rule for multiple candidate stories.
- Item-5 ledger read:
  no lawful persistence tuple can be frozen because more than one candidate
  story survives.
- Accepted visible-output fact:
  none; the accepted record here is a rejection policy rather than a visible
  recursive success.
- Lawful item-5 outcome classification:
  `fail-closed rejection`.
- Why this classification is bounded:
  item `4` explicitly rejects competing candidates instead of ranking them,
  and item `5` says a candidate that needs guesswork or multiple surviving
  stories cannot count as persistence.

### `C5` Binder-sensitive / owner-sensitive placement pressure

- Representative family / pressure:
  `P3 retained-child-owner-sensitive`,
  `P4 binder-sensitive-placement`, and
  `N2 unsoundness-guard`.
- Representative relation:
  this row reuses the same-lane retained-child admitted family as the
  representative binder-sensitive row. It is pressure on one accepted family,
  not a new family.
- Recursive-shape anchor:
  `boundVarTargetRoot` with a retained child whose `bndRoot` equals
  `boundVarTargetRoot`.
- Owner / binder frame:
  one owner-local retained-child frame tracked through `boundVarTargetRoot`
  and `boundHasForallFrom`; route hopping or frame drift is forbidden.
- Target / consumer route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`.
- Quantified-boundary state:
  clear only while no nested `TyForall`, nested owner, or nested scheme-root
  crossing appears.
- Controlling evidence:
  item-1 families `P3`, `P4`, `N2`; item-3 mechanism family
  `binder / owner placement`; item-4 rules `R3`, `R4`, and `R6`; item-5
  persistence-tuple fields `owner / binder frame` and `target / consumer
  route`.
- Item-5 ledger read:
  one lawful owner / binder story is admitted, but any frame drift would
  fail closed. The accepted record does not yet prove that this same
  owner-sensitive story survives elaboration, reconstruction, and both output
  surfaces.
- Accepted visible-output fact:
  the same bounded recursive output fact used in `C2` (`containsMu True`)
  remains visible for this owner-local retained-child route.
- Lawful item-5 outcome classification:
  `admitted but not reconstruction-visible / blocker debt`.
- Why this classification is bounded:
  accepted owner-sensitive placement evidence is real, but it remains bounded
  to one exact owner frame and one route. Broad binder-sensitive persistence
  would overclaim the accepted record.

### `C6` Termination-pressure under the inherited acyclic model

- Representative family / pressure:
  `N6 termination-pressure`, with explicit boundary interaction from
  `N4 cyclic-or-multi-scc-required` and
  `N5 second-interface-or-fallback-required`.
- Representative relation:
  this is the representative negative row for high-pressure cases that would
  need forbidden growth. It is not a third admitted positive family.
- Recursive-shape anchor:
  only the finite item-4 anchor set is lawful; pressure begins when a case
  would need reopening beyond those anchors.
- Owner / binder frame:
  only finite owner ancestry is lawful; cases needing owner-frame switching or
  reopened ancestry leave the accepted model.
- Target / consumer route:
  only the fixed admitted route vocabulary is lawful; neighboring-route
  reopening is forbidden.
- Quantified-boundary state:
  not the main issue in this row; the pressure is search growth beyond the
  finite acyclic model.
- Controlling evidence:
  item-1 families `N6`, `N4`, `N5`; item-2 audit read
  `non-cyclic-graph = unknown`; item-4 termination discipline plus reject
  boundary `R7`; item-5 reviewer-visible trail forbidding fallback-like
  rescue.
- Item-5 ledger read:
  cases that need cyclic graphs, multi-SCC search, fallback retries, or
  reopened rejected routes never enter a lawful full-pipeline persistence
  ledger.
- Accepted visible-output fact:
  none.
- Lawful item-5 outcome classification:
  `fail-closed rejection`.
- Why this classification is bounded:
  the accepted record supports bounded rejection for forbidden growth, but it
  does not convert high-pressure positive cases into success. The broader
  architecture risk remains open for item `7`.

### `C7` Reconstruction-heavy output-surface pressure

- Representative family / pressure:
  `P6 reconstruction-visible-output`.
- Representative relation:
  this row reuses the same-lane retained-child admitted family because it is
  the strongest bounded positive-output candidate. Row `C1` remains the
  non-local blocker-debt contrast rather than a separate reconstruction
  family.
- Recursive-shape anchor:
  `boundVarTargetRoot` inside the owner-local retained-child frame.
- Owner / binder frame:
  the same owner-local retained-child frame used by `C2` and `C5`.
- Target / consumer route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`.
- Quantified-boundary state:
  clear-boundary only; nested crossings still fail closed.
- Controlling evidence:
  item-1 family `P6`; item-3 mechanism family `reconstruction obligations`;
  item-4 admitted retained-child route family; item-5 phase-and-surface
  ledger, output-surface honesty rules, and reviewer-facing validation
  procedure.
- Item-5 ledger read:
  positive `P6` requires the same persistence tuple across solver,
  elaboration, reconstruction, internal output, public output, and the
  reviewer-visible trail. The accepted record currently provides only a
  bounded recursive output fact plus route visibility.
- Accepted visible-output fact:
  the bounded positive example keeps a recursive output fact
  (`containsMu True`), but item `5` does not yet certify matching internal
  and public output continuity end to end.
- Lawful item-5 outcome classification:
  `admitted but not reconstruction-visible / blocker debt`.
- Why this classification is bounded:
  internal/public agreement and reviewer-visible continuity are still not
  accepted. Without those ledger rows, reconstruction-heavy pressure cannot
  count as `stable visible persistence`.

## Aggregate Feasibility Read

Matrix tally under the accepted item-5 vocabulary:

- `stable visible persistence`: none.
- `admitted but not reconstruction-visible / blocker debt`:
  `C1`, `C2`, `C5`, and `C7`.
- `fail-closed rejection`: `C3`, `C4`, and `C6`.

The lawful item-6 feasibility read is:

`bounded subset only`

Why that is the strongest accepted read:

1. positive persistence does not extend beyond one narrow retained-child
   pocket because even that pocket (`C2`, `C5`, `C7`) remains blocker debt
   rather than `stable visible persistence`;
2. non-local coverage (`C1`) remains admitted but not reconstruction-visible,
   so the accepted record still does not justify a broad non-local recursive
   success claim;
3. nested-`forall` pressure (`C3`) remains reject-side only, so positive `P5`
   success is not yet in the accepted matrix;
4. ambiguity and termination pressure (`C4`, `C6`) are well-contained only as
   fail-closed discipline, not as broad positive coverage; and
5. reconstruction-visible output does not yet survive broadly enough to make
   `P6` credible beyond one bounded family, and even that family still lacks
   accepted end-to-end persistence proof.

Why this is not `broad generality`:

- the matrix contains zero `stable visible persistence` rows;
- both admitted families still carry blocker debt;
- nested-`forall` remains reject-side only; and
- the accepted item-2 `non-cyclic-graph = unknown` pressure is still live.

Why this is not `architectural dead end`:

- item `4` still admits two bounded family routes inside the inherited
  acyclic model;
- ambiguity and termination pressure are at least bounded by fail-closed
  rules rather than by uncontrolled growth; and
- the accepted record still supports one narrow retained-child pocket plus one
  admitted non-local pocket as bounded feasibility evidence, even though both
  remain below full-pipeline positive persistence.

This aggregate read does not consume item `7`. It says only that the current
accepted evidence supports a bounded-subset feasibility posture, not broad
generality and not a final architecture fork choice.

## Bounded Item-6 Result And Docs-Only Verification Note

Bounded item-6 result: the representative coverage matrix is now classified
under the inherited boundary, and the accumulated accepted evidence supports
one explicit feasibility read, `bounded subset only`, without consuming the
item-7 decision.

This artifact is written to satisfy the reviewer-facing item-6 expectations in
`orchestrator/verification.md`:

- it spans representative local / non-local, retained-child / alias-bound,
  nested-`forall`, ambiguity, termination, binder-sensitive, and
  reconstruction-heavy pressure rather than a convenience subset;
- it uses the accepted item-5 persistence tuple, phase-and-surface ledger,
  and outcome vocabulary row by row;
- it separates bounded positive-admission evidence from blocker debt and
  fail-closed territory without promoting predecessor packets into a
  repo-level generality claim; and
- it ends with one bounded feasibility read only.

This round is expected to change only documentation:

- this canonical item-6 coverage / feasibility artifact; and
- `orchestrator/rounds/round-087/implementation-notes.md`.

Because the round does not touch `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`, no full `cabal build all && cabal test` gate is triggered
unless the diff escapes that authorized docs-only surface.

Where the accepted record does not justify a stronger claim, this artifact
fails closed by recording blocker debt, fail-closed rejection, or later-item
ownership rather than by broadening the round.
