# General Automatic Iso-Recursive Inference Search Model

Date: 2026-03-25
Round: `round-085`
Roadmap item: `item-4`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one bounded search / admissibility / ambiguity / termination
model for general automatic iso-recursive inference inside the inherited
acyclic architecture
Artifact kind: canonical docs-only search / admissibility / ambiguity /
termination model

## Stage Contract Freeze

This artifact implements only roadmap item `4` for `attempt-1` with
`retry: null`.

Item `4` is docs-only and search-model-only. Its job is to state when
recursive inference is considered, which candidate shapes are lawful, which
cases must fail closed, and why the resulting search stays finite inside the
inherited architecture. It does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edits to `orchestrator/state.json`, `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, or `orchestrator/retry-subloop.md`;
- edits to `Bugs.md`;
- a second executable or interface;
- compatibility, convenience, or default-path fallback widening;
- equi-recursive equality or implicit unfolding;
- cyclic structural graph encoding or multi-SCC search;
- a fresh exact packet-selection or implementation slice;
- the full reconstruction contract, representative coverage campaign, or final
  item-7 architecture decision.

The inherited baseline remains fixed and unchanged:

- explicit recursive annotations remain the current production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized; and
- no second interface or fallback widening is authorized.

Accepted `N14` and the accepted item-3 mechanism map remain bounded
predecessor evidence only. They preserve the accepted non-local
`baseTarget -> baseC` packet and the accepted same-lane retained-child
`boundVarTarget -> targetC` packet as mechanism evidence, but they do not
prove repo-level general automatic recursive inference, do not pre-clear a
general search policy as already established, and do not authorize hidden
ranking or cross-family widening.

The binding audit read from item `2` also remains in force:

- `iso-recursive = keep`
- `non-equi-recursive = keep`
- `non-cyclic-graph = unknown`
- `no-fallback = keep`

This search model must therefore stay fail-closed inside the current acyclic
representation and must record unsupported pressure as blocker debt rather
than silently consuming the item-7 architecture decision.

## Controlling Search Vocabulary

The accepted controlling docs constrain item `4` through:

- item `1` family pressure: positive `P2`, `P3`, `P4`, and negative-only
  pressure around `P5`, with `P6` remaining an output-facing dependency for
  later item `5`;
- item `2` audit pressure: `non-cyclic-graph = unknown` is still the highest
  architectural risk, while `no-fallback = keep` requires fail-closed search
  on the existing pipeline only; and
- item `3` mechanism vocabulary: recursive-shape anchor, owner / binder
  placement, local versus non-local propagation, target / consumer alignment,
  quantified-boundary state, ambiguity class, and termination boundary.

Within those constraints, the lawful item-4 candidate vocabulary is:

| Axis | Lawful values in item `4` | Forbidden or unresolved territory |
| --- | --- | --- |
| Recursive-shape anchor | Scheme-root alias-bound / base-like anchor; owner-local retained-child anchor under `boundVarTargetRoot`. | Consumer-first discovery, synthetic anchors, cyclic or multi-SCC anchor search. |
| Owner / binder placement | The same review-visible owner frame that exposed the anchor: scheme-root alias binder plus owned bound, or one owner-local retained-child frame. | Owner-crossing, binder-crossing, non-ancestor repair, or hidden frame switching. |
| Propagation mode | Non-local alias-bound propagation; same-lane local retained-child propagation. | Mixed-lane reopening, neighboring-route hopping, or cross-family search. |
| Target / consumer alignment | `baseTarget -> baseC -> targetC`; `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`. | `schemeBodyTarget`, `rootFinal`, `boundTarget`, or any hidden alternate consumer as recursive candidates. |
| Quantified-boundary state | Clear boundary only: `boundHasForallFrom` is false and `not hasForall` holds. | Positive nested-`forall` / `P5` success remains unresolved blocker debt. |
| Ambiguity class | Zero or one surviving candidate per subject after admissibility filtering. | Multiple survivors resolved by ranking, locality preference, or post-hoc repair. |
| Termination boundary | Finite acyclic anchors, finite owner ancestry, finite lane-bounded routes, and no reopened rejection. | Cyclic graphs, multi-SCC search, fallback retries, or reopened rejected routes. |

Recursive inference is therefore considered only after a lawful anchor is
found. Downstream consumer choice is never allowed to invent a recursive
candidate on its own.

## Search Rule Rubric

### `R1` Anchor-first discovery

- Trigger: a subject exposes a recursive-shape anchor already present in the
  inherited acyclic structure.
- Required evidence: the anchor must match one of the accepted mechanism
  families before any downstream consumer route is consulted.
- Admitted candidate shape: a candidate token for either the non-local
  alias-bound / base-like family or the same-lane retained-child family.
- Fail-closed reject condition: if no lawful anchor exists, recursive
  inference is not considered at all.
- Ambiguity behavior: none yet; ambiguity is checked only after lane-bounded
  admission.
- Termination contribution: search starts from a finite anchor set rather than
  from consumer-directed reopening.

### `R2` Non-local alias-bound / base-like admission

- Trigger: the anchor is a scheme-root alias-bound / base-like root in a
  non-local owner frame.
- Required evidence: the owner binder remains the same scheme-root alias
  binder, the owned bound remains base-like, propagation stays non-local, and
  the downstream route is explicitly `baseTarget -> baseC -> targetC`.
- Admitted candidate shape: one non-local alias-bound candidate rooted in that
  same owner / bound pair.
- Fail-closed reject condition: reject if the binding becomes local, the bound
  is no longer base-like, the owner frame changes, or the candidate would need
  a different consumer route.
- Ambiguity behavior: if more than one non-local alias-bound candidate
  survives for the same subject, reject under `N1` instead of preferring one.
- Termination contribution: this rule inspects one existing owner-binder /
  owned-bound packet and one explicit downstream route only.

### `R3` Same-lane retained-child admission

- Trigger: the anchor is an owner-local retained-child frame under
  `boundVarTargetRoot`.
- Required evidence: `rootBindingIsLocalType` holds, the child stays on the
  same local `TypeRef` lane, the candidate bound root equals
  `boundVarTargetRoot`, and the downstream route is explicitly
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`.
- Admitted candidate shape: one same-lane retained-child candidate in that one
  owner frame.
- Fail-closed reject condition: reject if the child leaves the same lane, the
  bound root changes, the owner frame changes, or the candidate would need to
  land on `schemeBodyTarget`, `rootFinal`, `boundTarget`, or another route.
- Ambiguity behavior: if multiple retained children survive in the same frame,
  or if both a retained-child and a different recursive route remain plausible,
  reject instead of ranking them.
- Termination contribution: this rule enumerates only the finite retained
  children already present in one owner frame.

### `R4` Quantified-boundary admissibility

- Trigger: a candidate's bound walk or owner walk touches quantified
  structure.
- Required evidence: `boundHasForallFrom` must remain false and
  `not hasForall` must hold for the candidate under consideration.
- Admitted candidate shape: only a candidate whose bound walk does not cross a
  nested `TyForall`, nested owner, or nested scheme-root boundary.
- Fail-closed reject condition: any such crossing is rejected immediately and
  does not authorize a different target or a broader search.
- Ambiguity behavior: there is no alternate quantified rescue path; the case
  fails closed rather than choosing a neighboring route.
- Termination contribution: the quantified-boundary walk is over a finite
  acyclic ancestry and uses rejection rather than recursive reopening.

This rule lifts `boundHasForallFrom` and `not hasForall` into a general
negative admissibility rule only. It does not upgrade the current
negative-only record into a positive `P5 polymorphism-nested-forall` success
claim.

### `R5` Target / consumer alignment and neighboring-route exclusion

- Trigger: a lane-bounded candidate has survived owner / binder and quantified
  admissibility.
- Required evidence: one explicit downstream consumer route is already tied to
  that candidate family.
- Admitted candidate shape: only the route explicitly aligned with the
  admitted family remains live.
- Fail-closed reject condition: `schemeBodyTarget`, `rootFinal`,
  `boundTarget`, preserved local continuity lanes, and other adjacent routes
  are rejected as recursive candidates unless an explicit alignment rule
  chooses them.
- Ambiguity behavior: if two different consumer routes remain live for one
  subject after admissibility filtering, reject under `N1`; route order may
  not be used as a hidden ranking rule.
- Termination contribution: the route vocabulary is closed and finite, so
  rejected neighboring routes are not reopened later.

### `R6` Ambiguity and unsafe-case rejection

- Trigger: more than one recursive candidate survives, or a candidate would
  violate ownership, scope, or soundness discipline.
- Required evidence: none beyond the failure itself; ambiguity and unsoundness
  are reject conditions, not search opportunities.
- Admitted candidate shape: none.
- Fail-closed reject condition: reject under `N1` for competing candidates and
  under `N2` for owner / binder / scope violations.
- Ambiguity behavior: no locality ranking, lane-specific guessing, post-hoc
  repair, fallback, or second interface is allowed.
- Termination contribution: ambiguity rejection stops the search at the first
  unresolved overlap instead of expanding the candidate set.

### `R7` Unsupported-territory boundary

- Trigger: a case would require cyclic structure, multi-SCC search, hidden
  search reopening, or a second interface / fallback path.
- Required evidence: the need for forbidden growth is itself sufficient
  evidence that the case is outside the lawful item-4 search model.
- Admitted candidate shape: none.
- Fail-closed reject condition: reject as `N4`, `N5`, or `N6` territory rather
  than silently stretching the current model.
- Ambiguity behavior: unsupported territory is not converted into a lower-rank
  success path.
- Termination contribution: unsupported cases stop at the boundary instead of
  generating unbounded search.

## Named Guard Lift And Blocker Record

Item `3` left four packet-specific guard names as search debt. Item `4`
resolves them as follows:

| Guard or debt | Item-4 disposition | Bounded read |
| --- | --- | --- |
| `rootNonLocalSchemeAliasBaseLike` | Lifted into `R2`. | Generalized only as a non-local alias-bound / base-like admission rule tied to one anchor, one owner binder, and one explicit `baseTarget -> baseC -> targetC` handoff. |
| `sameLaneLocalRetainedChildTarget` | Lifted into `R3`. | Generalized only as a same-lane retained-child admission rule tied to one owner-local frame and one explicit `keepTargetFinal -> targetC` handoff. |
| `boundHasForallFrom` | Lifted into `R4`. | Generalized only as a reject-side quantified-boundary walk for nested `forall`, nested owner, or nested scheme-root crossings. |
| `not hasForall` | Lifted into `R4`. | Generalized only as the admissibility precondition that the quantified-boundary walk stayed clear. |
| Positive `P5` recursive-polymorphism success | Remains blocker debt. | Item `4` can state when quantified crossings are rejected, but the accepted record still does not justify admitting nested-`forall` recursion as a positive capability. |

These lifts are deliberately narrower than a claim that the repo already has
general recursive search. They merely replace packet folklore with bounded
admissibility rules for the two accepted mechanism families plus one explicit
quantified-boundary rejection rule.

## Fail-Closed Situations

Even when a lawful anchor exists, recursive inference is forbidden in all of
the following situations:

- the case crosses a nested `forall`, nested owner, or nested scheme-root
  boundary without new accepted positive evidence;
- the candidate leaves the owner / binder frame that exposed the anchor;
- the candidate would have to jump to a neighboring route such as
  `schemeBodyTarget`, `rootFinal`, `boundTarget`, or another adjacent lane
  that no explicit alignment rule selected;
- multiple recursive candidates survive after admissibility filtering;
- the case would need heuristic ranking, lane-specific guessing, post-hoc
  repair, or later fallback recovery;
- the case would require cyclic structural graphs, multi-SCC search, or a
  second interface; or
- the case would silently reinterpret bounded predecessor packets as proof of
  general capability.

This fail-closed policy keeps the family matrix honest:

- `N1 ambiguity-reject` owns competing-candidate rejection;
- `N2 unsoundness-guard` owns owner / binder / scope violations;
- `N4 cyclic-or-multi-scc-required` and
  `N5 second-interface-or-fallback-required` remain explicit boundary cases;
- `N6 termination-pressure` must be answered by bounded success or bounded
  rejection, never by silent search growth.

## Termination Discipline

The search model terminates only because it stays lane-bounded inside the
inherited acyclic representation.

Allowed growth:

- visiting the finite set of recursive-shape anchors already present in the
  acyclic node and binding structure;
- walking finite owner / binder ancestry and `boundHasForallFrom` paths once
  per candidate;
- enumerating the finite child / bound tuples already present in one owner
  frame; and
- checking the fixed downstream consumer vocabulary of the admitted route
  families.

Forbidden growth:

- inventing new anchors from failed consumers;
- reopening rejected neighboring routes after a failure;
- switching from one owner frame to unrelated owners or unrelated mechanism
  families;
- introducing cyclic structural graphs, graph cycles, or multi-SCC traversal;
- retrying through a second interface, alternate executable, or fallback
  branch; and
- using hidden ranking to keep expanding the candidate set after ambiguity.

This boundedness claim is therefore precise:

- anchors are finite because the current graph is acyclic and already built;
- owner / binder ancestry is finite because the binding tree is finite;
- candidate routes are finite because item `4` admits only lane-bounded
  explicit routes; and
- rejected routes stay rejected because no reopening loop is authorized.

This directly answers `N6 termination-pressure` only inside the current
boundary. It does not prove that every future `P2`-`P5` pressure point will
fit comfortably inside the acyclic model. If later representative coverage can
only proceed by violating one of the forbidden-growth clauses above, that is
blocker debt for later items rather than silent success inside item `4`.

## Bounded Item-4 Outcome

Bounded item-4 result: one principled search / admissibility / ambiguity /
termination model is now stated for the inherited acyclic architecture.

That model says:

- recursive inference starts from anchor-first recursive-shape discovery, not
  from consumer-first guessing;
- the lawful candidate vocabulary is limited to one non-local alias-bound /
  base-like family and one same-lane retained-child family, each tied to one
  owner / binder frame and one explicit downstream consumer route;
- quantified crossings remain reject-only boundaries unless later accepted
  evidence changes that read;
- ambiguity fails closed instead of being resolved by hidden ranking or
  fallback; and
- termination comes from finite acyclic anchors, finite ancestry, finite
  lane-bounded routes, and no reopening loops.

Named unresolved blocker debt remains explicit:

- positive `P5 polymorphism-nested-forall` success is still not justified;
- full-pipeline `P6 reconstruction-visible-output` proof remains item `5`
  work;
- representative `P2`-`P6` plus `N6` coverage remains item `6` work; and
- the final architecture decision, especially if `non-cyclic-graph` pressure
  stays unresolved, remains item `7` work.

This artifact therefore stops short of any claim that the current
architecture already delivers general automatic iso-recursive inference.

## Docs-Only Verification Note

This round is expected to change only documentation:

- this canonical item-4 search-model artifact; and
- `orchestrator/rounds/round-085/implementation-notes.md`.

Because the round does not touch `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`, no full `cabal build all && cabal test` gate is triggered
unless the diff escapes that docs-only surface.

Reviewer-facing expectations inherited from `orchestrator/verification.md`
that this artifact is written to satisfy:

- it defines candidate generation, ambiguity handling, rejection conditions,
  and termination discipline;
- it preserves the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback boundary;
- it treats accepted predecessor packets as bounded evidence only;
- it avoids hidden heuristic widening; and
- where the accepted record does not justify a stronger claim, it fails closed
  by naming blocker debt or the later roadmap item that still owns the open
  question.
