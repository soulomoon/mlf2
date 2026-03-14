# `RE1` Provenance-Authority Evidence Contract For `URI-R3-O4`

Date: 2026-03-14  
Round: 011  
Roadmap item: 1  
Stage: `RE1` provenance-authority evidence contract  
Obligation: `URI-R3-O4`  
Active subject: `URI-R2-C1`  
Artifact kind: evidence contract only

## Purpose

This document executes roadmap item 1 by defining what later review may count as authoritative provenance-stable evidence for `URI-R3-O4` inside the fixed `URI-R2-C1` boundary.

This document does **not** decide whether `URI-R3-O4` is cleared. It defines only:

- what evidence would count as authoritative for later review; and
- what evidence remains inadmissible and must keep the gate closed.

## Inherited Authority Chain

This round is controlled by inherited accepted artifacts rather than by new prototype or implementation evidence.

Direct authority inputs for this contract:

- `docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md` defines `RE1` as the first provenance-authority stage in the approved `RE1` -> `RE5` ladder and preserves the re-entry-only scope.
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` records `URI-R3-O4` as the decisive inherited blocker and preserves the bounded `research-stop`.
- `docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md` records the unresolved docs-only provenance gap and the no-go triggers that keep `URI-R2-C1` at `not-yet-go`.
- `docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md` defines the hard `URI-R3-O4` contract and its fail-closed rejection conditions.
- `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` remains the authoritative inherited audit for reconstruction, reification, witness replay, binding-tree discipline, acyclicity, and occurs-check or termination boundaries.
- `orchestrator/rounds/round-011/selection.md` is the current round-selection authority for item 1 only.

Interpretation:

- `RE1` carries forward the inherited invariant-audit authority rather than inventing new replay, reification, provenance, or binder semantics.
- `RE1` exists to make the admissible provenance-authority evidence shape reviewer-checkable.
- `RE1` does not upgrade inherited unresolved evidence into clearance.

## Fixed Boundary

The active subject and boundary remain unchanged and mandatory:

- `URI-R2-C1` only;
- `single-SCC` obligation-level recursion only;
- `single-binder-family` ownership only;
- no cross-family SCC linking;
- non-equi-recursive semantics only;
- non-cyclic structural graph only;
- no default-on widening;
- prototype-free evidence only.

If any claimed provenance authority would require broader search, broader ownership, cyclic graph modeling, equi-recursive reasoning, or prototype-backed invention, the claim is out of scope and the gate remains closed.

## Contract Subject

The bounded subject for provenance authority is exactly one local unannotated root or one equivalent local cluster inside `URI-R2-C1`.

That same bounded subject must remain identifiable as the same subject across:

- `generalizeWithPlan`;
- `schemeToType`;
- `reifyTypeWithNamedSetNoFallback`; and
- witness replay.

Required subject properties:

- the subject is finite and local to `URI-R2-C1`;
- the subject does not split into multiple replay domains;
- the subject does not change identity by fallback widening or surrogate substitution;
- witness replay remains total for that same subject;
- binder identity remains stable enough that replay and reification do not depend on unstable renaming or binder-family drift.

Equivalent local cluster means only a bounded cluster whose members are already part of the same local provenance subject and do not require late repair, post hoc cluster assembly, or widened search to be treated as one subject.

## Admissible Evidence

Later review may count evidence as authoritative for `URI-R3-O4` only if it satisfies all of the following reviewer-checkable requirements.

### 1. Finite bounded provenance root or cluster

The evidence must identify one finite provenance root or one finite equivalent local cluster already inside `URI-R2-C1`.

Admissible evidence must show:

- the root or cluster is local rather than search-derived from a widened domain;
- the root or cluster is singular rather than chosen from competing alternatives; and
- the root or cluster is already present in the bounded unannotated subject instead of being manufactured by an external anchor.

### 2. Continuity across the inherited replay path

The same bounded subject must survive the inherited path
`generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback`
and witness replay without identity drift.

Admissible evidence must show:

- continuity across generalization, reconstruction, reification, and witness replay;
- no fallback widening at any stage;
- no surrogate-root substitution at any stage; and
- no stage where the reviewer must guess which later representative still counts as the original bounded subject.

### 3. No manufactured authority

The evidence must show that authority already exists in the bounded unannotated subject.

Admissible evidence must therefore exclude any need for:

- explicit-anchor import to create the root that the unannotated subject lacks;
- late repair that retroactively stabilizes the subject after provenance drift; or
- post hoc selection that chooses one candidate only after ambiguity has already appeared.

### 4. No replay-domain ambiguity

The evidence must show one replay domain only.

Admissible evidence must show:

- no replay-domain widening;
- no multiple replay domains for the same claimed subject;
- no ambiguity about which witness domain owns the subject; and
- no requirement to compare parallel domains, parallel roots, or parallel cluster interpretations.

### 5. Binding-tree and witness-domain consistency

The evidence must remain consistent with the inherited invariant audit for binding-tree discipline and witness-domain safety.

Admissible evidence must show:

- one stable binder family only;
- stable enough binder identity that replay and reification stay about the same bounded subject;
- no binder-family drift;
- witness replay totality for the claimed subject; and
- no conflict with the inherited no-fallback reification or replay-contract constraints.

## Rejected Evidence

The following evidence remains inadmissible for `RE1` and must not be treated as provenance authority:

- manufactured provenance authority that is not already present in the bounded unannotated subject;
- late root repair;
- retroactive cluster choice;
- replay-domain widening;
- multiple replay domains;
- surrogate-root substitution;
- unstable binder naming;
- binder-family drift;
- competing roots;
- competing clusters, even if one appears preferable;
- explicit-anchor import used to create authority rather than to cite inherited contrast;
- heuristic ranking between bounded candidates;
- widened provenance search outside `URI-R2-C1`;
- prototype-backed claims;
- experiment-backed claims;
- implementation-drift-backed claims.

These rejections are fail-closed. If later evidence relies on any rejected class, `URI-R3-O4` remains uncleared.

## Reviewer Gate

A later reviewer may mark `URI-R3-O4` cleared only by pointing to evidence that demonstrates all of the following at once:

- one finite local provenance root or equivalent local cluster inside `URI-R2-C1`;
- continuity of that same subject across `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback`;
- witness replay totality for that same subject;
- no manufactured authority, late repair, or post hoc root or cluster selection;
- no replay-domain ambiguity, surrogate-root substitution, or widened provenance search;
- stable binder identity within one binder family, consistent with the inherited binding-tree and witness-domain constraints.

A later reviewer must keep `URI-R3-O4` uncleared if any of the following findings appear:

- more than one live local root or cluster remains relevant to the claim;
- the claimed subject becomes identifiable only after repair, heuristic choice, or widened search;
- continuity breaks across generalization, reconstruction, reification, or replay;
- witness replay is partial, ambiguous, or multi-domain for the claimed subject;
- binder naming is unstable enough that subject identity depends on renaming-sensitive interpretation;
- the claim depends on prototype evidence, experiment results, or implementation drift rather than inherited bounded authority.

## Fail-Closed Evidence Sources

This round uses inherited docs and accepted evidence only.

For this contract:

- no production code change may be used to manufacture provenance authority;
- no test change may be used to manufacture provenance authority;
- no Cabal change may be used to manufacture provenance authority; and
- if the contract cannot be satisfied without prototype-backed evidence, the later gate must remain closed.

This source discipline preserves the accepted `research-stop` posture until bounded evidence exists without implementation-backed invention.

## Stage Discipline

This document executes `RE1` only.

This document does **not**:

- settle `RE2` uniqueness;
- settle `RE3` positive evidence;
- settle `RE4` re-entry;
- settle `RE5` recommendation; or
- authorize implementation planning or production work.

This document is not a feasibility decision, not a re-entry verdict, not an implementation handoff, and not implementation clearance.

## Continuity

Completed rounds `001` through `010` and the predecessor recursive-types packet remain inherited evidence only and are not rewritten here.

Continuity statement:

- inherited accepted rounds remain historical authority;
- the accepted `R5` stop remains authoritative until a later stage clears the bounded gates;
- the approved re-entry design remains the staging authority for `RE1` through `RE5`; and
- this round does not reopen or rewrite predecessor packet logs or prior round artifacts.
