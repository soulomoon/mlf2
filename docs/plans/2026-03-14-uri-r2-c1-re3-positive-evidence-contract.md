# `RE3` Prototype-Free Positive-Evidence Contract For `URI-R3-O1` Through `URI-R3-O3`

Date: 2026-03-14  
Round: 013  
Roadmap item: 3  
Stage: `RE3` positive-evidence contract  
Obligations: `URI-R3-O1`, `URI-R3-O2`, `URI-R3-O3`  
Active subject: `URI-R2-C1`  
Artifact kind: evidence contract only

## Purpose

This document executes roadmap item 3 by defining what later review may count as prototype-free positive evidence for `URI-R3-O1`, `URI-R3-O2`, and `URI-R3-O3` inside the fixed `URI-R2-C1` boundary.

This document does **not** decide whether `URI-R3-O1` through `URI-R3-O3` are cleared. It defines only:

- what evidence would count as bounded positive evidence for later review; and
- what evidence remains inadmissible and must keep one or more gates closed.

## Inherited Authority Chain

This round is controlled by inherited accepted artifacts rather than by new prototype or implementation evidence.

Direct authority inputs for this contract:

- `docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md` defines `RE3` as the positive-evidence stage in the approved `RE1` -> `RE5` ladder and preserves the re-entry-only scope.
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` records `URI-R3-O1`, `URI-R3-O2`, and `URI-R3-O3` as unresolved blockers after `RE1` and `RE2`.
- `docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md` records the missing docs-only positive evidence and the inherited no-go classes that keep the bounded subject unresolved.
- `docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md` defines the hard `URI-R3-O1`, `URI-R3-O2`, and `URI-R3-O3` contracts and their fail-closed rejection conditions.
- `docs/plans/2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract.md` remains the prerequisite authority contract for one bounded provenance-stable subject and is not rewritten here.
- `docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md` remains the prerequisite uniqueness contract for that same bounded subject and is not rewritten here.
- `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` remains the authoritative inherited audit for acyclicity, binding-tree discipline, occurs-check or termination boundaries, reconstruction or reification or witness replay, and principality-risk boundaries.
- `orchestrator/rounds/round-013/selection.md` is the current round-selection authority for item 3 only.

Interpretation:

- `RE3` carries forward the inherited invariant-audit authority rather than inventing new acyclicity, ownership, replay, search, or termination semantics.
- `RE3` depends on `RE1` and `RE2` prerequisite authority for one bounded provenance-stable and uniquely admissible subject, but it does not collapse positive evidence into provenance authority or uniqueness clearance.
- `RE3` exists to make the admissible positive-evidence burden reviewer-checkable.
- `RE3` does not upgrade inherited unresolved evidence into clearance.

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

If any claimed positive evidence would require broader search, broader ownership, replay-domain expansion, cyclic graph modeling, equi-recursive reasoning, default-on widening, or prototype-backed invention, the claim is out of scope and the gate remains closed.

## Contract Subject

The bounded subject for `RE3` is exactly the already-bounded local root or one equivalent local cluster inside `URI-R2-C1` that `RE1` requires to be provenance-stable and that `RE2` requires to be uniquely admissible.

That same bounded subject determines all three positive-evidence obligations:

- for `URI-R3-O1`, the structural `TyNode` / constraint slice touched by that same local root or equivalent local cluster;
- for `URI-R3-O2`, the deterministic parent-chain owner for every obligation in that same local root or equivalent local cluster;
- for `URI-R3-O3`, the constructor-directed reasoning path used to classify and discharge that same local root or equivalent local cluster.

Required subject properties:

- the subject is finite and local to `URI-R2-C1`;
- the subject remains inside one `single-SCC` and one `single-binder-family`;
- the subject does not expand into a widened owner family, replay-domain expansion, or non-local search surface;
- the structural slice, owner chain, and constructor-directed reasoning path are all judged for that same bounded subject only; and
- no outside root, outside cluster, outside owner family, or widened search result may enter the subject definition.

Equivalent local cluster means only a bounded cluster whose members are already part of the same local admissible subject and do not require widened ownership, replay-domain repair, non-local search, or post hoc salvage to be treated as one subject.

## Admissible Evidence For `URI-R3-O1`

Later review may count evidence as positive structural-acyclicity evidence for `URI-R3-O1` only if it satisfies all of the following reviewer-checkable requirements.

### 1. One local obligation SCC only

The evidence must identify one local obligation SCC only for the bounded subject inside `URI-R2-C1`.

Admissible evidence must show:

- the obligation account stays inside one local SCC only;
- no second dependency component becomes necessary to state the claim; and
- the same bounded subject remains the only structural subject under review.

### 2. One structural slice that remains acyclic and binder-mediated

The evidence must show that the structural `TyNode` / constraint slice touched by the bounded subject remains acyclic and binder-mediated throughout.

Admissible evidence must show:

- one finite structural slice only;
- no structural back-edge in that slice;
- no cyclic `TyNode` encoding in that slice; and
- recursion is represented through binder-mediated structure rather than cyclic structural encoding.

### 3. No structural escape beyond the fixed boundary

The evidence must show that acyclicity does not depend on widening the subject beyond `URI-R2-C1`.

Admissible evidence must show:

- no second SCC or dependency component is imported;
- no non-local structural salvage is needed;
- no widened ownership or widened search is used to justify acyclicity; and
- no replay or termination exception is used to excuse a cyclic structure.

### 4. Consistency with inherited acyclicity and replay or termination boundaries

The evidence must remain consistent with the authoritative inherited invariant audit and the `R3` obligation contract.

Admissible evidence must show:

- structural acyclicity remains compatible with the inherited non-cyclic structural-graph requirement;
- the same bounded subject remains compatible with inherited replay constraints from `RE1`;
- the claim does not weaken inherited occurs-check or termination boundaries relevant to `URI-R3-O3`; and
- the claim does not reinterpret cyclic pressure as admissible just because the obligation graph has one SCC.

## Admissible Evidence For `URI-R3-O2`

Later review may count evidence as positive single-family ownership evidence for `URI-R3-O2` only if it satisfies all of the following reviewer-checkable requirements.

### 1. One closed binder family only

The evidence must identify one closed binder family only for the bounded subject inside `URI-R2-C1`.

Admissible evidence must show:

- every obligation in the bounded subject stays inside one binder family;
- no mixed-owner or sibling-family interpretation remains live; and
- no cross-family SCC linking enters the ownership account.

### 2. One deterministic parent-chain owner for every obligation

The evidence must show one deterministic parent-chain owner for every obligation in the bounded subject.

Admissible evidence must show:

- every obligation has exactly one owner;
- each owner is reachable by one deterministic parent chain only;
- owner identity remains stable across the bounded subject; and
- no reviewer must guess between parallel owner chains.

### 3. No ownership drift, repair, or family widening

The evidence must show that ownership remains stable without repair or widened family search.

Admissible evidence must show:

- no mixed-owner cluster appears;
- no shadow-driven ownership ambiguity appears;
- no sibling-family interaction is needed;
- no cross-family ownership drift appears;
- no owner repair or parent-chain repair is needed; and
- no widened ownership is needed to preserve the claim.

### 4. Consistency with inherited binding-tree, replay, and reification boundaries

The evidence must remain consistent with the authoritative inherited invariant audit and the prerequisite `RE1` and `RE2` contracts.

Admissible evidence must show:

- binding-tree discipline remains single-family and deterministic;
- replay and reification still consume the same bounded subject rather than a repaired owner family;
- ownership remains compatible with the provenance-stable subject required by `RE1`; and
- ownership remains compatible with the uniquely admissible subject required by `RE2`.

## Admissible Evidence For `URI-R3-O3`

Later review may count evidence as positive occurs-check or termination evidence for `URI-R3-O3` only if it satisfies all of the following reviewer-checkable requirements.

### 1. Constructor-directed local reasoning only

The evidence must show that the bounded subject is classified and discharged by constructor-directed local reasoning only.

Admissible evidence must show:

- the reasoning path stays local to the bounded subject;
- the reasoning path remains constructor-directed throughout; and
- no reviewer must appeal to non-local recursive search to explain the classification.

### 2. No implicit unfolding or equi-recursive search

The evidence must show that the claim does not depend on implicit unfolding or equi-recursive reasoning.

Admissible evidence must show:

- no implicit unfolding;
- no equi-recursive equality search; and
- no speculative recursive-equality chase.

### 3. No termination-class weakening or open-ended search

The evidence must show that the claim stays inside the inherited occurs-check and termination discipline.

Admissible evidence must show:

- no termination-class weakening;
- no open-ended recursive search;
- no repeated speculative refinement used to discover the bounded subject; and
- no change in the current occurs-check rejection boundary.

### 4. Consistency with inherited occurs-check, termination, and acyclicity boundaries

The evidence must remain consistent with the authoritative inherited invariant audit and the `R3` obligation contract.

Admissible evidence must show:

- the claim stays within the inherited occurs-check discipline;
- the claim stays within the inherited termination class;
- the claim remains compatible with the inherited non-cyclic structural-graph requirement relevant to `URI-R3-O1`; and
- the claim does not reinterpret forbidden recursive search as admissible local reasoning.

## Rejected Evidence

The following evidence remains inadmissible for `RE3` and must not be treated as positive evidence for `URI-R3-O1`, `URI-R3-O2`, or `URI-R3-O3`:

- prototype-backed claims;
- experiment-backed claims;
- implementation-drift-backed claims;
- explicit-anchor replay substitution used to manufacture positive evidence;
- widened ownership;
- owner repair;
- parent-chain repair;
- widened search;
- non-local propagation used to discover or stabilize the subject;
- multi-cluster salvage;
- multi-SCC salvage;
- cyclic structural encoding;
- structural back-edge dependence;
- implicit unfolding;
- equi-recursive reasoning;
- termination weakening.

These rejections are fail-closed. If later evidence relies on any rejected class, one or more of `URI-R3-O1`, `URI-R3-O2`, or `URI-R3-O3` must remain uncleared.

## Reviewer Gate

A later reviewer may mark `URI-R3-O1`, `URI-R3-O2`, and `URI-R3-O3` cleared only by pointing to evidence that demonstrates all of the following at once for the same bounded subject:

- one already-bounded local root or one equivalent local cluster inside `URI-R2-C1`, already authoritative under `RE1` and uniquely admissible under `RE2`;
- one local obligation SCC only for that same bounded subject;
- one structural `TyNode` / constraint slice for that same subject that remains acyclic, binder-mediated, and free of structural back-edges or cyclic encoding;
- one closed binder family only, with one deterministic parent-chain owner for every obligation in that same subject and no mixed-owner or cross-family drift;
- one constructor-directed local reasoning path for that same subject, with no implicit unfolding, no equi-recursive equality search, no speculative recursive-equality chase, and no termination-class weakening;
- consistency with the inherited invariant audit for acyclicity, binding-tree discipline, replay or reification boundaries, occurs-check, and termination.

A later reviewer must keep one or more of `URI-R3-O1`, `URI-R3-O2`, or `URI-R3-O3` uncleared if any of the following findings appear:

- more than one local SCC, more than one dependency component, or any multi-cluster or multi-SCC salvage remains necessary;
- the structural slice requires a structural back-edge, cyclic encoding, or any other non-cyclic exception;
- any obligation lacks one deterministic owner, needs owner repair, needs parent-chain repair, or drifts outside one closed binder family;
- the reasoning path requires implicit unfolding, equi-recursive equality search, speculative recursive-equality chase, widened search, non-local propagation, or termination weakening;
- the claim depends on explicit-anchor replay substitution, prototype evidence, experiment results, or implementation drift rather than inherited bounded authority;
- the claim cannot stay fixed to the same bounded subject already controlled by `RE1` and `RE2`.

## Fail-Closed Evidence Sources

This round uses inherited docs and accepted evidence only.

For this contract:

- no production code change may be used to manufacture positive evidence;
- no test change may be used to manufacture positive evidence;
- no Cabal change may be used to manufacture positive evidence; and
- if positive evidence cannot be shown without prototype-backed evidence, implementation drift, explicit-anchor replay substitution, widened ownership or widened search, cyclic encoding, or termination weakening, the later gate must remain closed.

This source discipline preserves the accepted bounded stop posture until prototype-free positive evidence exists without implementation-backed invention.

## Stage Discipline

This document executes `RE3` only.

This document does **not**:

- settle `RE4` re-entry;
- settle `RE5` recommendation;
- reopen or rewrite `RE1`;
- reopen or rewrite `RE2`; or
- authorize implementation planning or production work.

This document is not a feasibility decision, not a re-entry verdict, not an implementation handoff, and not implementation clearance.

## Continuity

Completed rounds `001` through `012` and the predecessor recursive-types packet remain inherited evidence only and are not rewritten here.

Continuity statement:

- inherited accepted rounds remain historical authority;
- the accepted `R5` stop remains authoritative until a later stage clears the bounded gates;
- the approved re-entry design remains the staging authority for `RE1` through `RE5`; and
- this round does not reopen or rewrite predecessor packet logs or prior round artifacts.
