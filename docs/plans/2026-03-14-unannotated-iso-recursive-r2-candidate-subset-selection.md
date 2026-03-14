# Unannotated Iso-Recursive `R2` Candidate Subset Selection

Date: 2026-03-14  
Round: 007  
Status: docs-only bounded subset-selection artifact for successor roadmap item 2.

## Inherited Baseline (`R0` -> `R1`)

This `R2` selection starts from accepted `R0` / `ARI-C1`, not from any claim that broad unannotated recursive inference is already available.

Authoritative inherited evidence inputs:

- `docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` is the immediate predecessor artifact and the narrowing input for this round.
- `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` remains the authoritative inherited invariant audit.
- `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md` remains the roadmap-design source for the fixed boundary model.

Interpretation:

- `R0` / `ARI-C1` fixed the accepted bounded baseline: annotation-anchored recursive-shape handling only.
- `R1` named the finite delta from that baseline to the bounded unannotated target, especially `G1` and `G2`.
- This document performs the `R2` selection only; it does not reopen the inherited audit or replace prior evidence.

## Selected Candidate Subset

Candidate ID: `URI-R2-C1`

Name: Unique local obligation root with one closed binder-family cluster.

Problem statement:

`URI-R2-C1` admits only those unannotated `single-SCC`, `single-binder-family` cases where local self-reference evidence inside one binder family yields exactly one stable candidate recursive root, or one stable local obligation cluster equivalent to that root, and bounded local dependency construction closes that evidence into one obligation cluster without cross-family search, heuristic tie-breaking, or structural-graph cycles.

## Admissibility Contract

This admissibility contract is provisional and fail-closed. It narrows `G1` and `G2` only, while carrying `G3` through `G6` forward as still-authoritative constraints rather than solved obligations.

| Rule ID | Positive admission rule for `URI-R2-C1` | `R1` gaps narrowed or carried forward | Authoritative inherited invariant classes | Cases kept outside the subset |
| --- | --- | --- | --- | --- |
| A1 | Obligation discovery may admit an unannotated case only when one local syntactic region inside one binder family yields one stable candidate recursive root, or one stable local obligation cluster, without explicit anchors and without non-local propagation. | Narrows `G1`; carries `G6` forward because uniqueness must remain non-heuristic. | Binding/tree discipline; occurs-check and termination; principality risk boundaries. | No discovered root, multiple competing roots, root evidence that appears only after non-local propagation, or any heuristic-search-dependent root choice. |
| A2 | Starting from the discovered root/cluster, bounded local dependency construction must produce one closed obligation cluster whose members all remain inside one `single-SCC`. | Narrows `G2`; depends on the `G1` root being stable. | Acyclicity assumptions; binding/tree discipline; occurs-check and termination. | Multi-cluster cases, ambiguous SCC cuts, dependencies that escape the local cluster, or any case that needs cross-cluster comparison to decide admissibility. |
| A3 | Every admitted obligation in the cluster must have one stable owner in one `single-binder-family`, with no cross-family SCC linking and no competing parent-chain ownership claims. | Keeps `G2` bounded; carries `G3` forward as an unsolved ownership contract. | Binding/tree discipline; reconstruction/reification/witness replay obligations. | Mixed-owner clusters, shadowed or sibling-family interactions, ownership that changes during discovery, or any cross-family cluster. |
| A4 | Admission may rely only on iso-recursive, constructor-directed local evidence that preserves a non-cyclic structural graph; no implicit unfolding, no equi-recursive reasoning, and no cyclic structural graph encoding are allowed. | Supports `G1`/`G2`; carries `G4` forward as a later obligation contract. | Acyclicity assumptions; occurs-check and termination. | Cases that require implicit unfolding, recursive equality reasoning, speculative recursive equalities, or cyclic `TyNode` / constraint-graph encodings. |
| A5 | The discovered root/cluster must remain provenance-stable enough that later stages can ask whether reconstruction, reification, witness replay, and principality checks have one bounded subject to validate; if that subject is unstable, the case fails closed here. | Carries `G5` and `G6` forward explicitly; does not solve them in `R2`. | Reconstruction/reification/witness replay obligations; binding/tree discipline; principality risk boundaries. | Provenance-unstable roots, binder naming that depends on later repair, witness-domain ambiguity, or any case where admissibility would depend on a widening-dependent guess. |

## Deferred Alternatives

The following alternatives remain deferred rather than admitted:

- Unannotated cases with more than one locally plausible root inside one binder family, if a later stage can justify a non-heuristic tie-break rule without widening beyond the fixed boundary model.
- Unannotated cases that still stay inside one binder family but require bounded cross-region propagation before the unique local cluster becomes visible.
- Unannotated cases whose ownership is still single-family but whose replay/provenance contract needs the later `R3` obligation-writing stage before they can be classified.

## Rejected Alternatives

The following alternatives are rejected by the fixed boundary model for this roadmap stage:

- Multi-SCC subsets.
- Any subset that admits cross-family SCC linking.
- Any equi-recursive or implicit-unfolding design.
- Any cyclic structural graph encoding.
- Any design that depends on ambiguous, provenance-unstable, or heuristic global search instead of one bounded local root/cluster.

## Stage Discipline

- This document selects the `R2` subset only.
- This document does not write the `R3` obligation contract.
- This document does not record an `R4` feasibility outcome.
- This document does not draft the `R5` handoff or research-stop artifact.
- `R3` through `R5` remain future stages and were not executed here.

## Continuity

Completed rounds `001` through `006` and the predecessor recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/` are cited as inherited evidence only.

Continuity statement:

- The inherited automatic-recursive-inference evidence chain remains authoritative for `R0` / `ARI-C1`.
- The inherited invariant audit remains authoritative and was mapped into this admissibility contract rather than re-audited from scratch.
- No predecessor packet history, no prior round artifact, and no inherited authoritative record was rewritten by this `R2` selection document.
