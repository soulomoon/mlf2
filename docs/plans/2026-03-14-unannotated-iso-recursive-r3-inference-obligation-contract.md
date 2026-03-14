# Unannotated Iso-Recursive `R3` Inference-Obligation Contract

Date: 2026-03-14  
Round: 008  
Status: docs-only obligation-contract artifact for successor roadmap item 3.

## Inherited Baseline (`R0` -> `R2`)

This `R3` contract starts from accepted `R0` / `ARI-C1` as the inherited starting point, using the accepted automatic-recursive-inference evidence chain as baseline context rather than reopening that earlier track.

Authoritative inherited evidence inputs:

- `docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` is the immediate gap-map predecessor.
- `docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md` is the chosen-subset predecessor and fixes `URI-R2-C1` as the only active subset.
- `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` remains the authoritative inherited invariant audit.
- `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md` remains the roadmap-design source for the successor staging and boundary model.
- The accepted `R0` / `ARI-C1` baseline remains the starting truth carried forward from the inherited automatic-recursive-inference chain rather than rederived here.

Interpretation:

- `R1` named the finite delta from accepted `R0` / `ARI-C1` to the bounded unannotated target.
- `R2` selected exactly one bounded subject, `URI-R2-C1`, and kept all broader alternatives deferred or rejected.
- `R3` writes the obligation contract for that already-selected subset only.

## Fixed Boundary Model

The contract preserves the roadmap's mandatory boundary without weakening it:

- `URI-R2-C1` remains `single-SCC` and `single-binder-family` only.
- Recursion remains obligation-level rather than cyclic structural-graph encoding.
- No cross-family SCC linking is admitted.
- No equi-recursive reasoning or implicit unfolding is admitted.
- No default-on widening is admitted.
- The structural `TyNode` / constraint graph remains non-cyclic, and ambiguous or widening-dependent cases fail closed.

## Obligation Matrix

### `URI-R3-O1` - Structural Acyclicity Contract

- Bounded subject inside `URI-R2-C1`: the unique local obligation root, or equivalent local obligation cluster, together with the structural `TyNode` / constraint slice that the cluster touches.
- Hard contract statement: any admitted `URI-R2-C1` obligation may form one SCC only in the obligation graph; the corresponding structural `TyNode` / constraint graph must remain acyclic and binder-mediated, never cyclically encoded.
- Authoritative inherited invariant classes: acyclicity assumptions; occurs-check and termination risk boundaries where structural reachability is consumed.
- Fail-closed rejection condition: reject any case whose recursive-shape account requires a structural back-edge, cyclic `TyNode` / constraint encoding, or a second dependency component beyond the one bounded obligation SCC.
- Future `R4` evidence shape needed: positive examples must show one local recursive obligation cluster whose structural graph remains acyclic; negative examples must show cyclic-encoding pressure or a second component that forces rejection.

### `URI-R3-O2` - Binder Ownership And Scope-Discipline Contract

- Bounded subject inside `URI-R2-C1`: every member obligation inside the one closed local cluster and its deterministic parent-chain owner.
- Hard contract statement: every admitted obligation must have one stable owner inside one closed binder family, with deterministic parent-chain ownership and scope discipline from discovery through replay; no cluster member may migrate owners or escape that family.
- Authoritative inherited invariant classes: binding/tree discipline; reconstruction/reification/witness replay obligations where stable binder ownership is consumed.
- Fail-closed rejection condition: reject mixed-owner clusters, shadow-driven ownership ambiguity, sibling-family interaction, cross-family SCC linking, or any case where parent-chain ownership becomes repair-dependent instead of deterministic.
- Future `R4` evidence shape needed: positive examples must show one stable binder family with one reproducible owner chain for every obligation; negative examples must show mixed-owner, shadowed, or cross-family cases that remain rejected.

### `URI-R3-O3` - Occurs-Check And Termination Contract

- Bounded subject inside `URI-R2-C1`: the constructor-directed reasoning path used to discharge the unique local obligation root or equivalent local cluster.
- Hard contract statement: `URI-R2-C1` admission may rely only on existing constructor-directed, iso-recursive reasoning under current occurs-check discipline; no implicit unfolding, no equi-recursive equality search, and no speculative recursive-equality chase may be introduced, and the termination class must remain unchanged.
- Authoritative inherited invariant classes: occurs-check and unification termination risks; acyclicity assumptions where traversal discipline bounds search.
- Fail-closed rejection condition: reject any case that requires recursive unfolding to classify the local root, repeated speculative refinement to discover the SCC, or any equality reasoning that exceeds constructor-directed matching.
- Future `R4` evidence shape needed: positive examples must stay decidable under constructor-directed local reasoning; negative examples must expose cases that only become classifiable after implicit unfolding or open-ended recursive search.

### `URI-R3-O4` - Reconstruction, Reification, And Witness Replay Contract

- Bounded subject inside `URI-R2-C1`: the one provenance-stable obligation root or equivalent local cluster that must survive generalization, reification, and witness replay as the same bounded subject.
- Hard contract statement: every admitted `URI-R2-C1` case must preserve one provenance-stable root/cluster through generalization, reconstruction, reification, and witness replay without widening, surrogate-root substitution, replay-domain ambiguity, or binder-name instability.
- Authoritative inherited invariant classes: reconstruction/reification/witness replay obligations; binding/tree discipline.
- Fail-closed rejection condition: reject any case where reconstruction would need multiple candidate roots, late root repair, replay-domain widening, unstable binder naming, or witness provenance that is not total for the admitted cluster.
- Future `R4` evidence shape needed: positive examples must show one stable subject roundtripping through generalization, reification, and replay; negative examples must show provenance drift, replay ambiguity, or root instability that keeps the case rejected.

### `URI-R3-O5` - Principality-Risk Boundary Contract

- Bounded subject inside `URI-R2-C1`: the admission decision for the chosen local root/cluster as a whole.
- Hard contract statement: admissibility for `URI-R2-C1` must remain unique, bounded, and fail-closed; if more than one local root, more than one cluster, or more than one solver-choice-dependent recursive shape is live, the case is out of scope rather than heuristically ranked.
- Authoritative inherited invariant classes: principality risk boundaries; occurs-check and termination risks; reconstruction/reification/witness replay obligations where bounded uniqueness is consumed.
- Fail-closed rejection condition: reject ambiguous or multi-root discovery, multi-cluster competition, heuristic tie-breaking, provenance-unstable admission, or any case that would widen the search space beyond the fixed boundary model.
- Future `R4` evidence shape needed: positive examples must show one uniquely admissible local root/cluster with no competing bounded interpretation; negative examples must show ambiguous admissions, heuristic ranking pressure, or widening-dependent success.

## Explicit Fail-Closed Rejection Classes

The following classes remain rejected at `R3` and are not softened by the obligation contract:

- Ambiguous or multi-root obligation discovery.
- Multi-cluster or multi-SCC dependency shapes.
- Mixed-owner or cross-family ownership claims.
- Implicit unfolding or equi-recursive pressure.
- Cyclic structural-graph encodings.
- Provenance-unstable reconstruction, reification, or witness replay subjects.
- Any case that would require widening beyond the fixed boundary model.

Interpretation:

- `R3` does not convert any rejection class into an admitted edge case.
- If an example falls into one of the classes above, the contract requires rejection rather than heuristic repair.

## Future Feasibility Handoff For `R4`

`R4` will have to judge evidence against this contract one obligation class at a time.

| Obligation class | Positive example classes `R4` must check | Negative example classes `R4` must check | No-go triggers for that class | Immediate stop conditions |
| --- | --- | --- | --- | --- |
| Acyclicity | One local obligation SCC whose structural graph remains acyclic and binder-mediated throughout. | Cases that need structural cycles, a second SCC, or escaping dependency components. | Any claimed success that depends on cyclic structural encoding or more than one dependency component. | Stop immediately if the structural graph ceases to be non-cyclic. |
| Binding and scope | One closed binder-family cluster with deterministic parent-chain ownership for every obligation. | Mixed-owner, shadowed-owner, sibling-family, or cross-family dependency cases. | Any ownership proof that depends on repair, guessing, or cross-family linking. | Stop immediately if owner identity is not stable for every obligation in the cluster. |
| Occurs-check and termination | Constructor-directed local self-reference cases that terminate under the current discipline. | Cases that require implicit unfolding, recursive equality search, or speculative reclassification. | Any success path that changes the termination class or weakens current occurs-check rejection. | Stop immediately if implicit unfolding is needed to classify the case. |
| Reconstruction, reification, witness replay | One provenance-stable subject that roundtrips through generalization, reification, and replay unchanged in scope. | Cases with replay-domain ambiguity, unstable binder naming, or late root substitution. | Any result that needs widening, fallback reinterpretation, or multiple replay subjects. | Stop immediately if the bounded subject cannot be preserved as one stable provenance root/cluster. |
| Principality boundary | Cases with one unique admissible root/cluster and no heuristic branching. | Cases with competing local roots, competing clusters, or widening-dependent admission. | Any success argument that depends on heuristic ranking or silent broadening of the search surface. | Stop immediately if uniqueness cannot be established without widening. |

## Stage Discipline

- This document writes the `R3` obligation contract only.
- This document does not reselect `URI-R2-C1` or admit a new candidate subset.
- This document does not record any `R4` feasibility outcome.
- This document does not draft the `R5` handoff or research-stop artifact.

## Continuity

Completed rounds `001` through `007` and the predecessor recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/` are cited here as inherited evidence only.

Continuity statement:

- The inherited automatic-recursive-inference rounds remain authoritative for accepted `R0` / `ARI-C1` baseline truth.
- The inherited item-2 invariant audit remains authoritative and is mapped into this obligation contract rather than re-audited from scratch.
- `R1` and `R2` remain authoritative narrowing stages and are not reopened by this document.
- No predecessor packet history, no completed-round artifact, and no inherited authoritative log is rewritten here.
