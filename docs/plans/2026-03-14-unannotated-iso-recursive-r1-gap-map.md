# Unannotated Iso-Recursive `R1` Gap Map from `ARI-C1`

Date: 2026-03-14  
Round: 006  
Status: docs-only gap-map artifact for successor roadmap item 1.

## Inherited Baseline (`R0`)

This successor track starts from accepted `ARI-C1` as `R0`: the bounded accepted handoff for annotation-anchored recursive-shape handling, not from any broad unannotated recursive inference claim.

Authoritative inherited evidence inputs for this gap map:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-candidate-subset-selection.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-ari-c1-feasibility-spike.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
- `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md`

Interpretation:

- `R0` already fixes the accepted bounded truth: recursive-shape handling is annotation-anchored, single-SCC, single-binder-family, iso-recursive, and non-cyclic-graph only.
- The inherited item-2 invariant audit remains authoritative for acyclic structural representation, binding discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality risk boundaries.
- This `R1` document only maps the remaining delta from `R0` to the unannotated target; it does not reopen or replace inherited evidence.

## Fixed Boundary Model

The gap map preserves the roadmap's mandatory boundary without weakening it:

- The unannotated target is still `single-SCC` and `single-binder-family` only.
- Recursion remains obligation-level, not cyclic structural-graph encoding.
- No equi-recursive reasoning or implicit unfolding is admitted.
- No default-on widening is allowed.
- The structural graph must remain acyclic, and the round fails closed if any gap would require forbidden widening.

## Finite Gap Map

| Gap ID | Delta from `R0` (`ARI-C1`) | Information currently supplied only by explicit anchors | Potentially locally recoverable without widening semantics | Still-blocked unannotated `single-SCC` cases | Constraining inherited invariant classes | Deferred owner |
| --- | --- | --- | --- | --- | --- | --- |
| G1 | Recursive-binder obligation discovery without explicit anchors | The explicit anchor identifies that recursion is intended, fixes the recursive-shape ingress point, and gives the first recursive binder obligation a concrete root. | Local self-reference patterns may reveal candidate obligation sites and a provisional recursive-shape demand when the dependency stays inside one syntactic region. | Programs where recursive shape is only implied after non-local propagation, where multiple candidate roots compete, or where anchor removal leaves no unique obligation root. | Binding/tree discipline; occurs-check and termination; principality risk boundaries. | `R2` |
| G2 | SCC detection and bounding under the `single-SCC` rule | The explicit anchor pre-bounds the recursive family, so `ARI-C1` never has to infer whether obligation edges form one SCC or multiple SCCs. | Local obligation-graph construction may recover a bounded SCC when every candidate recursive obligation is reachable within one closed dependency cluster and no second cluster appears. | Unannotated cases that need cross-cluster comparison, ambiguous SCC cuts, or evidence that only appears after following dependencies across more than one candidate component. | Acyclicity assumptions; binding/tree discipline; occurs-check and termination. | `R2` |
| G3 | `single-binder-family` ownership and no-cross-family-linking discipline | The explicit anchor tells the solver which recursive binder family owns the recursive shape and blocks accidental cross-family linking by construction. | Owner inference might be recoverable when every candidate obligation remains under one binder family with one stable parent chain and no competing family claims. | Unannotated cases with mixed owners, shadowed binders, sibling-family interactions, or any need to link obligations across binder-family boundaries. | Binding/tree discipline; reconstruction/reification/witness replay obligations. | `R3` |
| G4 | Local recoverability limits under occurs-check and termination constraints | The explicit anchor narrows search so recursive shape is not guessed through open-ended self-reference exploration. | Some local recursive-shape facts may be recoverable when constructor-directed reasoning, existing occurs-checks, and acyclic traversal already expose the self-reference boundary without implicit unfolding. | Cases that require chasing speculative recursive equalities, repeated obligation refinement, or any implicit unfolding to decide whether recursion should be admitted. | Occurs-check and unification termination; acyclicity assumptions. | `R3` |
| G5 | Reconstruction, reification, and witness replay after removing explicit anchors | The explicit anchor gives reconstruction a stable authoritative root for `TyMu` shape, binder naming, replay domains, and witness provenance. | Local provenance might be recoverable if one inferred obligation root can be carried unchanged through generalization, reification, and witness replay with a single binder family and no replay-domain ambiguity. | Unannotated cases where multiple candidate roots, unstable binder naming, or missing provenance would make reconstruction, reification, or witness replay non-total or non-deterministic. | Reconstruction/reification/witness replay obligations; binding/tree discipline. | `R4` |
| G6 | Principality-risk boundary for admitting unannotated recursive shape | The explicit anchor keeps `ARI-C1` out of the broad search space, so principality risk is bounded by explicit ingress. | Local admissibility may be defensible when the inferred recursive shape is unique, fail-closed, and does not require extra solver choices beyond the existing bounded path. | Unannotated cases where several recursive shapes compete, where admissibility depends on heuristic search, or where acceptance would silently broaden beyond one `single-SCC` family. | Principality risk boundaries; occurs-check and termination; reconstruction/reification/witness replay obligations. | `R4` |

## Gap Reading Notes

- `explicit anchor` in `R0` is doing more than syntax transport: it supplies the authoritative recursive ingress point, initial binder-family ownership, and the provenance root later consumed by reconstruction and witness checks.
- `locally recoverable` in this document means recoverable by bounded obligation-level reasoning that keeps the structural graph acyclic, stays inside one `single-SCC`, and avoids equi-recursive or default-on widening.
- `blocked` means the case cannot be admitted by this stage without first selecting a bounded subset, writing an obligation contract, or proving feasibility under the inherited invariant classes.

## Stage Discipline

This document preserves the approved `R1` -> `R5` ladder:

- This document does not choose the bounded subset for `R2`.
- This document does not define the `R3` obligation contract.
- This document does not record an `R4` feasibility outcome.
- This document does not draft the `R5` implementation handoff.

## Continuity

Completed rounds `001` through `005`, plus the predecessor recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/`, are cited here as inherited evidence only.

Continuity statement:

- Prior completed rounds remain authoritative for the accepted `ARI-C1` baseline and its evidence chain.
- The inherited item-2 invariant audit remains authoritative and is mapped into this gap list rather than re-audited here.
- No predecessor packet history, prior round artifact, or historical authoritative log is rewritten by this `R1` gap map.
