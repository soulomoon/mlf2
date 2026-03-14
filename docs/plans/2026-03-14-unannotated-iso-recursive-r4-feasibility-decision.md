# Unannotated Iso-Recursive `R4` Feasibility Decision

Date: 2026-03-14  
Round: 009  
Status: docs-only bounded feasibility decision for successor roadmap item 4.

## Inherited Baseline

This `R4` decision starts from accepted `R0` / `ARI-C1` as the bounded starting point, not from any claim that broad unannotated recursive inference is already available.

Authoritative inherited evidence inputs:

- accepted `R0` / `ARI-C1` from the completed automatic-recursive-inference chain is the starting baseline;
- `docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` is the gap-map predecessor;
- `docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md` is the chosen-subset predecessor and fixes `URI-R2-C1` as the only active subset;
- `docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md` is the obligation-contract predecessor and fixes `URI-R3-O1`, `URI-R3-O2`, `URI-R3-O3`, `URI-R3-O4`, and `URI-R3-O5` as the required obligation classes;
- `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` remains the authoritative inherited invariant audit;
- `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md` remains the roadmap-design source;
- `orchestrator/rounds/round-009/selection.md` remains the current round-selection authority.

Interpretation:

- `R1` named the finite gap from accepted `R0` / `ARI-C1` to the bounded unannotated target.
- `R2` fixed exactly one bounded subject, `URI-R2-C1`.
- `R3` fixed the obligation contract that `R4` must judge.
- This document executes the bounded feasibility decision only and uses inherited evidence from completed rounds `001` through `008` plus the predecessor recursive-types packet as reference-only support.

## Fixed Boundary Model

The fixed boundary remains unchanged and is not weakened by this round:

- `URI-R2-C1` remains `single-SCC` and `single-binder-family` only.
- Recursion remains obligation-level rather than cyclic structural-graph encoding.
- No cross-family SCC linking is admitted.
- No equi-recursive reasoning or implicit unfolding is admitted.
- No default-on widening is admitted.
- This round uses docs-only evidence and fails closed if that evidence is insufficient.
- The structural `TyNode` / constraint graph must remain non-cyclic throughout.

## Feasibility Evidence Matrix

| Obligation ID | Bounded subject inside `URI-R2-C1` | Positive example classes | Negative example classes | Inherited evidence available | No-go trigger | Immediate stop condition | Judgment contribution |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `URI-R3-O1` | The one unique local obligation root, or one equivalent closed local obligation cluster, together with the structural `TyNode` / constraint slice it touches. | Positive example classes: one local self-reference cluster inside one binder family whose obligation graph may form one SCC while the structural graph remains binder-mediated and non-cyclic. | Negative example classes: any case needing a structural back-edge, cyclic `TyNode` encoding, a second dependency component, or a dependency shape that escapes the one local cluster. | The authoritative inherited invariant audit already proves current acyclicity assumptions are structural-DAG based, Phase 3 rejects cyclic dependencies, and `TyMu` traversal must remain one-way and binder-mediated. `ARI-C1` feasibility evidence shows the explicit-anchor path can remain non-cyclic and that out-of-scope recursive pressure stays rejected, but it does not show anchor-free discovery of one admissible unannotated root/cluster. | No-go if structural acyclicity for the unannotated subject can only be justified by new prototype work, by cyclic graph modeling, or by evidence that depends on more than one obligation component. | Stop immediately if the claimed subject needs a structural cycle, a cyclic-graph exception, or a second SCC/dependency component. | Boundary preservation is supported, but unannotated clearance is not fully established from inherited evidence alone; this obligation contributes `not-yet-go`. |
| `URI-R3-O2` | Every obligation in the one closed local cluster and its deterministic parent-chain owner inside one binder family. | Positive example classes: one closed `single-binder-family` cluster whose obligations have one stable owner and one deterministic parent-chain from discovery through replay. | Negative example classes: mixed-owner clusters, shadowed ownership, sibling-family interaction, cross-family SCC linking, or any case where owner identity changes after discovery. | The authoritative inherited invariant audit documents single-parent binding-tree semantics, deterministic binder ordering, and replay consumers that require stable ownership. `R2` admission rule `A3` and `R3` already require one stable owner, but inherited evidence still comes from explicit-anchor contexts rather than from unannotated discovery. | No-go if owner stability can only be argued by inferred repair, by widening binder-family search, or by new prototype evidence that is not already present in accepted artifacts. | Stop immediately if any obligation lacks one deterministic owner or if any cross-family or mixed-owner claim appears. | The binding discipline stays explicit and fail-closed, but inherited docs do not clear stable unannotated ownership for `URI-R2-C1`; this obligation contributes `not-yet-go`. |
| `URI-R3-O3` | The constructor-directed reasoning path used to discharge the unique local obligation root or equivalent local cluster. | Positive example classes: locally recoverable self-reference that stays decidable under existing constructor-directed reasoning, current occurs-check rejection, and iso-recursive matching without implicit unfolding. | Negative example classes: any case needing implicit unfolding, equi-recursive equality search, speculative recursive-equality chase, or repeated SCC refinement to discover admissibility. | The authoritative inherited invariant audit already states that occurs checks run before unions, decomposition is constructor-directed, `TyMu` only decomposes against `TyMu`, and termination must remain unchanged. `R1` and `R3` identify the required class precisely, but no accepted artifact provides docs-only evidence that unannotated root discovery itself remains constructor-directed without new prototype support. | No-go if deciding the unannotated subject would require implicit unfolding, altered occurs-check behavior, speculative recursive search, or any prototype-only proof of termination. | Stop immediately if classification of the local root/cluster requires implicit unfolding or changes the current termination class. | Existing invariants strongly constrain what must stay forbidden, but they do not yet prove that `URI-R2-C1` can be admitted under current constructor-directed reasoning alone; this obligation contributes `not-yet-go`. |
| `URI-R3-O4` | The one provenance-stable root or cluster that must survive generalization, reconstruction, reification, and witness replay unchanged. | Positive example classes: one unique local root/cluster whose provenance stays stable through generalization, reification, and replay with one replay domain and stable binder naming. | Negative example classes: multiple candidate roots, late root repair, replay-domain widening, unstable binder naming, witness-domain ambiguity, or any surrogate-root substitution. | The authoritative inherited invariant audit names the exact reconstruction path (`generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback`) and the strict witness/replay contracts. The predecessor recursive-types evidence and `ARI-C1` chain show that explicit anchors provide an authoritative root for replay and reification, while `R1` `G5` explicitly marks anchor removal as unresolved. No accepted inherited artifact demonstrates that an unannotated `URI-R2-C1` subject preserves one provenance-stable root/cluster without new prototype evidence. | No-go if replay/reification safety depends on a manufactured provenance root, on late repair, on fallback widening, or on new prototype evidence to invent the missing authority normally supplied by explicit anchors. | Stop immediately if the admitted subject cannot be preserved as one provenance-stable root/cluster across generalization, reification, and witness replay. | This is the clearest unresolved inherited gap: explicit-anchor evidence does not transfer automatically to unannotated provenance, so this obligation contributes decisive `not-yet-go`. |
| `URI-R3-O5` | The admission decision for the chosen local root/cluster as one unique bounded subject. | Positive example classes: one uniquely admissible local root/cluster with no competing bounded interpretation, no heuristic tie-break, and no silent broadening. | Negative example classes: ambiguous or multi-root discovery, competing clusters, heuristic ranking, widening-dependent admission, or any case where more than one recursive shape stays live. | `R2` and `R3` already require uniqueness, fail-closed rejection, and no heuristic ranking. `ARI-C1` evidence shows the broader boundary can be preserved by keeping unannotated cases non-inferred, but it does not prove that one unannotated local root/cluster can be admitted uniquely inside `URI-R2-C1`. | No-go if uniqueness depends on heuristic search, solver-choice ranking, multi-root comparison, or any pressure toward broad automatic recursive inference. | Stop immediately if more than one local root/cluster remains live or if uniqueness cannot be established without widening. | Inherited evidence supports rejection behavior, not positive uniqueness clearance for unannotated admission; this obligation contributes `not-yet-go`. |

## Evidence-Source Discipline

This round makes its bounded method explicit:

- Conclusions about acyclicity, binding-tree discipline, occurs-check and termination, reconstruction/reification/witness replay, and principality-risk boundaries come from inherited docs and accepted-round evidence only.
- No new production code, test, or Cabal changes were used to manufacture feasibility evidence in this round.
- No prototype evidence was added in this round.
- If any obligation would need prototype evidence to clear, this document must resolve to `not-yet-go` rather than widening scope.
- The inherited evidence is strong enough to preserve fail-closed rejection boundaries, but not strong enough to prove positive admission for every `URI-R3-O1` through `URI-R3-O5` obligation class under the unannotated subject.

## No-Go And Immediate Stop Conditions

The following classes force `not-yet-go` for this round:

- any obligation that cannot be cleared from inherited evidence without new prototype work;
- any pressure toward ambiguous or multi-root obligation discovery;
- any pressure toward multi-cluster or multi-SCC dependency shapes;
- any mixed-owner or cross-family ownership claim;
- any implicit-unfolding, equi-recursive, or cyclic-graph pressure;
- any provenance-unstable reconstruction, reification, or replay subject;
- any principality or termination claim that would depend on heuristic search or widened solver behavior.

Immediate stop conditions remain:

- stop if the structural graph ceases to be non-cyclic;
- stop if owner identity is not stable for every obligation in the cluster;
- stop if implicit unfolding is needed to classify the subject;
- stop if one provenance-stable replay/reification subject cannot be maintained;
- stop if uniqueness cannot be established without widening.

## Final Decision

Decision outcome: not-yet-go

The inherited docs-only evidence clears the negative boundary model for `URI-R2-C1`: the round can state what must remain rejected and why the structural graph, binding tree, occurs-check discipline, reconstruction/reification/witness replay contracts, and principality boundaries must stay fail-closed. It does not clear all of `URI-R3-O1` through `URI-R3-O5` for positive unannotated admission. The largest unresolved inherited gaps are the missing authoritative unannotated provenance root/cluster for `URI-R3-O4`, the lack of docs-only uniqueness evidence for `URI-R3-O5`, and the absence of accepted prototype-free evidence that `URI-R3-O1` through `URI-R3-O3` can be satisfied without leaning on explicit anchors.

This decision is bounded to `URI-R2-C1` only. It does not generalize to broader unannotated recursive inference, multi-SCC designs, cross-family cases, equi-recursive reasoning, implicit unfolding, or cyclic structural-graph encodings.

`R5` is unlocked only by `feasible-continue`, while `not-yet-go` preserves fail-closed stopping behavior and keeps the roadmap from manufacturing an implementation-handoff target without sufficient bounded evidence.

## Stage Discipline

- This document executes the `R4` bounded feasibility decision only.
- This document does not reopen `R1`, reselect `R2`, or rewrite the `R3` obligation contract.
- This document does not draft the `R5` handoff / research-stop artifact itself.
- This document does not authorize production implementation work.

## Continuity

Completed rounds `001` through `008` and the predecessor recursive-types packet are cited as inherited evidence only and are not rewritten.

Continuity statement:

- The accepted automatic-recursive-inference chain remains authoritative for `R0` / `ARI-C1`.
- The authoritative inherited invariant audit remains authoritative and is consumed here rather than reopened.
- `R1`, `R2`, and `R3` remain authoritative narrowing artifacts and are not rewritten by this document.
- No predecessor packet history, prior round artifact, or inherited authoritative record was rewritten in this round.
