# Findings — Master 4-Phase Typed Two-Pass Edge DSL

## Key Discoveries

1. **Current presolution accepts non-`TyExp` left edges.**
   - `processInstEdge` currently branches on `TyExp` vs non-`TyExp`, allowing a fallback direct structural solve path.

2. **Phase-2 normalization already classifies residual edges but does not force paper shape.**
   - Non-graftable and type-error edges can remain as non-`TyExp` left in `cInstEdges`.

3. **Paper-shaped explicit expansion representation is already the codebase's intended model.**
   - `TyExp` notes and presolution comments describe explicit expansion-node handling as the primary representation.

4. **Edge-processing decomposition modules already exist and are reusable.**
   - Existing `EdgeProcessing.Unify` / `EdgeProcessing.Witness` are suitable interpreter primitives for a two-pass plan architecture.

5. **BLOCKER: Wrapping all residual inst edges with TyExp breaks 7 downstream tests.**
   - Synthesized TyExp wrappers route edges through the TyExp branch in `processInstEdge`, which uses `decideMinimalExpansion` + `unifyStructure` instead of `solveNonExpInstantiation`.
   - `solveNonExpInstantiation` has special binding-permission and scheme-root logic that `unifyStructure` does not replicate.
   - New TyExp wrapper nodes introduce binding-tree entries that elaboration (`PhiReorder`) doesn't expect: `"PhiReorder: missing order key for binders [NodeId {getNodeId = 29},NodeId {getNodeId = 30}]"`.
   - New `ExpVarId`s create expansion variable entries that elaboration was not designed to handle.
   - Affected tests: 7 pipeline/elaboration tests (Pipeline Phases 1-5, BUG-2026-02-06-002, Phase 6 Elaborate).

## Thesis Alignment Notes

- Enforcing `TyExp-left` for all Phase-4 instantiation edges strengthens paper-shape fidelity by making expansion variables explicit for every residual instantiation relation handled in presolution.
- Adding Phase-4 fail-fast converts an implicit assumption into an explicit invariant, reducing silent divergence risk.

## Open Questions

- Whether to keep phase-tagged errors as wrappers (`PlanError`/`ExecError`) or richer payload records for tooling/debug output.
- Whether to expose planner/interpreter modules publicly (currently planned as internal-only modules).
- **NEW: How to reconcile TyExp wrapping with the non-TyExp presolution branch.** Options:
  1. Port `solveNonExpInstantiation` logic into the TyExp branch so both paths are equivalent.
  2. Only wrap edges whose body is TyForall/TyExp (already presolution-ready), leaving Var≤Var edges unwrapped.
  3. Make the fail-fast assertion (Task 3) conditional: only assert TyExp-left for edges that the planner can handle, deferring full coverage to Phase 3.
  4. Implement Tasks 2+3+7 together as an atomic change (wrapping + fail-fast + planner/interpreter migration).

