# Findings — Master 4-Phase Typed Two-Pass Edge DSL

## Key Discoveries

1. **Current presolution accepts non-`TyExp` left edges.**
   - `processInstEdge` currently branches on `TyExp` vs non-`TyExp`, allowing a fallback direct structural solve path.

2. **Phase-2 normalization already classifies residual edges but does not force paper shape.**
   - Non-graftable and type-error edges can remain as non-`TyExp` left in `cInstEdges`.

3. **Paper-shaped explicit expansion representation is already the codebase’s intended model.**
   - `TyExp` notes and presolution comments describe explicit expansion-node handling as the primary representation.

4. **Edge-processing decomposition modules already exist and are reusable.**
   - Existing `EdgeProcessing.Unify` / `EdgeProcessing.Witness` are suitable interpreter primitives for a two-pass plan architecture.

## Thesis Alignment Notes

- Enforcing `TyExp-left` for all Phase-4 instantiation edges strengthens paper-shape fidelity by making expansion variables explicit for every residual instantiation relation handled in presolution.
- Adding Phase-4 fail-fast converts an implicit assumption into an explicit invariant, reducing silent divergence risk.

## Open Questions

- Whether to keep phase-tagged errors as wrappers (`PlanError`/`ExecError`) or richer payload records for tooling/debug output.
- Whether to expose planner/interpreter modules publicly (currently planned as internal-only modules).

