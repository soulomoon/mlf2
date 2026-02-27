# Findings

## 2026-02-27
- Started execution of `docs/plans/2026-02-27-thesis-exact-alignment-plan-full-a-e-aggressive-cleanup.md`.
- Task folder initialized under `tasks/todo/2026-02-27-thesis-exact-alignment-full-a-e-aggressive-cleanup/`.
- Gap scan vs plan shows unresolved hotspots:
  - `Presolution.EdgeProcessing.drainPendingUnifyClosure` still rewrites constraint with UF before every closure drain and does not seed closure from current UF.
  - `MLF.Constraint.Unify.Closure` exposes only `runUnifyClosure` (no seeded variant).
  - `Solved` still exports canonical-domain query helpers (`canonicalNodes`, `allCanonicalNodes`, `lookupCanonicalNode`, `lookupCanonicalVarBound`).
  - Reify/Fallback still consume canonical-domain node-map queries directly.
  - `runPipelineElabProjectionFirst` remains exported from `MLF.Elab.Run.Pipeline`.
- Existing code already has witness-domain-first ranking notes in Phi, but strict API split/explicit fallback paths are not yet enforced.
- Implemented seeded unification closure API: `runUnifyClosureWithSeed` in `MLF.Constraint.Unify.Closure`.
- Presolution edge loop now drains closure with UF seed (`psUnionFind`) and asserts no pending unify edges at per-edge boundaries.
- Added regression test showing seeded UF affects closure semantics (`BaseClash Int/Bool` when seed maps var to Int).
- Presolution producer contract is now explicit in `computePresolution`:
  - residual unify/inst edge queues fail with dedicated `PresolutionError` constructors,
  - residual `TyExp` nodes fail fast,
  - non-trivial edge witness/trace completeness is checked against input inst-edge IDs.
- Reify/fallback consumers can use projection-first queries by reading node/gen maps from `Solved.originalConstraint` + `Solved.genNodes`, while resolving representatives via `Solved.canonical`/`Solved.lookupNode`.
- Canonical-domain Solved query exports (`canonicalNodes`, `allCanonicalNodes`, `lookupCanonicalNode`, `lookupCanonicalVarBound`) were removable after migrating `Reify.Core` and `ResultType.Fallback`.
- `Reify.Core` must read solved nodes from `Solved.canonicalConstraint`; using original-constraint node domain there can regress reification behavior.
- Phi translation now follows strict witness-domain default semantics and only performs class-member recovery via explicit, instrumented fallback code paths.
- Runtime projection-first elaboration entrypoint is fully removed; parity validation now lives only in tests (`DualPathSpec`) as an implementation check.
- Full suite verification succeeded with the required command: `cabal build all && cabal test`.
