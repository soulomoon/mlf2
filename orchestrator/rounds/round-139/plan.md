# Round 139 Implementation Plan — Item 1 Cycle Detection + Automatic μ Introduction

- **roadmap_id**: `2026-03-29-00-automatic-iso-recursive-type-inference-implementation`
- **roadmap_revision**: `rev-001`
- **roadmap_dir**: `orchestrator/roadmaps/2026-03-29-00-automatic-iso-recursive-type-inference-implementation/rev-001`
- **item**: `item-1`

## Scope boundary for this round

This plan is bounded to solver-side cycle handling (Phase 3 boundary): detect instantiation-dependency cycles and rewrite the constraint graph to introduce `TyMu` so presolution receives an acyclic dependency order. No elaboration-term roll/unroll shaping and no documentation closeout work is included here.

## Chosen approach

Use **Approach B** with a dedicated cycle-breaking API added in `MLF.Constraint.Acyclicity` (not a new module):

1. Keep `checkAcyclicity :: Constraint -> Either CycleError AcyclicityResult` as the pure checker.
2. Add a new pass that repeatedly:
   - runs dependency-cycle detection,
   - rewrites one cycle by introducing `TyMu`,
   - re-runs acyclicity until success.
3. Feed the rewritten constraint forward to presolution.

This keeps existing checker semantics testable while enabling automatic μ-introduction in pipeline execution.

## Concrete implementation steps

1. **Extend `MLF.Constraint.Acyclicity` with a cycle-breaking entrypoint and deterministic rewrite loop.**
   - **File**: `src/MLF/Constraint/Acyclicity.hs`
   - **Modify exports**:
     - add `breakCyclesAndCheckAcyclicity` (or equivalent name) to module export list.
   - **Add functions/types** (in this module):
     - `breakCyclesAndCheckAcyclicity :: Constraint -> Either CycleError (Constraint, AcyclicityResult)`
     - internal loop helper (e.g. `breakCyclesUntilAcyclic`) that:
       - uses existing `buildDependencyGraph` + `topologicalSort` / `findCycle`,
       - selects a deterministic pivot edge from `ceEdgesInCycle` (e.g. smallest `EdgeId`),
       - rewrites exactly one cycle per iteration,
       - terminates when `checkAcyclicity` returns `Right`.
   - **How**:
     - preserve current `checkAcyclicity` behavior for direct callers/tests.
     - return both the rewritten `Constraint` and final `AcyclicityResult` so downstream phases consume matching graph + sorted edges.
   - **Verification command**:
     - `cabal build all`

2. **Implement one-cycle rewrite by introducing `TyMu` and substituting the recursive back-reference.**
   - **File**: `src/MLF/Constraint/Acyclicity.hs`
   - **Add internal helpers** (exact names can vary, but responsibilities must be explicit):
     - edge lookup by `EdgeId` in `cInstEdges`.
     - fresh node-id allocator seeded from `maxNodeIdKeyOr0`.
     - subgraph clone/substitution helper for RHS rewriting:
       - clone the chosen edge RHS root,
       - substitute occurrences of the cycle variable/root (`instLeft` pivot) with a fresh μ-binder variable,
       - preserve constructor shape (`TyArrow`, `TyForall`, `TyExp`, `TyCon`, `TyMu`, etc.).
     - graph patch helper that inserts:
       - fresh binder var (`TyVar`) for μ,
       - fresh μ node (`TyMu { tnBody = ... }`),
       - updated pivot `InstEdge` pointing to the μ-rooted RHS clone.
   - **Binding-tree updates**:
     - maintain `cBindParents` validity (`Binding.checkBindingTree` should continue to pass in later phases).
     - place cloned nodes under the same parent chain as the original RHS root where possible; place μ-binder as a direct child of the new `TyMu` node (`BindFlex`) consistent with existing `allocMu` conventions from `Frontend.ConstraintGen.Emit`.
   - **Non-recursive no-op guarantee**:
     - if no cycle is found, constraint must be returned byte-for-byte equivalent (no synthetic nodes).
   - **Verification command**:
     - `cabal test mlf2-test --test-options="--match 'Phase 3 — Acyclicity Check'"`

3. **Wire the new pass into the pipeline between normalization and presolution.**
   - **File**: `src/MLF/Elab/Run/Pipeline.hs`
   - **Modify function**: `runPipelineElabWith`
   - **How**:
     - replace direct `checkAcyclicity c1` call with the new API:
       - `(c1Broken, acyc) <- fromCycleError (breakCyclesAndCheckAcyclicity c1)`
       - pass `c1Broken` (not original `c1`) into `computePresolution` and all downstream uses that should observe cycle-broken graph state.
     - keep existing behavior unchanged when graph is already acyclic.
   - **Verification command**:
     - `cabal test mlf2-test --test-options="--match 'runPipelineElab'"`

4. **Add focused regression tests for cycle breaking and non-recursive stability.**
   - **Files**:
     - `test/AcyclicitySpec.hs`
     - `test/PipelineSpec.hs` (minimal smoke coverage only for item-1 boundary)
   - **Modify/add tests**:
     - `AcyclicitySpec`:
       - new examples for `breakCyclesAndCheckAcyclicity`:
         1) cyclic constraint now returns `Right (rewrittenConstraint, acyclicResult)`;
         2) rewritten constraint contains at least one `TyMu` node;
         3) acyclic input returns unchanged constraint and no `TyMu` injection.
     - `PipelineSpec`:
       - add one recursive expression smoke test (e.g. `let f = λx. f x in f`) asserting pipeline no longer fails specifically at acyclicity/cycle error.
       - add paired non-recursive control test to assert unchanged success path (same `Right`/`Left` class and stable expected type shape as existing baseline test).
   - **Verification command**:
     - `cabal test mlf2-test --test-options="--match 'Acyclicity\|Pipeline'"`

5. **Clean compile/lint boundary and run full contract gate.**
   - **Files**:
     - any touched files in steps 1–4.
   - **How**:
     - ensure no `-Wall` warnings introduced.
     - ensure no residual references expect old pipeline local variable (`c1`) where rewritten constraint (`c1Broken`) is required.
   - **Verification commands**:
     - `git diff --check`
     - `python3 -m json.tool orchestrator/state.json >/dev/null`
     - `roadmap_dir="$(python3 -c "import json; print(json.load(open('orchestrator/state.json'))['roadmap_dir'])")" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
     - `cabal build all && cabal test`

## Notes for implementer

- Keep all changes local to item-1 mechanics (cycle detection + μ insertion). Do not implement roll/unroll elaboration shaping here.
- Prefer reusing existing graph primitives and `TyMu` handling already exercised by:
  - `src/MLF/Constraint/Normalize.hs` (`TyMu` canonicalization path),
  - `src/MLF/Reify/Type.hs` (`TyMu` reification),
  - `src/MLF/Frontend/ConstraintGen/Emit.hs` (`allocMu` binding-parent conventions).
- Non-recursive programs are a hard invariant: cycle-breaking pass must be a strict no-op when no cycle is present.
