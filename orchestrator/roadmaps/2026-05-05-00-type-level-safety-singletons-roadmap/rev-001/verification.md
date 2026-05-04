# Verification Contract

Roadmap family: `2026-05-05-00-type-level-safety-singletons-roadmap`
Revision: `rev-001`

## Baseline Checks

Every round must satisfy all baseline checks that match its touched scope.

1. **Build and test gate**
   - Run `cabal build all && cabal test --test-show-details=direct` before
     approval. All 2524+ tests must pass.
   - If the round touches `src/MLF/Constraint/Types/`, run the focused
     type-construction tests first (`test/ElaborationSpec.hs`,
     `test/BindingSpec.hs`, `test/GraphOpsSpec.hs`).

2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
   - No orphaned imports, no dead code left behind.

3. **Scope discipline**
   - Confirm the round stays inside the selected milestone/direction scope.
   - Confirm no new dependencies are added outside milestone 2 (singletons).
   - Confirm the public API surface (`src-public/`) is unchanged unless the
     milestone explicitly requires it.

4. **Type safety regression**
   - For milestone 1: verify `expectTypeRef` is deleted and no new
     `error`/`undefined` calls replace it for node-kind discrimination.
   - For milestone 3: verify impossible phase accesses are type errors, not
     runtime failures.
   - For milestone 4: verify no `!!` or partial indexing remains on binder
     arrays.

## Task-Specific Checks

- **milestone-1 (NodeRef GADT)**
  - Verify `NodeRef` is a GADT indexed by `RefTag`.
  - Verify `expectTypeRef` and `expectGenRef` are deleted.
  - Verify `BindParents` still works correctly with the new NodeRef type.
  - Verify `IntMapUtils.hs` compiles without partial pattern matches on
    NodeRef.
  - Verify `Reify/Type/Core.hs` compiles without redundant NodeRef matches.

- **milestone-2 (singletons dependency)**
  - Verify `singletons-th` is in `build-depends` in `mlf2.cabal`.
  - Verify `Phase` kind and its singleton type compile.
  - Verify a test can pattern-match on `Sing ('Raw :: Phase)`.

- **milestone-3 (phase-indexed Constraint)**
  - Verify `Constraint` has a phantom `Phase` parameter.
  - Verify each pipeline stage produces `Constraint (Next p)` from
    `Constraint p`.
  - Verify the pipeline orchestrator threads the indexed type.
  - Verify impossible phase accesses are compile-time errors.

- **milestone-4 (Vec-indexed ForallSpec)**
  - Verify `ForallSpec` uses a length-indexed `Vec` instead of `Int` count.
  - Verify `Phi/Translate.hs` has no `!!` or `atMay` on binder arrays.
  - Verify `forallSpecFromForall` produces a correctly-indexed spec.

- **milestone-5 (witness smart constructors)**
  - Verify all `EdgeWitness` construction goes through `mkEdgeWitness`.
  - Verify `mkEdgeWitness` validates all well-formedness invariants.
  - Verify `Elab/Phi/Translate.hs` no longer has redundant witness checks.

- **milestone-6 (integration)**
  - Verify no `unsafeCoerce` migration shims remain.
  - Verify docs are updated with type-level conventions.
  - Verify no performance regression in test runtime.

## Approval Criteria

Approval requires all applicable baseline and task-specific checks to pass,
the build/test gate to be green, and the round to stay inside the
milestone scope without introducing untracked type-level machinery.
