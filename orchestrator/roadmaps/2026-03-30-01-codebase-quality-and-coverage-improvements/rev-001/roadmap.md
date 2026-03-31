# Codebase Quality and Coverage Improvements

Roadmap family: `2026-03-30-01-codebase-quality-and-coverage-improvements`
Revision: `rev-001`
Base branch: `codex/automatic-recursive-type-inference`
Created: 2026-03-30

## Goal

Systematically improve code quality, test coverage, and structural health of the MLF2 codebase across 9 dimensions identified by a full codebase review. Each item leaves the repo in a coherent, green-gate state.

## Ordering Rationale

Items are sequenced to maximize early value and minimize cross-item interference:
- Items 1-2 (test coverage + bug fix) are highest risk-reduction and are independent of each other.
- Item 3 (property-based testing) builds on the spec files added in item 1.
- Item 4 (large module decomposition) is a structural refactor that benefits from having the test safety net from items 1-3.
- Item 5 (research module hygiene) is independent and low-risk.
- Items 6-9 are lower priority and can proceed in any order after earlier items.

---

## Items

### 1. Add test coverage for untested core modules (MLF.Reify.*, MLF.Util.*)

- Item id: item-1
- Depends on: none
- Parallel safe: yes
- Parallel group: tier-1
- Merge after: none
- Status: [completed]
- Completion notes: Merged in round-160 (commit f7bdf40). 6 new spec files, 96 new test examples (1177→1273). All gates passed.

**Deliverable**: New spec files for `MLF.Reify.Type`, `MLF.Reify.Core`, `MLF.Reify.Named`, `MLF.Reify.TypeOps`, `MLF.Util.Graph`, and `MLF.Util.UnionFind`. Each spec must exercise the module's core exported functions with at least 3 meaningful examples per function. Wire all new specs into `test/Main.hs` and `mlf2.cabal`.

**Verification**: `cabal build all && cabal test` passes with increased example count. No pre-existing tests broken.

---

### 2. Fix BUG-2026-03-16-001 (InstBot replay mismatch)

- Item id: item-2
- Depends on: none
- Parallel safe: yes
- Parallel group: tier-1
- Merge after: none
- Status: [completed]
- Completion notes: Merged in round-161 (commit 1ec29a9). Added {- Note [InstBot replay-bound match] -}, 1 regression test, Bugs.md resolved. 1274 examples, 0 failures. All gates passed.

**Deliverable**: Fix `MLF.Elab.Inst.applyInstantiation` so the `InstBot` branch accepts the non-bottom shape (`t9 -> t9`) carried by the bounded no-fallback replay path. Add a regression test that exercises the URI-R2-C1 replay path through the fixed `InstBot` precondition. Update `Bugs.md` to mark BUG-2026-03-16-001 resolved with regression test paths.

**Verification**: `cabal build all && cabal test` passes. The new regression test exercises the previously-failing replay path. `Bugs.md` updated.

---

### 3. Expand property-based testing (QuickCheck)

- Item id: item-3
- Depends on: item-1
- Parallel safe: no
- Parallel group: none
- Merge after: item-1
- Status: [completed]
- Completion notes: Merged in round-162 (commit c98d1a8). 14 QuickCheck properties in test/Property/QuickCheckPropertySpec.hs: 4 reification round-trip, 3 canonicalization idempotency (200 cases each), 4 binding tree invariants, 3 unification symmetry. Test count 1274→1288. All gates passed.

**Deliverable**: Add QuickCheck properties for: (a) reification round-trip well-formedness, (b) canonicalization idempotency (`canonicalize . canonicalize === canonicalize`), (c) binding tree parent-child invariant preservation through presolution operations, (d) unification symmetry where applicable. Each property must have an `Arbitrary` instance or generator and run at least 100 cases. Add to existing spec files or create new property-focused spec files.

**Verification**: `cabal build all && cabal test` passes with new property tests included. Property test count visible in test output.

---

### 4. Decompose large modules (>800 lines)

- Item id: item-4
- Depends on: item-1, item-3
- Parallel safe: no
- Parallel group: none
- Merge after: item-3
- Status: [done]
- Completion notes: Merged in round-163 (commit 3dccd09). 5 modules split into focused submodules with re-export facades: Normalize (121L), Reify.Type (179L), Fallback (138L), Plan (69L), Interpret (22L). 9 new submodules in mlf2.cabal. Test count unchanged at 1288, 0 failures. All gates passed.

**Deliverable**: Split the top 5 non-research modules by line count: `Elab.Phi.Omega.Interpret` (1226 lines), `Constraint.Normalize` (848 lines), `Reify.Type` (822 lines), `Elab.Run.ResultType.Fallback` (822 lines), `Constraint.Presolution.Plan` (821 lines). Each split must preserve the original module as a re-export facade so downstream imports don't break. Update `mlf2.cabal` with new submodules. No behavioral changes.

**Verification**: `cabal build all && cabal test` passes with identical test results. Line count of each split parent module drops below 200 (re-export facade). New submodules listed in `mlf2.cabal`.

---

### 5. Research module hygiene (extract to separate Cabal component)

- Item id: item-5
- Depends on: none
- Parallel safe: yes
- Parallel group: tier-2
- Merge after: none
- Status: [done]
- Completion notes: Merged in round-164 (commit d2fdc4f). Moved 10 Research modules to `src-research/` with new `mlf2-research` internal library stanza. Promoted 4 hidden modules to exposed in `mlf2-internal`. Updated `AGENTS.md`. Tests unchanged at 1288, 0 failures. All gates passed.

**Deliverable**: Move `src/MLF/Research/` into a separate internal library stanza in `mlf2.cabal` (e.g., `mlf2-research`) or into a clearly-marked `research/` source directory with its own Cabal component. The main `mlf2-internal` library must not depend on research modules. Any test files that import research modules should depend on the new component. Update `AGENTS.md` if module organization guidance changes.

**Verification**: `cabal build all && cabal test` passes. `mlf2-internal` builds without research modules in scope. Research modules still compile and their tests (if any) still pass.

---

### 6. Parameter bundling for high-arity functions

- Item id: item-6
- Depends on: item-4
- Parallel safe: no
- Parallel group: none
- Merge after: item-4
- Status: [done]
- Completion notes: Merged in round-165 (commit 5898e42). 5 record types created (BinderSelectionEnv, SelectBindersArgs, AliasEnv, EdgeWitnessInput, EdgeExpansionInput), 8 functions refactored across 8 files. Tests unchanged at 1288 examples, 0 failures. All gates passed.

**Deliverable**: Identify functions with 6+ parameters in the constraint-processing and presolution modules (post-split from item-4). Bundle parameters into named record types following the `ElabConfig`/`ElabEnv` pattern already used in the elaboration layer. Target at least 5 high-arity call sites. No behavioral changes.

**Verification**: `cabal build all && cabal test` passes with identical test results. Targeted functions have reduced arity (<=4 non-self parameters).

---

### 7. Golden test expansion

- Item id: item-7
- Depends on: none
- Parallel safe: yes
- Parallel group: tier-2
- Merge after: none
- Status: [done]
- Completion notes: Merged in round-166 (commit 9ec1618). 8 golden tests (5 xMLF pretty-print + 3 constraint graph summary), 8 golden files under test/golden/, GOLDEN_ACCEPT=1 workflow documented. Test count 1288→1296. All gates passed.

**Deliverable**: Add golden tests for: (a) pretty-printed xMLF output for at least 5 canonical pipeline examples (identity, church booleans, polymorphic let, rank-2 application, choose), (b) constraint graph summary dumps for at least 3 examples. Store golden files under `test/golden/`. Use Hspec golden-test helpers or a simple file-comparison pattern. Wire into the test suite.

**Verification**: `cabal build all && cabal test` passes with new golden tests. Golden files checked in. `--accept` workflow documented in test file comments.

---

### 8. Public API enrichment (error reporting + pipeline configuration)

- Item id: item-8
- Depends on: item-4
- Parallel safe: no
- Parallel group: none
- Merge after: item-6
- Status: [done]
- Completion notes: Merged in round-167 (commit dc2616a). Added formatPipelineError, pipelineErrorPhase, pipelineErrorPhaseName to MLF.Pipeline; CycleError(..) re-export; constraint graph introspection types + constraintNodeCount, constraintEdgeCount to MLF.API; text dependency; 6 new tests in PublicSurfaceSpec.hs. Tests 1296→1302, 0 failures. All gates passed.

**Deliverable**: Extend `src-public/MLF/Pipeline.hs` with: (a) structured error formatting/reporting (`formatPipelineError :: PipelineError -> Text`), (b) pipeline configuration options (`PipelineConfig` with trace verbosity and optional phase selection). Extend `src-public/MLF/API.hs` with constraint graph introspection helpers if useful types exist in the internal API. Add doc-comments for all new exports. Add tests for new public API functions.

**Verification**: `cabal build all && cabal test` passes. New exports documented with Haddock. At least one test per new exported function.

---

### 9. {- Note -} block audit and documentation sync

- Item id: item-9
- Depends on: item-4
- Parallel safe: no
- Parallel group: none
- Merge after: item-8
- Status: [done]
- Completion notes: Merged in round-168 (commit fd85b86). 6 `{- Note -}` blocks added to item-4 submodules, implementation_notes.md updated with 4 module-split entries, CHANGELOG.md documentation-hygiene entry. Tests unchanged at 1302, 0 failures. All gates passed.

**Deliverable**: Audit all `{- Note [...] -}` blocks across `src/` for: (a) stale function/type name references that no longer exist after item-4 splits, (b) missing Notes for newly-created submodules from item-4, (c) alignment with current `implementation_notes.md` content. Fix stale references, add Notes to new modules that lack design rationale, and update `implementation_notes.md` if needed. Update `CHANGELOG.md` with a documentation-hygiene entry.

**Verification**: `cabal build all && cabal test` passes. Grep for function names mentioned in Notes confirms no dangling references. `implementation_notes.md` reflects current module structure.
