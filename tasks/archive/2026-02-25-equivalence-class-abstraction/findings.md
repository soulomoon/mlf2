# Findings: 2026-02-25 Equivalence-Class Abstraction

## Baseline observations
- `MLF.Constraint.Solved` already exists and is exported from `mlf2.cabal`.
- `Elaborate.hs`, `Phi/Translate.hs`, and `Omega.hs` already import `MLF.Constraint.Solved`.
- `SolveResult(..)` is still exported by `MLF.Constraint.Solve` and many modules still access `srConstraint`/`srUnionFind` directly.
- Pre-existing local edits mostly migrate tests/helpers from ad hoc `SolveResult` construction to `Solved.mkSolved` and wrappers.

## Task extraction snapshot
From `docs/plans/2026-02-25-equivalence-class-abstraction-plan.md`, tasks are:
1..17 (Milestones 1..5), ending with docs and deviation tracker updates.

## Risks identified at start
- Plan assumes broad migration completeness before Task 7; codebase still has many direct `SolveResult` field uses.
- Existing local edits may overlap with plan tasks; each subagent must preserve and build on current state.

## Task 6 outcome
- Task 6 completed in commit `898b733` (`refactor: migrate test infrastructure to Solved API`).
- Full validation for the task commit passed:
  - `cabal test --test-show-details=direct`
  - `802 examples, 0 failures`
- Spec compliance review verdict: ✅ compliant with Task 6 requirements.
- Code-quality review verdict: ✅ approved with minor non-blocking notes.

## Quality review notes (non-blocking)
- `mkSolved` was added/exported from `src/MLF/Constraint/Solved.hs` to support test migration; reviewer flagged this as a potential opacity leak for production API surface.
- Several tests still bridge `Solved -> SolveResult` via `Solved.toSolveResult`; this is acceptable for incremental migration but may increase churn when escape hatches are removed.

## Task 7 outcome
- Task 7 completed in commit `07b2947` (`refactor: hide SolveResult fields — all access through Solved API`).
- Full validation for the task commit passed:
  - `cabal build`
  - `cabal test --test-show-details=direct`
  - `802 examples, 0 failures`
- Spec compliance review verdict: ✅ compliant with Task 7 requirements.
- Code-quality review verdict: ✅ approved with minor non-blocking notes.

## Task 7 review notes (non-blocking)
- Minor refactor smell: repeated `fromSolveResult -> mkSolved -> toSolveResult` round-trips appear in several modules; likely worth centralizing with a helper in `MLF.Constraint.Solved`.
- Minor formatting: trailing whitespace in some `test/SolveSpec.hs` lines introduced during migration.

## Task 7 recovery findings
- After hiding `SolveResult(..)` in `MLF.Constraint.Solve`, the remaining breakage was concentrated in `MLF.Elab.Run.*` and white-box tests (`SolveSpec`, `Constraint/SolvedSpec`).
- `MLF.Constraint.Solved` escape hatches (`solvedConstraint`, `unionFind`, `mkSolved`, `toSolveResult`) were sufficient to remove direct field dependence without changing solver behavior.
- Final grep audit confirms direct field usage is now confined to:
  - `src/MLF/Constraint/Solve.hs`
  - `src/MLF/Constraint/Solved.hs`
  - `src/MLF/Constraint/Solve/Internal.hs`

## Task 8 outcome
- Task 8 completed by extending `Solved` internals from `newtype SolveResult` to a sum backend:
  - `LegacyBackend` (current runtime path via `fromSolveResult`)
  - `EquivBackend` (staged, not yet wired)
- All `MLF.Constraint.Solved` API functions now dispatch on both backends.
- Legacy runtime behavior is preserved:
  - `fromSolveResult` and `mkSolved` construct `LegacyBackend`.
  - Existing consumers continue to observe current behavior and test outcomes.
- Degraded-but-meaningful Equiv handling added:
  - `classMembers` uses `ebEquivClasses` with canonical singleton fallback.
  - `originalNode` / `originalBindParent` consult `ebOriginalConstraint` and fall back to canonical views.
  - `wasOriginalBinder` checks class members against original `TyForall` nodes.
- Verification evidence:
  - `cabal build` -> PASS
  - `cabal test --test-show-details=direct` -> PASS (`802 examples, 0 failures`)

## Task 8 quality follow-up
- Initial code-quality review for `128c388` raised important findings and was marked **Needs changes**:
  - Equiv canonicalization was one-hop (not chase-based/root-stable).
  - Equiv `classMembers`/`originalNode`/`originalBindParent` could silently fallback and hide missing snapshot data.
- Follow-up fix commit `c3ec0a3` (`refactor: harden EquivBackend staged query semantics`) resolved these points:
  - Added chase-based, cycle-safe Equiv canonicalization helper.
  - Removed silent legacy fallback in Equiv original/snapshot queries.
  - Preserved legacy backend behavior and API signatures.
- Re-validation after follow-up:
  - `cabal build` -> PASS
  - `cabal test --test-show-details=direct` -> PASS (`802 examples, 0 failures`)
  - Quality re-review verdict: ✅ approved (minor note only for malformed cyclic map + class key mismatch).

## Task 9 outcome
- Added snapshot exposure in `src/MLF/Constraint/Solve.hs` without runtime-path swap:
  - `solveUnifyWithSnapshot :: TraceConfig -> Constraint -> Either SolveError SolveOutput`
  - `SolveOutput` contains both `soResult :: SolveResult` and `soSnapshot :: SolveSnapshot`
  - `SolveSnapshot` stores `snapUnionFind` (UF captured at the same pre-`applyUFConstraint` point) and `snapPreRewriteConstraint` (captured before `applyUFConstraint`)
- Kept `solveUnify` behavior stable by defining it as `soResult <$> solveUnifyWithSnapshot ...`.
- Added `fromPreRewriteState :: IntMap NodeId -> Constraint -> Solved` in `src/MLF/Constraint/Solved.hs`:
  - Builds `EquivBackend` snapshot data from pre-rewrite constraint + UF captured at the same pre-`applyUFConstraint` point.
  - Constructs canonical map and equivalence classes over original node ids.
  - Canonicalizes the snapshot constraint via rewrite helpers mirroring `applyUFConstraint` shape (nodes/inst/unify/bind parents/eliminated/weakened/gen nodes).
- Validation evidence:
  - `cabal build` -> PASS

## Task 9 gate-closure follow-up
- Spec-review gap fix commit `ccc687a`:
  - Aligned `SolveSnapshot.snapUnionFind` with the same pre-rewrite capture point as `snapPreRewriteConstraint` (`uf` before `applyUFConstraint`).
- Quality-review hardening commit `aba07a0`:
  - Removed rewrite drift risk by introducing shared `rewriteConstraintWithUF` in `MLF.Constraint.Solve` and reusing it from `MLF.Constraint.Solved`.
  - Added direct snapshot-path regression coverage:
    - `test/SolveSpec.hs`: snapshot rewrite consistency check.
    - `test/Constraint/SolvedSpec.hs`: legacy-vs-snapshot equivalence for core `Solved` queries.
- Re-validation after follow-up:
  - `cabal build` -> PASS
  - `cabal test --test-show-details=direct` -> PASS (`804 examples, 0 failures`)
  - Spec re-review verdict: ✅ compliant.
  - Quality re-review verdict: ✅ approved.

## Task 10 outcome
- Task 10 completed in commit `a36a7a0` (`test: equivalence-class backend unit tests with legacy comparison`).
- Added explicit EquivBackend assertion tests in `test/Constraint/SolvedSpec.hs` for:
  - `classMembers`
  - `wasOriginalBinder`
  - `originalNode`
  - `originalBindParent`
  - legacy-oracle comparisons for `canonical` and `lookupNode`
- Full suite validation:
  - `cabal test --test-show-details=direct` -> PASS (`810 examples, 0 failures`)
- Review gate outcomes:
  - Spec review -> ✅ compliant.
  - Quality review -> ✅ approved (minor notes only: potential brittleness from strict list equality/order coupling and repeated fixture scaffolding).

## Task 10 outcome
- Added explicit EquivBackend-focused tests in `test/Constraint/SolvedSpec.hs` using `fromPreRewriteState`:
  - `classMembers` returns all original nodes in the class.
  - `wasOriginalBinder` is `True` for a unified-away binder class.
  - `originalNode` returns pre-solving node payload.
  - `originalBindParent` returns pre-solving binding parent links.
- Refactored snapshot parity fixture into explicit legacy-oracle tests:
  - `canonical` staged-vs-legacy equality.
  - `lookupNode` staged-vs-legacy equality.
- Kept an additional parity assertion for related snapshot queries (`lookupVarBound`, `instEdges`, `bindParents`, `genNodes`, `allNodes`) to preserve prior coverage intent.
- Verification evidence:
  - `cabal test --test-show-details=direct` -> PASS (`810 examples, 0 failures`)

## Task 11 outcome
- Task 11 completed in commit `b8d1754` (`feat: wire equivalence-class backend into solver output path`).
- Added `fromSolveOutput :: SolveOutput -> Solved` in `src/MLF/Constraint/Solved.hs`:
  - Builds `EquivBackend` from `SolveOutput.soSnapshot` pre-rewrite constraint + canonical rewritten constraint from `SolveOutput.soResult`.
  - Constructs canonical map and equivalence-class membership from snapshot/original nodes.
- Switched primary pipeline solved-view construction in `src/MLF/Elab/Run/Pipeline.hs`:
  - `solveUnifyWithSnapshot` is now used in the run path.
  - `Solved.fromSolveOutput` is used for the primary solved abstraction consumed by downstream pipeline logic.
- Migrated affected helper/tests to snapshot-aware construction:
  - `test/SpecUtil.hs`
  - `test/ConstraintGenSpec.hs`
  - `test/ElaborationSpec.hs`
  - `test/Constraint/SolvedSpec.hs`
- Added direct parity assertion:
  - `fromSolveOutput` matches explicit `fromPreRewriteState` on snapshot data in `test/Constraint/SolvedSpec.hs`.
- Verification evidence:
  - `cabal build` -> PASS
  - `cabal test --test-show-details=direct` -> PASS (`811 examples, 0 failures`)
- Review gate outcomes:
  - Spec review -> ✅ compliant.
  - Code quality review -> ✅ approved.

## Task 13 compile-recovery + spec-gap fix
- Reproduced failure: `cabal test --test-show-details=direct` failed to compile because `MLF.Constraint.Solved` imported `rewriteEliminatedBinders`, which is not exported from `MLF.Constraint.Solve`.
- Snapshot-path regression confirmed after import-only fix:
  - `fromSolveOutput` was constructing `EquivBackend` directly from pre-rewrite nodes, which diverged from legacy query behavior (`lookupNode`, `lookupVarBound`, `bindParents`, and downstream elaboration/pipeline tests).
- Applied fix in `src/MLF/Constraint/Solved.hs`:
  - Removed the stale `rewriteEliminatedBinders` import.
  - Changed `fromSolveOutput` to build from snapshot via `fromPreRewriteState` (uses snapshot data only, not `soResult.srConstraint`).
  - Kept compatibility replay in `fromPreRewriteState` via `solveResultFromSnapshot` with a fallback for handcrafted test snapshots.
- Verification:
  - `cabal test --test-show-details=direct` -> PASS (`821 examples, 0 failures`).

## Task 13 quality-gate closure
- Quality review identified two important risks in the first Task 13 patch:
  - partial constructor paths via `error` in `Solved` constructors
  - silent fallback that could mask snapshot replay regressions
- Final Task 13 shape:
  - `fromSolveOutput :: SolveOutput -> Either SolveError Solved`
  - `fromPreRewriteState :: IntMap NodeId -> Constraint -> Either SolveError Solved`
  - `src/MLF/Elab/Run/Pipeline.hs` now propagates solved-construction errors through `fromSolveError`.
  - Test helpers/specs updated to handle `Either` (`SpecUtil`, `ConstraintGenSpec`, `ElaborationSpec`, `Constraint/SolvedSpec`).
- Snapshot-authoritative regression protection retained:
  - `fromSolveOutput derives canonical constraint from snapshot, not soResult payload`.
- Verification:
  - `cabal test --test-show-details=direct` -> PASS (`821 examples, 0 failures`).
  - Final code-quality re-review: merge-ready with one low-severity non-blocking note.

## 2026-02-26 binder-identity collapse follow-up (Task 15 regression hardening)
- Reproduced reviewer finding:
  - with binders `1`, `2` merged into alias `31`, `lookupBinderIndex` collapsed both direct binder lookups to `Just 0`.
- Root cause:
  - `IdentityBridge.sourceKeysForNode` class-member expansion fed into exact matching, so distinct binders in the same solved class exposed indistinguishable exact-key sets.
- Fix shape in `src/MLF/Elab/Phi/IdentityBridge.hs`:
  - internal split between exact-key lookup (no class fallback) and fallback-key lookup (with class members);
  - `lookupBinderIndex` ranking tiers now:
    1. exact identity matches (`sourceKeysForNodeNoClassFallback`)
    2. class fallback matches (only when target has no exact keys)
    3. canonical alias fallback.
- New regression:
  - `test/Phi/IdentityBridgeSpec.hs`: `preserves raw binder identity before class-member fallback`.
- Compatibility guard:
  - existing `OpWeaken on an alias target recovers binder via equivalence class and emits InstElim` remains green after the ranking change.
- Verification:
  - targeted regressions pass:
    - `--match "preserves raw binder identity before class-member fallback"`
    - `--match "OpWeaken on an alias target recovers binder via equivalence class and emits InstElim"`
  - full gate: `cabal build all && cabal test` -> PASS (`825 examples, 0 failures`).

## 2026-02-26 Task 17 findings (deviation/claims audit)
- `docs/thesis-deviations.yaml` already reflects Milestone 5 completion:
  - `DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP` is absent.
  - Remaining deviations are proof/representation choices, not solved-identity-loss behavior.
- `docs/thesis-claims.yaml` is already synced:
  - `CLM-PHI-CORRECTNESS` has `deviations: []`.
  - no claim statuses needed promotion for this deviation removal.
- Claims/deviation consistency remains valid:
  - `./scripts/check-thesis-claims.sh` -> PASS (`21 claims`, `4 deviations`, cross-links green).

## 2026-02-26 review follow-up regressions
- `IdentityBridge.lookupBinderIndex` class-fallback path now has a direct regression proving behavior when the target has no exact binder key:
  - `uses class-member fallback when target has no exact binder key`
- `Omega`/`Φ` integration now has a deterministic shared-class alias weakening regression:
  - `OpWeaken on a shared alias class deterministically picks the first binder without trace`
  - observed output is `N` (eliminate first binder) under no-trace tie-breaking.
- Full suite remains green after adding both regressions (`827 examples, 0 failures`).
