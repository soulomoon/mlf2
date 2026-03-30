# Bugs

Canonical bug tracker for implementation defects and thesis-faithfulness gaps.

## Open

(No open bugs.)

## Resolved

### BUG-2026-03-16-001
- Status: Resolved
- Priority: High
- Discovered: 2026-03-16
- Resolved: 2026-03-30
- Summary: The authoritative `URI-R2-C1` replay path fails at `MLF.Elab.Inst.applyInstantiation` because the `InstBot` branch still requires bottom while the bounded no-fallback replay path carries the localized shape `t9 -> t9` / `t5 -> t5`.
- Minimal reproducer:
  - `python3 -m json.tool orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-W.json`
  - `python3 -m json.tool orchestrator/rounds/round-021/evidence/D2/attempt-1/check-D2-L.json`
  - `python3 -m json.tool orchestrator/rounds/round-022/evidence/D3/attempt-1/trace-bundle.json`
- Expected vs actual:
  - Expected: the bounded `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback -> witness replay` path should preserve the authoritative `P1` subject through replay, with `applyInstantiation` accepting the replay shape established by the no-fallback path at the localized boundary.
  - Actual before fix: `P2-W` finalized as `partial-replay`; the localized replay step reaches `witness-replay/applyInstantiation-instbot-precondition`, where `InstBot` rejects the carried non-bottom shape (`InstBot expects ⊥, got: t9 -> t9`).
- Suspected/owning area:
  - `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs`
  - `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
- Thesis impact:
  - This blocks the paper-faithful bounded replay continuity required to carry the authoritative `P1` subject through the `P2` provenance-preservation path for `URI-R2-C1`.
  - The successor diagnostic roadmap (`D1` through `D4`) narrowed the defect to one owner boundary and justified a later bounded repair-track reopen, but did not repair runtime behavior.
- Fix:
  - The bug was functionally fixed during diagnostic round R2/round-025 via `allowReplayBoundMatch` in `MLF.Elab.Inst.applyInstantiation`. This function detects when replay-environment variable resolution transforms the InstBot argument to match the scrutinee, accepting the replay step.
  - Added `{- Note [InstBot replay-bound match] -}` documentation block in `src/MLF/Elab/Inst.hs`.
- Regression tests:
  - `test/ElaborationSpec.hs` (`BUG-2026-03-16-001 regression: InstBot accepts replay-resolved bound match`)
  - `test/ElaborationSpec.hs` (`URI-R2-C1 witness replay stays alpha-equivalent to the locked no-fallback shape`)
  - `cabal build all && cabal test`

### BUG-2026-03-09-001
- Status: Resolved
- Priority: High
- Discovered: 2026-03-09
- Resolved: 2026-03-09
- Summary: The `master` baseline stopped building after the public `MLF.Constraint.Solved` facade dropped `fromPreRewriteState` while test/frozen-parity utilities still called it directly.
- Minimal reproducer:
  - `cabal build all && cabal test`
  - `rg -n 'fromPreRewriteState' test/Parity/FrozenArtifacts.hs test/SpecUtil.hs test/PipelineSpec.hs test/Constraint/SolvedSpec.hs src/MLF/Constraint/Solved.hs src/MLF/Constraint/Solved/Internal.hs`
- Expected vs actual:
  - Expected: the baseline branch should compile and test cleanly, with test-only snapshot reconstruction using a thesis-safe, exposed replay seam instead of a removed public facade helper.
  - Actual before fix: test/frozen-parity callers still referenced `Solved.fromPreRewriteState`, so `cabal build all && cabal test` failed before any new simplification round could start.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Solved.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Solved/Internal.hs`
  - `/Volumes/src/mlf4/test/SolvedFacadeTestUtil.hs`
  - `/Volumes/src/mlf4/test/SpecUtil.hs`
  - `/Volumes/src/mlf4/test/Parity/FrozenArtifacts.hs`
  - `/Volumes/src/mlf4/test/PipelineSpec.hs`
  - `/Volumes/src/mlf4/test/Constraint/SolvedSpec.hs`
  - `/Volumes/src/mlf4/mlf2.cabal`
- Thesis impact:
  - The defect did not change the thesis model itself, but it blocked the verification gate required to keep future simplifications thesis-exact and trustworthy.
- Fix:
  - Kept the public `MLF.Constraint.Solved` facade narrow and moved test/frozen-parity snapshot reconstruction onto `SolvedFacadeTestUtil.solvedFromSnapshot`.
  - Updated that helper to mirror the checked replay path via `SolveSnapshot`, `solveResultFromSnapshot`, and `Solved.fromSolveOutput`.
  - Migrated the remaining test callers and wired `SolvedFacadeTestUtil` into `frozen-parity-gen` in `mlf2.cabal`.
  - Merged on `master` via `165c6dff8f85a7ca2a8a2a4c1627ce6fe9f405eb` (branch commit `658717fcfaa25c063c3e24440ae879ad719ca93e`).
- Regression tests:
  - `/Volumes/src/mlf4/test/SpecUtil.hs`
  - `/Volumes/src/mlf4/test/Parity/FrozenArtifacts.hs`
  - `/Volumes/src/mlf4/test/PipelineSpec.hs`
  - `/Volumes/src/mlf4/test/Constraint/SolvedSpec.hs`
  - `rg -n 'Solved\.fromPreRewriteState|\bfromPreRewriteState\b' test src src-public -g '!src/MLF/Constraint/Solved/Internal.hs'`
  - `cabal build mlf2-test`
  - `cabal test mlf2-test --test-show-details=direct`
  - `cabal build all && cabal test`

### BUG-2026-03-07-008
- Status: Resolved
- Priority: Low
- Discovered: 2026-03-07
- Resolved: 2026-03-07
- Summary: After the χp/view-native cleanup, duplicate `...View` / `...FromView` aliases still lingered on the runtime/reify surface, leaving two names for the same `PresolutionView`-typed operations.
- Minimal reproducer:
  - `rg -n "bindingScopeRefCanonicalView|schemeBodyTargetView|canonicalizeScopeRefView|resolveCanonicalScopeView|letScopeOverridesView|inlineBoundVarsTypeView|inlineBoundVarsTypeForBoundView|generalizeAtWithBuilderView|mkGeneralizeAtWithBuilderView|generalizeWithPlanView|reifyTypeFromView|reifyTypeWithNamesFromView|reifyTypeWithNamesNoFallbackFromView|reifyTypeWithNamedSetFromView|reifyTypeWithNamedSetNoFallbackFromView|reifyBoundWithNamesFromView|reifyBoundWithNamesBoundFromView|namedNodesFromView" src`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "duplicate ...View aliases are retired from runtime and reify modules"'`
- Expected vs actual:
  - Expected: one canonical `PresolutionView`-typed name per runtime/reify helper.
  - Actual before fix: the unsuffixed names and `...View` / `...FromView` names coexisted and pointed at the same implementation.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Scope.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/ResultType/Util.hs`
  - `/Volumes/src/mlf4/src/MLF/Reify/Core.hs`
- Thesis impact:
  - No semantic divergence remained, but the duplicate naming weakened the χp/view-native closeout by leaving two internal surfaces for the same operations.
- Fix:
  - Removed the duplicate alias exports/definitions and updated callers to the unsuffixed names only.
- Regression tests:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`duplicate ...View aliases are retired from runtime and reify modules`)
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`runtime and reify modules no longer adapt Solved through fromSolved`, `row2 absolute thesis-exact guard`, `checked-authoritative`, `Dual-path verification`)
  - `/Volumes/src/mlf4/test/ScopeSpec.hs` (`ga scope`)
  - `/Volumes/src/mlf4/test/GeneralizeSpec.hs` (`Generalize shadow comparator`)
  - `cabal build all && cabal test`

### BUG-2026-03-07-007
- Status: Resolved
- Priority: Medium
- Discovered: 2026-03-07
- Resolved: 2026-03-07
- Summary: The runtime/reify surface still exposed non-legacy `Solved -> PresolutionView` convenience wrappers, leaving χp/view-native cleanup incomplete after the row2 closeout.
- Minimal reproducer:
  - `rg -n "fromSolved" src/MLF/Elab/Run src/MLF/Reify/Core.hs`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "runtime and reify modules no longer adapt Solved through fromSolved"'`
- Expected vs actual:
  - Expected: runtime elaboration/reify helpers should consume `PresolutionView` directly, with `fromSolved` confined to the boundary, legacy compatibility, and tests.
  - Actual before fix: `MLF.Elab.Run.Scope`, `MLF.Elab.Run.TypeOps`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.ResultType.Util`, and `MLF.Reify.Core` still exposed `Solved` convenience wrappers implemented via `fromSolved`.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Scope.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/TypeOps.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/ResultType/Util.hs`
  - `/Volumes/src/mlf4/src/MLF/Reify/Core.hs`
- Thesis impact:
  - This was a row2/chi-first hardening gap: the live runtime and reify surfaces still allowed solved-compat convenience APIs to creep back into non-legacy code.
- Fix:
  - Removed the non-legacy `fromSolved` wrappers and made `PresolutionView` the primary internal/runtime API across those modules.
  - Updated internal/test callers to pass explicit `PresolutionView`s; `MLF.Elab.Legacy` remains the designated non-test compatibility zone.
- Regression tests:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`runtime and reify modules no longer adapt Solved through fromSolved`, `row2 absolute thesis-exact guard`, `ResultType|Phase 6 — Elaborate|chi-first gate stays green`)
  - `/Volumes/src/mlf4/test/ScopeSpec.hs` (`ga scope`)
  - `/Volumes/src/mlf4/test/GeneralizeSpec.hs` (`Generalize shadow comparator`)
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`checked-authoritative`, `Dual-path verification`)
  - `cabal build all && cabal test`

### BUG-2026-03-07-006
- Status: Resolved
- Priority: Medium
- Discovered: 2026-03-07
- Resolved: 2026-03-07
- Summary: Library-side Φ test hooks (`MLF.Elab.Phi.TestOnly` and `MLF.Elab.Phi.IdentityBridge`) lingered in `mlf2-internal` even though their remaining behavior was test-only or diagnostics-only.
- Minimal reproducer:
  - `rg -n "MLF\.Elab\.Phi\.TestOnly|MLF\.Elab\.Phi\.IdentityBridge" mlf2.cabal`
  - `rg -n "import qualified MLF\.Elab\.Phi\.TestOnly|import qualified MLF\.Elab\.Phi\.IdentityBridge" test`
- Expected vs actual:
  - Expected: pure witness-domain helpers and test-only fail-fast shims should live in the test package, while production `Ω` keeps only the runtime behavior it actually needs.
  - Actual before fix: the main library still exposed `MLF.Elab.Phi.TestOnly` and `MLF.Elab.Phi.IdentityBridge`, and tests imported them directly.
- Suspected/owning area:
  - `/Volumes/src/mlf4/mlf2.cabal`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/TestOnly.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/IdentityBridge.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
  - `/Volumes/src/mlf4/test/Phi/WitnessDomainUtil.hs`
- Thesis impact:
  - This was an architecture hygiene/theory-boundary gap: test-only support code remained on the main-library surface after the runtime path had already been narrowed to direct replay-spine behavior.
- Fix:
  - Removed both modules from `mlf2-internal`.
  - Exposed `MLF.Elab.Generalize` for test use and moved pure witness-domain helpers to `test/Phi/WitnessDomainUtil.hs`.
  - Replaced the `IdentityBridge` production dependency in `MLF.Elab.Phi.Omega` with a tiny local diagnostic helper.
- Regression tests:
  - `/Volumes/src/mlf4/test/Phi/WitnessDomainSpec.hs` (`WitnessDomain`)
  - `/Volumes/src/mlf4/test/GeneralizeSpec.hs` (`Generalize shadow comparator`)
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`no-trace test entrypoint fails fast with MissingEdgeTrace`)
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`elab-input thesis-exact guard`, `elab-input absolute thesis-exact guard`, `row9-11 direct-target guard`)
  - `cabal build all && cabal test`

### BUG-2026-03-06-001
- Status: Resolved
- Priority: Medium
- Discovered: 2026-03-06
- Resolved: 2026-03-07
- Summary: Φ identity handling remained split across an active runtime reconciliation object model and a legacy internal helper surface (`MLF.Elab.Phi.Binder`).
- Minimal reproducer:
  - `rg -n "MLF\.Elab\.Phi\.Binder|lookupBinderIndexM|binderIndexM" src/MLF/Elab/Phi.hs mlf2.cabal`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row9-11 facade cleanup guard"'`
- Expected vs actual:
  - Expected: Φ identity handling should reduce to thesis-shaped witness-domain identity only (node/source identity from witness artifacts and replay-spine position), with no separate runtime reconciliation subsystem or solved/canonical fallback helper surface.
  - Actual before fix: production code still split identity handling across `Translate`, `IdentityBridge`/`Omega`, and the internal helper `MLF.Elab.Phi.Binder`.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/IdentityBridge.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi.hs`
  - `/Volumes/src/mlf4/mlf2.cabal`
- Thesis impact:
  - The codebase now matches the accepted direct-runtime Φ identity model: `Omega` stays direct/fail-fast, and `IdentityBridge` remains utility/test-only rather than a runtime repair engine.
- Fix:
  - Removed `src/MLF/Elab/Phi/Binder.hs` and its `mlf2.cabal` entry.
  - Retired the helper re-exports from `src/MLF/Elab/Phi.hs` and pruned the dead inverse-copy accessor path from `src/MLF/Elab/Phi/Env.hs`.
  - Tightened `src/MLF/Elab/Phi/Omega.hs` and `src/MLF/Elab/Phi/IdentityBridge.hs` notes to document the accepted direct-runtime vs utility/test split.
- Regression tests:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`row9-11 facade cleanup guard: Phi no longer re-exports or compiles Binder helpers`)
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`row9-11 direct-target guard: Omega does not define source-candidate reconciliation helpers`)
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`OpWeaken on binder target missing from quantifier spine fails fast`)
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`OpGraft on binder target missing from quantifier spine still fails fast even when IdentityBridge finds witness-domain matches`)
  - `/Volumes/src/mlf4/test/Phi/IdentityBridgeSpec.hs` (`IdentityBridge`)
  - `cabal build all && cabal test`

### BUG-2026-03-07-005
- Status: Resolved
- Priority: Medium
- Discovered: 2026-03-07
- Resolved: 2026-03-07
- Summary: Graph-phase explicitness lacked an explicit campaign-level negative guardrail keeping graph-sensitive phases out of broad recursion-schemes rewrites.
- Minimal reproducer:
  - `rg -n 'Graph-Phase Explicitness Guardrail|graph-phase|non-goal|explicit-by-default' docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md implementation_notes.md TODO.md`
  - `rg -n 'Driver|Validation|WitnessCanon|Reify/Core' src/MLF/Constraint/Presolution src/MLF/Reify`
- Expected vs actual:
  - Expected: the campaign should include a verifier-owned negative guardrail that explicitly keeps graph-sensitive phases out of generic recursion-schemes pressure unless row-specific thesis evidence says otherwise.
  - Actual before fix: the live code remained explicit and well-tested, but the campaign docs did not yet give that negative guardrail enough explicit force to close row8.
- Suspected/owning area:
  - `/Volumes/src/mlf4/docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md`
  - `/Volumes/src/mlf4/implementation_notes.md`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Driver.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Validation.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessCanon.hs`
  - `/Volumes/src/mlf4/src/MLF/Reify/Core.hs`
- Thesis impact:
  - Reopened row8 `Graph-Phase Explicitness Guardrail` until the campaign explicitly marked graph-sensitive phases as non-goals for broad recursion-schemes rewrites.
- Fix:
  - Added the explicit negative graph-phase guardrail to `implementation_notes.md`.
  - Refreshed row8 in the recursion-refactor mechanism table to point at that explicit non-goal boundary.
- Regression tests:
  - `/Volumes/src/mlf4/test/TranslatablePresolutionSpec.hs`
  - `/Volumes/src/mlf4/test/Presolution/EnforcementSpec.hs`
  - `cabal build all && cabal test`

### BUG-2026-03-07-004
- Status: Resolved
- Priority: Medium
- Discovered: 2026-03-07
- Resolved: 2026-03-07
- Summary: Binder-safe tree recursion coverage lacked a verifier-owned exhaustive inventory separating safe tree folds from binder-sensitive or graph-boundary traversals.
- Minimal reproducer:
  - `rg -n 'cata|para|hylo|Recursive|Corecursive' src/MLF src/Util`
  - `rg -n 'Binder-Safe Tree Recursion Coverage|safe fold|graph-boundary|keep explicit' docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md implementation_notes.md`
- Expected vs actual:
  - Expected: the campaign should have a complete per-traversal audit that tags remaining manual traversals as `safe fold`, `keep explicit`, or `graph-boundary`.
  - Actual before fix: there were good positive examples and partial notes, but no exhaustive verifier-owned inventory, so row7 could not close.
- Suspected/owning area:
  - `/Volumes/src/mlf4/docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md`
  - `/Volumes/src/mlf4/implementation_notes.md`
  - `/Volumes/src/mlf4/src/MLF/Frontend/Normalize.hs`
  - `/Volumes/src/mlf4/src/MLF/Frontend/Desugar.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/TermClosure.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessCanon.hs`
- Thesis impact:
  - Reopened row7 `Binder-Safe Tree Recursion Coverage` until the campaign carried an explicit exhaustive traversal inventory.
- Fix:
  - Added the exhaustive active-campaign traversal inventory to `implementation_notes.md`.
  - Refreshed row7 in the recursion-refactor mechanism table to treat that inventory as the authoritative scope boundary.
- Regression tests:
  - `cabal build all && cabal test`

### BUG-2026-03-07-003
- Status: Resolved
- Priority: Medium
- Discovered: 2026-03-07
- Resolved: 2026-03-07
- Summary: Typing-environment construction lacked a direct verifier-owned production-path anchor for `Definition 15.3.6` / `Property 15.3.7`.
- Minimal reproducer:
  - `rg -n 'Definition 15\.3\.6|Property 15\.3\.7|O15-ENV|row5 typing-environment' docs/thesis-obligations.yaml test/ElaborationSpec.hs implementation_notes.md docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row5 typing-environment"'`
- Expected vs actual:
  - Expected: the production elaboration path should have direct row-owned regressions proving the lambda-side and let-side `Γ_{a′}` rules plus their well-scoped environment entries on the live path.
  - Actual before fix: adjacent scope/`ga′` behavior was tested, but the verifier still had only indirect evidence for row5.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Scope.hs`
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs`
  - `/Volumes/src/mlf4/docs/thesis-obligations.yaml`
- Thesis impact:
  - Reopened recursion-refactor row5 `Typing Environment Construction` until the live path had direct `Γ_{a′}` / well-formedness anchors.
- Fix:
  - Added direct `O15-ENV-LAMBDA`, `O15-ENV-LET`, and `O15-ENV-WF` anchors to `docs/thesis-obligations.yaml`.
  - Added live-path row5 regressions in `test/ElaborationSpec.hs` for the lambda-side and let-side environment rules.
  - Refreshed the recursion-refactor mechanism table and implementation notes to treat row5 as closed under the new production-path evidence.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`row5 typing-environment O15-ENV-LAMBDA O15-ENV-WF: lambda body inherits the enclosing binder on the live path`)
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`row5 typing-environment O15-ENV-LET O15-ENV-WF: let body receives x : Typ(b) on the live path`)
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`ga′ redirect stability`)
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`letScopeOverrides`)
  - `/Volumes/src/mlf4/test/ScopeSpec.hs` (`ga scope`)
  - `cabal build all && cabal test`

### BUG-2026-03-07-002
- Status: Resolved
- Priority: Medium
- Discovered: 2026-03-07
- Resolved: 2026-03-07
- Summary: Row8 translatability normalization claimed full thesis alignment, but the live path did not yet run §15.2.8’s stronger all-inert `W` normalization.
- Minimal reproducer:
  - `rg -n "rigidifyTranslatablePresolutionM|weakenInertNodes c0|weakenInertNodes c2" /Volumes/src/mlf4/src/MLF/Constraint/Presolution/Validation.hs`
  - `cabal test mlf2-test --test-show-details=direct --test-option=--match --test-option='row8 thesis-exact guard'`
- Expected vs actual:
  - Expected: the live pre-elaboration path should weaken all inert nodes (or an equivalent `W` normalization) before translation.
  - Actual before fix: the live path enforced Definition 15.2.10 / Theorem 15.2.11 constructive translatability only.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Validation.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Inert.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs`
- Thesis impact:
  - Reopened Transformation Mechanism Table row `Translatability normalization` until the production path performed §15.2.8 all-inert `W` normalization.
- Fix:
  - `rigidifyTranslatablePresolutionM` now weakens all inert nodes before and after rigidification.
  - `test/golden/legacy-replay-baseline-v1.json` is refreshed to freeze the resulting thesis-exact solved artifacts.
- Regression tests:
  - `/Volumes/src/mlf4/test/TranslatablePresolutionSpec.hs` (`row8 thesis-exact guard`)
  - `/Volumes/src/mlf4/test/TranslatablePresolutionSpec.hs` (`Translatable presolution`)
  - `/Volumes/src/mlf4/test/Presolution/EnforcementSpec.hs` (`O15-TRANS*`)
  - `/Volumes/src/mlf4/test/InertSpec.hs` (`O05-*`)
  - `/Volumes/src/mlf4/test/FrozenParitySpec.hs` (`Frozen parity artifact baseline`)
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`checked-authoritative`, `Dual-path verification`)
  - `cabal build all && cabal test`

### BUG-2026-03-07-001
- Status: Resolved
- Priority: High
- Discovered: 2026-03-07
- Resolved: 2026-03-07
- Summary: Row2 result-type reconstruction still used a hidden solved-compat adapter on the live path (`fromSolved` + `ChiQuery.chiSolved`).
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
- Expected vs actual:
  - Expected: row2 reconstructs result-type context directly from finalized `PresolutionView` artifacts with no solved-compat adapter in the live path.
  - Actual before fix: the live path still seeded `PresolutionView` from `Solved` and `ResultType.View` validated via `ChiQuery.chiSolved`.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Finalize.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/ResultType/View.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/ChiQuery.hs`
- Thesis impact:
  - Reopened Transformation Mechanism Table row `Result-type context wiring` until the live path stopped rebuilding solved-compatible state.
- Fix:
  - Added `Finalize.finalizePresolutionViewFromSnapshot` and rewired the live pipeline to build finalized clean/generalized `PresolutionView` artifacts directly from snapshot/finalize data.
  - Changed `ResultType.View` to validate canonical graph structure directly from `PresolutionView` canonical artifacts.
  - Removed `chiSolvedCompat` / `chiSolved` from `MLF.Elab.Run.ChiQuery`.
- Regression tests:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`row2 absolute thesis-exact guard`, `row2 closeout guard`, `checked-authoritative`, `Dual-path verification`)
  - `/Volumes/src/mlf4/test/FrozenParitySpec.hs` (`Frozen parity artifact baseline`)
  - `cabal build all && cabal test`

### BUG-2026-03-05-003
- Status: Resolved
- Priority: High
- Discovered: 2026-03-05
- Resolved: 2026-03-05
- Summary: Replay-mode contract refactor over-activated strict replay-map
  requirements on no-replay paths, causing widespread
  `WitnessNormalizationError ReplayMapIncomplete` regressions.
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
  - `cabal build all && cabal test`
- Expected vs actual:
  - Expected: replay/no-replay contract handling keeps row1-5 sanity gates and
    full baseline green while advancing row6 strictness.
  - Actual before fix: full gate failed with `126` failures, dominated by
    `ReplayMapIncomplete`; required sanity gates above failed.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Types/Witness.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessValidation.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Driver.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
- Thesis impact:
  - Blocked row6 closeout and regressed previously-stable thesis-aligned sanity
    slices in full baseline verification.
- Fix:
  - Completed replay-contract cutover plumbing (`ReplayContract` + `etReplayContract`) and preserved producer-owned contract propagation through canonicalization/rewrite paths.
  - Aligned Phi/Omega consumer behavior to trace contract metadata and removed no-replay repair hooks from translation/runtime.
  - Realigned replay/no-replay regression expectations to current contract behavior while keeping strict contract checks in producer/driver/validation.
- Regression tests:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "replay-map"'`
  - `cabal build all && cabal test`

### BUG-2026-03-05-001
- Status: Resolved
- Priority: High
- Discovered: 2026-03-05
- Resolved: 2026-03-05
- Summary: Row3 ordering refactor initially applied delayed weakens too early in
  edge traversal, causing `OperationOnLockedNode` regressions on reused
  constructor instantiations and frozen parity drift.
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "generalizes reused constructors via make const"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V1"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'`
- Expected vs actual:
  - Expected: reused-constructor path elaborates/solves without locked-node
    failures and frozen parity baseline remains stable.
  - Actual before fix: `ExecError (BindingTreeError (OperationOnLockedNode ...))`
    in pipeline/elaboration slices, plus frozen parity mismatch
    (`cWeakenedVars` drift).
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeUnify.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Driver.hs`
- Thesis impact:
  - Broke stable Phase 4 behavior while tightening row3 ordering and prevented
    closeout verification from reaching green.
- Fix:
  - Preserved Driver-global flush removal and explicit finalization stage.
  - Adjusted edge-loop weaken scheduling so delayed weakens can remain pending
    intra-loop while per-edge unify closure remains strict; queue is drained
    and asserted empty at loop-final boundary.
  - Kept `flushPendingWeakens` idempotence hardening in edge-unify helper.
- Regression tests:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`generalizes reused constructors via make const`)
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`BUG-002-V1`)
  - `/Volumes/src/mlf4/test/FrozenParitySpec.hs` (`Frozen parity artifact baseline`)
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`row3 ordering thesis-exact guard`, `checked-authoritative`, `Dual-path verification`)
  - `/Volumes/src/mlf4/test/Presolution/UnificationClosureSpec.hs` (`Phase 4 thesis-exact unification closure`)
  - `/Volumes/src/mlf4/test/TranslatablePresolutionSpec.hs` (`Translatable presolution`)
  - `cabal build all && cabal test`

### BUG-2026-03-05-002
- Status: Resolved
- Priority: High
- Discovered: 2026-03-05
- Resolved: 2026-03-05
- Summary: Wave-3 owner-boundary scheduling left residual pending weakens at the post-edge-loop boundary because the boundary key (planner owner) could differ from the pending-node-derived owner bucket.
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
- Expected vs actual:
  - Expected: no residual pending weakens at `after-inst-edge-closure`; closure/translatability slices stay green.
  - Actual before fix: `InternalError "presolution boundary violation (after-inst-edge-closure): pending unify edges = [], pending weakens = [...]"` in multiple Phase-4 closure examples.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeUnify.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/StateAccess.hs`
- Thesis impact:
  - Broke strict boundary-empty invariant expected by thesis-shaped `SolveConstraint` ordering and blocked full gate verification.
- Fix:
  - Stabilized pending-weaken owner provenance by stamping owner buckets at
    enqueue-time in `EdgeUnify` (instead of deriving ownership at flush-time
    from mutable graph state).
  - At owner boundaries, scheduler now flushes only the closed-owner bucket
    (plus `PendingWeakenOwnerUnknown`) and then fails fast if non-next-owner
    buckets remain.
  - Removed the flush-all-owner fallback shape while preserving strict
    boundary/finalization diagnostics.
- Regression tests:
  - `/Volumes/src/mlf4/test/Presolution/UnificationClosureSpec.hs` (`Phase 4 thesis-exact unification closure`)
  - `/Volumes/src/mlf4/test/TranslatablePresolutionSpec.hs` (`Translatable presolution`)
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`row3 absolute thesis-exact guard`, `generalizes reused constructors via make const`, `checked-authoritative`, `Dual-path verification`)
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`BUG-002-V1`)
  - `/Volumes/src/mlf4/test/FrozenParitySpec.hs` (`Frozen parity artifact baseline`)
  - `cabal build all && cabal test`

### BUG-2026-03-04-002
- Status: Resolved
- Priority: Medium
- Discovered: 2026-03-04
- Resolved: 2026-03-05
- Summary: Elaboration-input path was not absolutely thesis-exact under strict
  all-path criterion due to residual solved-backed/test-only surfaces.
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input absolute thesis-exact guard"'`
- Expected vs actual:
  - Expected: no residual solved-backed Phi env surface, no ga-scope
    error-swallowing fallback, and no synthetic auto-trace test helper.
  - Actual before fix: guard failed (residual markers present in source).
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Env.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Scope.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/TestOnly.hs`
  - `/Volumes/src/mlf4/test/PipelineSpec.hs`
- Thesis impact:
  - Blocked strict all-path absolute claim for row `Elaboration input`.
- Fix:
  - Removed solved-backed Phi env surface (`peResult`/`askResult`).
  - Propagated binding-tree errors in ga-scope preference path
    (removed `Left _ -> ref` swallowing).
  - Removed synthetic `phiFromEdgeWitnessAutoTrace` from test-only Phi helper
    surface while preserving no-trace `MissingEdgeTrace` fail-fast behavior.
- Regression tests:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`elab-input absolute thesis-exact guard`)
  - `/Volumes/src/mlf4/test/ScopeSpec.hs` (`ga scope` error-propagation checks)
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`MissingEdgeTrace` no-trace fail-fast path)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
  - `cabal build all && cabal test`

### BUG-2026-03-03-001
- Status: Resolved
- Priority: High
- Discovered: 2026-03-03
- Resolved: 2026-03-03
- Summary: Replay-free runtime finalization omitted eliminated-binder snapshot finalization steps, causing Phase 6 Φ regressions after the replay/mediation cutover.
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- Expected vs actual:
  - Expected: replay-free runtime boundary preserves Phase 6 elaboration behavior and keeps paper baseline matrix green.
  - Actual before fix: 9 failures with `PhiInvariantError "PhiReorder: missing binder identity ..."` and `PhiTranslatabilityError "OpRaise (non-spine): missing computation context"` in paper-alignment baselines.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Finalize.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Solve.hs`
  - `/Volumes/src/mlf4/test/PipelineSpec.hs`
- Thesis impact:
  - Broke Chapter 15.3.4-aligned Phase 6 behavior under the runtime replay-removal migration, invalidating strict cutover claims.
- Fix:
  - Added shared snapshot finalization helper `finalizeConstraintWithUF` in `MLF.Constraint.Solve`.
  - Routed `MLF.Constraint.Finalize` through full snapshot finalization semantics (UF rewrite, eliminated-binder rewrite, UF substitution update, bind-parent pruning, strict validation).
  - Updated migration guardrail map comparison to shared live-node domain while retaining strict canonical-constraint and solved-query parity checks.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`Phase 6 — Elaborate`)
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`migration guardrail: thesis-core boundary matches legacy outcome`, `Dual-path verification`)
  - `cabal build all && cabal test`

### BUG-2026-02-27-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-27
- Resolved: 2026-02-28
- Summary: Strict replay-map producer normalization over-activated source binders and raised `ReplayMapIncomplete` on valid edges, causing broad presolution/pipeline regressions.
- Minimal reproducer:
  - `cd /Volumes/src/mlf4-strict-replay-cutover && cabal build all && cabal test`
  - `cd /Volumes/src/mlf4-strict-replay-cutover && cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi" --match "IdentityBridge" --match "Witness" --match "OpWeaken" --match "OpRaise" --match "MissingEdgeTrace" --match "A6" --match "BUG-002" --match "BUG-003"'`
- Expected vs actual:
  - Expected: strict replay hard-reject policy removes runtime repair while preserving green thesis/pipeline regressions (`cabal test` passes).
  - Actual before fix: full gate was red (`889 examples, 97 failures`), dominated by `WitnessNormalizationError (ReplayMapIncomplete ...)` as `PipelinePresolutionError`.
- Suspected/owning area:
  - `/Volumes/src/mlf4-strict-replay-cutover/src/MLF/Constraint/Presolution/WitnessNorm.hs`
  - `/Volumes/src/mlf4-strict-replay-cutover/src/MLF/Constraint/Presolution/Driver.hs`
  - `/Volumes/src/mlf4-strict-replay-cutover/src/MLF/Elab/Phi/Omega.hs`
  - `/Volumes/src/mlf4-strict-replay-cutover/src/MLF/Elab/Phi/Translate.hs`
- Thesis impact:
  - Previously blocked thesis-faithful strict replay cutover by rejecting valid Φ/Ω paths.
- Fix:
  - Driver replay-map codomain rejection is now unconditional at presolution boundary validation.
  - Witness normalization and producer no-replay translation were aligned so valid producer traces satisfy strict driver contract without runtime repair.
- Regression tests:
  - `/Volumes/src/mlf4-strict-replay-cutover/test/Presolution/WitnessSpec.hs` (`Driver replay-map boundary validation`)
  - `/Volumes/src/mlf4-strict-replay-cutover/test/ElaborationSpec.hs` (`BUG-002-V*`, `BUG-003-V*`, `rejects OpGraft on out-of-scheme target`, non-spine `OpRaise`)
  - `/Volumes/src/mlf4-strict-replay-cutover/test/PipelineSpec.hs` (`A6 parity`, `BUG-2026-02-17-002`)
  - `cabal build all && cabal test` (green on 2026-02-28: `893 examples, 0 failures`)

### BUG-2026-02-28-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-28
- Resolved: 2026-02-28
- Summary: Presolution Driver replay-map codomain validation was conditional on non-empty replay binder domain, allowing malformed replay targets to bypass the hard-reject boundary contract.
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Driver replay-map boundary validation"'`
- Expected vs actual:
  - Expected: any replay-map codomain target outside replay binder domain hard-fails, including empty replay binder domains.
  - Actual before fix: `validateReplayMapTraceContract` logic skipped outside-domain rejection when replay binder domain was empty.
- Suspected/owning area:
  - `/Volumes/src/mlf4-strict-replay-cutover/src/MLF/Constraint/Presolution/Driver.hs`
- Thesis impact:
  - Violates strict producer-boundary replay-domain codomain rejection required by Task 5.2 and weakens hard-reject invariants before elaboration.
- Fix:
  - Extracted replay-map trace validation into driver helper `validateReplayMapTraceContract`.
  - Made replay-domain codomain rejection unconditional (removed empty-domain guard) while preserving existing diagnostics.
- Regression tests:
  - `/Volumes/src/mlf4-strict-replay-cutover/test/Presolution/WitnessSpec.hs` (`Driver replay-map boundary validation`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Driver replay-map boundary validation"'`

### BUG-2026-02-26-004
- Status: Resolved
- Priority: High
- Discovered: 2026-02-26
- Resolved: 2026-02-26
- Summary: Presolution closure draining invoked `runUnifyClosure` on intermediate states with no pending unify work, producing false `BindingTreeError` failures (`multiple roots`) on valid programs.
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "O10-EXP-DECIDE"'`
- Expected vs actual:
  - Expected: presolution succeeds and progresses to witness normalization checks.
  - Actual before fix: `InternalError "presolution runUnifyClosure failed: BindingTreeError ... multiple roots"`.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Unify/Closure.hs`
- Thesis impact:
  - Violates Phase 4 SolveConstraint obligations by rejecting valid intermediate states before required unification work is present.
- Fix:
  - `drainPendingUnifyClosure` is now a no-op when `cUnifyEdges` is empty; closure is invoked only when pending unify work exists.
- Regression tests:
  - `test/Presolution/ExpansionSpec.hs` (`O10-EXP-DECIDE`)
  - `cabal test mlf2-test --offline`

### BUG-2026-02-26-003
- Status: Resolved
- Priority: High
- Discovered: 2026-02-26
- Resolved: 2026-02-26
- Summary: Switching production pipeline solved construction to no-replay (`fromPresolutionResult`) regressed elaboration invariants (`alias bounds survived scheme finalization`).
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "O15-ELAB-LET: elaborates polymorphic let-binding"'`
- Expected vs actual:
  - Expected: elaboration of `let id = (\x. x) in id` succeeds with `∀a. a -> a`.
  - Actual before fix: `PipelineElabError (ValidationFailed ["alias bounds survived scheme finalization: [\"a\"]"])`.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Solved.hs`
- Thesis impact:
  - Breaks production Phase 6 behavior and baseline paper-alignment elaboration paths.
- Fix:
  - Aligned `fromPresolutionResult` to replay-finalized snapshot semantics and switched production to presolution-native solved construction as the default path.
  - After two consecutive green full-gate runs, removed the temporary dual-run parity guard from the production path.
  - Follow-up: removed internal legacy fallback entrypoint (`runPipelineElabViaLegacySolve`) and replaced live native-vs-legacy parity tests with frozen baseline artifact checks.
- Regression tests:
  - `test/ElaborationSpec.hs` (`O15-ELAB-LET: elaborates polymorphic let-binding`)
  - `test/PipelineSpec.hs` (`uses presolution-native solved artifacts`)
  - `test/FrozenParitySpec.hs` (`Frozen parity artifact baseline`)
  - `cabal test mlf2-test --offline`

### BUG-2026-02-26-002
- Status: Resolved
- Priority: High
- Discovered: 2026-02-26
- Resolved: 2026-02-26
- Summary: `IdentityBridge.lookupBinderIndex` could collapse distinct scheme binders to the same spine index when multiple binders shared one solved equivalence class.
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "preserves raw binder identity before class-member fallback"'`
- Expected vs actual:
  - Expected: two binders in the same class still resolve to their own spine positions (`b1 -> 0`, `b2 -> 1`) when targeted directly.
  - Actual before fix: both resolved to the lowest matching spine index (`b1 -> 0`, `b2 -> 0`) after class-member key expansion made exact key sets indistinguishable.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/IdentityBridge.hs`
- Thesis impact:
  - Impacts Def. 15.3.4 replay exactness by allowing binder-targeted Ω ops to eliminate the wrong quantifier under merged-class aliasing.
- Fix:
  - Split source-key matching into:
    - exact identity keys (excluding class-member fallback),
    - class-fallback keys (only when no exact keys exist),
    - canonical-alias fallback.
  - Updated `lookupBinderIndex` ranking to prefer exact identity matches before class-member and canonical alias fallback.
- Regression tests:
  - `/Volumes/src/mlf4/test/Phi/IdentityBridgeSpec.hs` (`preserves raw binder identity before class-member fallback`)
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`OpWeaken on an alias target recovers binder via equivalence class and emits InstElim`) to ensure alias recovery still works.
  - `cabal build all && cabal test`

### BUG-2026-02-26-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-26
- Resolved: 2026-02-26
- Summary: Ω `OpWeaken` could collapse to identity when the witness target was a non-binder alias in the same solved equivalence class as a scheme binder.
- Minimal reproducer:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "OpWeaken on an alias target recovers binder via equivalence class and emits InstElim"'`
- Expected vs actual:
  - Expected: Φ emits quantifier elimination (`InstElim`, rendered `∀(a ⩾) N` in the regression fixture).
  - Actual before fix: Φ emitted `ε` (skip path) for alias targets.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/IdentityBridge.hs`
- Thesis impact:
  - Directly affects Def. 15.3.4 Ω replay exactness for weaken/elimination steps on solved-away alias targets.
- Fix:
  - `Omega.OpWeaken` now recovers binders from `Solved.classMembers` before skipping non-binder targets.
  - `IdentityBridge.sourceKeysForNode` now includes solved-class members to preserve binder identity across canonical aliases.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`OpWeaken on an alias target recovers binder via equivalence class and emits InstElim`)
  - `/Volumes/src/mlf4/test/Phi/IdentityBridgeSpec.hs` (`includes solved class members for canonical alias recovery`)
  - `cabal build all && cabal test`

### BUG-2026-02-20-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-20
- Resolved: 2026-02-20
- Summary: Stepping elaborated `let` terms from RHS-coercion identities could expose unresolvable internal type names (`t0`) after substitution.
- Minimal reproducer (surface expression):
  - `ELet "f" (EAnn (ELam "x" (EVar "x")) (mkForalls [("a", Nothing)] (STArrow (STVar "a") (STVar "a")))) (EApp (EVar "f") (ELit (LInt 7)))`
- Expected vs actual:
  - Expected: `typeCheck term == typeCheck (step term)` for the elaborated result.
  - Actual before fix: `typeCheck term` succeeded, but `typeCheck (step term)` failed with `TCArgumentMismatch (TVar "t0") ...`.
- Root cause:
  - `MLF.Elab.Elaborate` (`deriveLambdaBinderSubst`) suppressed alternate node-key entries when they mapped to the same binder name, so the substitution map kept only one key and missed the `t0` key used in elaborated lambda parameter annotations.
- Fix:
  - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`
    - `deriveLambdaBinderSubst` now augments key aliases for lambda binder substitution when binder/parameter arity matches and binders are unbounded, instead of discarding same-name alternate keys.
  - `/Volumes/src/mlf4/test/PipelineSpec.hs`
    - added regression `BUG-2026-02-20-001: stepped annotated-let identity remains type-checkable`.
- Regression tests:
  - `cabal test mlf2-test --allow-newer=base --test-show-details=direct --test-options='--match "BUG-2026-02-20-001"'`
  - `cabal test mlf2-test --allow-newer=base --test-show-details=direct`

### BUG-2026-02-17-002
- Status: Resolved
- Priority: High
- Discovered: 2026-02-17
- Resolved: 2026-02-17
- Summary: Applied bounded/coercion-heavy A6 path now typechecks to `Int` in both unchecked and checked pipelines.
- Minimal reproducer (surface expression):
  - `ELet "c" (EAnn (ELam "x" (ELam "y" (EVar "x"))) (mkForalls [("a", Nothing), ("b", Just (STVar "a"))] (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a"))))) (EApp (EApp (EAnn (EVar "c") (STForall "a" Nothing (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a"))))) (ELit (LInt 1))) (ELit (LInt 2)))`
- Root cause:
  - `MLF.Elab.Elaborate` let-fallback classification only treated raw `ALam` as lambda RHS, so annotated lambdas skipped the lambda fallback path and preserved a mismatch-prone let scheme/closure shape.
  - `MLF.Elab.Elaborate` application recovery only promoted to `InstApp` when the argument source was a named variable; literal arguments stayed on `InstElim` fallback and bottomized polymorphic application.
- Fix:
  - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`
    - let fallback now recognizes `AAnn`-wrapped lambdas/apps for RHS-shape checks.
    - lambda fallback candidates use coherent empty substitution and avoid unnecessary RHS closure wrapping when fallback is selected.
    - application recovery now allows non-variable arguments to drive `InstApp` selection from checked argument type.
  - `/Volumes/src/mlf4/test/PipelineSpec.hs`
    - converted sentinel into strict success regression: `BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines`.
- Regression tests:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "A6 parity"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002 strict target matrix"'`
  - `cabal build all && cabal test`

### BUG-2026-02-17-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-17
- Resolved: 2026-02-17
- Summary: Φ translation over-retained keep-keys for empty target-binder sets and mishandled unbounded `OpGraft -> OpRaise -> OpWeaken` triples, causing bottomization/over-quantification in paper baseline identity + annotation programs.
- Minimal reproducers:
  - `\y. let id = (\x. x) in id y` should elaborate to `∀a. a -> a`, but regressed to codomain `⊥`.
  - `(\x. x) : Int -> Int` and annotation-baseline variants regressed with extra bounded foralls or solver-name leakage.
- Reproducer commands:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "id y should have type" --seed 529747475'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaborates polymorphic instantiation"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaborates term annotations"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "term annotation can instantiate a polymorphic result"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "explicit forall annotation preserves foralls in bounds"'`
- Expected vs actual:
  - Expected: paper baselines remain thesis-exact (`∀a. a -> a` identity shape; bounded-forall annotation forms preserved).
  - Actual before fix: identity codomain collapsed to `⊥`; annotation paths accumulated extra bounded foralls and leaked solver vars (`t0`).
  - Current (2026-02-17): targeted repros above are green and full gate is green (`cabal build all && cabal test`).
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs` (`computeTargetBinderKeys`)
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs` (`OpGraft`/`OpRaise`/`OpWeaken` translation)
  - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs` (`AAnnF` inst-adjustment)
- Thesis impact:
  - Directly affects Def. 15.3.4/Fig. 10 Φ behavior and κσ-style term-annotation instantiation in paper-alignment baselines.
- Fix:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`
    - `computeTargetBinderKeys` now keeps replay keys only by strict intersection with target binders.
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
    - preserves alias/eliminate behavior for empty-context spine `Raise`.
    - collapses unbounded same-binder `OpGraft -> OpRaise -> OpWeaken` triples to direct `InstApp`.
    - refines bound handling for spine `Raise` to avoid destructive alias-bound collapse in inferred-variable cases.
  - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`
    - keeps non-var fallback disabled in generic `reifyInst`.
    - adds localized `AAnnF` fallback for non-variable annotation sources when `inst == InstId` and `expectedBound` is available.
- Regression tests:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "id y should have type" --seed 1070685141'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaborates polymorphic instantiation"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaborates term annotations"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "term annotation can instantiate a polymorphic result"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "explicit forall annotation preserves foralls in bounds"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1925916871'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V" --seed 1593170056'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004" --seed 1593170056'`

### BUG-2026-02-16-010
- Status: Resolved
- Priority: High
- Discovered: 2026-02-16
- Resolved: 2026-02-17
- Summary: Replay-key normalization contract across presolution traces/hints, Φ bridge, and Ω lookup is implemented, including fail-fast behavior for missing replay binder mappings.
- Minimal reproducers:
  - Phase-6 matrix variants can still hit replay-domain under-coverage, for example:
    - `PhiInvariantError "trace/replay binder key-space mismatch ... op: OpGraft ... raw key: NodeId 1 ... replay-map domain: [] ... scheme keys: [0,10]"`
  - Reproducer commands:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V" --seed 1593170056'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Systematic bug variants (2026-02-11 matrix)/BUG-002-V2: alias indirection elaborates to Int/" --seed 1593170056'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Systematic bug variants (2026-02-11 matrix)/BUG-002-V3: intermediate annotation elaborates to Int/" --seed 1593170056'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "fails fast when OpWeaken targets a trace binder source with no replay binder mapping"'`
- Expected vs actual:
  - Expected: replay mapping is total for semantically valid trace-binder targets; fail-fast triggers only for true contract drift.
  - Actual (2026-02-17):
    - fixed/green with deterministic seed `1593170056`: `BUG-002-V1..V4`, `BUG-002-V2`, `BUG-002-V3`, `BUG-004-V2`.
    - missing-replay fail-fast now emits the expected invariant class (`PhiInvariantError` with `trace/replay binder key-space mismatch`) via `resolveTraceBinderTarget`.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs` (`computeTraceBinderReplayBridge`)
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs` (`resolveTraceBinderTarget`)
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs` (hint derivation coverage)
- Thesis impact:
  - Source→replay bridge direction and missing-replay fail-fast behavior are thesis-aligned for strict-paper baseline variants.

### BUG-2026-02-14-003
- Status: Resolved
- Priority: High
- Discovered: 2026-02-14
- Resolved: 2026-02-17
- Summary: `OpRaise` source target could be falsely rejected as outside thesis `I(r)` because Φ/Omega remapped `etInterior` keys through copy/canonical aliases before membership checks.
- Minimal reproducers:
  - Synthetic Φ regression:
    - `OpRaise` target in `etInterior` source domain with `etCopyMap` alias (`1 -> 30`) must not be rejected.
  - Pipeline thesis anchor:
    - BUG-002-V4 (`factory-under-lambda`) where source-domain `OpRaise` must remain admissible against trace `I(r)`.
- Reproducer commands:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "OpRaise accepts source-domain interior membership even when etCopyMap aliases the target"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V4"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004"'`
- Expected vs actual:
  - Expected: `OpRaise` admissibility checks use source-domain `I(r)` directly; alias-only membership is treated as contract drift; BUG-002/BUG-004 anchors remain green.
  - Actual before fix: `PhiTranslatabilityError "OpRaise target outside I(r)"` with remapped interiors (for example `interiorSet=[30,100]` for `OpRaise 1`).
  - Current (2026-02-17): targeted source-domain guards remain green (`OpRaise accepts source-domain interior membership ...`, `BUG-002-V4`, `BUG-004`) and full gate is green (`cabal build all && cabal test`).
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`
- Thesis impact:
  - Directly affects Def. 15.3.4/Fig. 10 translatability around source-domain `I(r)` membership for `Raise`.
  - Fix preserves strict outside-`I(r)` rejection while preventing false negatives caused by identity-domain drift.

### BUG-2026-02-11-004
- Status: Resolved
- Priority: High
- Discovered: 2026-02-11
- Resolved: 2026-02-17 (reopened and reclosed on 2026-02-17)
- Summary: Higher-arity bounded alias chains (`BUG-003-V1`, `BUG-003-V2`) regressed to bottomized elaboration; target behavior is restored by removing variable-annotation `InstInside(InstBot)->InstApp` conversion and restoring trace-free Φ keep-key behavior.
- Minimal reproducers (surface expressions):
  - `ELet "c" (EAnn (ELam "x" (ELam "y" (ELam "z" (EVar "x")))) (mkForalls [("a",Nothing),("b",Just (STVar "a")),("c",Just (STVar "b"))] (STArrow (STVar "a") (STArrow (STVar "b") (STArrow (STVar "c") (STVar "a")))))) (EAnn (EVar "c") (STForall "a" Nothing (STArrow (STVar "a") (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a"))))))`
  - `ELet "c" (EAnn (ELam "x" (ELam "y" (ELam "z" (EVar "x")))) (mkForalls [("a",Nothing),("b",Just (STVar "a")),("c",Just (STVar "a"))] (STArrow (STVar "a") (STArrow (STVar "b") (STArrow (STVar "c") (STVar "a")))))) (EAnn (EVar "c") (STForall "a" Nothing (STArrow (STVar "a") (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a"))))))`
- Reproducer command:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1925916871'`
- Expected vs actual:
  - Expected: checked and unchecked both elaborate to `∀a. a -> a -> a -> a`.
  - Historical actual (2026-02-16): both pipelines failed with `PipelineTypeCheckError (TCLetTypeMismatch ...)`; elaborated RHS remained bottomized (`∀a. ⊥ -> t1 -> ⊥ -> ⊥`) instead of `∀a. a -> a -> a -> a`.
  - Actual (2026-02-16, normalization deterministic-graft pass): failure class on BUG-003 currently shifts earlier to `PipelineElabError (PhiInvariantError "trace/replay binder key-space mismatch ... op: OpGraft+OpWeaken ... raw key: NodeId 6 ... replay-map domain: [0,1,2,4] ...")`.
  - Actual (2026-02-16, replay-bridge follow-up): source-key `6` replay under-coverage is repaired (`traceBinderReplayMap` now includes `6`), and the temporary `OpGraft+OpWeaken(bound-match)` `InstBot expects ⊥` invariant crash is removed; BUG-003 returns to the baseline strict failure bucket (`PipelineTypeCheckError (TCLetTypeMismatch ...)` with bottomized RHS).
  - Additional evidence (2026-02-16): BUG-003 edge-0 trace binder keys are `{0,1,2,4,6}`, but edge-0 scheme/subst is keyed only by `{4,8,38}`. Operations still target `0` (`OpGraft ... 0`, `OpRaise 0`) even though binder key `0` is absent in solved nodes and absent from the scheme key-space.
  - Bridge status (2026-02-16): Φ had explicit source→replay bridge metadata and fail-fast contracts, but BUG-003 still remained in the bottomized mismatch bucket.
  - Closure actual (2026-02-17): `BUG-003-V1/V2` pass (`∀a. a -> a -> a -> a`) and edge-0 presolution leaves no self-bound binder metas (`BUG-003-PRES` pass).
  - Regression actual (2026-02-17, current workspace): deterministic seeded repro fails again with
    - `BUG-003-V1`: expected `∀a. a -> a -> a -> a`, got `⊥ -> ⊥ -> ⊥ -> ⊥`
    - `BUG-003-V2`: expected `∀a. a -> a -> a -> a`, got `⊥ -> ⊥ -> ⊥ -> ⊥`
  - Companion drift evidence (2026-02-17): Φ-step interleaving regression is also present in this workspace
    - `interleaves StepIntro with Omega ops in Φ translation`: expected `"O; ∀(u0 ⩾) N"`, got `"O"`.
  - Current actual (2026-02-17, post-fix): targeted repros are green again
    - `BUG-003-V1/V2` pass (`∀a. a -> a -> a -> a`) with seed `1925916871`.
    - `BUG-003-PRES` remains pass (no self-bound edge-0 metas).
    - StepIntro companion sentinel also passes after trace-free keep-key fix.
  - Root-cause update (2026-02-17):
    - `MLF.Elab.Elaborate` (`AAnnF`) converted variable-annotation `InstInside (InstBot t)` into `InstApp t`; BUG-003 then instantiated `c` with `InstApp TBottom`.
    - `MLF.Elab.Phi.Translate.computeTargetBinderKeys` retained keep-keys for `mTrace = Nothing`, suppressing weaken in trace-free Φ tests.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Plan/Target/GammaPlan.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Plan/BinderPlan/Build.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Plan/Finalize.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeUnify.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs`
- Thesis impact:
  - Target-fixed in scoped matrix: bounded-alias higher-arity chains and edge-0 self-bound invariants now align with thesis expectations again.
- Fix:
  - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`
    - removed variable-annotation conversion of `InstInside (InstBot t)` to `InstApp t` in `AAnnF` inst-adjustment.
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`
    - restored trace-free behavior in `computeTargetBinderKeys` (`mTrace = Nothing` now returns empty keep-key set).
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeUnify.hs`
    - added same-UF-class guard before RaiseMerge emission writes,
    - added edge-local same-root no-op guard for bound writes (`setVarBoundM`) to prevent canonical `n -> n` artifacts.
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs`
    - added `BUG-003-PRES` regression to assert no self-bound edge-0 binder metas remain in `prConstraint`.
- Regression tests:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "interleaves StepIntro with Omega ops in Φ translation" --seed 1925916871'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1925916871'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-PRES"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004" --seed 1925916871'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "fails fast when OpWeaken targets a trace binder source with no replay binder mapping"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "OpRaise accepts source-domain interior membership even when etCopyMap aliases the target"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "does not require Merge for bounded aliasing (b ⩾ a)"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines"'`

### BUG-2026-02-16-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-16
- Resolved: 2026-02-17
- Summary: Edge planner let-edge classification no longer crashes on synthesized-wrapper fixtures with sparse body-root bind-parent ancestry.
- Root cause:
  - `planEdge` unconditionally resolved `eprSchemeOwnerGen` from TyExp body root (`rteBodyId`).
  - For synthesized wrappers, test/fixture topology may place the wrapper under a gen root while body-root ancestry lacks a direct `GenRef`, triggering `InternalError "scheme introducer not found ..."`.
- Fix:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`
    - added `resolveSchemeOwnerGen`:
      - non-synth TyExp path remains strict body-root lookup via `findSchemeIntroducerM`,
      - synthesized-wrapper path uses body-first lookup with wrapper-root fallback.
    - added `firstGenOnPath` helper using `bindingPathToRootUnderM`.
  - `/Volumes/src/mlf4/test/Presolution/EdgePlannerSpec.hs`
    - strengthened let-edge classification regression to also assert `eprSchemeOwnerGen == GenNodeId 0`.
- Regression tests:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Edge plan types/planner classification/threads let-edge flag into allowTrivial/" --seed 1481579064'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge plan types" --seed 1481579064'`
- Thesis impact:
  - Restores deterministic χe planning for let-edge flag classification without loosening frontend TyExp scheme-owner strictness.

### BUG-2026-02-16-002
- Status: Resolved
- Priority: High
- Discovered: 2026-02-16
- Resolved: 2026-02-17
- Summary: Edge planner annotation-edge classification no longer crashes in the same missing-introducer bucket as BUG-2026-02-16-001.
- Resolution linkage:
  - Resolved by BUG-2026-02-16-001 planner scheme-owner fallback in:
    - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`
    - `/Volumes/src/mlf4/test/Presolution/EdgePlannerSpec.hs`
- Regression tests:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Edge plan types/planner classification/threads ann-edge flag into suppressWeaken/" --seed 1481579064'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge plan types" --seed 1481579064'`
- Thesis impact:
  - Restores robust χe planning for annotation-edge policy flags.

### BUG-2026-02-16-007
- Status: Resolved
- Priority: High
- Discovered: 2026-02-16
- Resolved: 2026-02-16
- Summary: BUG-003-V1 sentinel no longer regresses to `SchemeFreeVars` (`__rigid24`); failure class is stabilized in the strict `InstBot` invariant bucket.
- Root cause:
  - Pipeline/result-type generalization fallback handled `BindingTreeError GenSchemeFreeVars` only, not plain `SchemeFreeVars`.
  - BUG-003-V1 therefore aborted early with `PipelineElabError (SchemeFreeVars ...)` before reaching the known strict-instantiation failure bucket.
- Fix:
  - `MLF.Elab.Run.Pipeline`: root generalization fallback now treats plain `SchemeFreeVars` the same as `BindingTreeError GenSchemeFreeVars` and falls back to direct `reifyType` when both GA and non-GA attempts are in the same fallback class.
  - `MLF.Elab.Run.ResultType.Util`: `generalizeWithPlan` now mirrors elaboration fallback semantics for both `BindingTreeError GenSchemeFreeVars` and `SchemeFreeVars`, including reify fallback.
  - `test/ElaborationSpec.hs` BUG-003 sentinels now assert the stabilized strict-instantiation failure string (`InstBot expects TBottom`) instead of the transient `SchemeFreeVars` class.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`BUG-003-V1: triple bounded chain sentinel reproduces known Phi invariant failure`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Systematic bug variants (2026-02-11 matrix)/BUG-003-V1: triple bounded chain sentinel reproduces known Phi invariant failure/" --seed 1481579064'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'`
- Thesis impact:
  - Removes a generalization-closure drift (`SchemeFreeVars`) that masked the underlying thesis-faithfulness bounded-alias gap tracked by BUG-2026-02-11-004.

### BUG-2026-02-16-008
- Status: Resolved
- Priority: High
- Discovered: 2026-02-16
- Resolved: 2026-02-16
- Summary: BUG-003-V2 sentinel now matches the same stabilized strict-instantiation failure class as V1 instead of `SchemeFreeVars` (`__rigid24`).
- Resolution linkage:
  - Resolved by BUG-2026-02-16-007 fallback-alignment fix in:
    - `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs`
    - `/Volumes/src/mlf4/src/MLF/Elab/Run/ResultType/Util.hs`
    - `/Volumes/src/mlf4/test/ElaborationSpec.hs`
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`BUG-003-V2: dual-alias sentinel reproduces known Phi invariant failure`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Systematic bug variants (2026-02-11 matrix)/BUG-003-V2: dual-alias sentinel reproduces known Phi invariant failure/" --seed 1481579064'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'`
- Thesis impact:
  - Clears mirrored sentinel drift for the dual-alias shape while preserving BUG-2026-02-11-004 as the active higher-arity bounded-alias thesis gap.

### BUG-2026-02-16-009
- Status: Resolved
- Priority: High
- Discovered: 2026-02-16
- Resolved: 2026-02-16
- Summary: Explicit forall annotation baseline now round-trips; Φ no longer fails non-spine `OpRaise` after copy-map adoption.
- Root cause:
  - In `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`, non-spine `OpRaise` unconditionally adopted `etCopyMap` source→copied targets before context reconstruction.
  - For the failing edge, source target had a valid context (`C^r_n`) but the adopted target did not, so Ω raised `PhiTranslatabilityError "OpRaise (non-spine): missing computation context"`.
- Fix:
  - Kept adopted-target handling as primary path.
  - Added source-target root-context fallback in non-spine `OpRaise`: when adopted-target context/root insertion is unavailable, retry root-context insertion using the source-domain raise target.
  - This preserves BUG-004 behavior while restoring the explicit-forall paper baseline.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`explicit forall annotation round-trips on let-bound variables`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Explicit forall annotation edge cases/explicit forall annotation round-trips on let-bound variables/" --seed 1481579064'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004" --seed 1481579064'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V4" --seed 1481579064'`
- Thesis impact:
  - Restores Def. 15.3.4 non-spine `Raise` computation-context reconstruction for explicit-forall let-bound annotation baselines.

### BUG-2026-02-16-003
- Status: Resolved
- Priority: High
- Discovered: 2026-02-16
- Resolved: 2026-02-16
- Summary: Redirected let-use polymorphism (`let id = \\x.x in id id`) no longer regresses to `TCArgumentMismatch`.
- Root cause:
  - `AAppF.argInstFromFun` in `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs` inlined inferred instantiation arguments via `inlineBoundVarsType`, which expanded inferred meta-vars (e.g. `t18`) into concrete bound arrows (e.g. `t14 -> t14`) and over-specialized the argument-side `id` instantiation.
- Fix:
  - Removed `inlineBoundVarsType` from `argInstFromFun` inferred-argument construction, preserving the solver variable chosen by `inferInstAppArgs`.
- Regression tests:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`redirected let-use sites keep polymorphic schemes`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Pipeline (Phases 1-5)/redirected let-use sites keep polymorphic schemes/" --seed 1481579064'`
- Thesis impact:
  - Restores paper-baseline let-polymorphic dual instantiation behavior (`id id`).

### BUG-2026-02-16-004
- Status: Resolved
- Priority: High
- Discovered: 2026-02-16
- Resolved: 2026-02-16
- Summary: Checked-authoritative property replay for generated `let id = \\x.x in id id` now passes.
- Resolution linkage:
  - Resolved by BUG-2026-02-16-003 root-cause fix in `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`.
- Regression tests:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`Checked-authoritative invariant`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Pipeline (Phases 1-5)/Checked-authoritative invariant/runPipelineElab type matches typeCheck(term) and checked pipeline type/" --seed 1481579064'`

### BUG-2026-02-16-005
- Status: Resolved
- Priority: High
- Discovered: 2026-02-16
- Resolved: 2026-02-16
- Summary: Phase 6 dual-instantiation elaboration case (`id id`) now elaborates/typechecks.
- Resolution linkage:
  - Resolved by BUG-2026-02-16-003 root-cause fix in `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`elaborates dual instantiation in application`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Polymorphism and Generalization/elaborates dual instantiation in application/" --seed 1481579064'`

### BUG-2026-02-16-006
- Status: Resolved
- Priority: High
- Discovered: 2026-02-16
- Resolved: 2026-02-16
- Summary: Paper baseline `let id = (\\x. x) in id id` again elaborates to `∀a. a -> a`.
- Resolution linkage:
  - Resolved by BUG-2026-02-16-003 root-cause fix in `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`let id = (\\x. x) in id id should have type ∀a. a -> a`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "id id should have type" --seed 1481579064'`
- Thesis impact:
  - Restores a central thesis/paper baseline program.

### BUG-2026-02-11-002
- Status: Resolved
- Priority: High
- Discovered: 2026-02-11
- Resolved: 2026-02-11
- Summary: Thesis-hardening completion for extended polymorphic-factory variants (`BUG-002-V1..V4`) with strict Ω non-binder rejection guardrails.
- Minimal reproducers (surface expressions):
  - `ELet "make" (ELam "x" (ELam "y" (EVar "x"))) (ELet "c1" (EApp (EVar "make") (ELit (LInt 1))) (ELet "c2" (EApp (EVar "make") (ELit (LBool True))) (EApp (EVar "c1") (ELit (LBool False)))))`
  - `ELam "k" (ELet "make" (ELam "x" (ELam "y" (EVar "x"))) (ELet "c1" (EApp (EVar "make") (EVar "k")) (EApp (EVar "c1") (ELit (LBool True)))))`
- Final expected/actual:
  - Expected: V1..V3 elaborate to `Int`; V4 elaborates to `∀a. a -> a`; Ω rejects out-of-scheme/non-binder targets instead of fallback translation.
  - Actual (2026-02-11 verification): `BUG-002-V1..V4` pass in checked/unchecked pipelines; strict reject diagnostics for non-binder/out-of-scheme targets are preserved; full validation gate is green (`647 examples, 0 failures`).
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`BUG-002-V1..V4`, `BUG-003-V1`, `BUG-003-V2`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V"'` (`4 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "out-of-scheme target"'` (`2 examples, 0 failures`)
  - Validation gate: `cabal build all && cabal test` (`647 examples, 0 failures`)
- Thesis impact:
  - Restores thesis-aligned Φ/Ω translatability behavior for polymorphic-factory paths and removes non-binder fallback translation in favor of strict rejection.

### BUG-2026-02-11-003
- Status: Resolved (Thesis-exact)
- Priority: High
- Discovered: 2026-02-11
- Resolved: 2026-02-12
- Summary: Nested annotation variants for BUG-004 (`V2`, `V4`) now pass under strict-only elaboration/typechecking with no compatibility fallback paths.
- Minimal reproducers (surface expressions):
  - `ELet "id" (ELam "x" (EVar "x")) (ELet "use" (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int")) (EApp (EVar "f") (ELit (LInt 0)))) (EApp (EVar "use") (EAnn (EVar "id") (STArrow (STBase "Int") (STBase "Int")))))`
  - `EApp (ELamAnn "seed" (STBase "Int") (ELet "id" (ELam "x" (EVar "x")) (ELet "use" (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int")) (EApp (EVar "f") (EVar "seed"))) (EApp (EVar "use") (EVar "id"))))) (ELit (LInt 1))`
- Root cause:
  - V2: call-site annotation elaborated to a bounded-forall term and then received an additional inferred `InstApp`; strict `InstBot` rejects `InstApp` on already bounded foralls (`InstBot expects TBottom`).
  - V4: `generalizeAtNode` wrapped monomorphic annotations in trivially bounded foralls (`∀(a:Int→Int).a`), causing downstream `InstApp` to fail on non-⊥ bounds.
- Fix:
  - Omega.hs: tightened bare `InstBot` production to require `TBottom` input (changed `ty == TBottom || alphaEqType ty argTy` to `alphaEqType ty TBottom`).
  - Elaborate.hs ALamF: collapse trivially bounded foralls `∀(name:B).name` to bound type `B` for annotated lambda parameters.
  - Elaborate.hs AAppF: normalize inferred argument instantiation to `InstElim` when argument is already `∀(⩾ τ)` (annotation updated the bound), and `InstId` when argument is already monomorphic.
  - PhiReorder: restrict reorder identity to scheme-owned binder positions only.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`BUG-004-V1..V4`)
  - `/Volumes/src/mlf4/test/TypeCheckSpec.hs` (strict InstBot regressions: `InstInside(InstBot)` accept/reject, bare `InstBot` reject)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004"'` (`4 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 7 typecheck"'` (`13 examples, 0 failures`)
  - Validation gate: `cabal build all && cabal test` (`652 examples, 0 failures`)
- Thesis impact:
  - Strict `InstBot` checker semantics are unchanged; only instantiation *production* was corrected. The checker still rejects `InstBot` on any non-⊥ input. This is thesis-exact: the paper's `⊥ ← τ` rule requires the input to be `⊥`.

### BUG-2026-02-06-002
- Status: Resolved
- Priority: High
- Discovered: 2026-02-06
- Resolved: 2026-03-06
- Summary: Historical polymorphic-factory success sentinel. Strict Ω fail-fast on unresolved non-root `OpWeaken` remains correct, but this specific path was carrying a stale producer-side non-root weaken after the `c1` scheme had already become monomorphic; upstream witness normalization now prunes that dead residue so the path elaborates successfully again.
- Reproducer (surface expression):
  - `ELet "make" (ELam "x" (ELam "y" (EVar "x"))) (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4)))) (EApp (EVar "c1") (ELit (LBool True))))`
- Final expected/actual:
  - Expected: checked and unchecked pipelines elaborate to `Int`, while Ω still fails fast on genuine unresolved non-root `OpWeaken`.
  - Actual (2026-03-06 verification): `WitnessNorm` prunes the stale non-root `OpWeaken` before Phi for `let-c1-apply-bool`; checked and unchecked pipelines return `Int`; the under-lambda `BUG-002-V4` strict non-root weaken is still preserved; full gate is green (`956 examples, 0 failures`).
- Regression tests:
  - `/Users/ares/.config/superpowers/worktrees/mlf4/row6-replay-contract-recovery-20260306/test/PipelineSpec.hs`
    - `make let-c1-apply-bool path typechecks to Int`
    - `make let-c1-apply-bool prunes the stale non-root OpWeaken before Phi`
    - `BUG-002-V4 keeps the strict non-root OpWeaken when c1 stays abstract under lambda`
  - `/Users/ares/.config/superpowers/worktrees/mlf4/row6-replay-contract-recovery-20260306/test/ThesisFixDirectionSpec.hs`
    - `BUG-2026-02-06-002 thesis target`
  - Validation gate: `cabal build all && cabal test` (`956 examples, 0 failures`)
- Thesis impact:
  - Keeps thesis-shaped Ω strictness intact. The fix is producer-side dead-op elimination after witness finalization, not a consumer fallback or no-op weaken rule.

### BUG-2026-02-08-004
- Status: Resolved (superseded by strict OpWeaken fail-fast baseline)
- Priority: High
- Discovered: 2026-02-08
- Resolved: 2026-02-11
- Superseded: 2026-02-26
- Summary: Historical nested-let annotated-lambda success sentinel; under strict non-root `OpWeaken` replay, this path is now expected to fail fast when binder replay cannot be resolved.
- Reproducer (surface expression):
  - `ELet "id" (ELam "x" (EVar "x")) (ELet "use" (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int")) (EApp (EVar "f") (ELit (LInt 0)))) (EApp (EVar "use") (EVar "id")))`
- Final expected/actual:
  - Expected (2026-02-26 strict policy): unresolved non-root `OpWeaken` fails fast in Phase 6 (no permissive identity fallback).
  - Actual (2026-02-26 verification): checked and unchecked variants fail fast with `PhiTranslatabilityError` rooted at unresolved `OpWeaken`; full gate is green (`829 examples, 0 failures`) after regression rebaseline.
- Regression tests:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004"'` (`1 example, 0 failures`)
  - Validation gate: `cabal build all && cabal test` (`829 examples, 0 failures`)
- Thesis impact:
  - Makes unresolved replay explicitly non-translatable instead of silently preserving historical permissive behavior.

### BUG-2026-02-11-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-11
- Resolved: 2026-02-11
- Summary: Phase 3 paper-shaped residual-edge wrapping regressed elaboration/typechecking due synthesized-wrapper misclassification and strict Phi reorder keying.
- Reproducer (test command):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 atomic wrapping equivalence gates"'`
- Expected:
  - Gate suite remains equivalent to the Phase 2 baseline (all 7 gate cases pass) under strict paper-shaped wrapping.
- Actual (before fix):
  - Gate suite failed with `SchemeFreeVars`, `TCTypeAbsVarInScope`, then `PhiInvariantError "PhiReorder: missing order key ..."` depending on intermediate fixes.
- Suspected/owning area:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Normalize.hs`
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`
  - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
- Fix:
  - Reserved negative `ExpVarId` space for synthesized wrappers in normalization.
  - Switched synthesized-wrapper dispatch to `ExpVarId < 0` (instead of TyExp-body-shape heuristic), preserving real expansion semantics for frontend TyExp edges.
  - Added Phi reorder fallback to full order-key map when narrowed binder-key map is incomplete.
- Regression tests:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs` (`describe "Phase 3 atomic wrapping equivalence gates"`)
  - Validation gate: `cabal build all && cabal test`
- Thesis impact:
  - Restores strict paper-shaped residual-edge representation without changing elaboration outcomes on the guarded thesis target matrix.

### BUG-2026-02-10-001
- Status: Resolved
- Priority: Medium
- Discovered: 2026-02-10
- Resolved: 2026-02-10
- Summary: Fig. 15.3.4 witness normalization/emission closure lacked an explicit 15-row matrix contract with row-id evidence in tests/docs.
- Reproducer (test command):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match R-'`
- Expected:
  - Explicitly named and executable 15-row matrix coverage for `Graft`/`Weaken`/`Merge`/`Raise`/`RaiseMerge` valid/invalid/norm cases, with deterministic outcomes and green closure gate.
- Actual (before fix):
  - Fig. 15.3.4 witness coverage existed but was not fully represented as a row-ID closure matrix (`R-*-*-NN`) and Task 4 remained marked partial/open.
- Suspected area:
  - `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs`
  - `/Volumes/src/mlf4/test/Presolution/MergeEmissionSpec.hs`
  - `/Volumes/src/mlf4/.kiro/specs/paper-faithfulness-remaining-deltas/*`
- Fix:
  - Added/renamed row-labeled tests covering all 15 matrix rows (`R-GRAFT-VALID-01`..`R-RAISEMERGE-NORM-15`).
  - Added missing Raise VALID/NORM row assertions with strict expected outcomes.
  - Updated `.kiro` requirements/tasks status and supporting docs to reflect closure evidence.
- Regression tests:
  - `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs`
  - `/Volumes/src/mlf4/test/Presolution/MergeEmissionSpec.hs`
  - Matrix gate: `cabal test mlf2-test --test-show-details=direct --test-options='--match R-'`
  - Full gate: `cabal build all && cabal test`
- Thesis impact:
  - Converts Fig. 15.3.4 witness alignment from partial coverage to explicit, auditable closure contract evidence without introducing behavior deviations.

### BUG-2026-02-08-006
- Status: Resolved
- Priority: High
- Discovered: 2026-02-08
- Resolved: 2026-02-08
- Summary: Parser clean-break drift left legacy wrapper API (`parseEmlfExpr` / `parseEmlfType`) publicly exported.
- Reproducer:
  - `rg -n "\\bparseEmlfExpr\\b|\\bparseEmlfType\\b" /Volumes/src/mlf4/src/MLF/Frontend/Parse.hs /Volumes/src/mlf4/src-public/MLF/API.hs /Volumes/src/mlf4/test/FrontendParseSpec.hs`
- Expected:
  - Clean-break parser API exposes explicit staged entrypoints only (`parseRaw*`, `parseNorm*`), with no compatibility wrappers.
- Actual:
  - Legacy aliases remained exported and tests explicitly preserved them.
- Suspected area:
  - `/Volumes/src/mlf4/src/MLF/Frontend/Parse.hs`
  - `/Volumes/src/mlf4/src-public/MLF/API.hs`
  - `/Volumes/src/mlf4/test/FrontendParseSpec.hs`
- Fix:
  - Removed `parseEmlfExpr`/`parseEmlfType` exports and alias definitions from parser/API modules.
  - Updated frontend parse/pretty specs to use explicit raw parser entrypoints only.
- Regression tests:
  - `/Volumes/src/mlf4/test/FrontendParseSpec.hs`
  - `/Volumes/src/mlf4/test/FrontendPrettySpec.hs`
- Thesis impact:
  - Restores the locked staged-boundary migration decision (clean break, no compatibility wrappers), reducing ambiguity in raw-vs-normalized phase entry.

### BUG-2026-02-08-005
- Status: Resolved
- Priority: High
- Discovered: 2026-02-08
- Resolved: 2026-02-08
- Summary: `normalizeType` had a reachable runtime crash in nested alias-bound normalization (`error "normalizeBound: unreachable"`).
- Reproducer (source type):
  - `STForall "x" (Just (STForall "b" (Just (STVar "a")) (STVar "b"))) (STVar "x")`
- Expected:
  - Total normalization through `Either NormalizationError ...` (typed `Left`), never process crash.
- Actual:
  - Runtime exception from `normalizeBound` `STVar` branch.
- Suspected area:
  - `/Volumes/src/mlf4/src/MLF/Frontend/Normalize.hs`
- Fix:
  - Added `NonStructuralBoundInStructContext SrcType` to `NormalizationError`.
  - Replaced the `STVar` crash path in `normalizeBound` with `Left (NonStructuralBoundInStructContext subtree)`.
- Regression test:
  - `/Volumes/src/mlf4/test/FrontendNormalizeSpec.hs` case `rejects nested alias bound that normalizes to a non-structural variable bound`
- Thesis impact:
  - Preserves deterministic, total frontend normalization at the raw→normalized boundary, matching the explicit `Either`-based failure model.

### BUG-2026-02-06-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-06
- Resolved: 2026-02-08
- Summary: Eliminated Phase 6 `MissingNode` crash on nested let + annotated-lambda application.
- Root cause:
  - `reifyWithGaBase` accepted `solvedToBasePref` entries whose base node did not exist in `gaBaseConstraint`, then called base-constraint reification on that stale node id.
- Fix:
  - Added a base-node existence guard in `/Volumes/src/mlf4/src/MLF/Elab/Generalize.hs` so base-constraint reification is only used when that node is present; otherwise elaboration falls back to solved-order reification.
- Regression test:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` (`nested let + annotated lambda application does not crash in Phase 6 (BUG-2026-02-06-001)`).
- Follow-up:
  - The prior type-checking follow-up for the same surface program is now resolved as `BUG-2026-02-08-004` (2026-02-11).

### BUG-2026-02-06-003
- Status: Resolved
- Priority: High
- Discovered: 2026-02-06
- Resolved: 2026-02-08
- Summary: Bounded aliasing (`b ⩾ a`) Merge/RaiseMerge path now elaborates end-to-end.
- Fix:
  - RaiseMerge gating now uses live structural graph queries in `shouldRecordRaiseMerge` (canonical bound lookup + ancestry/interior + elimination state), with no precomputed binder-bound snapshots.
  - Edge-local elimination persists binder substitution targets before elimination, preserving witness inputs required for thesis-aligned Φ translation.
  - Witness normalization interior widening is restricted to multi-binder edge traces so required alias-path evidence is preserved without broad translatability regressions.
- Regression tests:
  - `/Volumes/src/mlf4/test/ElaborationSpec.hs` case: `bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines`
  - `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs` section: `Witness normalization invariants (US-010 regression)`
- Thesis impact:
  - Closes the bounded-alias paper-faithfulness gap by restoring the thesis-aligned `∀a. a -> a -> a` baseline in both checked and unchecked elaboration pipelines.

### BUG-2026-02-08-001
- Status: Resolved
- Priority: High
- Discovered: 2026-02-08
- Resolved: 2026-02-08
- Summary: Phase 6 fallback generalization shadow comparison rejected semantically equivalent solved/base reifications when names diverged (`t14 -> t14` vs `a -> a`).
- Root cause:
  - `shadowCompareTypes` in `/Users/ares/.config/superpowers/worktrees/mlf4/solved-order-shadow-cutover/src/MLF/Elab/Generalize.hs` only used `alphaEqType`, which permits bound-variable renaming but rejects free-name-only renaming from solved/base reify paths.
  - Fallback comparison in `generalizeAt:fallbackSchemeType` compared solved reification (`rpSubst`) against base-path shadow reification (`rpSubstBaseByKey`), and those maps can assign different names to the same shape.
- Fix:
  - Added a bijective variable-renaming comparator (`alphaEqTypeModuloVarRenaming`) and used it in `shadowCompareTypes`.
  - Added regression coverage in `/Users/ares/.config/superpowers/worktrees/mlf4/solved-order-shadow-cutover/test/GeneralizeSpec.hs` for:
    - accepting same-structure free-name divergence
    - rejecting inconsistent (non-bijective) reuse
- Reproducer now passes:
  - `cabal test --test-show-details=direct --test-options='--match=redirected --skip=instantiation'`
- Regression checks:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match="US-004"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match="id y should have type"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match="mapped-base elaboration"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match="redirected let-use"'`
  - `cabal build all && cabal test`
  - Task 7 closure gate rerun (2026-02-08, local `+0800`): `cabal build all && cabal test` passed 5/5 consecutively
    - `1/5` `2026-02-08T16:17:43+0800` -> `2026-02-08T16:17:46+0800`
    - `2/5` `2026-02-08T16:17:46+0800` -> `2026-02-08T16:17:49+0800`
    - `3/5` `2026-02-08T16:17:49+0800` -> `2026-02-08T16:17:52+0800`
    - `4/5` `2026-02-08T16:17:52+0800` -> `2026-02-08T16:17:55+0800`
    - `5/5` `2026-02-08T16:17:55+0800` -> `2026-02-08T16:17:58+0800`
- Thesis impact:
  - Restores thesis-faithful elaboration for redirected let-use identity without weakening mismatch detection for structurally different types.
