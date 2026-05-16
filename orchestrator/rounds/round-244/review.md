### Checks Run
- Command: `git status --short --branch`
  Result: pass. Worktree is `/Volumes/src/mlf4/orchestrator/worktrees/round-244` on branch `orchestrator/round-244-snapshot-finalization-seam`; no controller state edit was present.
- Command: `jq '{roadmap_id, roadmap_revision, roadmap_dir, active_rounds, roadmap_update, resume_errors}' orchestrator/state.json`
  Result: pass. Active roadmap metadata is `2026-05-16-00-architecture-deepening-roadmap` / `rev-001`; `active_rounds` is empty, `roadmap_update` is null, and `resume_errors` is empty.
- Command: `git diff --check`
  Result: pass. No whitespace errors.
- Command: `rg -n "presolutionViewFromSolved|stepSanitizeSnapshotUf|stepCanonicalizeConstraint|stepPruneSolvedBindParents|stepValidateSolvedStrict" src src-public app test`
  Result: pass. Stepwise/fixture helper names are absent from production `MLF.Constraint.Finalize`; remaining hits are test helper use, guard strings, or `MLF.Constraint.Finalize.TestSupport`.
- Command: `rg -n "MLF.Constraint.Finalize|MLF.Constraint.Finalize.TestSupport|MLF.Constraint.Solved.Internal" src src-public app test mlf2.cabal`
  Result: pass. `Finalize.TestSupport` is registered for internal test-support use and consumed only by tests/research specs; `Finalize.Internal` is registered as an implementation module.
- Command: `rg -n "^import .*MLF\.Constraint\.Finalize(\.TestSupport|\.Internal)?" src src-public app test src-research`
  Result: pass. Production imports use the production `Finalize` seam; `Finalize.TestSupport` appears only in tests/research; `Finalize.Internal` is imported only by `Finalize` and `Finalize.TestSupport`.
- Command: `rg -n "^import .*MLF\.Constraint\.Finalize" src/MLF/Constraint/Presolution/Plan/Env.hs`
  Result: pass. No matches; `Presolution.Plan.Env` no longer imports `MLF.Constraint.Finalize`.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'`
  Result: pass. 10 examples, 0 failures; guards cover the production facade exclusions and the new `Finalize.TestSupport` fixture seam.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Solved"'`
  Result: pass. 51 examples, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repo guard"'`
  Result: pass but not coverage. The literal matcher selected 0 examples, so it was followed by the actual guard-suite matcher below.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'`
  Result: pass. 29 examples, 0 failures; includes the new Snapshot Finalization internal-import guard.
- Command: `cabal build all && cabal test`
  Result: pass. Full gate completed with 2586 examples, 0 failures.

### Plan Compliance
- Step 1 reference re-check: met. Searches show removed public step helpers are no longer exported by production `Finalize`; solved-to-view fixtures now route through `Finalize.TestSupport`.
- Step 2 create `MLF.Constraint.Finalize.Internal`: met. `src/MLF/Constraint/Finalize/Internal.hs` owns snapshot sanitizing, canonicalization, solved bind-parent pruning, strict solved validation, and solved-to-view record construction.
- Step 3 narrow `MLF.Constraint.Finalize`: met. The facade exports only `presolutionViewFromSnapshot`, `finalizePresolutionViewFromSnapshot`, `validateCanonicalSnapshotStrict`, `finalizeSolvedFromSnapshot`, and `finalizeSolvedForConstraint`.
- Step 4 add `MLF.Constraint.Finalize.TestSupport`: met. The module exposes the solved-to-view fixture helper and prune helper for tests without widening the production facade.
- Step 5 replace tests/research imports: met. Elaboration, pipeline, reify, and research specs import `Finalize.TestSupport` for fixture mechanics.
- Step 6 move `Presolution.Plan.Env` off `Finalize`: met. It imports `sanitizedViewCanonicalMap` from `MLF.Constraint.Presolution.View` and has no `Finalize` import.
- Step 7 update Cabal, guards, and architecture docs: met. `mlf2.cabal` registers both new modules; `RepoGuardSpec` and `PresolutionFacadeSpec` cover the new boundaries; `docs/architecture.md` documents the production/internal/test-support split.
- Step 8 run focused and full validation: met. Focused facade, solved, repository guard, and full Cabal gates passed.

### Decision
**APPROVED**

### Evidence
The diff matches the selected `round-244-finalize-internal-test-support-split` scope. `MLF.Constraint.Finalize` remains the production Snapshot Finalization construction seam, stepwise mechanics moved behind `MLF.Constraint.Finalize.Internal`, and solved-to-view fixture mechanics moved behind `MLF.Constraint.Finalize.TestSupport`.

`MLF.Constraint.Presolution.Plan.Env` now depends only on the narrow `sanitizedViewCanonicalMap` read-model accessor from `MLF.Constraint.Presolution.View`; `Presolution.View` still does not expose `fromSolved`, `toRawPresolutionViewForLegacy`, or a broad raw-view adapter.

Production `src`, `src-public`, and `app` code do not import `MLF.Constraint.Finalize.TestSupport`. The intentional `Finalize.Internal` import allowance is documented in `docs/architecture.md` and guarded by `RepoGuardSpec`.

The `AGENTS.md` change from `gpt-5.4` to `gpt-5.5` is legitimate synchronization with the direct subagent instruction for this repo, not unrelated churn.

The full gate rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` with worktree-local paths during validation; that generated side effect was restored before review artifacts were written.

Closeout classification: `status-only`. The round completes the selected milestone without changing future coordination meaning, sequencing, extraction scope, verification meaning, or retry policy.
