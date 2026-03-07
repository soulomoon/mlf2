# Implementation Notes

## Thesis Alignment (Phase A–E)

### 2026-03-08 canonicalization helper extraction
- Extracted the duplicated canonicalization helpers shared by `MLF.Constraint.Solved` and `MLF.Constraint.Presolution.View` into `MLF.Constraint.Canonicalization.Shared`.
- Kept behavior unchanged by rewiring both consumer modules to the same implementation and preserving the existing solved/view semantic parity tests.
- Added a direct source guard ensuring those two modules do not each reintroduce local copies of the helper block.
- Verification:
  - `Canonicalization helper dedup guards` — PASS (`1 example, 0 failures`)
  - `PresolutionView mirrors solved canonical/node/bound queries` — PASS (`1 example, 0 failures`)
  - `Canonicalizer` — PASS (`5 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`975 examples, 0 failures`)

### 2026-03-08 guard-first surface `Expr` fold refactor
- Added direct row1 desugaring-contract tests in `test/FrontendDesugarSpec.hs` for annotated-term lowering, annotated-lambda lowering, nested structural recursion, and typed-let coercion-only behavior.
- Added recursion-schemes support only for `Expr 'Surface ty` in `MLF.Frontend.Syntax` and refactored `MLF.Frontend.Desugar.desugarSurface` to a local `cata`.
- `MLF.Frontend.Normalize` remains explicit and unchanged because binder/capture semantics still dominate there.
- Verification:
  - `MLF.Frontend.Desugar` — PASS (`4 examples, 0 failures`)
  - `desugars annotated lambda parameters via let` — PASS (`1 example, 0 failures`)
  - `ELet with EAnn RHS does not create explicit-scheme instantiation structure` — PASS (`1 example, 0 failures`)
  - `row1 closeout guard|checked-authoritative` — PASS (`2 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`974 examples, 0 failures`)

### 2026-03-07 remove final χp `...View` alias duplicates
- Removed the remaining duplicate `...View` / `...FromView` aliases from runtime and reify helpers; the unsuffixed `PresolutionView`-typed names are now the only canonical APIs.
- Updated runtime, result-type, elaboration, Phi, and test call sites to use the unsuffixed names only.
- Added a direct source guard that duplicate alias names are retired from runtime and reify modules.
- Verification:
  - `ga scope` — PASS (`2 examples, 0 failures`)
  - `Generalize shadow comparator` — PASS (`8 examples, 0 failures`)
  - `runtime and reify modules no longer adapt Solved through fromSolved` — PASS (`1 example, 0 failures`)
  - `duplicate ...View aliases are retired from runtime and reify modules` — PASS (`1 example, 0 failures`)
  - `row2 absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `checked-authoritative` — PASS (`8 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`970 examples, 0 failures`)

### 2026-03-07 χp/view-native elaboration closeout
- Removed non-test/non-legacy `fromSolved` usage from `MLF.Elab.Run.Scope`, `MLF.Elab.Run.TypeOps`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.ResultType.Util`, and `MLF.Reify.Core`.
- `PresolutionView` is now the primary internal/runtime API for scope resolution, bound/alias inlining, generalization helpers, result-type generalization, and the non-legacy reify surface.
- The planning/generalization reify context now carries `PresolutionView` snapshots directly; `fromSolved` remains only in `MLF.Constraint.Presolution.View`, `MLF.Elab.Legacy`, and tests.
- Verification:
  - `chi-p global cleanup guard: runtime elaboration helpers no longer import fromSolved` — PASS (`1 example, 0 failures`)
  - `chi-p wrapper retirement guard: primary helper signatures are PresolutionView-native` — PASS (`1 example, 0 failures`)
  - `resolveCanonicalScope propagates binding tree cycle errors` — PASS (`1 example, 0 failures`)
  - `Generalize shadow comparator` — PASS (`8 examples, 0 failures`)
  - `row2 absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `cabal build all && cabal test` — PASS (`969 examples, 0 failures`)

### 2026-03-07 finish χp/view-native elaboration cleanup
- Removed the remaining non-legacy `fromSolved` wrappers from `MLF.Elab.Run.Scope`, `MLF.Elab.Run.TypeOps`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.ResultType.Util`, and `MLF.Reify.Core`.
- `PresolutionView` is now the primary internal/runtime API for elaboration scope helpers, bound/alias inlining, generalization builders, result-type fallback generalization, and reification helpers; `fromSolved` remains only in `MLF.Constraint.Presolution.View`, `MLF.Elab.Legacy`, and tests.
- Added a direct source guard asserting runtime/reify modules no longer adapt `Solved` through `fromSolved`.
- Verification:
  - `ga scope` — PASS (`2 examples, 0 failures`)
  - `Generalize shadow comparator` — PASS (`8 examples, 0 failures`)
  - `runtime and reify modules no longer adapt Solved through fromSolved` — PASS (`1 example, 0 failures`)
  - `row2 absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `ResultType|Phase 6 — Elaborate|chi-first gate stays green` — PASS (`1 example, 0 failures`)
  - `checked-authoritative` — PASS (`8 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`969 examples, 0 failures`)

### 2026-03-07 retire library-side Φ test hooks
- Removed `MLF.Elab.Phi.TestOnly` and `MLF.Elab.Phi.IdentityBridge` from the main library; no test-only Φ helper surface remains exposed from `mlf2-internal`.
- Moved the pure witness-domain ranking/de-dup logic into `test/Phi/WitnessDomainUtil.hs` and renamed the dedicated unit suite to `WitnessDomain`.
- `MLF.Elab.Phi.Omega` keeps the same direct replay-spine fail-fast runtime behavior, but now computes witness-domain diagnostic matches locally instead of importing a test-facing bridge module.
- Verification:
  - `WitnessDomain` — PASS (`23 examples, 0 failures`)
  - `Generalize shadow comparator` — PASS (`8 examples, 0 failures`)
  - `no-trace test entrypoint fails fast with MissingEdgeTrace` — PASS (`1 example, 0 failures`)
  - `elab-input thesis-exact guard` — PASS (`2 examples, 0 failures`)
  - `elab-input absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `row9-11 direct-target guard` — PASS (`1 example, 0 failures`)
  - `cabal build all && cabal test` — PASS (`966 examples, 0 failures`)

### 2026-03-07 Thesis-exact Φ identity cleanup
- Removed the stale compiled `MLF.Elab.Phi.Binder` module and retired its helper re-exports from `MLF.Elab.Phi`, so no compiled Phi surface still advertises the old canonical/base-key/copy-map reconciliation helpers.
- `MLF.Elab.Phi.Omega` remains on the accepted direct replay-spine fail-fast contract; `MLF.Elab.Phi.IdentityBridge` is now documented explicitly as a witness-domain utility/diagnostic/test surface rather than a runtime target-repair engine.
- Added a row9-11 facade cleanup source guard and a dedicated `OpGraft` missing-from-spine regression alongside the existing `OpWeaken` fail-fast coverage.
- Verification:
  - `row9-11 facade cleanup guard` — PASS (`1 example, 0 failures`)
  - `row9-11 direct-target guard` — PASS (`1 example, 0 failures`)
  - `OpWeaken on binder target missing from quantifier spine fails fast` — PASS (`1 example, 0 failures`)
  - `OpGraft on binder target missing from quantifier spine still fails fast even when IdentityBridge finds witness-domain matches` — PASS (`1 example, 0 failures`)
  - `IdentityBridge` — PASS (`24 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`966 examples, 0 failures`)

### 2026-03-07 TMT improving-loop rerun reclosed row2 and row8
- Row2 `Result-type context wiring` is back to `Yes`: the live pipeline now builds finalized clean/generalized `PresolutionView` artifacts directly from `Finalize.finalizePresolutionViewFromSnapshot`, `ResultType.View` validates from canonical constraint + canonical map, and `ChiQuery` no longer exposes solved-compat shims.
- Row8 `Translatability normalization` is back to `Yes`: `rigidifyTranslatablePresolutionM` now applies §15.2.8 all-inert `W`-normalization before and after rigidification, and the frozen parity oracle is refreshed to freeze the resulting solved artifacts.
- Verification:
  - `row2 absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `row2 closeout guard` — PASS (`3 examples, 0 failures`)
  - `row8 thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `Translatable presolution` — PASS (`10 examples, 0 failures`)
  - `O15-TRANS` — PASS (`5 examples, 0 failures`)
  - `O05-` — PASS (`3 examples, 0 failures`)
  - `Frozen parity artifact baseline` — PASS (`1 example, 0 failures`)
  - `checked-authoritative` — PASS (`8 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS.
- This supersedes the temporary “row2 and row8 reopened” classification recorded below.

### 2026-03-07 TMT per-row fresh review reopened row2 and row8
- A new per-row audit reviewed every TMT row with one fresh reviewer agent per row against the live codebase and `papers/these-finale-english.txt`.
- The audit refreshed wording/evidence for rows 1, 3, 6, 12, and 13, and kept rows 4, 5, 7, 9, 10, 11, and 14 materially unchanged.
- It also reopened two thesis-faithfulness gaps:
  - row2 `Result-type context wiring`: the live path still seeds `PresolutionView` from `Solved` and validates through `ChiQuery.chiSolved`, so a hidden solved-compat adapter remains;
  - row8 `Translatability normalization`: live finalization enforces Definition 15.2.10 / Theorem 15.2.11 constructive translatability, but not §15.2.8’s stronger all-inert `W` normalization.
- This supersedes the blanket “all 14 mechanisms yes” closeout claim from the earlier 2026-03-07 verifier sweep section below.

### 2026-03-06 stale non-root `OpWeaken` pruning for BUG-2026-02-06-002
- Recovered `let-c1-apply-bool` without relaxing Ω strictness.
- `MLF.Constraint.Presolution.WitnessNorm` now prunes a non-root `OpWeaken`
  only after finalized source/replay binder domains are known, and only when
  the target is absent from those finalized domains and its bound skeleton is
  no longer fully abstract.
- This keeps the producer-side distinction between:
  - dead residue: top-level stale weakens whose target path has concretized
    leaves (for `let-c1-apply-bool`, the bound chain reaches `Int`);
  - live semantic weaken: under-lambda strict weakens whose target path remains
    fully abstract (`BUG-002-V4`).
- Updated regressions:
  - `test/PipelineSpec.hs`
    - `make let-c1-apply-bool path typechecks to Int`
    - `make let-c1-apply-bool prunes the stale non-root OpWeaken before Phi`
    - `BUG-002-V4 keeps the strict non-root OpWeaken when c1 stays abstract under lambda`
    - BUG-002 sentinel/strict-target matrix rows now assert the actual checked
      success outputs (`TBottom -> Int` for `make-app` / `let-c1-return`,
      `Int` for `let-c1-apply-bool`)
  - `test/ThesisFixDirectionSpec.hs`
    - checked + unchecked BUG-002 thesis target now both assert `Int`
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "bottom-int arrow"'`: PASS
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "let-c1-apply-bool"'`: PASS
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004"'`: PASS
  - `cabal build all && cabal test`: PASS (`956 examples, 0 failures`)

### 2026-03-07 TMT full-sweep closeout sync
- The 2026-03-05 row6 `MAXIMUMRETRY` orchestrator run is now historical-only evidence.
- It was superseded first by the 2026-03-06 row6 replay-contract recovery and
  then by the fresh round-2 full verifier sweep archived under
  `/Volumes/src/mlf4/tasks/archive/2026-03-06-tmt-improving-loop-orchestrator-fresh-round-2/`.
- Historical result at the time: that fresh sweep re-evaluated all 14 TMT mechanisms from a green base and returned `YES` for every row, with `cabal build all && cabal test` passing (`959 examples, 0 failures`).
- Later on 2026-03-07, a stricter per-row thesis/code review reopened row2 and row8; treat the sweep as historical verifier evidence, not the final live classification.

### 2026-03-06 Task 48 row6 replay-contract recovery closeout
- Closed the post-orchestrator replay-contract recovery from a clean green base
  after the fresh round-1 `MAXIMUMRETRY` regression.
- `MLF.Constraint.Presolution.WitnessNorm` now treats no-replay projection as a
  source-domain contract:
  - wrapper-vs-semantic classification uses restored/source identities rather
    than rewritten canonical ids;
  - no-replay `OpWeaken` keeps the historical graft-target heuristic
    (`graftTargetCount`) in source space, so valid success paths
    (`\y. let id = (\x. x) in id y`, A6, annotation-heavy baselines) stay green;
  - strict no-replay remains available only when a surviving source-domain
    non-root `OpWeaken` remains (bug-002 path).
- Producer fail-fast is now narrowed and explicit:
  - residual no-replay replay-family rejection applies to single-target,
    source-interior rogue `OpGraft` shapes that cannot be projected away;
  - wrapper `OpRaise` under `GenRef` / missing type-tree binding is pruned
    before Phi, while type-tree-bound invalid raises still fail via
    `R-RAISE-INVALID-11`.
- Recovery verification evidence:
  - row6/no-replay witness obligations: PASS
  - `checked-authoritative`: PASS
  - `Dual-path verification`: PASS
  - `cabal build all && cabal test`: PASS (`954 examples, 0 failures`)

### 2026-03-05 Row6 orchestrated execution (historical blocked run; superseded)
- Historical record only: this blocked run was superseded by the 2026-03-06
  replay-contract recovery and the 2026-03-07 fresh round-2 full sweep.
- Executed
  `/Volumes/src/mlf4/docs/plans/archive/2026-03-05-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-1.md`
  against task tracker
  `/Volumes/src/mlf4/tasks/archive/2026-03-05-tmt-improving-loop-orchestrator-fresh/`.
- Round-1 target was row6 (`Replay-map producer normalization`), with strict
  role-separated gates across six attempts.
- Terminal outcome for this run: attempt-limit exhaustion (`MAXIMUMRETRY`).
- Final attempt baseline regressed system health:
  - `cabal build all && cabal test` -> FAIL (`126 failures`), dominated by
    `WitnessNormalizationError ReplayMapIncomplete`.
  - Prior required sanity slices now unstable in full baseline:
    `Phase 4 thesis-exact unification closure`,
    `checked-authoritative`, `Dual-path verification`.
- Historical follow-up direction captured at the time:
  - treat replay contract redesign as cross-phase work (producer + consumer),
    then recover baseline green before re-attempting row6 closeout.

### 2026-03-05 Task 47 row3 strict owner-boundary scheduling closeout (agent-team execution)
- Closed the remaining row3 strict gap around owner-boundary delayed-weaken
  scheduling in presolution.
- Removed flush-all-owner boundary fallback shape from
  `MLF.Constraint.Presolution.EdgeProcessing` and kept strict boundary
  invariants (`closed owner` flush + residual-owner fail-fast checks).
- Added stable owner provenance for pending weakens:
  - `EdgeUnify` now stamps owner buckets at enqueue-time and carries them in
    presolution state (`psPendingWeakenOwners`) so boundary selection is not
    derived from mutable post-merge graph shape.
  - Edge-local omega weaken queueing uses edge-local meta identity and
    edge-owner context wiring from `runExpansionUnify`.
- Hardened diagnostics:
  - boundary/finalization violations now report pending owner buckets from both
    edge-loop and driver finalization checks.
- Added/updated strict guards:
  - `Pipeline (Phases 1-5) / row3 absolute thesis-exact guard` now asserts the
    flush-all-owner fallback pattern is absent.
  - `Phase 4 thesis-exact unification closure` remains green with owner-stamped
    boundary scheduling.
- Verification evidence:
  - `--match "row3 absolute thesis-exact guard"` -> PASS (`6 examples`)
  - `--match "Phase 4 thesis-exact unification closure"` -> PASS (`11 examples`)
  - `--match "Translatable presolution"` -> PASS (`8 examples`)
  - `--match "generalizes reused constructors via make const"` -> PASS (`1 example`)
  - `--match "BUG-002-V1"` -> PASS (`1 example`)
  - `--match "Frozen parity artifact baseline"` -> PASS (`1 example`)
  - `--match "checked-authoritative"` -> PASS (`8 examples`)
  - `--match "Dual-path verification"` -> PASS (`4 examples`)
  - `cabal build all && cabal test` -> PASS.

### 2026-03-05 Task 42 row2 absolute thesis-exact hardening (agent-team execution)
- Completed strict wave-based hardening for TMT row `Result-type context wiring`
  with ownership-enforced Team A-E execution.
- Retired row2-local solved-overlay/materialization surfaces:
  - removed `rtvSolved`, `rtvOriginalConstraint`, and `solveFromInputs` from
    `MLF.Elab.Run.ResultType.View`;
  - migrated `Ann`/`Fallback`/`Util` consumers from `View.rtvSolved` and
    solved-only scope/reify helpers to view-native `PresolutionView` paths.
- Preserved strict malformed-view fail-fast semantics at
  `buildResultTypeView` via canonical graph validation and `ValidationFailed`
  error propagation.
- Added and locked regression guard:
  - `Pipeline (Phases 1-5) / Integration Tests / row2 absolute thesis-exact guard`
    asserting row2 solved-overlay surfaces remain absent.
- Verification evidence (required order):
  - RED proof before Wave 1:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
      -> FAIL (`1 example, 1 failure`).
  - GREEN after integration:
    - `--match "row2 absolute thesis-exact guard"` -> PASS (`1 example`)
    - `--match "row2 closeout guard"` -> PASS (`3 examples`)
    - `--match "checked-authoritative"` -> PASS (`8 examples`)
    - `--match "Dual-path verification"` -> PASS (`4 examples`)
    - `cabal build all && cabal test` -> PASS (`935 examples, 0 failures`)

### 2026-03-05 Task 41 absolute strict all-path hardening (agent-team execution)
- Completed strict-wave hardening for row `Elaboration input` with explicit
  ownership splits and integration gates.
- Removed residual non-thesis surfaces targeted by the absolute guard:
  - `MLF.Elab.Phi.Env` no longer carries solved-backed `peResult` /
    `askResult` helper surface.
  - `MLF.Elab.Run.Scope.preferGenScope` no longer swallows binding-tree
    errors (`Left _ -> ref` removed; errors now propagate).
  - `MLF.Elab.Phi.TestOnly` no longer exports/implements
    `phiFromEdgeWitnessAutoTrace`; no-trace helper remains strict fail-fast
    (`MissingEdgeTrace`).
- Added and locked regression guard:
  - `Pipeline (Phases 1-5) / Integration Tests / elab-input absolute thesis-exact guard`
    asserts absence of the three residual surfaces above.
- Verification evidence (required order):
  - RED proof before implementation:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input absolute thesis-exact guard"'`
      -> FAIL (guard sees residual surface).
  - GREEN after integration:
    - `--match "elab-input absolute thesis-exact guard"` -> PASS (`1 example`)
    - `--match "checked-authoritative"` -> PASS (`8 examples`)
    - `--match "Dual-path verification"` -> PASS (`4 examples`)
    - `cabal build all && cabal test` -> PASS (`934 examples, 0 failures`)

### 2026-03-04 docs closeout: strict-policy elaboration-input alignment
- Strict table policy remains unchanged: thesis-exact classification includes
  test-only code paths.
- Retired solved-typed test-only Φ surfaces in `MLF.Elab.Phi.TestOnly`:
  `phiFromEdgeWitnessNoTrace`, alias `phiFromEdgeWitness`, and
  `phiFromEdgeWitnessAutoTrace` now use chi-native `GeneralizeAtWith` callback
  shape (no `Solved`-typed helper signatures).
- No-trace test entrypoint remains strict fail-fast (`MissingEdgeTrace`), so
  the migration preserves the trace contract while removing solved-typed test
  surfaces.
- Row `Elaboration input` is now `Thesis-exact = Yes` under the strict policy,
  conditional on the existing closeout gates (`elab-input thesis-exact guard`,
  `checked-authoritative`, `Dual-path verification`, and full gate).

### 2026-03-04 Task 39 strict legacy-retirement closeout (Team E verification)
- Closed the strict elaboration-input criterion that includes test-only paths:
  - production elaboration/Phi modules no longer expose solved-typed
    compatibility entrypoints;
  - test-only Phi helpers use the chi-native callback shape
    (`GeneralizeAtWith` without a solved-typed callback argument);
  - fail-fast no-trace invariant is preserved (`MissingEdgeTrace`).
- Updated TMT row `Elaboration input` to `Thesis-exact = Yes` only after all
  required closeout gates passed in this workspace.
- Verification evidence (required gates, run with temporary
  `HOME=/tmp/codex-home` and `XDG_CACHE_HOME=/tmp/codex-cache` to avoid cache
  permission failures while keeping command semantics unchanged):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
    - PASS (`2 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`
    - PASS (`931 examples, 0 failures`)

### 2026-03-04 Wave 4 docs closeout (Task 38 agent-team replan)
- Closed Team E docs/verifier handoff for
  `tasks/archive/2026-03-04-elab-input-thesis-exact-agent-team-replan/`.
- Thesis contract references for this migration are now explicit in closeout
  docs:
  - `papers/these-finale-english.txt` Def. 15.3.12 (translation starts from
    translatable `χp` and chosen per-edge witnesses)
  - `papers/these-finale-english.txt` §15.3.6 / Fig. 15.3.5 (edge-witness
    translation pipeline into term elaboration)
- Runtime closure references recorded in TMT:
  - `runPipelineElabWith` threads `ecGeneralizeAtWith` and
    `eePresolutionView` into `elaborateWithEnv`
    (`src/MLF/Elab/Run/Pipeline.hs:112-141`);
  - `reifyInst` calls `phiFromEdgeWitnessWithTrace` with `presolutionView`
    (`src/MLF/Elab/Elaborate.hs:917-949`);
  - active Φ entry/core signatures are `PresolutionView`-based
    (`src/MLF/Elab/Phi/Translate.hs:284-317`).
- Recorded verification evidence from already-run Wave 3 gates:
  - `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`931 examples, 0 failures`)

### 2026-03-04 Task 35 elaboration-input thesis-exact closeout
- Elaboration input row is now closed as thesis-exact for active runtime flow:
  - active elaboration path no longer depends on `ChiQuery.chiSolved` materialization in `elaborateWithEnv`;
  - active Elaborate/Phi generalize callback shape uses `χp`-native inputs;
  - checked-authoritative behavior remains unchanged on representative slices.
- Verification evidence (Task 35 gates):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
    - PASS (`2 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`
    - PASS (`931 examples, 0 failures`)
- Historical note (superseded by Task 39 strict closeout):
  - this task closed active-path boundaries; Task 39 subsequently retired the
    remaining solved-typed production compatibility surfaces and updated
    test-only callback contracts.

### 2026-03-04 Wave 3 Task 6 verifier closeout (row-2 adapter retirement evidence)
- Row-2 adapter retirement is closed in runtime boundaries:
  - `ResultTypeInputs` no longer exposes `rtcSolvedCompat`/`rtcSolveLike`.
  - `ElabConfig` no longer includes `ecSolved`.
  - Guard search confirms adapter symbols now appear only in row2 closeout
    tests, not in `src/` runtime modules.
- Verification evidence captured for closeout:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
    - PASS (`3 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`
    - PASS (`929 examples, 0 failures`)
- Post-row2 priorities:
  1. Reduce `ResultType.View` solved-overlay scaffolding (`rtvSolved` +
     bound-overlay rebuild path) where equivalent `χp`-native queries exist.
  2. Continue simplifying compatibility-shaped helper signatures that still
     thread `Solved` through generalize/reify flows.
  3. Keep row2 closeout guard slices in regular regression cadence.

### 2026-03-03 Wave 3 Task 6 verifier closeout (row-1 chi-first boundary evidence)
- Row-1 runtime boundary shape is now explicit and stable:
  - `ElabEnv` carries `eePresolutionView`, GA parents, edge artifacts, and
    scope overrides (no `eeSolvedCompat` field).
  - `elaborateWithEnv` no longer performs entry-time
    `Solved.rebuildWithConstraint`; chi-first queries flow through `ChiQuery`.
  - Production pipeline remains checked-authoritative; result-type
    reconstruction remains diagnostic-only.
- Explicit adapters still present (row-2 follow-up surface):
  - `rtcSolvedCompat` + `rtcSolveLike` at the result-type boundary.
  - `ElabConfig.ecSolved` as an elaboration compatibility input for existing
    generalize/reify helper signatures.
- Verification evidence for this closeout:
  - Requested combined matcher
    `--match "row1 closeout guard|checked-authoritative|Dual-path verification"`
    selected `0 examples` (PASS, empty selection).
  - Required narrow fallback slices:
    - `--match "row1 closeout guard"`: PASS (`2 examples, 0 failures`)
    - `--match "checked-authoritative"`: PASS (`7 examples, 0 failures`)
    - `--match "Dual-path verification"`: PASS (`4 examples, 0 failures`)
  - Full gate: `cabal build all && cabal test` PASS.
- Ordered next steps (row-2):
  1. Move result-type bound-overlay/materialization to `χp`-native views.
  2. Remove `rtcSolveLike` from `ResultType.View` construction.
  3. Remove `rtcSolvedCompat` from `ResultTypeInputs` and pipeline wiring.
  4. Re-evaluate `ElabConfig.ecSolved` removal once row-2 adapters are gone.

### 2026-03-03 Task 31 chi-first elaboration/result-type internal cleanup (Tasks 1-6 complete)
- Completed the chi-first migration plan at
  `docs/plans/2026-03-03-chi-p-query-first-elab-resulttype-agent-team-implementation-plan.md`
  with wave gates green.
- Added shared chi-query facade for elaboration/result-type internals:
  - new `MLF.Elab.Run.ChiQuery` centralizes `PresolutionView` reads
    (canonical, node lookup, var-bound lookup, bind-parent lookup, and
    canonical-constraint access).
- Result-type internals now prefer `χp` reads:
  - `ResultType.View` routes runtime node/bound reads through `ChiQuery`,
    retaining solved compatibility only in `rtcSolveLike` and bound-overlay
    materialization needed by legacy helper signatures.
- Elaborate internals now prefer `χp` reads:
  - `ElabEnv` carries `eePresolutionView` plus runtime edge/scope artifacts
    (no `eeSolvedCompat` field);
  - `elaborateWithEnv` uses `ChiQuery` for canonicalization/boundary queries
    and avoids local solved-from-constraint materialization.
- Pipeline boundary integration:
  - added `mkResultTypeInputs` and wired pipeline result-type/elaboration
    setup through explicit compatibility inputs (`rtcSolvedCompat`,
    `ecSolved`) instead of ad hoc internal reconstruction.
- Guardrails and verification:
  - source-level chi-first guard tests and phase-gate matcher aliases were
    added in `PipelineSpec`/`ElaborationSpec`;
  - Gate A: `--match "chi-first guard"` PASS;
  - Gate B: `--match "ResultType|Phase 6 — Elaborate|chi-first"` PASS;
  - Task 6 closeout slice:
    `--match "Phase 6 — Elaborate|ResultType|Dual-path verification"` PASS;
  - Gate C: `cabal build all && cabal test` PASS (`923 examples, 0 failures`).

### 2026-03-03 Task 29 solved follow-up closure (Phases 1-6 complete)
- Completed solved-boundary follow-ups from `TODO.md` with end-to-end green validation (`cabal build all && cabal test`: `913 examples, 0 failures`).
- Consolidated solved-to-view projection at the presolution boundary:
  - shared adapter `MLF.Constraint.Presolution.View.fromSolved`
  - removed duplicated runtime adapters in elaboration/pipeline call sites.
- Replaced runtime construction dependencies on test-only naming:
  - added production-safe `Solved.fromConstraintAndUf`
  - migrated runtime planner/reify uses off `mkTestSolved` naming.
- Tightened solved/base mapping handling in presolution planning:
  - introduced explicit `SolvedToBaseResolution` classification (`mapped | same-domain | missing`)
  - routed scope/target fallback handling through typed resolution.
- Expanded and hardened guard coverage:
  - solved invariant checks (`validateOriginalCanonicalAgreement`, `canonicalizedBindParents`)
  - isolated O15 empty-sequence translation guard (`Trχ(ε)=ε` with `Σ(g)=ε`)
  - AAnn result-type primary vs fallback equivalence with populated GA mapping assertions.
- Closed review follow-ups:
  - removed tautological constructor parity test and replaced with semantic constructor invariants,
  - narrowed adapter guard wording to avoid overclaiming corpus scope.

### 2026-03-03 Task 30 solved compatibility-read reduction (Waves 0-4 complete)
- Reduced compatibility-oriented internal solved reads across generalize/result-type internals while preserving behavior:
  - removed unused context compatibility fields (`gcConstraintForReify`, `rbConstraintForReify`);
  - added lightweight context trace when `SolvedToBaseMissing` is hit for a node present in base-domain constraints.
- Generalize reify flow cleanup:
  - alias solved rebuild is now gated to non-OnConstraint branches;
  - explicit-bound helper reification now uses OnConstraint bound reads when structural-scheme path is authoritative.
- Result-type solved-read centralization:
  - added `MLF.Elab.Run.ResultType.View` as a read boundary;
  - confined `rtcSolveLike` usage to view construction;
  - refactored `ResultType`, `Ann`, and `Fallback` to consume the view interface.
- Fallback high-risk path replacement:
  - removed local `Solved.rebuildWithNodes` patching in fallback core;
  - introduced bound-overlay materialization at the view boundary while preserving target-selection and `bindParentsGaFinal` semantics.
- Regression coverage additions:
  - `generalizeWithPlan` GA->no-GA fallback ladder on `SchemeFreeVars` and double-`SchemeFreeVars` reify fallback;
  - integrated result-type fallback handling for `gaSolvedToBase` `same-domain` and `missing` roots.
- Validation:
  - focused carry-forward + new checks: pass;
  - full gate: `cabal build all && cabal test` => `917 examples, 0 failures`.

### 2026-03-03 Runtime thesis-exact elaboration-input strict checklist closeout
- Closed the remaining runtime row-1 boundary gaps tracked in
  `docs/plans/2026-03-02-runtime-thesis-exact-elab-input-implementation-plan.md`
  with a strict 5-item checklist.
- Runtime replay/mediation removal:
  - removed direct production `Solved.fromPreRewriteState` /
    `solveResultFromSnapshot` calls from `MLF.Elab.Run.Pipeline`;
  - deleted inline `setSolvedConstraint` replay helper path from
    `Pipeline.hs` and removed `MLF.Elab.Run.PipelineBoundary`.
  - introduced `MLF.Constraint.Finalize` as the shared runtime finalization
    boundary used by pipeline/runtime paths.
  - restored full snapshot-finalization semantics in the shared boundary by
    reusing `Solve.finalizeConstraintWithUF` (UF rewrite, eliminated-binder
    rewrite, UF substitution update, bind-parent pruning, strict validation).
- Result-type replay removal:
  - `rtcSolveLike` no longer calls replay reconstruction; it now materializes
    solved state from `PresolutionView` canonical data (`pvCanonicalConstraint`
    + `pvCanonicalMap`).
- Elaboration boundary wiring completion:
  - `ElabEnv` no longer carries solved compatibility state (`eeSolvedCompat`);
  - `elaborateWithEnv` no longer performs entry-time solved reconstruction
    (`Solved.rebuildWithConstraint`);
  - compatibility solved access remains explicit in `ElabConfig.ecSolved`.
- Added executable closeout tests (exact plan-matcher names):
  - `row1 boundary uses thesis-core elaboration input contract`
  - `elaborateWithEnv consumes thesis-core input`
  - `row1 boundary validates-only and does not mediate input`
  - `migration guardrail: thesis-core boundary matches legacy outcome`
  - `final row1 state uses single thesis-core boundary path`
  - `Dual-path verification`
- Migration guardrail alignment note:
  - thesis-core vs legacy canonical-map checks now compare on shared live-node
    domain, while preserving strict canonical-constraint and solved-query parity
    assertions; legacy eliminated-node-only canonical links are treated as
    historical metadata, not runtime-domain divergence.
- Validation:
  - targeted closeout slices above: PASS;
  - regression anchors: `Phase 6 — Elaborate` PASS, `Pipeline (Phases 1-5)` PASS, `Dual-path verification` PASS;
  - full gate: `cabal build all && cabal test` PASS.

### 2026-03-05 TMT row `Ordering of transformations` wave execution (Task 44)
- Implemented the row-ordering agent-team plan from
  `docs/plans/2026-03-05-tmt-row-ordering-of-transformations-thesis-exact-agent-team-implementation-plan.md`.
- Wave 0 RED guard:
  - Added `row3 ordering thesis-exact guard` checks in `PipelineSpec` and
    a semantic closure characterization in `UnificationClosureSpec`.
  - Confirmed RED baseline (`2 examples, 2 failures`) before refactors.
- Wave 1+2 core refactor:
  - `MLF.Constraint.Presolution.EdgeProcessing` now integrates delayed-weaken
    flushing within the edge-loop boundary machinery and preserves per-edge
    unify-closure fail-fast checks.
  - `MLF.Constraint.Presolution.EdgeUnify.flushPendingWeakens` was hardened for
    repeated invocation/no-op safety on stale targets.
  - `MLF.Constraint.Presolution.Driver` no longer performs global post-loop
    `flushPendingWeakens`; post-loop work is now explicit
    `runFinalizationStage`:
    - materialization,
    - rewrite/canonicalization,
    - rigidification for translatability,
    - witness normalization,
    with construction checkpoints for pending queues, TyExp coverage/removal,
    and witness/trace domain alignment.
- Regression handling:
  - Initial per-edge weaken flushing triggered
    `OperationOnLockedNode` regressions in reused-constructor paths
    (`generalizes reused constructors via make const`, `BUG-002-V1`) and frozen
    parity drift.
  - Edge-loop scheduling was adjusted so weaken queues are allowed intra-loop
    while preserving per-edge unify-closure boundaries; strict queue drain is
    enforced at the loop-final boundary.
- Verification:
  - `row3 ordering thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `Phase 4 thesis-exact unification closure`: PASS (`8 examples, 0 failures`)
  - `Translatable presolution`: PASS (`8 examples, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - full gate: `cabal build all && cabal test` PASS
  - full-suite direct evidence: `cabal test mlf2-test --test-show-details=direct`
    PASS (`938 examples, 0 failures`)
- Thesis-exact classification note:
  - Row remains `No` in the TMT because weaken flushing is still loop-final
    rather than strictly per-edge after each propagation step.

### 2026-03-05 TMT row3 absolute ordering follow-up execution (Task 45)
- Executed the follow-up agent-team plan from
  `docs/plans/2026-03-05-tmt-row3-ordering-absolute-thesis-exact-agent-team-implementation-plan.md`
  with Wave 0..4 ownership boundaries and sequential gates.
- Wave 0 RED contracts:
  - added strict matcher `row3 absolute thesis-exact guard` in
    `PipelineSpec` and `Presolution.UnificationClosureSpec`;
  - confirmed RED baseline before implementation (`4 examples, 4 failures`).
- Wave 1 integration:
  - added pending-weaken ownership APIs (`PendingWeakenOwner`,
    owner-lookup helpers, owner-boundary flush API surface) in presolution
    base/state/edge-unify layers;
  - rewired `EdgeProcessing` loop to owner-boundary scheduling hooks:
    `scheduleWeakensByOwnerBoundary`,
    `flushPendingWeakensAtOwnerBoundary`,
    `assertNoPendingWeakensOutsideOwnerBoundary`;
  - removed loop-final-only fallback shape
    (`flushPendingWeakens` + `drainPendingUnifyClosureIfNeeded`).
- Wave 2/3 regression + fix:
  - verification exposed residual pending-weaken boundary failures in
    `Phase 4 thesis-exact unification closure`
    (`pending weakens` remained after edge-loop boundary);
  - root cause: planner-owner boundary key and pending-node owner buckets could
    diverge;
  - fix: boundary scheduler now flushes all currently pending owner buckets at
    each owner boundary and reasserts owner-bucket emptiness post-flush.
- Verification evidence (strict required stack):
  - `row3 absolute thesis-exact guard`: PASS (`4 examples, 0 failures`)
  - `Phase 4 thesis-exact unification closure`: PASS (`10 examples, 0 failures`)
  - `Translatable presolution`: PASS (`8 examples, 0 failures`)
  - `generalizes reused constructors via make const`: PASS (`1 example, 0 failures`)
  - `BUG-002-V1`: PASS (`1 example, 0 failures`)
  - `Frozen parity artifact baseline`: PASS (`1 example, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - final gate `cabal build all && cabal test`: PASS
    (`942 examples, 0 failures` from `mlf2-test` log summary)
- Classification note:
  - Row remains `Thesis-exact = No` under strict criterion.
  - Current boundary scheduler is thesis-shape aligned and no longer
    loop-final-only, but remains compatibility-conservative
    (flush-all-pending-owner-buckets at boundaries) rather than a fully proven
    per-edge owner-local weaken schedule.

### 2026-03-01 TMT3 Wave 3 docs closeout (all-aligned policy)
- Transformation Mechanism Table (`docs/notes/2026-02-27-transformation-mechanism-table.md`) is now fully all-aligned for the current branch: every row is `Aligned` and no row references active `DEV-TMT-*` IDs.
- `docs/thesis-deviations.yaml` now moves all `DEV-TMT-*` records out of active `deviations` and into `history.resolved`.
- Each resolved `DEV-TMT-*` history entry includes:
  - `resolution_date: 2026-03-01`
  - replacing commit metadata from TMT3 Wave 1/Wave 2 integration commits where relevant
  - regression test evidence anchors (Phi alignment, IdentityBridge, replay-map and pipeline closeout slices)
- Campaign closeout status: TMT3 is documentation-complete and ready for final integration handoff.

### 2026-03-01 Transformation-mechanism thesis-exact classification campaign
- Historical Wave 0-2 campaign work introduced explicit `DEV-TMT-*` tracking to
  classify remaining non-aligned rows and bind them to code/test evidence.
- Wave 3 completed the closeout: those IDs are now retired to
  `docs/thesis-deviations.yaml` `history.resolved`, and the table itself is
  fully `Aligned`.

### 2026-03-01 Single-solved elaboration input migration
- Elaboration input wiring now uses a single solved snapshot handle in `ElabEnv` (`eeSolved`) and in result-type context (`rtcSolved`).
- Split solved field names were removed from elaboration and result-type wiring (`eeResPhi`, `eeResReify`, `eeResGen`, `rtcSolvedForGen`, `rtcSolvedClean`).
- Checked-authoritative output policy is preserved; `runPipelineElab`/`runPipelineElabChecked` parity remains locked by targeted regression tests.
- Generalization-context differences remain explicit through `GaBindParents`, scope overrides, redirects, and plan-builder-driven generalization.
- For behavior stability in this migration, the authoritative single solved snapshot threaded by pipeline elaboration/result-type wiring is `solvedForGen`.

### Solved Semantics
- `Solved` is now a projection-first layer over equivalence classes.
- `originalConstraint` is the primary accessor for pre-solve data.
- `canonicalConstraint` is used only when post-solve canonical data is explicitly needed.
- Canonical chasing (`canonical`) reconciles aliases but is not the primary source of semantic inputs.

### Phi Translation
- Phi/Omega/IdentityBridge resolve binder identity from witness domain (EdgeTrace/EdgeWitness) first.
- Runtime class-member fallback search is removed from Phi/Omega binder resolution.
- `sourceKeysForNode` is strict witness-domain ranking (raw + copy/trace provenance only; no canonical/class-member expansion fallback).
- Non-root replay resolution is replay-map/source-alias deterministic and fail-fast on contract misses.

### 2026-03-06 TMT identity row re-audit

- Thesis §§15.3.1-15.3.6 carries elaboration identity directly through named nodes, computation contexts, and witness-derived computations (`ε`, `ϕR`, `T(e)`); it does not introduce a separate identity-reconciliation object.
- The accepted runtime path is now witness-domain exact: `Translate` validates trace/replay key-space contracts, while `Omega` uses direct replay/source targets with fail-fast behavior and no local source-candidate recovery helpers.
- `MLF.Elab.Phi.IdentityBridge` remains as a witness-domain utility/diagnostic module and test surface only; it is no longer authoritative for runtime target repair.
- The source-domain interior-membership exception is intentionally narrow: direct forward `etCopyMap` alias evidence is still accepted as witness-authoritative provenance, but reverse-copy/canonical candidate expansion is not used for runtime repair.

### 2026-02-27 Phi strict replay-map normalization (upfront, no runtime fallback search)

- `EdgeTrace` now carries required `etBinderReplayMap` metadata (source binder key -> replay binder node).
- Presolution normalization/validation enforces replay-map completeness, TyVar codomain, and injectivity contracts.
- 2026-02-27 strict runtime contract: producer normalization must emit an active-source/replay-domain replay map; runtime bridge logic validates and passes through only (no projection/repair).
- Phi `computeTraceBinderReplayBridge` aligns replay candidates with scheme quantifier IDs first, validates domain/targets, and fails fast on mismatches.
- 2026-03-01 strict pass-through follow-up: `computeTraceBinderReplayBridge` no longer carries projection-helper fallback paths (`projectReplayTarget`/`projectOne`); runtime bridge checks are domain parity + codomain membership only, then pass-through.
- Omega consumes replay-map targets in replay raw-ID space (no eager canonical rewrite), then resolves binder indices deterministically.
- Source-space replay targets are hard errors (strict fail-fast), not repairable runtime cases.
- No-trace Phi entrypoint is strict fail-fast (`MissingEdgeTrace`) for production parity.

### Pipeline Boundary
- Presolution owns all graph transformations.
- Elaboration path does not mutate Solved or the constraint graph.
- Pipeline setup (before elaboration) may use `rebuildWithConstraint` for canonicalization and `pruneBindParentsSolved` for cleanup.
- Generalize creates local Solved variants via `rebuildWithConstraint` for alias reification; these do not propagate to the pipeline's Solved handle.

### 2026-02-27 Thesis exactness cleanup (A-E)

- Phase A:
  - Added seeded closure API `runUnifyClosureWithSeed` and switched presolution closure drains to seed from `psUnionFind`.
  - Removed per-drain UF rewrite from presolution closure loop.
  - Added hard presolution edge-boundary assertions that reject pending unify edges before and after each inst-edge closure cycle.
- Phase B:
  - `computePresolution` now enforces producer-boundary artifact invariants with explicit errors:
    - `ResidualUnifyEdges`, `ResidualInstEdges`, `ResidualTyExpNodes`,
    - `MissingEdgeWitnesses`, `MissingEdgeTraces`.
  - Witness/trace completeness is checked against non-trivial input instantiation edge IDs (let-edge trivials excluded).
- Phase C:
  - Removed canonical-domain query exports from `MLF.Constraint.Solved`:
    - `canonicalNodes`, `allCanonicalNodes`, `lookupCanonicalNode`, `lookupCanonicalVarBound`.
  - Migrated reify/result-type call sites to projection-first access patterns via:
    - `Solved.lookupNode`, `Solved.lookupVarBound`, `Solved.canonical`,
    - `Solved.originalConstraint`/`Solved.canonicalConstraint` as explicit domain selectors.
- Phase D:
  - `IdentityBridge.sourceKeysForNode` is now strict (no implicit class fallback).
  - Removed class-fallback APIs from IdentityBridge and runtime Phi/Omega binder lookup.
  - Translate/Omega now consume replay-map/source-alias contracts directly and fail fast when targets are unresolved.
- Phase E:
  - Removed transitional runtime entrypoint `runPipelineElabProjectionFirst`.
  - Kept dual-path validation only in test harness (`DualPathSpec`) by comparing native solved artifacts against legacy snapshot reconstruction.

### 2026-02-26 Legacy replay removal + frozen parity baseline

- Removed internal legacy fallback elaboration entrypoints:
  - `runPipelineElabViaLegacySolve` is no longer defined/exported in `MLF.Elab.Run.Pipeline`,
    `MLF.Elab.Run`, and `MLF.Elab.Pipeline`.
- Removed legacy fallback test harness helpers from `test/SpecUtil.hs`.
- Replaced live native-vs-legacy parity tests with a frozen artifact oracle:
  - deterministic artifact builder/renderer in `test/Parity/FrozenArtifacts.hs`,
  - checked-in baseline `test/golden/legacy-replay-baseline-v1.json`,
  - authoritative parity spec `test/FrozenParitySpec.hs`,
  - generator executable `frozen-parity-gen`,
  - regen script `scripts/update-frozen-parity-artifacts.sh` with two-pass deterministic check.
- Scope note:
  - low-level snapshot APIs (`solveUnifyWithSnapshot`, `fromSolveOutput`) remain available for snapshot-centric unit tests, but no longer drive production parity behavior.

### 2026-02-26 Thesis-exact unification ordering + regression hardening

- Presolution now enforces thesis `SolveConstraint` order in Phase 4:
  - drain initial pending unification closure before inst-edge traversal,
  - process edges in topological order,
  - drain closure after each edge when unification work is pending.
- Presolution now carries UF metadata explicitly (`prUnionFind`) while keeping `prConstraint` as the raw translation input graph.
- Shared unification closure logic is centralized in `MLF.Constraint.Unify.Closure` and reused by both Solve and Presolution.
- `Solved.fromPresolutionResult` now uses replay-equivalent snapshot finalization (shared semantics with `fromSolveOutput`).
- Production decision update (2026-02-26): default elaboration pipeline now uses presolution-native solved construction directly (`fromPresolutionResult`) without dual-run legacy replay in the production path.
- Regression hardening:
  - presolution closure drain is now a no-op when `cUnifyEdges` is empty (avoids forcing closure over transient intermediate binding-tree shapes),
  - strengthened parity coverage with explicit legacy-vs-native solved and elaboration anchors.
- Verification snapshot:
  - `cabal test mlf2-test --offline` => `838 examples, 0 failures`.

### 2026-02-26 Milestone 5 gap-closure (OpWeaken alias recovery + IdentityBridge class-members)

- Closed a remaining Ω replay gap where `OpWeaken` could degrade to `ε` when the witness target resolved to a non-binder alias in canonical space.
- `MLF.Elab.Phi.Omega` now attempts binder recovery from `Solved.classMembers` when `OpWeaken` lands on a non-binder replay target; if a recoverable binder index exists in the current `VSpine`, Φ emits `InstElim` at that binder instead of skipping.
- `MLF.Elab.Phi.IdentityBridge.sourceKeysForNode` now includes solved equivalence-class members (`Solved.classMembers`) in source-key expansion, so binder identity can be recovered without relying only on trace/copy-map reverse links.
- Added regressions:
  - `test/ElaborationSpec.hs`: `OpWeaken on an alias target recovers binder via equivalence class and emits InstElim`.
  - `test/Phi/IdentityBridgeSpec.hs`: `includes solved class members for canonical alias recovery`.
- Validation: `cabal build all && cabal test` (`824 examples, 0 failures`).

### 2026-02-26 IdentityBridge binder-identity disambiguation follow-up

- Discovered a remaining replay ambiguity after class-member expansion: when multiple scheme binders shared one solved class, `lookupBinderIndex` could give both binders the same spine index via class-expanded exact-key ties.
- Root cause: class-member keys were participating in exact-match ranking, so direct binder targets lost raw identity distinction and fell back to lowest spine index.
- Fix in `MLF.Elab.Phi.IdentityBridge`:
  - split key matching into exact (no class fallback) vs class fallback vs canonical alias fallback;
  - `lookupBinderIndex` now ranks exact raw/copy/trace identity keys first, then class fallback only when no exact keys exist, then canonical alias fallback.
- Added regression:
  - `test/Phi/IdentityBridgeSpec.hs`: `preserves raw binder identity before class-member fallback`.
- Guarded existing behavior:
  - revalidated `test/ElaborationSpec.hs` alias-target weaken regression (`OpWeaken on an alias target recovers binder via equivalence class and emits InstElim`) to ensure class fallback still recovers alias targets.
- Validation: `cabal build all && cabal test` (`825 examples, 0 failures`).

### 2026-02-26 solvedConstraint migration phase 6 (`ebCanonicalConstraint` removal)

- Removed `ebCanonicalConstraint :: Constraint` from `MLF.Constraint.Solved.EquivBackend`.
- Replaced canonical-storage strategy with explicit canonical graph slices in backend state:
  - `ebCanonicalNodes`, `ebCanonicalInstEdges`, `ebCanonicalUnifyEdges`
  - `ebCanonicalBindParents`, `ebCanonicalPolySyms`, `ebCanonicalEliminatedVars`, `ebCanonicalWeakenedVars`
  - `ebCanonicalAnnEdges`, `ebCanonicalLetEdges`, `ebCanonicalGenNodes`
- Added internal helpers:
  - `canonicalConstraintFromBackend` (reconstruct full canonical `Constraint`)
  - `setCanonicalConstraint` (replace canonical slices from a `Constraint`)
- Updated constructors/rebuilders/mutation helpers to operate on canonical slices while keeping public API behavior unchanged (`canonicalConstraint`, `rebuildWithConstraint`, `patchNode`, `rebuildWithNodes`, etc.).
- Verification after refactor: `cabal build all && cabal test` (`822 examples, 0 failures`).

### 2026-02-26 solvedConstraint migration batch 2 (Reify/Fallback canonical-domain API)

- Removed direct `Solved.solvedConstraint` usage from:
  - `MLF.Reify.Core` (`reifyWith` bind-parent access)
  - `MLF.Elab.Run.ResultType.Fallback` (canonical bound lookup + scope-root post-check path)
- Added `Solved.canonicalizedBindParents :: Solved -> Either BindingError BindParents` so reification can keep canonical bind-parent normalization semantics without extracting raw canonical constraints.
- Added `bindingScopeRefCanonical :: Solved -> NodeId -> Either BindingError NodeRef` in `MLF.Elab.Run.Scope` for canonical-domain scope-root lookup over `Solved.canonicalBindParents`.
- Fallback bound-resolution no longer depends on raw canonical `Constraint`; it now traverses canonical nodes/var-bounds via `Solved.lookupCanonicalNode` and `Solved.lookupCanonicalVarBound`.
- Verification after migration: `cabal build all && cabal test` (`822 examples, 0 failures`).

### 2026-02-24 Eliminate DEV-PHI-STANDALONE-GRAFT-EXTENSION (thesis-exact standalone graft)

- Retired DEV-PHI-STANDALONE-GRAFT-EXTENSION: deeper thesis analysis (Def. 15.3.4) reveals the standalone `OpGraft` handler (`atBinderKeep` + `InstInside(InstBot σ)`) IS thesis-exact — the deviation description was backwards. The paired `OpGraft+OpWeaken` handler producing `InstApp σ` is a sound optimization (equivalent by normalizeInst Rule 1).
- Paired handler retained: deleting it breaks Omega's incremental type-state when multiple graft+weaken pairs interact (binder elimination via `atBinder` changes type-state for subsequent ops; `atBinderKeep` preserves it).
- Added normalizeInst Rule 1b: collapses context-wrapped `InstSeq (InstUnder v (InstInside (InstBot t))) (InstUnder v InstElim)` → `InstUnder v (InstApp t)` for single-level non-front binders.
- Added Rule 1b test in ElaborationSpec.
- Deviation register 6 → 5 entries.
- Verification: 786 examples, 0 failures; conformance gate green; claims checker green.

### 2026-02-24 DEV-PHI-STANDALONE-GRAFT-EXTENSION investigation (deviation retained)

- Investigated eliminating DEV-PHI-STANDALONE-GRAFT-EXTENSION by reversing coalescing direction in `coalesceDelayedGraftWeakenWithEnv` (move graft forward instead of weaken backward).
- Finding: moving descendant ops before the graft changes Omega's type-state evolution — the graft applies `InstInside(InstBot argTy)` which descendant ops depend on. Reordering produces incorrect instantiations (proven by `TCArgumentMismatch` failures on `\y. let id = (\x. x) in id y` and other baselines).
- Conclusion: the standalone graft handler with `atBinderKeep` is semantically load-bearing. The deviation cannot be eliminated without redesigning Omega's incremental type-state translation.
- Added `StandaloneGraftRemaining` error constructor to `WitnessValidation.hs` and `assertNoStandaloneGrafts` validation function to `WitnessCanon.hs` (exported for targeted testing).
- Added characterization tests: "leaves graft standalone when middle ops touch protected set" and "rejects standalone graft with no matching weaken" in `WitnessSpec.hs`.
- Updated deviation description in `thesis-deviations.yaml` with root-cause analysis and additional test evidence matchers.
- Verification: 785 examples, 0 failures; conformance gate green; claims checker green.

### 2026-02-24 Eliminate DEV-PHI-WITNESS-WEAKEN-SUPPRESSION (thesis-exact witness emission)

- Witness emission now always emits `OpWeaken` for unbounded binders (thesis-exact Def. 15.3.4): removed `suppressWeaken` and `argIsGenBound` guards from `classify` in `Witness.hs`, simplified `witnessAlg` stepper signature (removed `Bool` parameter and suffix flag computation), deleted `argIsGenBound` helper.
- Removed annotation-edge blanket weaken stripping: deleted `dropWeakenOps` from `EdgeProcessing/Witness.hs`, removed `suppressWeaken` parameter from `edgeWitnessPlan`, removed `eprSuppressWeaken` field from `EdgePlanResolved` in `Plan.hs`, updated `Planner.hs` and `Interpreter.hs` call sites.
- Extended Omega translation to handle previously-suppressed weakens: added graceful skip in standalone `OpWeaken` case when binder is no longer in the identity list (already eliminated by prior operation).
- Retired `DEV-PHI-WITNESS-WEAKEN-SUPPRESSION` from deviation register (7 → 6 entries).
- Verification: 782 examples, 0 failures; conformance gate green; claims checker green.

### 2026-02-23 Phi thesis-purity follow-up (deviation sync + Omega alignment)

- Registered three load-bearing Chapter 15.3 implementation choices in `docs/thesis-deviations.yaml` and linked them from `CLM-PHI-CORRECTNESS` in `docs/thesis-claims.yaml`:
  - `DEV-PHI-WITNESS-WEAKEN-SUPPRESSION`
  - `DEV-PHI-KEEP-BINDER-WEAKEN-SUPPRESSION`
  - `DEV-PHI-STANDALONE-GRAFT-EXTENSION`
- Removed the `OpGraft; OpRaise; OpWeaken` `mergeIntoApp` peephole from the operation replay loop in `MLF.Elab.Phi.Omega` so operations are replayed de-fused.
- Moved binder-aware bottom rescue (`TBottom -> TVar binder`) into `reifyTypeArg`; call sites now pass binder context directly instead of post-reify rescue helpers.
- Updated bounded `OpGraft+OpWeaken` bound-match behavior:
  - Φ now emits literal thesis-shaped `InstApp boundTy` when graft arg matches explicit bound.
  - Internal replay-state evolution still uses elimination for type-state compatibility; mismatch path remains fail-fast.
- Added/adjusted Omega normalization for de-fused left-associated `OpGraft;OpRaise;OpWeaken` shapes so historical elaboration baselines (notably `\y. let id = (\x. x) in id y`) do not bottom-collapse.

### 2026-02-22 Defensible exactness (traceable evidence chains)

- Added machine-checked thesis claims registry (`docs/thesis-claims.yaml`, 21 claims across Ch. 7-15) and deviation register (`docs/thesis-deviations.yaml`, 5 deviations) with cross-link validation via `scripts/check-thesis-claims.sh`.
- Added `supports_claims` back-links to obligations ledger (`docs/thesis-obligations.yaml`) so every obligation references the claims it supports.
- Added three new test modules for thesis property coverage:
  - `test/TranslatablePresolutionSpec.hs` — Def. 15.2.10 translatable presolution (3 examples).
  - `test/PhiSoundnessSpec.hs` — Def. 15.3.4 Phi soundness (3 examples).
  - `test/ExpansionMinimalitySpec.hs` — Def. 10.1.1 expansion minimality (4 examples).
- Upgraded conformance gate (`scripts/thesis-conformance-gate.sh`) with claims checker and three new anchor matchers.
- Migrated `docs/paper-map.md` Known Deviations and Audit Checklist sections to reference machine-checked artifacts.
- Closed spec drift: all open `.kiro` spec tasks annotated with deferred notes and deviation cross-references.
- Upgraded claim statuses: CLM-TRANSLATABLE-PRESOLUTION, CLM-PHI-CORRECTNESS, CLM-EXPANSION-MINIMALITY from `partial` to `defended`.
- Verification: 781 examples, 0 failures; conformance gate green; claims checker green.

### 2026-02-20 Theorem-proxy scope upgrade (structurally rich generators + new proxies)

- Upgraded `test/TypeSoundnessSpec.hs` generator from flat `elements` pool to sized typed-by-construction:
  - `genTermAtType` builds terms top-down with a typing context (`TyCtx`), using `sized`/`frequency` to produce nested `ELam`, `EApp`, `ELet` at depth.
  - `genAtom` generates type-correct leaves (variables from context, literals at matching ground type, lambda fallback for arrow types).
  - Top-level `genClosedWellTypedElabTerm` occasionally wraps in vacuous `ETyAbs` for type-abstraction coverage.
- Added two new theorem proxies:
  - Multi-step preservation: `typeCheck t = Right tau => typeCheck (normalize t) = Right tau`.
  - Determinism: `step t == step t` (referential transparency guard for small-step).
- Existing proxies (1-step preservation, progress) retained and upgraded to 300 max-success with 30% coverage thresholds.
- Executable theorem-proxy scope:
  - **Progress**: well-typed closed term is value or steps (property-based, 300 samples).
  - **1-step preservation**: type preserved across single `step` (property-based, 300 samples).
  - **n-step preservation**: type preserved across full `normalize` (property-based, 300 samples).
  - **Determinism**: `step` is a pure function (property-based, 300 samples).
  - **Not mechanized**: full inductive proofs of progress/preservation/determinism remain non-mechanized; these are executable proxies only.

### 2026-02-19 Phase 7 theorem obligations executable proxies

- Added `/Volumes/src/mlf4/test/TypeSoundnessSpec.hs` with two property-style checks:
  - preservation proxy: if `typeCheck t = Right tau` and `step t = Just t'`, then `typeCheck t' = Right tau`.
  - progress proxy for closed terms: if `typeCheck t = Right tau` and term is closed, then `isValue t || isJust (step t)`.
- Scope is intentionally Phase 7 local (ElabTerm generator only), so failures isolate `MLF.Elab.TypeCheck`/`MLF.Elab.Reduce` behavior rather than upstream pipeline stages.
- Wired into test harness:
  - `/Volumes/src/mlf4/mlf2.cabal`
  - `/Volumes/src/mlf4/test/Main.hs`
- Added mandatory gate anchor:
  - `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh` now runs matcher `Phase 7 theorem obligations` (min `2` examples).
- This closes part of the remaining non-semantic formalization debt by making theorem proxies executable, while not claiming a mechanized proof.

### 2026-02-19 Formal obligations ledger (thesis Ch. 4–15) hard-fail enforcement

- Added canonical obligations ledger source:
  - `/Volumes/src/mlf4/docs/thesis-obligations.yaml`
- Added generated Markdown view:
  - `/Volumes/src/mlf4/docs/thesis-obligations.md`
- Added ledger tooling:
  - `/Volumes/src/mlf4/scripts/render-thesis-obligations-ledger.rb`
  - `/Volumes/src/mlf4/scripts/check-thesis-obligations-ledger.sh`
- Added mandatory gate stage:
  - `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh` now calls `check-thesis-obligations-ledger.sh` before legacy anchor slices.
- Scope/contract:
  - Covers Chapters 4–15 operational obligations (sections `4.2`–`4.4`, `5.2`–`5.3`, `7.3`, `8.2`, `9.4`, `10.1`–`10.4`, `11.2`–`11.6`, `12.1`–`12.4`, `14.2`–`14.3`, `15.2`–`15.3`).
  - Exact obligation inventory is fixed at `99` IDs (scope: Ch. 4–15 operational rules).
  - Checker hard-fails on count/id drift, missing/duplicate/unmapped obligations, non-anchored status, missing code/test anchors, markdown drift, zero-example matchers, or failing matched examples.
- Verification snapshot:
  - `./scripts/check-thesis-obligations-ledger.sh` (PASS)
  - `./scripts/thesis-conformance-gate.sh` (PASS)
  - `cabal build all && cabal test` (PASS)

### 2026-02-19 Historical status cleanup

- This document is chronological; several 2026-02-16 entries capture intermediate debugging checkpoints.
- Any "red/open/in progress" status in those entries is historical and superseded by later closure entries and by `/Volumes/src/mlf4/Bugs.md` (Open: none).
- Current thesis-faithfulness status is:
  - Chapter 14/15 operational obligations are hard-enforced via the obligations ledger + conformance gate.
  - Semantic paper-faithfulness deltas tracked in `.kiro/specs/paper-faithfulness-remaining-deltas/` are closed.
  - Remaining debt is non-semantic (proof/formalization and assurance breadth).

### 2026-02-18 Thesis conformance gate command/profile

- Added canonical gate entrypoint:
  - `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh`
- Gate behavior:
  - Runs thesis-anchor focused test slices with deterministic matcher strings.
  - Enforces minimum matched-example thresholds per slice so stale matcher strings cannot silently pass with `0 examples`.
  - Current thresholds:
    - `R-` matrix rows: min `15` examples
    - `A6 parity`: min `3`
    - `BUG-2026-02-17-002`: min `1`
    - `Phase 3 atomic wrapping equivalence gates`: min `7`
    - `has type forall a. a -> a`: min `1`
- CI enforcement:
  - Added `/Volumes/src/mlf4/.github/workflows/thesis-conformance.yml`.
  - CI job builds all targets (`cabal build all`) and then runs `./scripts/thesis-conformance-gate.sh`.
- Verification snapshot:
  - `./scripts/thesis-conformance-gate.sh` (PASS)
  - `cabal build all && cabal test` (PASS)

### 2026-02-18 A5 (P3) totality/harness hardening closure

- Frontend coercion-copy failure typing:
  - Added `UnexpectedBareCoercionConst` to `MLF.Frontend.ConstraintGen.Types.ConstraintError`.
  - `MLF.Frontend.ConstraintGen.Translate.buildExprRaw` now rejects bare `ECoerceConst` with the typed constructor instead of stringly `InternalConstraintError`.
- STCon coercion-copy totalization:
  - Refactored constructor-argument internalization into `internalizeConArgs` (`NonEmpty` recursion) and removed in-branch `NE.head`/`NE.tail` + ad hoc accumulator plumbing from `STCon` handling.
  - Preserved existing sharing/rebinding semantics (`SharedEnv` threading and rigid child rebind behavior remain unchanged).
- Harness wiring hardening:
  - `test/Main.hs` now wires presolution via `PresolutionSpec.spec` only (single-source umbrella).
  - Added fail-fast wiring guard: `IORef` marker is set at presolution wiring and checked immediately after; test binary aborts if presolution umbrella wiring is removed.
- Regression coverage added/updated:
  - `test/ConstraintGenSpec.hs`:
    - `bare ECoerceConst rejects with typed UnexpectedBareCoercionConst (not InternalConstraintError string)`
    - `STCon coercion-copy failures surface as typed errors`
    - `nested STCon coercion-copy preserves binding-tree validity`
  - `test/PresolutionSpec.hs` + `test/Main.hs`: umbrella wiring consolidation and guard path.
- Verification snapshot:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "bare ECoerceConst rejects"'` (PASS)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "STCon coercion-copy failures surface as typed errors"'` (PASS)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 — Principal Presolution"'` (PASS)
  - `cabal build all && cabal test` (PASS)

### 2026-02-17 BUG-2026-02-17-002 applied bounded/coercion A6 closure

- Root-cause chain (systematic-debugging):
  - `MLF.Elab.Elaborate` `ALetF` fallback shape checks only recognized raw `ALam`/`AApp`; annotated lambdas (`AAnn (ALam ...)`) skipped the lambda fallback path and retained mismatch-prone let scheme shaping.
  - `MLF.Elab.Elaborate` `AAppF` recovery upgraded `InstApp` only when the argument source was a named variable; literal arguments fell back to `InstElim`, bottomizing applications through unbounded binders.
- Implemented behavior:
  - `ALetF` now unwraps `AAnn` when classifying RHS shape (`rhsIsLam`/`rhsIsApp`).
  - Lambda fallback candidates now use `IntMap.empty` substitution and avoid extra RHS closure wrapping when a fallback scheme is selected.
  - `AAppF` `funInstRecovered` now permits non-variable arguments to drive `InstApp` recovery using checked argument type (still constrained by existing binder-shape guards).
- Result:
  - The applied A6 bounded/coercion variant now elaborates to `Int` in both unchecked and checked pipelines.
  - Regression sentinel was upgraded to strict success assertion in `test/PipelineSpec.hs`.
- Verification snapshot:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002"'` (PASS)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "A6 parity"'` (PASS)
  - `cabal build all && cabal test` (PASS)

### 2026-02-17 A1 strict Ω normalization closure audit

- Audited A1 acceptance criteria against production witness normalization:
  - `MLF.Constraint.Presolution.WitnessCanon.normalizeInstanceOpsFull` rejects malformed merge direction as `MergeDirectionInvalid`.
  - `MLF.Constraint.Presolution.WitnessNorm.normalizeEdgeWitnessesM` surfaces normalization failures as `WitnessNormalizationError` without permissive fallback acceptance.
- Verification evidence:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match R-MERGE-NORM-09'` (PASS)
  - `cabal test mlf2-test --test-show-details=direct --test-options=\"--match=\\\"fails fast with MergeDirectionInvalid via presolution normalization\\\"\"` (PASS)
  - `cabal build all && cabal test` (PASS)
- Synced tracker closure in `TODO.md` (`A1 (P1)` entries now closed with dated AC status).

### 2026-02-17 A4 paper-faithfulness doc/spec sync

- Synced `.kiro/specs/paper-faithfulness-remaining-deltas/` to current state:
  - `requirements.md`: all semantic requirements in this spec are now marked present with evidence.
  - `design.md`: removed stale wording that described still-open Φ/witness semantic deltas; added explicit non-semantic remaining deltas only.
  - `tasks.md`: added closure note marking semantic plan complete and redirecting remaining backlog to `TODO.md`.
- Synced `TODO.md` A4 entries to done with dated closure note.
- Current residual non-thesis-exact scope is non-semantic:
  - proof/formalization debt,
  - full formal Phase-7 linkage to thesis proof obligations,
  - broader regression/docs/API cleanup backlog items tracked outside semantic bug tracking.

### 2026-02-17 BUG-2026-02-17-001 Φ keep-key + Graft/Raise/Weaken stabilization

- Root-cause cluster:
  - `MLF.Elab.Phi.Translate.computeTargetBinderKeys` retained replay keys when the target binder set was empty, suppressing `OpWeaken` elimination in edge traces that should discharge binders.
  - Ω translation emitted over-complex instantiations for unbounded same-binder triples `OpGraft -> OpRaise -> OpWeaken`, which drifted paper baselines (`id y`, annotation instantiation shapes).
  - Annotation handling needed a localized `InstId` fallback in `AAnnF` for non-variable annotation sources with explicit expected bounds.
- Implemented behavior:
  - `MLF.Elab.Phi.Translate`:
    - keep-keys are now strict intersection with target binders (no empty-target “keep everything” fallback).
  - `MLF.Elab.Phi.Omega`:
    - preserve spine Raise alias/eliminate behavior for empty intermediate contexts,
    - collapse unbounded same-binder `OpGraft -> OpRaise -> OpWeaken` triples to direct `InstApp`,
    - retain conservative bound normalization for explicit bounds while avoiding destructive collapse for inferred unbounded variable cases.
  - `MLF.Elab.Elaborate` (`AAnnF`):
    - maintain strict generic `reifyInst` fallback policy (`Nothing -> False`),
    - add local non-variable annotation fallback from `InstId` to `InstInside (InstBot expectedBound)`.
- Verification snapshot:
  - PASS: `id y should have type`, `elaborates polymorphic instantiation`, `elaborates term annotations`, `term annotation can instantiate a polymorphic result`, `explicit forall annotation preserves foralls in bounds`.
  - PASS: `BUG-002-V` (seed `1593170056`), `BUG-003-V` (seed `1925916871`), `BUG-004` (seed `1593170056`), OpRaise source-domain interior guard.
  - `cabal build all` passes.
  - Intermediate checkpoint (superseded by closure section below): full `cabal test` then had 3 remaining failures in unrelated buckets (pipeline ann-redirect invariant + two Φ contract tests).

### 2026-02-17 BUG-2026-02-17-001 closure pass (remaining 3 buckets)

- Residual failures closed:
  - `MLF.Elab.Phi.Omega.resolveTraceBinderTarget` now enforces binder-domain fail-fast for trace-source operands when replay binder candidates are absent (`PhiInvariantError "trace/replay binder key-space mismatch"`), matching the strict Φ/Ω contract tests.
  - Non-spine `OpRaise` no longer rejects non-`⊥` bounds when a valid `C^m_n` context is available; Ω now executes the context-path intro/bot/alias translation directly in that case.
  - `PipelineSpec` canonicalization sentinel now asserts non-empty canonicalized scheme roots only when solve produced non-empty `union-find`; stale-node/root canonicalization checks remain strict.
- Verification snapshot:
  - PASS: `/Phase 6 — Elaborate (xMLF)/.../fails fast when OpWeaken targets a trace binder source with no replay binder mapping/`
  - PASS: `/Phase 6 — Elaborate (xMLF)/.../Φ translates non-spine OpRaise using binding edges and ≺ ordering (non-spine)/`
  - PASS: `/Pipeline (Phases 1-5)/applyRedirectsToAnn and canonicalizeAnn rewrite every node occurrence consistently/`
  - PASS: `cabal build all && cabal test` (`678 examples, 0 failures`).

### 2026-02-16 BUG-2026-02-11-004/010 hybrid bridge follow-up (historical checkpoint; superseded)

- Extended edge trace metadata (`MLF.Constraint.Presolution.Base.EdgeTrace`) with:
  - `etBinderReplayHints :: IntMap NodeId`
  - Contract: source binder key -> replay-domain binder candidate (live TyVar only).
- Presolution now derives and persists replay hints during witness normalization:
  - `MLF.Constraint.Presolution.WitnessNorm` computes deterministic source/rewrite hint maps from canonicalized binder args + solved-node liveness.
  - normalized traces in `psEdgeTraces` now carry `etBinderReplayHints`.
- Witness validation now carries replay hints in normalization env (`binderReplayHints`) and rejects hinted operands that are not live TyVars (`HintedOperandNotLiveTyVar`), preventing silent replay through dead source keys when hints exist.
- Φ bridge construction now consumes hints + positional replay seeding:
  - `MLF.Elab.Phi.Translate.computeTraceBinderReplayBridge` now:
    - prefers hint candidates where valid,
    - adds positional source→replay seed from trace-order source binders and replay-subst keys,
    - keeps name-based/alias-based deterministic fallback.
  - Ω diagnostics include hint-domain payload in binder target mismatch errors.
- Verification snapshot (at that checkpoint):
  - PASS:
    - `fails fast when OpWeaken targets a trace binder source with no replay binder mapping`
    - `OpRaise accepts source-domain interior membership even when etCopyMap aliases the target`
    - bounded-alias baseline (`b ⩾ a`) anchors
    - strict matrix guard: `make-app keeps codomain Int without bottom-domain collapse`
  - At that checkpoint (before 2026-02-17 closure), open:
    - `BUG-003-V1/V2` remain deterministic `TCLetTypeMismatch` (`∀a. ⊥ -> t1 -> ⊥ -> ⊥` vs expected `∀a. a -> a -> a -> a`).
  - At that checkpoint, full gate was red (`674 examples, 33 failures`) and BUG-2026-02-11-004 / BUG-2026-02-16-010 were open.

### 2026-02-16 BUG-2026-02-11-004 Φ/Ω source→replay binder bridge (historical checkpoint; superseded)

- Implemented the planned bridge at the Φ→Ω boundary:
  - `MLF.Elab.Phi.Translate` now computes, once per edge (after `siForOmega` finalization),:
    - `traceBinderSources :: IntSet` (deduped `etBinderArgs` binder keys, trace order)
    - `traceBinderReplayMap :: IntMap NodeId` (source binder key -> replay binder key)
  - mapping remains deterministic and name-driven:
    - scheme names come from `siScheme`,
    - replay keys come from `siSubst` with per-name key selection ranked by `traceOrderRank` (`IdentityBridge`),
    - pairing is `zip` of scheme binder names with trace binder sources.
- `OmegaContext` now carries the bridge contract explicitly:
  - `ocTraceBinderSources`
  - `ocTraceBinderReplayMap`
- `MLF.Elab.Phi.Omega` now resolves binder-target operands before execution for:
  - `OpGraft _ bv`
  - `OpWeaken bv`
  - `OpRaise n` (execution target only)
  - `OpMerge n m`
  - `OpRaiseMerge n m`
- Thesis-preserving split retained:
  - `OpRaise` translatability/interior checks continue to use raw source key (`nSource`) for `I(r)` checks.
  - replay execution paths use the resolved replay key.
- New fail-fast invariant:
  - If an Ω binder-target key is a trace binder source but has no replay-key mapping, Φ now returns `PhiInvariantError` with edge/op/raw-key/source-set/replay-domain/scheme-keys diagnostics.
  - This replaces prior silent drift into non-binder/bottomized behavior.
- Focused validation status (at that checkpoint):
  - PASS: new fail-fast regression (`OpWeaken` unmapped trace binder target).
  - PASS: source-domain interior alias regression (`OpRaise accepts source-domain interior membership ...`).
  - PASS: bounded alias baseline (`b ⩾ a`) non-regression anchors.
  - At that checkpoint, `BUG-003-V1/V2` remained in `TCLetTypeMismatch` (`∀a. ⊥ -> t1 -> ⊥ -> ⊥` vs expected `∀a. a -> a -> a -> a`).
  - At that checkpoint, full gate remained red in broader buckets (`cabal build all && cabal test`: `674 examples, 47 failures`).

### 2026-02-16 BUG-2026-02-16-007/008 `SchemeFreeVars` sentinel-drift closure

- Root cause:
  - BUG-003-V1/V2 hit plain `SchemeFreeVars (NodeId 27) ["__rigid24"]` in pipeline/result-type generalization paths.
  - Those paths retried only `BindingTreeError GenSchemeFreeVars`, so plain `SchemeFreeVars` escaped as a top-level `PipelineElabError` and masked the underlying strict-instantiation failure bucket.
- Fix:
  - `MLF.Elab.Run.Pipeline`: root generalization fallback now treats `SchemeFreeVars` and `BindingTreeError GenSchemeFreeVars` uniformly (`GA -> non-GA -> reifyType`).
  - `MLF.Elab.Run.ResultType.Util`: `generalizeWithPlan` now mirrors the same fallback policy.
  - `test/ElaborationSpec.hs`: BUG-003-V1/V2 sentinels now assert the stabilized strict-instantiation class (`InstBot expects TBottom`) instead of the transient `SchemeFreeVars` class.
- Result:
  - BUG-003 sentinel variants no longer fail with `SchemeFreeVars/__rigid24`.
  - Both variants are back in the shared bounded-alias failure bucket tracked by BUG-2026-02-11-004.
- Verification snapshot (2026-02-16):
  - PASS:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Systematic bug variants (2026-02-11 matrix)/BUG-003-V1: triple bounded chain sentinel reproduces known Phi invariant failure/" --seed 1481579064'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Systematic bug variants (2026-02-11 matrix)/BUG-003-V2: dual-alias sentinel reproduces known Phi invariant failure/" --seed 1481579064'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'`

### 2026-02-16 BUG-2026-02-16-009 non-spine `OpRaise` context fallback

- Root cause:
  - In `MLF.Elab.Phi.Omega`, non-spine `OpRaise` adopted source targets through `etCopyMap` before context reconstruction.
  - For explicit-forall let-bound annotation baseline, source target had a valid `C^r_n` path while adopted target had none, causing `PhiTranslatabilityError "OpRaise (non-spine): missing computation context"`.
- Fix:
  - `OpRaise` now computes both adopted and source-domain raise/context targets.
  - Non-spine translation keeps adopted-target handling as primary.
  - If adopted-target non-spine context/root insertion cannot be constructed, Ω retries root-context insertion using the source-domain target.
- Result:
  - Restores explicit-forall round-trip baseline without regressing BUG-004/BUG-002 targeted anchors.
  - Preserves strict context behavior (`contextToNodeBound` still does not descend via forall-body fallback).
- Verification snapshot (2026-02-16):
  - PASS:
    - explicit-forall round-trip baseline
    - `BUG-004`
    - `BUG-002-V4`
    - strict target matrix
    - `contextToNodeBound does not descend through forall body fallback`
  - At that checkpoint, full gate remained red in separate buckets (`cabal build all && cabal test`: `672 examples, 4 failures`).

### 2026-02-16 BUG-2026-02-14-003 source-domain `I(r)` contract (surgical Omega/Translate)

- `MLF.Elab.Phi.Omega` now enforces `OpRaise` admissibility against trace-domain `I(r)` directly:
  - `etInterior` is consumed as-is (no canonical/copy-map remap in membership checks).
  - If an `OpRaise` source target is absent from `etInterior` but present only via a copy-map alias, Φ now raises a contract-level `PhiInvariantError` (identity-domain mismatch) instead of silently treating alias-domain membership as valid.
- `OpRaise` semantic execution now adopts the copied target when `etCopyMap` provides a source→copied mapping, while keeping the admissibility check in source-ID space. This preserves the source-domain contract and avoids over-specialization regressions on BUG-004 call-site annotation paths.
- `MLF.Elab.Phi.Translate` keeps trace semantics unchanged, but canonicalizes `etInterior` keys only for `namedSet` intersection because `namedSet0` is canonical-node keyed.
- New regressions:
  - `test/ElaborationSpec.hs`: `OpRaise accepts source-domain interior membership even when etCopyMap aliases the target`.
  - `test/PipelineSpec.hs`: `BUG-002-V4 keeps OpRaise targets inside etInterior after witness/trace canonicalization`.
- Verification snapshot (2026-02-16):
  - Targeted anchors pass: `BUG-002-V4`, `BUG-2026-02-06-002 strict target matrix`, `BUG-004`, copy-map anchor, canonicalizer contract, and both new regressions.
  - At that checkpoint, full gate still reported unrelated buckets (`cabal build all && cabal test`: `672 examples, 9 failures`).

### 2026-02-16 BUG-2026-02-16-003 (`id id`) instantiation over-specialization fix

- Root cause (Phase 1 evidence):
  - In `MLF.Elab.Elaborate` (`AAppF`), `argInstFromFun` inferred an instantiation argument from function parameter type correctly, but then applied `inlineBoundVarsType` to the inferred argument list.
  - On `let id = \\x. x in id id`, this rewrote the inferred meta-var argument (`t18`) into a concrete arrow bound (`t14 -> t14`), over-specializing the argument-side `id` and triggering `TCArgumentMismatch`.
- Fix:
  - Keep inferred arguments unchanged in `argInstFromFun` (`instSeqApps args`), removing bound-variable inlining at this point.
  - No change to `inferInstAppArgs`, witness translation, or strict checker rules.
- Impact:
  - Restores let-polymorphic dual-instantiation behavior for the `id id` class while preserving previously fixed strict-target BUG-002 anchors.
- Verification snapshot (2026-02-16):
  - PASS:
    - `/Pipeline (Phases 1-5)/redirected let-use sites keep polymorphic schemes/`
    - `/Pipeline (Phases 1-5)/Checked-authoritative invariant/runPipelineElab type matches typeCheck(term) and checked pipeline type/`
    - `/Phase 6 — Elaborate (xMLF)/Polymorphism and Generalization/elaborates dual instantiation in application/`
    - `id id should have type`
    - `BUG-002-V2`, `BUG-002-V4`, strict target matrix, and `BUG-002-V4` OpRaise interior canonicalization gate
  - At that checkpoint, full gate remained red in separate buckets (`cabal build all && cabal test`: `672 examples, 5 failures`).

### 2026-02-17 BUG-2026-02-16-001/002 planner scheme-owner fallback (targeted closure)

- Context:
  - `EdgePlan` carries `eprSchemeOwnerGen`, resolved in planner.
  - planner classification tests for let/ann flags used synthesized wrappers (`ExpVarId < 0`) with sparse bind-parent maps.
- Root cause:
  - `MLF.Constraint.Presolution.EdgeProcessing.Planner.planEdge` resolved scheme owner strictly from TyExp body root.
  - For synthesized-wrapper topology, wrapper root can be in gen scope while body root path has no direct `GenRef`; strict body lookup threw `InternalError "scheme introducer not found ..."`.
- Implemented fix:
  - Added `resolveSchemeOwnerGen` in planner:
    - non-synth TyExp path remains strict body-root lookup (`findSchemeIntroducerM`),
    - synth-wrapper path does body-first lookup with wrapper-root fallback (`firstGenOnPath` + `bindingPathToRootUnderM`).
  - Strengthened `test/Presolution/EdgePlannerSpec.hs` repros to assert both flag threading and concrete scheme-owner resolution (`GenNodeId 0`).
- Verification snapshot (2026-02-17):
  - PASS `threads let-edge flag into allowTrivial` (seed `1481579064`)
  - PASS `threads ann-edge flag into suppressWeaken` (seed `1481579064`)
  - PASS `Edge plan types` matcher (`7 examples, 0 failures`)
  - PASS `Edge interpreter` matcher (`4 examples, 0 failures`)

### 2026-02-12 BUG-004-V2/V4 strict InstBot production fix (thesis-exact)

- Strict `InstBot` checker semantics are unchanged: `instBot` in `TypeCheck.hs` still requires the input type to be `TBottom`. This matches the paper's `⊥ ← τ` rule exactly.
- Only instantiation *production* was corrected in three places:
  1. `Omega.hs`: bare `InstBot argTy` was produced when `ty == TBottom || alphaEqType ty argTy`; tightened to `alphaEqType ty TBottom` so bare `InstBot` is only emitted when the input is actually `⊥`.
  2. `Elaborate.hs` ALamF: `generalizeAtNode` wraps monomorphic annotations in trivially bounded foralls (`∀(a:B).a`); these are now collapsed to the bound type `B` before use as lambda parameter types.
  3. `Elaborate.hs` AAppF: when an annotation has already updated a forall's bound from `⊥` to `τ`, the inferred argument instantiation is normalized to `InstElim` (which substitutes the bound without calling `instBot`) instead of `InstApp` (which would call `instBot` on the now-non-⊥ bound).
- The `InstInside(InstBot(t))` pattern (used by `instInsideFromArgsWithBounds` for unbounded binders) remains correct: `InstInside` enters the forall, then `InstBot` operates on the bound which IS `⊥`.
- Verification: `652 examples, 0 failures` including 3 new strict InstBot regression tests.

### 2026-02-11 EdgePlan cleanup (remove `EdgeStage`)

- `MLF.Constraint.Presolution.EdgeProcessing.Plan` now exposes a concrete resolved `EdgePlan` record.
  - Removed the single-constructor stage index (`EdgeStage`) and the `edgePlanStage` helper.
- `planEdge` and interpreter entrypoints now use `EdgePlan` directly (no phantom stage parameter).
- `EdgePlannerSpec` now checks concrete plan fields instead of a stage-tag assertion.
- Rationale: the stage index had no real transition boundary in production code (only `StageResolved`), so removing it tightens abstraction without semantic impact.
- Verification:
  - `cabal build mlf2-test` => pass.
  - `cabal build all && cabal test` => 631 examples, 0 failures.

### 2026-02-11 Phase 6 unified execution (wrapper-bridge removal)

- `MLF.Constraint.Presolution.EdgeProcessing.Interpreter` now runs one expansion-oriented execution function for all TyExp-left plans.
  - The prior separate synthesized-wrapper bridge function was removed.
- Wrapper semantics are preserved in the unified path:
  - synthesized wrappers still force `ExpIdentity` for their `ExpVarId`;
  - wrapper body/target instantiation pairs still use direct instantiation solving (`solveNonExpInstantiation`).
- Added characterization regression in `EdgeInterpreterSpec` for synthesized wrapper + forall target, asserting identity expansion assignment retention.
- Verification:
  - `Edge interpreter` matcher: 4 examples, 0 failures.
  - `Phase 3 atomic wrapping equivalence gates`: 7 examples, 0 failures.
  - Full gate remains green after bridge removal (`cabal build all && cabal test` => 631 examples, 0 failures).

### 2026-02-11 Phase 5 abstraction polish (type-level invariants + ID boundary)

- Resolved edge-plan payload now carries a refined `ResolvedTyExp` value; `eprMode`/`EdgePlanMode` were removed.
  - Effect: resolved plans encode TyExp-left shape directly instead of carrying a redundant runtime mode tag.
- Planner fail-fast is now structured, not stringly:
  - Added `ExpectedTyExpLeftInPlanner EdgeId TyNode` in `PresolutionError`.
  - Planner emits `PlanError (ExpectedTyExpLeftInPlanner edgeId leftNode)` for invariant violations.
- Synthesized wrapper `ExpVarId` allocation/checks are centralized in `MLF.Constraint.Types.SynthesizedExpVar`:
  - `initSynthExpVarSupply`, `takeSynthExpVar`, and `isSynthesizedExpVar`.
  - `Normalize` and interpreter now share this boundary instead of ad hoc negative-ID helpers.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge plan types"'` => 7 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge interpreter"'` => 3 examples, 0 failures.
  - Full gate remains green after polish changes.

### 2026-02-11 Phase 4 error-tag + regression-matrix completion

- Presolution phase boundaries now expose explicit error context:
  - `PlanError` wraps planner-surface failures (e.g. non-`TyExp` edge invariant).
  - `ExecError` wraps interpreter/runtime failures while preserving inner payloads.
- Added Phase 4 regression-matrix checks across presolution + pipeline suites:
  - expansion constructor coverage (identity / instantiate / forall-intro / compose),
  - identity trace-shape assertion,
  - compose witness-step shape assertion,
  - annotation-edge weaken suppression with preserved expansion assignments.
- One pre-existing occurs-check assertion was widened to accept wrapped errors (`PlanError`/`ExecError`) without changing semantic expectation.
- Verification: full suite is green after Phase 4 (`cabal build all && cabal test` => 630 examples, 0 failures).

## Summary of Changes

**Current status:** The pipeline records presolution witnesses and produces explicit generalization plans in `MLF.Constraint.Presolution.Plan`; elaboration applies these plans via `MLF.Elab.Generalize` without re-solving. Semantic paper-faithfulness deltas tracked in `.kiro/specs/paper-faithfulness-remaining-deltas/` are closed; remaining open work is non-semantic (proof/formalization and assurance breadth).

### 2026-02-11 Phase 3 wrapping equivalence recovery

- Normalization now stamps synthesized wrapper `TyExp` nodes with reserved negative `ExpVarId`s.
  - Rationale: preserve strict paper-shaped `TyExp <= τ` residual-edge invariant while retaining an unambiguous wrapper discriminator.
- Edge interpreter now dispatches synthesized-wrapper behavior by `ExpVarId < 0`, not TyExp body shape.
  - This prevents frontend TyExp edges from being misclassified as wrappers, restoring expansion-bearing semantics on real TyExp paths.
- Φ binder reorder now uses full order-key fallback when narrowed binder-key maps are incomplete.
  - Rationale: avoid false invariant failures (`PhiReorder: missing order key ...`) observed only under wrapped normalization shape, while keeping deterministic ordering via existing order-key comparison.
- Verification: Phase 3 equivalence gate suite (7/7) and full validation (`cabal build all && cabal test`, 626 examples) are green.

### 2026-02-08 A7 Group 1 binding-core shared-helper consolidation (docs sync)

- [x] Removed duplicated binding-path traversal helpers; canonical module is `MLF.Binding.Path` (`bindingPathToRootWithLookup`, `bindingPathToRoot`, `bindingPathToRootLocal`, `firstGenAncestorFromPath`).
- [x] Removed duplicated node-ref enumeration/existence helpers; canonical module is `MLF.Binding.NodeRefs` (`allNodeRefs`, `nodeRefExists`).
- [x] Removed duplicated scope-graph helper logic; canonical module is `MLF.Binding.ScopeGraph` (`buildTypeEdgesFrom`, `buildScopeNodesFromPaths`, `rootsForScope`).
- [x] Removed duplicated bound-child collection loops; canonical module is `MLF.Binding.Children` (`collectBoundChildrenWithFlag`, `collectBoundChildren`).
- Migration landing points:
  - `MLF.Binding.Queries`, `MLF.Binding.Validation`, `MLF.Binding.Tree`, and `MLF.Binding.Canonicalization` now import the canonical helper modules.
  - `MLF.Constraint.BindingUtil.firstGenAncestorFrom` now delegates to `MLF.Binding.Path.firstGenAncestorFromPath`.
  - `MLF.Constraint.Presolution.Base.bindingPathToRootUnderM` now delegates to `MLF.Binding.Path.bindingPathToRootLocal` after quotient bind-parent canonicalization.
- Behavioral impact: none intended; this was an abstraction-only consolidation.

### 2026-02-09 H15 lambda-parameter source guard (implemented)

- Context:
  - After H13+H14, the `make` reproducer still failed in Phase 7 with a naming mismatch (`t23` vs `b`) even though let-scheme generalization was already correct (`forall a b. a -> b -> a`).
- Root cause:
  - In `MLF.Elab.Elaborate` (`ALam` case), unannotated lambdas could source parameter type reification from `resolvedLambdaParamNode lamNodeId` (copy-derived solved nodes) rather than lexical `paramNode`.
  - In the failing path this produced `ELam "y" (TVar "t23") ...`, while the let scheme stayed `... (TVar "b") ...`, causing `TCLetTypeMismatch`.
- Implemented fix:
  - Added `hasInformativeVarBound` and guarded param-source selection:
    - annotated-lambda desugaring keeps resolved-node behavior;
    - unannotated lambdas use resolved node only when its bound-chain reaches a non-`TyVar` bound (informative structural/base bound);
    - otherwise fall back to lexical `paramNode`.
  - This avoids solved-node-name leakage while preserving prior behavior for application typing paths that require resolved informative bounds.
- Regression coverage:
  - Added `PipelineSpec` test:
    - `does not leak solved-node names in make let mismatch`.
- Verification:
  - `cabal test mlf2-test --test-options='--match "does not leak solved-node names in make let mismatch"' --test-show-details=direct`
  - `cabal test mlf2-test --test-options='--match "runPipelineElab type matches typeCheck(term) and checked pipeline type"' --test-show-details=direct`
  - `cabal build all && cabal test`

### 2026-02-08 A7 group 2 dedup checklist

- [x] Frontend translate scope/parent wiring now routes through local helpers (`withScopedBuild`, `attachUnder`, `rebindScopeRoot`) across let/coercion/forall-internalization paths.
- [x] Elab run annotation node rewriting now routes through shared `mapAnnNodes`, reused by `applyRedirectsToAnn`, `canonicalizeAnn`, and debug edge-origin traversal.
- Result: duplicated control-flow wiring was collapsed into shared local helpers without changing behavior.

### 2026-02-18 A7 non-binding dedup closure (test harness)

- Added shared pipeline-stage helpers in `test/SpecUtil.hs`:
  - `runConstraintDefault`
  - `runToPresolutionWithAnnDefault`
  - `runPipelineArtifactsDefault` (`PipelineArtifacts` record for normalized constraint + presolution + solved + annotation + root).
- Migrated remaining non-binding harness duplication to shared helpers:
  - `test/PipelineSpec.hs`: replaced local pipeline setup chains and removed local `runPipelineWithPresolution`.
  - `test/ElaborationSpec.hs`: removed local `unsafeNormalize`/`generateConstraintsDefault`; moved binding-coverage + Φ-soundness setup to `runPipelineArtifactsDefault` and `runToPresolutionWithAnnDefault`.
  - `test/ConstraintGenSpec.hs`: default graph inference now reuses shared `unsafeNormalizeExpr`.
- Behavioral impact: none intended; this is consolidation-only refactoring to keep the solve chain single-sourced for A7 acceptance criteria.

### 2026-02-06 strict checked-authoritative follow-up

- `runPipelineElab` now uses checked type authority end-to-end while keeping reconstruction paths for diagnostics only.
- Top-level closure now falls back to explicit free-variable closure when root generalization yields no binders but the elaborated term is still type-open.
- Shared closure (`MLF.Elab.TermClosure`) now freshens scheme binders against existing `ETyAbs` names and rewrites free type-variable occurrences in term types/instantiations to avoid capture/regressions.
- Annotation elaboration aligns `InstInside (InstBot ...)` with the generalized annotation-bound head when available, reducing bound-erasure in explicit-forall annotation paths.
- Regression expectations in `test/ElaborationSpec.hs` were updated for checked-authoritative term/type shapes (top-level `ETyAbs` wrappers, `Bool`-authoritative result, and closed `∀a. a -> a` fallback for `\\y. let id = ... in id y`).
- Historical note: bounded aliasing requiring thesis Merge/RaiseMerge witness translation was still unresolved at this checkpoint.
- Root-cause clarification at that time: the gap was not pipeline order (desugaring before presolution remained correct), but alias-bound information being erased on a coercion path before edge-local RaiseMerge gating.
- This gap is now resolved by the 2026-02-08 staged-normalization + structural-gating implementation (see `BUG-2026-02-06-003` in `Bugs.md`).

### 2026-02-07 syntax frontend + canonical pretty migration

- Added eMLF parser/pretty modules:
  - `src/MLF/Frontend/Parse.hs`
  - `src/MLF/Frontend/Pretty.hs`
- Added paper-faithful xMLF syntax/parser/pretty modules:
  - `src/MLF/XMLF/Syntax.hs`
  - `src/MLF/XMLF/Parse.hs`
  - `src/MLF/XMLF/Pretty.hs`
- Added public xMLF API module: `src-public/MLF/XMLF.hs`.
- Extended `MLF.API` with explicit eMLF parse/pretty entry points (`parseRawEmlfExpr`, `parseRawEmlfType`, `parseNormEmlfExpr`, `parseNormEmlfType`, `prettyEmlfExpr`, `prettyEmlfType`) and parse error rendering helpers.
- Added canonical syntax spec document: `docs/syntax.md` (legacy output, canonical target grammar, migration deltas, normalization rules, and implementation extensions).
- Migrated `MLF.Elab.Types` pretty-printing to syntax-driven rendering through `MLF.XMLF.Pretty`/`MLF.XMLF.Syntax` conversion helpers:
  - canonical xMLF computation forms are now printed (`ε`, `⊲σ`, `α⊳`, explicit `∀(⩾ ϕ)`/`∀(α ⩾) ϕ`, and derived `InstApp` as `∀(⩾ ⊲σ); N`);
  - unbounded binders are printed with explicit bottom bounds (`⩾ ⊥`);
  - term/type binder syntax now follows canonical parenthesized forms (`λ(x : σ)`, `Λ(α ⩾ σ)`).
- Added parser/pretty coverage tests:
  - `test/FrontendParseSpec.hs`
  - `test/FrontendPrettySpec.hs`
  - `test/XMLFParseSpec.hs`
  - `test/XMLFPrettySpec.hs`
- Updated existing elaboration pretty-output expectations in `test/ElaborationSpec.hs` to canonical syntax forms.

### 2026-02-08 solved-order shadow cutover semantics

- Generalize now treats solved-order as the solved-authoritative output order for reification/quantifier emission.
- After the 5/5 green gate, runtime fallback in `MLF.Elab.Generalize` no longer reifies or compares base-path shadow output.
- Solved-order output is authoritative in runtime generalization fallback (no runtime base-shadow compare).
- Shadow comparator helpers (`shadowCompareTypes`, `selectSolvedOrderWithShadow`) remain available for focused unit tests/debugging.

### 2026-02-08 staged frontend normalization + structural RaiseMerge gating (implemented)

- Implemented staged frontend boundaries:
  - Frontend types are now one indexed family: `SrcTy (n :: SrcNorm) (v :: SrcTopVar)`.
  - Backward-compatible aliases remain: `SrcType`, `NormSrcType`, `StructBound`, `RawSrcType`.
  - Forall bounds use `SrcBound n`; normalized bounds unwrap to `StructBound` via `unNormBound`.
- Implemented explicit normalization boundary:
  - `MLF.Frontend.Normalize` provides `normalizeType`/`normalizeExpr` with capture-avoiding alias inlining and explicit typed errors (`SelfBoundVariable`, `NonStructuralBoundInStructContext`) instead of runtime crashes.
  - Parser API has explicit raw and normalized entrypoints only (`parseRaw*`, `parseNorm*`); legacy compatibility aliases were removed for clean-break alignment.
- Implemented normalized-only compiler contracts:
  - `desugarSurface`, `generateConstraints`, and pipeline graph/elaboration entrypoints accept normalized expressions only.
- Implemented structural RaiseMerge gating:
  - `shouldRecordRaiseMerge` now uses only live canonical bound queries, binding-tree ancestry, edge-interior membership, same-root exclusion, and elimination state.
  - Precomputed binder-bound snapshots (`eusBinderBounds`) were removed from edge-unify state.
- Bounded aliasing baseline is restored end-to-end:
  - `runPipelineElab` and `runPipelineElabChecked` now both elaborate the bounded aliasing baseline to a type alpha-equivalent to `∀a. a -> a -> a`.
  - Regression test anchor: `test/ElaborationSpec.hs` case `bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines`.
- Tracking:
  - Ralph task: `tasks/todo/2026-02-08-staged-src-types-structural-raise-merge/prd.json`
  - Related bug: `BUG-2026-02-06-003` (resolved in `Bugs.md`)

### 2026-02-08 strict SrcTy indexed model + staged pretty (implemented)

- Consolidated split frontend type declarations into one indexed AST in `MLF.Frontend.Syntax`:
  - `SrcNorm = RawN | NormN`
  - `SrcTopVar = TopVarAllowed | TopVarDisallowed`
  - `SrcTy` constructors (`STVar`, `STArrow`, `STBase`, `STCon`, `STForall`, `STBottom`) shared across raw/normalized paths.
- Added `SrcBound` wrappers and helpers (`mkSrcBound`, `mkNormBound`, `unNormBound`) so normalized forall bounds remain structurally rooted by type.
- Parser/normalizer/constraintgen internals now consume alias-aware wrappers instead of separate concrete `NST*`/`SB*` node declarations.
- Pretty printing is now staged/generic:
  - `prettyEmlfType :: SrcTy n v -> String`
  - `prettyEmlfExpr :: Expr 'Surface (SrcTy n v) -> String`
  while preserving canonical output syntax.
- Regression anchors:
  - `test/ElaborationSpec.hs` — `SrcTy indexed aliases compile shape`
  - `test/FrontendParseSpec.hs` — `parses raw forall binder and keeps raw alias type`
  - `test/FrontendPrettySpec.hs` — `pretty-prints normalized staged types`
  - `test/ConstraintGenSpec.hs` — `internalizes normalized forall bounds using indexed StructBound alias`

### 2026-03-07 thesis-exact recursion-refactor verifier sweep

- Fresh verifier sweep outcome: rows 1–4 and 6 in `docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md` are thesis exact against the live codebase and the thesis references named there; rows 5, 7, and 8 remain open evidence/guardrail gaps.
- Row 1 `Surface Preprocessing Exactness`:
  - `MLF.Frontend.Normalize` remains the alias-bound normalization boundary; capture-avoiding substitution and alpha-renaming keep it binder-sensitive, so this module stays explicit rather than a blind recursion-schemes target.
  - `MLF.Frontend.Desugar.desugarSurface` is the tree-only preprocessing rewrite boundary.
  - `MLF.Frontend.ConstraintGen.Translate` remains the semantic boundary where annotations become explicit coercion/graphic constraints; typed lets stay coercion-only sugar (`ELet x (EAnn rhs σ) body`), not declared-scheme syntax.
- Row 2 `Leftmost-Lowermost Quantifier Ordering`:
  - `sigmaReorder` plus the O15 reorder guard cases are the authoritative freeze points for `§15.2.4` / `§15.3.4` binder-order semantics.
- Row 3 `Let-Scope Translation Discipline`:
  - The live parser/frontend/constraint path preserves the revised let-scope translation from `§15.2.6`; typed-let syntax remains sugar for an annotated RHS and does not reintroduce declared-scheme behavior.
- Row 4 `Translatable Presolution Boundary`:
  - Validation/rigidification is an explicit graph/presolution guardrail, not a recursion-schemes target.
- Row 5 `Typing Environment Construction`:
  - Scope and environment helpers stay under fail-fast binding-tree error propagation, and row5 now has direct production-path anchors for `Definition 15.3.6` / `Property 15.3.7` in `docs/thesis-obligations.yaml` plus live-path elaboration regressions in `test/ElaborationSpec.hs`.
- Row 6 `Computation Context Construction`:
  - `Phi.Context` / `Phi.Omega` remain explicit context-search logic; future cleanup must preserve context-find/reject behavior and binder/order-sensitive insertion points.
- Row 7 `Binder-Safe Tree Recursion Coverage` per-traversal audit:
  - Exhaustive active-campaign traversal inventory:
    - `src/MLF/Frontend/Desugar.hs` — `safe fold`: pure tree rewrite over normalized surface syntax; candidate for `cata`-style cleanup only.
    - `src/MLF/Frontend/Normalize.hs` — `keep explicit`: alias inlining uses capture-avoiding substitution and alpha-renaming, so binder/capture semantics remain the primary concern.
    - `src/MLF/Elab/TermClosure.hs` — `already recursion-schemes-backed`: retain as the positive reference example for genuinely tree-shaped elaboration helpers.
    - `src/MLF/Elab/Reduce.hs` — `already recursion-schemes-backed`: keep as a second positive reference example for tree-shaped term/type substitution.
    - `src/MLF/Elab/Elaborate.hs` — `keep explicit`: environment threading and subterm-specific elaboration rules are semantic control flow, not a blanket fold target.
    - `src/MLF/Constraint/Presolution/WitnessCanon.hs` — `graph-boundary`: localized folds are acceptable internally, but witness normalization remains graph-sensitive overall.
    - `src/MLF/Constraint/Presolution/Driver.hs` — `graph-boundary`: presolution scheduling/finalization is not a recursion-schemes simplification target.
    - `src/MLF/Constraint/Presolution/Validation.hs` — `graph-boundary`: translatability validation/rigidification is an explicit thesis guardrail.
    - `src/MLF/Reify/Core.hs` — `graph-boundary`: reification follows graph-aware naming/boundary rules and is outside broad tree-fold refactors.
  - Inventory rule: future recursion-refactor work may touch only entries classified `safe fold` or `already recursion-schemes-backed` unless a new verifier-owned thesis audit explicitly reclassifies a module.
- Row 8 `Graph-Phase Explicitness Guardrail`:
  - Explicit negative guardrail: broad recursion-schemes rewrites are a non-goal for graph-sensitive phases unless a later row-specific verifier pass says otherwise.
  - Guardrail-owned modules for this campaign are: `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Constraint/Presolution/Validation.hs`, `src/MLF/Constraint/Presolution/WitnessCanon.hs`, `src/MLF/Reify/Core.hs`, and their graph-aware companions.
  - Allowed local changes in those modules are limited to thesis-preserving helper extraction, documentation, or tests; proposals to replace the explicit graph algorithms themselves require new row-specific thesis evidence first.

### 2026-02-08 Phase 6 crash hardening (BUG-2026-02-06-001)

- Before the solved-order cutover, `MLF.Elab.Generalize.reifyWithGaBase` validated `solvedToBasePref` targets before any base-constraint reification.
- After the cutover gate passed, runtime elaboration no longer depends on `reifyWithGaBase`; fallback now reifies from solved-order roots/substitutions.
- The nested let + annotated-lambda reproducer remains covered by `test/ElaborationSpec.hs` and no longer crashes in Phase 6.
- The follow-up Phase 7 mismatch path (`BUG-2026-02-08-004`) is now resolved (2026-02-10) with thesis-green checked/unchecked `Int` behavior.

## Module Structure (Post-Refactor)

The codebase has been refactored for improved navigation and paper-faithfulness auditing:

### Graph Types (`MLF.Constraint.Types.Graph`)

The monolithic `Graph` module has been split into focused submodules:

| Submodule | Contents |
|-----------|----------|
| `Graph.NodeEdge` | Core node and edge definitions (`NodeId`, `TyNode`, `InstEdge`, `UnifyEdge`, etc.) |
| `Graph.Binding` | Binding-related types (`BindFlag`, `BindParents`, `BindingError`) |
| `Graph.Accessors` | Accessor utilities (`maxNodeIdKeyOr0`) |

`MLF.Constraint.Types.Graph` re-exports all submodules as a facade.

### Presolution (`MLF.Constraint.Presolution`)

Presolution modules now use shared state-access helpers:

| Module | Purpose |
|--------|---------|
| `StateAccess` / `Ops` | Shared `MonadPresolution` accessors (`getConstraint`, `modifyConstraint`, `liftBindingError`) |
| `EdgeProcessing` | Edge-local logic with explicit `EdgeCtx` |
| `EdgeProcessing.Witness` | Witness construction helpers |
| `EdgeProcessing.Unify` | Edge-local unification |

### Unification (`MLF.Constraint.Unify`)

Shared unification core for consistent behavior across phases:

| Module | Purpose |
|--------|---------|
| `Unify.Core` | Policy-driven unification with `UnifyStrategy` |
| `Unify.Decompose` | Structural decomposition helpers |

### Elaboration (`MLF.Elab`)

Elaboration now uses structured config records:

| Record | Purpose |
|--------|---------|
| `ElabConfig` | Static configuration (debug flags, etc.) |
| `ElabEnv` | Per-elaboration environment (naming, etc.) |

Legacy code is isolated in `MLF.Elab.Legacy` (e.g., `expansionToInst`).

### Documentation

- `docs/paper-map.md` — Paper-to-code mapping for auditing
- `docs/phase-notes.md` — Phase invariants and test references

### 1. src/MLF/Constraint/Presolution/Driver.hs (+ EdgeUnify/Witness)
- **`unifyStructure` / `unifyStructureEdge`**: Recursively unify structural children (TyArrow, TyForall, plus TyVar bounds) so `Arrow A B ~ Arrow C D` propagates `A~C` and `B~D` (Driver for global merges; EdgeUnify for edge-local χe execution).
- **`processInstEdge`**:
  - Uses `unifyStructure`/`unifyStructureEdge` instead of raw `unifyAcyclic`.
  - Eagerly materializes non-Identity expansions (`applyExpansionEdgeTraced`), binds the expansion root like the target, and unifies the expansion result with the target (plus the original TyExp wrapper).
  - Guards against `Identity` expansion cycles by skipping `TyExp ~ Target` unification when expansion is `Identity` (relying on `decideMinimalExpansion` unifications instead).
- **Per-edge instance witnesses (`Φ` input) + traces**:
  - Presolution records `EdgeWitness` + `EdgeTrace` per instantiation edge (`psEdgeWitnesses` / `psEdgeTraces`, surfaced as `prEdgeWitnesses` / `prEdgeTraces`).
  - Witnesses combine expansion-derived steps (`witnessFromExpansion`) with edge-local unification ops from `EdgeUnify` (Raise/Merge/Weaken).
  - `ExpForall` yields `StepIntro` entries (xMLF quantifier-introduction `O`) in `ewSteps`, not Ω ops; `ExpInstantiate` yields per-binder Ω ops (`OpGraft`/`OpWeaken`/`OpMerge`).
  - Witness steps are normalized in `normalizeEdgeWitnessesM` via `normalizeInstanceStepsFull` (coalesces Raise+Merge into RaiseMerge, enforces “Weaken-last” ordering, avoids double elimination).
  - `ExpInstantiate` witness/application logic skips “vacuous” `TyForall` wrappers (quantifier levels with no binders) so `Φ` construction doesn’t fail on nested/structural ∀ nodes.
  - `ExpInstantiate` witnesses avoid invalid grafts under non-⊥ bounds: if a binder has an instance bound that is another in-scope variable (e.g. `b ⩾ a`), presolution emits `OpMerge(b, a)` rather than `OpGraft` (paper Fig. 10 “alias + eliminate”).
    - Current behavior: RaiseMerge recording uses live structural graph facts (`shouldRecordRaiseMerge`) rather than alias-metadata survivability; this closed `BUG-2026-02-06-003`.
  - When an expansion includes a later `ExpForall`, `ExpInstantiate` witnesses suppress `OpWeaken` so binder metas stay flexible until the new quantifier is introduced (avoids empty Q(n) and lost ∀ in bounded-aliasing cases).
  - Edge-local unification can record `OpRaiseMerge(b, m)` when unification forces a **bounded** binder’s instantiation meta to unify with a `TyVar` bound **above the instantiation-edge root** in the binding tree (recorded as `OpRaise` + `OpMerge`, then normalized to `OpRaiseMerge`), matching the paper’s “escape to bound-above node” shape.
    - Implemented behavior: this emission path is no longer gated by edge-local `binderBounds`; it queries live canonical bounds and structural ancestry/interior predicates directly.
- **Scope tracking (paper `Raise` as graph transformation)**:
  - TyVar/TyVar unions harmonize binding parents by executing the paper `Raise(n)` graph operation as a binding-edge rewrite on `Constraint.cBindParents` (`MLF.Binding.Adjustment` / `MLF.Binding.GraphOps`).
  - During instantiation-edge solving (χe), the same per-step raises are also recorded as `OpRaise` in the edge witness Ω (`unifyAcyclicRawWithRaiseTracePrefer` → `unifyAcyclicEdge` / `unifyAcyclicEdgeNoMerge`), aligning with `papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.4 / Fig. 10).
  - Variable bounds and eliminations are stored in `Constraint.cVarBounds` / `Constraint.cEliminatedVars` (`MLF.Constraint.VarStore`) and are looked up by canonical `NodeId`, so they stay consistent as binding edges and UF representatives change.
- **`materializeExpansions`**: Avoids duplicating fresh nodes by reusing the already-unified expansion result for non-Identity expansions; Identity expansions still rewrite `TyExp` wrappers to their bodies.
- **`rewriteConstraint`**: Ensures Identity `TyExp` wrappers are erased even when they are not the Union-Find root (redirecting the whole UF class to the wrapper’s body). This fixes over-generalization bugs in paper-alignment baselines like `let id = (\x. x) in id id` and `\y. let id = (\x. x) in id y`.

### 2. src/MLF/Constraint/Normalize.hs
- **`applyUnionFindToConstraint`**: Enhanced to perform "grafting". When a `TyVar` node is unified with a structural node (e.g., `TyBase`), the `TyVar` node in the graph is destructively updated to become a copy of that structure. This ensures that external references to the variable (like the expression root) see the inferred structure.
- **Binding-edge Raise harmonization**: Var-var merging harmonizes `Constraint.cBindParents` (paper `Raise(n)`) before unioning, keeping scope stable regardless of UF representative choice.

### 3. src/MLF/Constraint/Solve.hs
- **Binding-edge Raise harmonization**: Phase 5 harmonizes `Constraint.cBindParents` (paper `Raise(n)`) before unioning, keeping scope stable regardless of UF representative choice.
- **Elimination rewrite**: `solveUnify` now rewrites eliminated binders into their bounds (or explicit `TyBottom` nodes), removes them from the graph, and clears `cEliminatedVars` before elaboration.
  - The solve-time union-find map is extended with the elimination substitution so witness ops that mention eliminated ids still canonicalize to live nodes.

### 4. src/MLF/Elab/Generalize.hs + src/MLF/Elab/Generalize/* + src/MLF/Elab/Elaborate.hs + src/MLF/Elab/Run.hs (reexported via `MLF.Elab.Pipeline`)
- **Generalize is now an orchestrator**:
  - Phase-oriented logic moved into focused modules: `Generalize/Plan`, `SchemeRoots`, `BinderPlan`, `Ordering`, `ReifyPlan`, `Normalize`, and `Helpers`.
  - The top-level `generalizeAt`/`generalizeAtWith` functions now read as a linear pipeline of plan → binders → ordering → reify → normalize, with local helpers split by concern.
- **`generalizeAt`**:
  - Optimized to handle structural `TyForall` nodes (avoiding double quantification).
  - Returns the `subst` (renaming map) alongside the scheme.
- **Scope follows the solved graph**:
  - Binder discovery is binding-tree driven (`Constraint.cBindParents`): `TyForall` scopes use the body as the ≺ root, while non-Forall scopes use binding-parent paths to the nearest gen ancestor.
  - Presolution rewrite reconstructs binding parents and reattaches unparented nodes to the root gen node, keeping expansion/copy roots in-scope for generalization.
  - `generalizeAt` + `reifyTypeWithNamesNoFallback` rely solely on binding-tree enumeration (no free-variable fallback).
  - Rigid binding edges are treated as inline bounds, and bounds are included in reachability when ordering binders.
  - Elaboration no longer consults `cEliminatedVars`; eliminated binders are already rewritten out of the graph. Vacuous `TyForall` wrappers (no binders) are elided during reification.
- **`substInTerm` / `substInType`**: Implemented in `MLF.Elab.Elaborate` to apply the renaming map from `generalizeAt` to the elaborated term body. This ensures that terms use the same variable names as their type schemes (e.g., `Λa. λx:a. x` instead of `Λa. λx:t0. x`).
  - 2026-02-20: `deriveLambdaBinderSubst` now preserves alternate node-key aliases for the same binder name when lambda arity matches unbounded scheme arity, so elaborated RHS-coercion lets do not lose the `tN -> binder` rewrite needed for step/typeCheck stability.
- **`elaborate`**: Applies substitution to the RHS of let-bindings.
- **Witness translation (`Φ`) + quantifier reordering (`Σ`)**:
  - Elaboration reifies instantiations from recorded per-edge witnesses (`prEdgeWitnesses`) via `phiFromEdgeWitnessWithTrace` (rather than `expansionToInst`), using `EdgeTrace` for copy maps/interiors. Production elaboration requires trace; no-trace entry points are test/debug-only.
  - `Φ` consumes interleaved `ewSteps` (`StepIntro` for `O`, `StepOmega` for Ω); `OpGraft`+`OpWeaken` maps to `InstApp` (⟨τ⟩), `OpGraft` alone maps to an `InstBot` inside the binder, and `OpMerge`/`OpRaise`/`OpRaiseMerge` map to the paper’s alias/raise instantiations (Fig. 10).
- `phiFromEdgeWitnessWithTrace` targets binders using `InstUnder` contexts (`C{·}`) and prefixes Ω-translation with the ≺-based reordering ϕR/Σ(g) when `Typ` vs `Typexp` disagree (thesis Def. 15.3.4); missing non-spine contexts are errors, and normalized ω ops that violate translatability (e.g. `OpRaise` outside `I(r)`, non-transitive-flex `OpRaise` targets, non-binder targets, rigid-only-on-non-operated-endpoint for Merge/RaiseMerge) are rejected rather than silently skipped. Rigid identity behavior follows the literal thesis condition on operated node `n` for Raise/Merge/RaiseMerge.
  - Implemented explicit quantifier reordering instantiations (`sigmaReorder`) using adjacent swaps per `papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.4).
  - Implemented `applyInstantiation` (in `MLF.Elab.Inst`, reexported via `MLF.Elab.Pipeline`) to check/apply xMLF instantiations to xMLF types (see `papers/these-finale-english.txt`; `papers/xmlf.txt` Fig. 3), used by tests to validate that `Φ(e)` transforms the source type into the target type.
- **`expansionToInst`**: Kept as a legacy/debug conversion from `Expansion` to `Instantiation` (no longer the main path for elaboration, and no longer re-exported via `MLF.Elab.Pipeline`).
- **`runPipelineElab`**: Generalizes the top-level result using the nearest gen ancestor of the expression root (root gen node for top-level), keeps reconstruction checks for diagnostics, and reports the type-checker result as the authoritative pipeline type.

## Testing
- **`test/ElaborationSpec.hs`**: Updated expectations to reflect correct polymorphic behavior and variable naming. Added integration tests for polymorphic instantiation.
- **Witness translation tests**: Added focused tests for `Σ(g)` reordering and for `Φ` soundness (`applyInstantiation source Φ(e) == target` for representative instantiation edges).
- **`test/PresolutionSpec.hs`**: Verified that instantiation edges merge nodes correctly.
- **`test/TypeCheckSpec.hs` + `test/ReduceSpec.hs`**: Cover xMLF type-checking and reduction/instantiation semantics.

Note: `test/ElaborationSpec.hs` also contains **paper-alignment baseline tests** that serve as regression coverage while we continue aligning witnesses toward `papers/these-finale-english.txt` (see also `papers/xmlf.txt`, especially around Merge/RaiseMerge and aliasing behavior).

## `papers/these-finale-english.txt` study: thesis ↔ repo mapping (with `papers/xmlf.txt` cross-reference)

This repo’s design is primarily informed by:

- `papers/these-finale-english.txt` (thesis) for **xMLF**’s explicit types/instantiations/terms and the **elaboration** story; see `papers/xmlf.txt` for supplemental xMLF presentation details and figure numbering.
- The earlier “graphic constraints” papers (ICFP’08 / TLDI’07) for the **solver pipeline** that produces presolutions.

### Paper anchors (from `papers/these-finale-english.txt`; `papers/xmlf.txt` figure numbers for reference)

- **Fig. 1–4**: xMLF grammar, instantiation judgments, instantiation-as-a-function on types, and xMLF term typing rules.
- **§3.1–§3.5 + Fig. 7/9/10**: elaboration from (graphical) eMLF presolutions to xMLF:
  - `/)(g) = Λ(Q(g))` (insert type abstractions for flexible bindings at a level)
  - `Φ(e)` (compute instantiation witnesses from solved instantiation edges)
  - `S/Q/T` (map presolution nodes to xMLF types)
  - `Σ(g)` (quantifier reordering when the expansion’s quantifier order differs)

### Mapping: paper notation → repo types/functions

| Paper | Meaning | Repo |
|------:|---------|------|
| `b` | eMLF surface term | `src/MLF/Frontend/Syntax.hs` (`Expr` + indexed `SrcTy` aliases) |
| `χ` | constraint graph | `src/MLF/Constraint/Types.hs` (`Constraint`) |
| `n` | type node in the graph | `NodeId` + `TyNode` in `Constraint.cNodes` |
| `g` | binding-tree node (generalization site) | `GenNodeId`/`GenNode` + `Constraint.cBindParents` |
| `≤` edge | instantiation constraint | `InstEdge` (`Constraint.cInstEdges`) |
| `=` edge | unification constraint | `UnifyEdge` (`Constraint.cUnifyEdges`) |
| `s·τ` | expansion node / expansion variable | `TyExp{ tnExpVar :: ExpVarId }` + `Expansion` recipes in `Presolution` |
| `χp` | (principal) presolution | `MLF.Constraint.Presolution.PresolutionResult` (plus `prEdgeExpansions`) |
| `τ` | xMLF type | `src/MLF/Elab/Types.hs` (`ElabType`) |
| `φ` | xMLF instantiation witness | `src/MLF/Elab/Types.hs` (`Instantiation`) |
| `a` | xMLF term | `src/MLF/Elab/Types.hs` (`ElabTerm`) |

### Mapping: solver + elaboration phases → modules

| Phase | Role (paper) | Repo entry point |
|------:|--------------|------------------|
| 1 | Constraint generation | `MLF.Frontend.ConstraintGen.generateConstraints` |
| 2 | Local simplification (grafting/merging) | `MLF.Constraint.Normalize.normalize` |
| 3 | Acyclicity / dependency ordering | `MLF.Constraint.Acyclicity.checkAcyclicity` |
| 4 | Presolution (minimal expansions) | `MLF.Constraint.Presolution.computePresolution` |
| 5 | Global unification | `MLF.Constraint.Solve.solveUnify` |
| 6 | Elaborate to xMLF | `MLF.Elab.Pipeline.elaborate` / `MLF.Elab.Pipeline.runPipelineElab` |

### Alignment notes / known gaps vs `papers/these-finale-english.txt` (see `papers/xmlf.txt` §3 for numbering)
- **Witness translation (`Φ`)**: `papers/these-finale-english.txt` translates *normalized instance-operation witnesses* into xMLF instantiations (see `papers/xmlf.txt` Fig. 10). This repo records a per-edge `EdgeWitness` during presolution and translates it to an xMLF `Instantiation` via `MLF.Elab.Pipeline.phiFromEdgeWitnessWithTrace` in production paths (`phiFromEdgeWitnessNoTrace` remains test/debug-only).
  - Quantifier-introduction (`O`) is not part of Ω in the thesis (see `papers/xmlf.txt`); the repo records these steps as `StepIntro` entries in `EdgeWitness.ewSteps` (from `ExpForall`) and translates them interleaved with Ω segments when constructing Φ(e).
  - Ω ops emitted today include `OpGraft`+`OpWeaken`, `OpMerge` (bounded aliasing like `b ⩾ a`, plus unification-induced aliasing during instantiation-edge solving), `OpRaise` (paper-general binding-edge raising on arbitrary interior nodes), and `OpRaiseMerge` for bounded-binder “escape” patterns. χe execution is paper-shaped for binding-tree ops: Raise/Weaken are executable binding-edge rewrites, and `EdgeTrace.etInterior` records the exact paper interior `I(r)` for filtering.
    - Bounded-aliasing caveat (`BUG-2026-02-06-003`) is resolved: RaiseMerge gating now uses structural live-graph predicates, and bounded aliasing elaborates to the thesis-aligned baseline in both checked and unchecked pipelines.
  - Φ requires a representable translation context; missing contexts and other non-translatable cases are hard failures. Rigid identity handling is literal for Raise/Merge/RaiseMerge on operated node `n`; rigid only on the non-operated endpoint is rejected as non-translatable.
- **Trace root/interior coherence**: `EdgeTrace` root/interior refresh and normalization share a single root-selection helper (`traceInteriorRootRef`) so `etRoot`, `etInterior`, and witness normalization all use the same interpretation of `r`/`I(r)`.
- **Witness merge-direction strictness**: Ω normalization rejects malformed merge direction (`MergeDirectionInvalid`) in all normalization entrypoints (helper + production); there is no permissive merge-direction fallback.
- **`OpRaise` translatability strictness**: non-rigid `OpRaise` now requires the operated node to be transitively flexibly bound to expansion root `r`; otherwise normalization fails fast with `NotTransitivelyFlexBound` (with direct validator and presolution-path regressions).
- **Fig. 15.3.4 witness matrix closure (2026-02-10)**: witness normalization/emission now has an explicit 15-row closure contract encoded as row-labeled tests (`R-GRAFT-VALID-01`..`R-RAISEMERGE-NORM-15`) across `test/Presolution/WitnessSpec.hs` and `test/Presolution/MergeEmissionSpec.hs`, with matrix gate green via `cabal test mlf2-test --test-show-details=direct --test-options='--match R-'` and full gate green via `cabal build all && cabal test`.
- **Context search strictness**: `contextToNodeBound` follows thesis context grammar (under-quantifier / inside-bound) and does not use non-thesis fallback descent through `TyForall` body.
- **Quantifier reification (binding-tree based)**: `Q(n)`/reification quantifies flexibly bound `TyVar` binders using binding-parent edges (bounds included in reachability), so bounds and contexts remain representable in Φ and generalization.
- **Quantifier reordering (`Σ(g)` / `ϕR`)**: implemented via `MLF.Elab.Sigma` / `MLF.Elab.Pipeline.sigmaReorder` (adjacent swaps per `papers/these-finale-english.txt` Def. 15.3.4 / Fig. 15.3.5; see `papers/xmlf.txt` §3.4). Φ translation (`phiFromEdgeWitnessWithTrace` → `phiWithSchemeOmega`) prefixes Ω-translation with this reordering whenever `Typ(a′)` and `Typexp(a′)` disagree in binder order — even when Ω contains no Raise steps — while still targeting binders for Ω using `InstUnder` instantiation contexts (paper’s `C{·}`). The computation is deterministic and fail-fast: missing <P order keys or bound-dependency cycles produce `InstantiationError` messages prefixed `PhiReorder:` rather than silently returning `InstId`.
- **Application elaboration shape**: now matches Fig. 7 — constraint generation emits instantiation edges for both function and argument, and elaboration wraps each side with `ETyInst` when non-identity.
- **Constraint representation differences**: the thesis's graphical presentation (see also `papers/xmlf.txt`) uses a term-dag plus a binding tree with flexible/rigid edges and node classes (inert/instantiable/restricted/locked). The repo mirrors the same split (`Constraint.cNodes` + `Constraint.cBindParents` with `BindFlex`/`BindRigid`); some paper machinery remains simplified (e.g. witness normalization/ordering is implemented but not yet backed by formal proofs).
- **xMLF Phase 7**: the repo includes type-checking and reduction for xMLF terms/instantiations (`MLF.Elab.TypeCheck`, `MLF.Elab.Reduce`) and uses them in tests, but still lacks a fully formalized/verified connection to the thesis presentation (e.g., proof obligations and full evaluation-context coverage).

## Kiro spec planning
- Paper-faithfulness deltas are captured in `.kiro/specs/paper-faithfulness-remaining-deltas/`, including evidence pointers to the thesis and code, plus a concrete implementation plan.

## 2026-02-10 BUG-2026-02-06-002 staged closure notes

- `MLF.Elab.Phi.Omega` now treats delayed binder-local `OpGraft ... OpWeaken` pairs as a single binder application path when no intervening op touches that binder, and rescues binder-arg `TBottom` reification to binder TVar naming when available.
- `MLF.Elab.Elaborate` let elaboration now computes an env-aware RHS type (`typeCheckWithEnv`) and uses a guarded fallback scheme only when the generalized scheme and RHS-derived generalized scheme are not alpha-equivalent.
- `MLF.Elab.Elaborate` application elaboration extends non-polymorphic-arg repair to `InstApp TForall{}` fun-instantiation payloads, reifying argument type from the argument annotation node.
- Current test evidence:
  - `BUG-2026-02-06-002 strict target matrix`: green (`4/4`).
  - `BUG-2026-02-06-002 thesis target`: green (checked + unchecked).
  - focused guards (make-const generalization, redirected let-use polymorphism, H15 non-leak): green.
  - sentinel matrix has been graduated to strict assertions (no pending cases under `BUG-2026-02-06-002`).


## 2026-02-10 BUG-2026-02-06-002 final closure notes

- Witness normalization now enforces thesis-shape upstream for graft/weaken interactions:
  - canonical ambiguous mapping rejects multiple canonical graft args for one weakened binder,
  - delayed graft/weaken pairs are coalesced safely before Ω translation.
- Ω translation is local again:
  - standalone `OpGraft` no longer performs delayed non-local weaken scan,
  - binder `TBottom` rescue is scoped to adjacent `OpGraft+OpWeaken` only.
- Scheme simplification preserves named structured bounds (`simplifySchemeBindings` blocks structured-bound inline for named binders), preventing Phase 6 dependency/bound erasure regressions.
- ALet fallback now has two scoped branches:
  - existing app/unbounded/Int-codomain path,
  - lambda replacement path with env-aware RHS typing and `subst = IntMap.empty` when replacing the scheme.
- Verification:
  - `BUG-2026-02-06-002 strict target matrix`: PASS (`4/4`)
  - full gate: `cabal build all && cabal test` => PASS (`604 examples, 0 failures`)

## 2026-02-10 BUG-2026-02-08-004 thesis-green closure notes

- Dedicated sentinel in `test/PipelineSpec.hs` was flipped from rejection-shape guarding to thesis-expected success (`Int`) for both `runPipelineElab` and `runPipelineElabChecked`.
- Root-cause seam was in `MLF.Elab.Elaborate` application elaboration:
  - witness-derived `InstApp` could survive onto a function term whose elaborated type was already monomorphic arrow, yielding invalid `InstElim` during type checking;
  - polymorphic-argument repair previously only inferred args from syntactic `ELam`, missing equivalent typed-arrow cases after function-side instantiation.
- Fix in `AApp`:
  - guard `InstApp` by `typeCheckWithEnv` of the function term (`InstApp` kept only for `TForall{}`);
  - extend arg-instantiation inference to variable arguments when the (possibly instantiated) function term typechecks to `TArrow paramTy _`.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004"'` => PASS (`1 example, 0 failures`).
  - `cabal build all && cabal test` => PASS (`604 examples, 0 failures`).

## 2026-02-10 delayed-weakening diagnostics alignment

- Condition (5) (`delayed weakenings`, thesis Definition 11.5.2 in `papers/these-finale-english.txt`) is now surfaced explicitly in witness validation errors:
  - `OmegaNormalizeError` adds `DelayedWeakenViolation weakenedBinder offendingNode`.
- Previous behavior reused `OpUnderRigid` for this case, which conflated two independent failure modes:
  - rigid-path interior failures, and
  - delayed-weaken ordering failures.
- The explicit constructor keeps normalization failure reporting paper-faithful and improves targeted regression assertions in `test/Presolution/WitnessSpec.hs`.

## 2026-02-11 BUG-2026-02-11-003 closure notes

- BUG-004 nested annotation variants (`V2`, `V4`) are now strict-success regressions (`Int`) in both unchecked and checked pipelines.
- V2 closure aligns scheme/finalization identity ownership with Φ reorder requirements:
  - `MLF.Constraint.Presolution.Plan.Finalize` now includes quantified binder names in `usedNames`, preserving binder identity through scheme finalization.
  - `MLF.Elab.Phi.Omega` reorder identity checks now require identity only for scheme-owned quantifier positions.
- Removed non-thesis compatibility paths:
  - `MLF.Elab.Elaborate.reifyInst` no longer synthesizes fallback instantiation sequences from expansion traces when `phi == InstId`; elaboration uses `phiFromEdgeWitnessWithTrace` only.
  - `MLF.Elab.TypeCheck` / `MLF.Elab.Inst` are strict-only for `InstBot` (`InstBotMode`/mode APIs removed).
- Producer-side annotation/elaboration shaping is now explicit:
  - Desugared `ELamAnn` parameter recovery uses coercion-domain form matching only (`∀(v ⩾ b). v` → `b`) instead of broad bounded-identity collapse.
  - In `AApp`, inferred `InstApp τ` is normalized to `InstElim` when the argument term is already `∀(⩾ τ) ...`, avoiding strict-instantiation failure on bounded-forall terms.
- Guardrail from debugging iteration:
  - broad Omega relaxations (empty-binder-key short-circuit, weaken keep-all on empty keep-set, graft skip outside keep-set) were reverted after they regressed legacy make/Φ suites.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "strict"'` => `15 examples, 0 failures`.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V2"'` => `2 examples, 0 failures`.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V4"'` => `2 examples, 0 failures`.
  - `cabal build all && cabal test` => pass.

## 2026-02-16 BUG-003 normalization-side deterministic graft+weaken contract (historical checkpoint; superseded by 2026-02-17 closure)

- Implemented an annotation-edge-only pre-normalization pass in `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs`:
  - scope: Ω-segment local (`StepIntro` boundaries preserved),
  - trigger: ambiguous multi-graft/no-weaken shape,
  - action: synthesize exactly one deterministic `OpGraft+OpWeaken` pair for the replay binder.
- Added explicit fail-fast surface for synthesis dead ends:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessValidation.hs` now includes `DeterministicGraftWeakenSynthesisFailed NodeId [NodeId]`.
- Deterministic chooser provenance:
  - source ordering from `etBinderArgs`,
  - source->replay mapping from normalized replay hints (`etBinderReplayHints` bridge path),
  - arg selection from trace args rewritten into normalization space.
- Added targeted regressions in `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs`:
  - synthesis success on annotation-edge ambiguous shape,
  - fail-fast on missing live candidate args,
  - non-annotation guard (no synthesis).
- Verification outcomes for this pass:
  - targeted synthesis tests: green,
  - strict anchors + BUG-010 matrix reproducer: green,
  - at that checkpoint, `BUG-003-V1/V2` were red (stricter replay key-space mismatch bucket on synthesized `OpGraft+OpWeaken` targeting source key `6`),
  - at that checkpoint, full gate remained red (`677 examples, 33 failures`) in that workspace.

## 2026-02-16 BUG-003 replay-bridge follow-up (historical checkpoint; superseded by 2026-02-17 closure)

- Applied a focused replay-bridge candidate expansion in `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`:
  - `computeTraceBinderReplayBridge` now seeds alias candidates from source binders that share the same replay-hint class (`etBinderReplayHints`) before final replay-map selection.
  - This closes the synthesized-key under-coverage case where BUG-003 edge-0 source key `6` had no replay-map entry despite sharing hint provenance with mapped sources.
- Applied a strict Ω bounded-branch correction in `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`:
  - `OpGraft+OpWeaken(bound-match)` now emits binder elimination (`InstElim`) instead of bounded `InstApp`, avoiding the `InstBot expects ⊥` invariant violation for non-`⊥` bounds.
  - This keeps strict `InstBot` behavior unchanged while aligning bounded graft+weaken semantics with elimination.
- Verification (at that checkpoint):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'`
    - now returns to the baseline strict bucket: `PipelineTypeCheckError (TCLetTypeMismatch ...)` (no replay key-space mismatch, no `InstBot` invariant crash).
  - PASS:
    - `--match "fails fast when OpWeaken targets a trace binder source with no replay binder mapping"`
    - `--match "OpRaise accepts source-domain interior membership even when etCopyMap aliases the target"`
    - `--match "does not require Merge for bounded aliasing (b ⩾ a)"`
    - `--match "bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines"`
    - `--match "make-app keeps codomain Int without bottom-domain collapse"`
    - synthesis regressions in `test/Presolution/WitnessSpec.hs` (3/3)
  - At that checkpoint, full gate remained red: `cabal build all && cabal test` => `677 examples, 33 failures`.
- Root-cause evidence from traced BUG-003 edge-0 replay:
  - replay map now includes source key `6` (`traceBinderReplayMap=[(0,4),(1,8),(2,38),(4,4),(6,4)]`),
  - edge-0 Φ becomes `InstElim` (instead of failing earlier on key-space mismatch),
  - At that checkpoint, elaborated RHS remained bottomized (`∀a. ⊥ -> t1 -> ⊥ -> ⊥`), so BUG-003 strict-success closure remained open in the original semantic bucket.

## 2026-02-16 BUG-003 baseline trace: why bounds are bottomized before Φ

- Additional edge-local tracing isolated the first irreversible drift before Φ translation:
  - during edge `0` presolution execution (`runExpansionUnify`), extra χe ops include:
    - `OpRaise 2 ; OpMerge 2 2`
    - `OpRaise 0 ; OpMerge 0 0`
  - at the same point, copied binder metas become self-bound and eliminated:
    - `35 = TyVar { tnBound = Just 35 }`
    - `37 = TyVar { tnBound = Just 37 }`.
- Copy provenance confirms source->meta mapping for these binders:
  - edge-0 `etCopyMap` includes `(0 -> 35)` and `(2 -> 37)`.
- Presolution/solve state chain:
  - `prConstraint` already contains:
    - edge-0 expansion rewritten as `ExpInstantiate [30,35,32,33,34]` (arg `31 -> 35`),
    - `cEliminatedVars = {35}`,
    - bound-arrow nodes referencing `35` (for example nodes `7`, `10`).
  - `solveUnify` rewrites eliminated self-bound `35` to `TyBottom 46`, yielding bottomized bounds in those arrows (`7 dom=46`, `10 dom/cod=46`).
- Consequence:
  - edge-0 scheme bounds are semantically bottomized in presolution+solve transitions, so Φ replay starts from an already-bottomized graph for BUG-003.

## 2026-02-17 BUG-003 thesis-exact closure: edge self-merge/self-bound guards

- Implemented surgical presolution guards in `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeUnify.hs`:
  - RaiseMerge emission now skips same-UF-class endpoints before recording/writing (`repRoot == extRoot` => no-op).
  - Edge-local bound writes now skip canonical same-root writes in `setVarBoundM` (`findRoot nid == findRoot bnd` => no-op), preventing `n -> n` self-bound artifacts.
- Added focused regression in `/Volumes/src/mlf4/test/ElaborationSpec.hs`:
  - `BUG-003-PRES: edge-0 presolution does not leave self-bound binder metas`.
  - Test inspects edge-0 trace (`etBinderArgs` + `etCopyMap`) and asserts no surviving binder-meta in `prConstraint` is `TyVar { tnBound = Just self }`.
- Verification (sequential, targeted):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-PRES"'` -> PASS
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'` -> PASS (`2 examples, 0 failures`)
  - strict anchors -> PASS:
    - `fails fast when OpWeaken targets a trace binder source with no replay binder mapping`
    - `OpRaise accepts source-domain interior membership even when etCopyMap aliases the target`
    - `does not require Merge for bounded aliasing (b ⩾ a)`
    - `bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines`
- Outcome:
  - BUG-003 V1/V2 now elaborate to the thesis-expected type (`∀a. a -> a -> a -> a`).
  - Presolution no longer leaves edge-0 self-bound binder metas for the BUG-003 shape.

## 2026-02-17 BUG-002 replay-key contract completion

- Replay-key normalization contract is now enforced across all three layers involved in Φ replay:
  - presolution trace/hint restoration (`MLF.Constraint.Presolution.WitnessNorm`),
  - bridge construction (`MLF.Elab.Phi.Translate`),
  - Ω binder target lookup (`MLF.Elab.Phi.Omega`).
- Bridge resolution now prefers replay-binder-domain keys derived from replay scheme metadata and rejects non-binder drift for binder-target ops.
- Reify-time scheme-bound normalization (`MLF.Constraint.Presolution.Plan.ReifyPlan`) now rewrites binder self-references inside bounds to `⊥` before bound admission:
  - removes illegal self-bound forms (`∀(a ⩾ a)`),
  - preserves structural information for bounded shapes (for example `b -> a` becomes `⊥ -> a` for binder `b`),
  - keeps strict alias-bound rejection (`∀(b ⩾ a)`) intact.
- Deterministic BUG-002 matrix (`BUG-002-V1..V4`, seed `1593170056`) is green in this workspace after this pass.

## 2026-02-26 OpWeaken no-op fallback removal (Fig. 15.3.4 / §15.3.5)

- Removed both non-root `OpWeaken` no-op fallback exits in `MLF.Elab.Phi.Omega`:
  - non-binder alias target with no recoverable binder-in-spine,
  - binder target that cannot be located in current `vSpineIds`.
- Non-root `OpWeaken` now has a strict invariant:
  - resolve replay binder and emit thesis-shaped `InstElim`, or
  - fail fast with `PhiTranslatabilityError` (no silent identity/no-op path).
- Error payloads are normalized across both former fallback sites and now include:
  - op/replay/canonical targets,
  - solved class members considered,
  - recoverable binders,
  - current spine ids,
  - replay/hint map domains.
- Added focused regressions in `test/ElaborationSpec.hs` for both former fallback branches and retained alias-recovery success coverage.
- Rebaselined legacy fallback-dependent pipeline regressions to assert strict fail-fast behavior rather than permissive success.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "OpWeaken"'` (green),
  - `cabal build all && cabal test` (green),
  - `cabal test --test-show-details=direct` (green).

## 2026-03-02 Wave 2 solved-indirection closeout (Tasks 7-9)

- Presolution planner boundary is now view-first:
  - `PresolutionPlanBuilder` closure migrated from `Solved -> ...` to `PresolutionView -> ...`.
  - `MLF.Constraint.Presolution.Plan.buildGeneralizePlans` now consumes `PresolutionView` canonical data directly.
  - `MLF.Elab.Run.Generalize.generalizeAtWithBuilder` adapts runtime solved handles into `PresolutionView` before invoking the plan builder.
- `MLF.Constraint.Presolution.View.fromPresolutionResult` now accepts any `PresolutionSnapshot`:
  - signature generalized to `PresolutionSnapshot a => a -> PresolutionView`,
  - this removes the `Presolution.Base <-> Presolution.View` cycle pressure introduced by the builder-signature migration.
- `MLF.Constraint.Solved` was reduced by removing `fromPresolutionResult` (production-only builder surface):
  - no production call sites depended on it,
  - compatibility/test paths continue through `fromSolveOutput`, `fromPreRewriteState`, and `mkTestSolved`.
- Planner solved-compat reconstruction note:
  - first attempt (`Solved.fromPreRewriteState` from view snapshot) regressed paper baseline tests with `InvalidBindingTree ... node ... not in constraint`,
  - final approach uses `Solved.mkTestSolved` over `pvCanonicalConstraint` plus a live-node sanitized canonical map, restoring baseline behavior.
- Hygiene guard scope note (Task 9):
  - enforce no direct `MLF.Constraint.Solved` imports in elaboration entrypoint/public modules (`MLF.Elab.Run`, `MLF.Elab.Pipeline`, `MLF.API`, `MLF.Pipeline`),
  - internal elaboration modules still keep compatibility solved reads where removal would require broader architectural changes.

## 2026-03-07 row9-11 direct-target Ω closeout

- Ω no longer defines the local source-candidate recovery helpers (`sourceCandidates`, `pickExistingSource`, `adoptOpNode`, `graftArgFor`).
- `resolveTraceBinderTarget` remains the only target-selection bridge from witness-domain source ids to replay ids for ω operations.
- `OpRaise` now fails fast when no direct replay/source target exists, including the formerly silent non-trace no-op case.
- Source-domain interior membership keeps one bounded exception: direct forward `etCopyMap` alias evidence may justify interior membership, but reverse-copy/canonical candidate expansion no longer participates in runtime target recovery.
- `IdentityBridge` remains in the codebase as a witness-domain utility/test surface; runtime Ω binder selection is direct and no longer driven by local candidate ranking helpers.
