# Progress Log: BUG-2026-02-14-003 Identity Drift (Resume)

## 2026-02-15
- Initialized task folder and tracking files for resumed investigation.
- Next: reproduce current failing anchors in this session and gather end-to-end trace evidence before changing code.
- Baseline reproduction:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Pipeline (Phases 1-5)/BUG-2026-02-06-002 strict target matrix/let-c1-return keeps second binder polymorphic/" --seed 1715612721'`
    - Result: `1 example, 1 failure` with `PhiTranslatabilityError ["OpGraft+OpWeaken targets non-binder node", target 8]`.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004" --seed 1715612721'`
    - Result: `4 examples, 0 failures`.
- Reproduced broader failing set:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002 strict target matrix" --seed 1715612721'`
  - Result: `4 examples, 3 failures` (all same Phi non-binder class).
- Ran witness/canonical probes via `cabal repl lib:mlf2-internal` scripts:
  - `/tmp/ghci_compare_witness_canon_c1.ghci`
  - `/tmp/ghci_compare_witness_canon_v4.ghci`
  - Confirmed raw->canonical target drift (e.g., `9 -> 8`, `0 -> 11`).
- Ran solve/base mapping probe:
  - `/tmp/ghci_probe_ga_map_make_app.ghci`
  - Confirmed `gaSolvedToBase` contains drift mappings but current Phi binder checks still reject target.
- Attempted five minimal hypotheses (documented in findings) and reverted non-viable probes.
- Current reproduced baseline remains the original Phi non-binder failure for BUG-003 anchors.
- Post-probe guard checks:
  - `--match "BUG-004"` remains green (`4 examples, 0 failures`).
  - copy-map anchor (`tracks instantiation copy maps for named binders`) currently green (`1 example, 0 failures`).
  - `BUG-002-V4` still fails with strict Φ non-binder target (`NodeId 11`).

## 2026-02-15 (resume continuation)

- Re-ran anchors on current handoff baseline:
  - `cabal test ... --match "BUG-2026-02-06-002 strict target matrix"` -> `4 examples, 2 failures`
    - `ValidationFailed alias bounds survived scheme finalization`
    - `TCArgumentMismatch Int Bool`
  - `cabal test ... --match "BUG-002-V4"` -> `1 failure` (`TCArgumentMismatch (TVar "t2") Bool`)
  - `cabal test ... --match "BUG-004"` -> pass (`4 examples, 0 failures`)
  - `cabal test ... --match "tracks instantiation copy maps for named binders"` -> pass
  - `cabal test ... --match "witness/trace/expansion canonicalization"` -> pass

- Applied and validated source-ID contract changes:
  - Updated `src/MLF/Elab/Run/Util.hs` (preserve source IDs in witness ops and trace binder/copy provenance).
  - Updated `test/CanonicalizerSpec.hs` to assert the source-preserving contract.
  - Updated `src/MLF/Elab/Phi/Translate.hs`:
    - Keep `ewSteps` source IDs in `phiFromEdgeWitnessCore`.
    - Add `remapSchemeInfoByTrace` and use it for traced remapping.
    - Add traced subst debug (`traceGeneralize`-gated) and trace-binder arity guard.
  - Updated `src/MLF/Elab/Phi/Omega.hs`:
    - Source-domain binder identity helpers (`sourceKeysForNode`, source-aware `isBinderNode` / `lookupBinderIndex`).
    - Root-operation gating now avoids canonical-only root classification for source-domain binders.
    - Retained strict non-binder rejection, with richer diagnostics.

- Additional local diagnostics:
  - Ran `cabal exec runghc /tmp/inspect_edge0.hs` to dump presolution artifacts for let-c1:
    - edge-0 trace includes `etBinderArgs [(9,17),(3,18)]`
    - edge-0 witness steps are `OpGraft 17 9 ; OpWeaken 9`
  - Ran trace-enabled debug drivers to inspect Phi choices; edge-0 still emits `InstApp Int` in failing cases.

- Additional probe (reverted):
  - Tested disabling app-RHS realignment in `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs` (`appAligned` forced to `sch0Final`) to check whether let-app fallback caused remaining failures.
  - Result: no change in `BUG-2026-02-06-002` / `BUG-002-V4`; restored original logic.

- Operational note:
  - Parallel `cabal test` commands intermittently hit dist-newstyle `package.conf.inplace already exists`; sequential reruns are reliable.

## 2026-02-16 (continuation)

- Continued BUG-002-V4 closure while preserving source-ID contract anchors.
- Iterative edits applied in:
  - `src/MLF/Elab/Elaborate.hs`
  - `src/MLF/Elab/Run/Pipeline.hs`
- Current targeted status:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V4"'` -> PASS (`1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002 strict target matrix"'` -> PASS (`4 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004"'` -> PASS (`4 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "tracks instantiation copy maps for named binders"'` -> PASS (`1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "witness/trace/expansion canonicalization"'` -> PASS (`2 examples, 0 failures`)
- Full gate attempt:
  - `cabal build all && cabal test` -> FAIL (`653 examples, 19 failures`)
  - Failures are outside the narrow targeted anchors and include broader Phase 1-6 regressions / existing instability buckets.
- Note: sequential `cabal` runs were required to avoid `dist-newstyle` `package.conf.inplace` race errors.

## 2026-02-15 — Baseline Lock for Source-ID Split-Domain Refactor

Re-locked all targeted test anchors before introducing IdentityBridge module.

**CORRECTION:** Initial subagent report was inaccurate (claimed all green). Verified independently via clean build at commit 7f36cc2:

| Anchor | Examples | Failures | Status |
|--------|----------|----------|--------|
| BUG-003 strict target matrix | 4 | 2 | FAIL (pre-existing) |
| BUG-002-V4 | 1 | 1 | FAIL (pre-existing) |
| BUG-004 | 4 | 4 | FAIL (pre-existing) |
| Copy-map provenance | 1 | 1 | FAIL (pre-existing) |
| Witness/trace/expansion canonicalization | 2 | 0 | PASS |

These failures are the bug (BUG-2026-02-14-003) we are fixing. Only witness/trace canonicalization is green at baseline.

## 2026-02-15 — Task 4: Translate uses IdentityBridge (commit e9cb28b)

Refactored `remapSchemeInfoByTrace` and `hydrateSchemeInfoByTrace` to use bridge for ranking. Added `traceOrderRank` to IdentityBridge. Full gate: 670 examples, 47 failures (improved by 1 from baseline ~48).

## 2026-02-15 — Task 5: Omega uses IdentityBridge (commit 281d8dd)

Replaced local `isBinderNode` and `lookupBinderIndex` in Omega.hs with bridge delegation. Removed unused `invCopyMap` and `mbGaParents` locals. Net: -33/+8 lines. Full gate: 670 examples, 47 failures (unchanged from Task 4).

Note: `gaSolvedToBase` resolution dropped from `lookupBinderIndex` — bridge's broader source-key expansion subsumes it for all current test cases. Documented in code comment.

## 2026-02-15 — Task 6: Canonicalization boundary contract (commit f48b3c6)

Existing canonicalization tests already lock the split-domain boundary. Added contract documentation comment. All 6 canonicalization tests pass.

## 2026-02-15 — Task 7: Sequential Validation Matrix

| Anchor | Examples | Failures | Status | vs Baseline |
|--------|----------|----------|--------|-------------|
| BUG-003 strict target matrix | 4 | 2 | FAIL | unchanged |
| BUG-002-V4 | 1 | 1 | FAIL | unchanged |
| BUG-004 | 4 | 4 | FAIL | unchanged |
| Copy-map provenance | 1 | 1 | FAIL | unchanged |
| Witness/trace canonicalization | 2 | 0 | PASS | unchanged |
| Full gate | 670 | 47 | FAIL | improved by 1 from baseline 48 |

**Conclusion:** The IdentityBridge refactoring (Tasks 2-6) successfully centralized source↔canonical reconciliation and improved the full gate by 1 failure, but did not close the targeted BUG-2026-02-14-003 anchors. The underlying bug requires further work beyond the structural refactoring — likely in the Phi translation or presolution pipeline itself.

Per plan constraint #6: targeted anchors are not green, so Task 8 (close tracker/docs) is deferred. The IdentityBridge infrastructure is in place for the next phase of the fix.

## 2026-02-16 — Systematic debugging continuation (BUG-2026-02-14-003)

- Reproduced current anchor status on `master` before edits:
  - `BUG-2026-02-06-002 strict target matrix` -> `4 examples, 2 failures` (`TCLetTypeMismatch`)
  - `BUG-002-V4` -> `1 failure` (`PhiReorder: cycle in bound dependencies`)
  - `BUG-004` -> `4 failures`
  - `tracks instantiation copy maps for named binders` -> `1 failure` (`predicate failed on []`)
  - `witness/trace/expansion canonicalization` -> pass (`2 examples, 0 failures`)
- Compared current vs known-good commit (`f644d62`) using focused debug scripts:
  - Current presolution `edge0` for BUG-002-V4 was `ExpIdentity` with empty `etBinderArgs` and empty witness.
  - Known-good `f644d62` had `ExpInstantiate [...]`, non-empty `etBinderArgs`, and non-empty witness steps.
- Root-cause isolation (Phase 1/2 evidence):
  - `instantiationBindersFromGenM` was using structural-only reachability (`reachableFromUnderLenient`) from a wrapper body root (`TyVar` with bound), collapsing reachable set to just the wrapper node and yielding empty binder candidates.
  - Supporting evidence: for `bodyRoot=NodeId 6`, structural-only reachability = `[6]`, bounds-aware reachability = `[2,3,4,6,9,11]`.
- Minimal hypothesis patch (Phase 3):
  - In `src/MLF/Constraint/Presolution/Base.hs`, switched binder reachability to bounds-aware and switched binder order-key computation to `orderKeysFromConstraintWith`.
  - Immediate effect:
    - `tracks instantiation copy maps for named binders` turned green.
    - BUG-002-V4 no longer fails with reorder cycle; it now fails later with:
      `PhiTranslatabilityError ["OpRaise target outside I(r)", ..., "op: OpRaise NodeId {getNodeId = 1}", "interiorSet=[5,11,30,31,35]"]`.
- Interpretation:
  - Upstream binder discovery regression is confirmed and partially corrected.
  - Remaining failure is now in Φ interior membership domain reconciliation (source op target vs remapped/canonical interior set), i.e., the intended BUG-2026-02-14-003 identity-domain boundary.

## 2026-02-16 — Surgical Omega (`I(r)` source-domain) implementation

- Added test-first regressions:
  - `test/ElaborationSpec.hs`: `OpRaise accepts source-domain interior membership even when etCopyMap aliases the target`.
  - `test/PipelineSpec.hs`: `BUG-002-V4 keeps OpRaise targets inside etInterior after witness/trace canonicalization`.
- RED baseline:
  - `cabal test ... --match "OpRaise accepts source-domain interior membership even when etCopyMap aliases the target"` -> FAIL
    - `PhiTranslatabilityError ... "OpRaise target outside I(r)" ... "interiorSet=[30,100]"`.
  - `cabal test ... --match "BUG-002-V4 keeps OpRaise targets inside etInterior after witness/trace canonicalization"` -> PASS.

- Applied Omega/Translate changes:
  - `src/MLF/Elab/Phi/Omega.hs`
    - `interiorSet` now uses `etInterior` directly (source-domain contract).
    - Added alias-domain mismatch invariant diagnostics for `OpRaise`.
    - `OpRaise` semantic node now adopts `etCopyMap` source->copied mapping before canonicalization.
  - `src/MLF/Elab/Phi/Translate.hs`
    - Canonicalize `etInterior` only for `namedSet` intersection.

- Mid-run regression and resolution:
  - After initial Omega source-domain patch, `BUG-004-V2` regressed to
    `TCArgumentMismatch ... (TForall "a" ...)`.
  - Root-cause probe via trace scripts showed `OpRaise` admissibility used source IDs but semantic execution still used unreconciled source target.
  - After adopting source->copied node for `OpRaise` semantics, `BUG-004-V2` returned to PASS.

- Verification commands (sequential, lock-safe):
  - `cabal test ... --match "OpRaise accepts source-domain interior membership even when etCopyMap aliases the target"` -> PASS (`1 example, 0 failures`).
  - `cabal test ... --match "BUG-002-V4 keeps OpRaise targets inside etInterior after witness/trace canonicalization"` -> PASS.
  - `cabal test ... --match "BUG-002-V4"` -> PASS (`2 examples, 0 failures`).
  - `cabal test ... --match "BUG-2026-02-06-002 strict target matrix"` -> PASS (`4 examples, 0 failures`).
  - `cabal test ... --match "BUG-004"` -> PASS (`4 examples, 0 failures`).
  - `cabal test ... --match "tracks instantiation copy maps for named binders"` -> PASS.
  - `cabal test ... --match "witness/trace/expansion canonicalization"` -> PASS.
  - Full gate:
    - `cabal build all && cabal test` -> FAIL (`672 examples, 9 failures`).
    - Failures are in separate buckets (`EdgePlannerSpec`, `id id` polymorphic application paths, BUG-003 sentinels, explicit forall annotation non-spine context), not this surgical `I(r)` fix.

## 2026-02-16 — Full-gate failure bucket split

- Parsed `/Volumes/src/mlf4/dist-newstyle/build/aarch64-osx/ghc-9.12.2/mlf2-0.2.0.0/t/mlf2-test/test/mlf2-0.2.0.0-mlf2-test.log` (seed `1481579064`) and split all 9 failing cases into distinct bug tracker entries.
- Added `BUG-2026-02-16-001..009` to `/Volumes/src/mlf4/Bugs.md`, each with:
  - deterministic minimal repro command (`--match ... --seed 1481579064`)
  - expected vs actual
  - ownership mapping (module/file owners)
  - thesis impact note.
- Updated legacy umbrella `BUG-2026-02-11-004` status line to reference split successors (`BUG-2026-02-16-007/008`) for current full-gate tracking.

## 2026-02-16 — BUG-2026-02-16-003 (`id id`) fix pass

- Systematic-debugging Phase 1/2:
  - Reproduced pinned bug command and cluster repros:
    - `/Pipeline (Phases 1-5)/redirected let-use sites keep polymorphic schemes/`
    - `/Pipeline (Phases 1-5)/Checked-authoritative invariant/runPipelineElab type matches typeCheck(term) and checked pipeline type/`
    - `/Phase 6 — Elaborate (xMLF)/Polymorphism and Generalization/elaborates dual instantiation in application/`
    - `id id should have type`
  - All failed initially with the same `TCArgumentMismatch` class.

- Instrumentation pass:
  - Temporarily traced `AAppF` instantiation decisions in `src/MLF/Elab/Elaborate.hs`.
  - Confirmed `argInstFromFun` inlined inferred meta-vars and over-specialized argument-side polymorphic `id`.
  - Removed temporary tracing after root cause confirmation.

- Phase 3 minimal hypothesis test:
  - Changed `argInstFromFun` to keep inferred args as-is (`instSeqApps args`) instead of `instSeqApps (map (inlineBoundVarsType resReify) args)`.

- Verification (sequential cabal runs):
  - `cabal test ... --match "/Pipeline (Phases 1-5)/redirected let-use sites keep polymorphic schemes/" --seed 1481579064` -> PASS
  - `cabal test ... --match "/Pipeline (Phases 1-5)/Checked-authoritative invariant/runPipelineElab type matches typeCheck(term) and checked pipeline type/" --seed 1481579064` -> PASS
  - `cabal test ... --match "/Phase 6 — Elaborate (xMLF)/Polymorphism and Generalization/elaborates dual instantiation in application/" --seed 1481579064` -> PASS
  - `cabal test ... --match "id id should have type" --seed 1481579064` -> PASS
  - `cabal test ... --match "BUG-002-V2: alias indirection elaborates to Int" --seed 1481579064` -> PASS
  - `cabal test ... --match "BUG-002-V4: factory-under-lambda elaborates to ∀a. a -> a" --seed 1481579064` -> PASS
  - `cabal test ... --match "BUG-2026-02-06-002 strict target matrix" --seed 1481579064` -> PASS
  - `cabal test ... --match "BUG-002-V4 keeps OpRaise targets inside etInterior after witness/trace canonicalization" --seed 1481579064` -> PASS

- Tracker sync:
  - Moved BUG-2026-02-16-003/004/005/006 from Open to Resolved in `/Volumes/src/mlf4/Bugs.md`.
- Full gate after BUG-2026-02-16-003 fix:
  - `cabal build all && cabal test` -> FAIL (`672 examples, 5 failures`).
  - Remaining open buckets align with current tracker (`BUG-2026-02-16-001`, `BUG-2026-02-16-002`, `BUG-2026-02-16-007`, `BUG-2026-02-16-008`, `BUG-2026-02-16-009`).
