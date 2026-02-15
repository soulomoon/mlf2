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
