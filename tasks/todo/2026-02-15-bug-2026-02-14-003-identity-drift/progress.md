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

### BUG-003 strict target matrix
```
cabal test mlf2-test --test-show-details=direct \
  --test-options='--match "BUG-2026-02-06-002 strict target matrix"'
```
Result: **4 examples, 0 failures** (PASS)

### BUG-002-V4 guardrail
```
cabal test mlf2-test --test-show-details=direct \
  --test-options='--match "BUG-002-V4"'
```
Result: **1 example, 0 failures** (PASS)

### BUG-004 guardrail
```
cabal test mlf2-test --test-show-details=direct \
  --test-options='--match "BUG-004"'
```
Result: **4 examples, 0 failures** (PASS)

### Provenance: copy-map anchor
```
cabal test mlf2-test --test-show-details=direct \
  --test-options='--match "tracks instantiation copy maps for named binders"'
```
Result: **1 example, 0 failures** (PASS)

### Canonicalization: witness/trace/expansion
```
cabal test mlf2-test --test-show-details=direct \
  --test-options='--match "witness/trace/expansion canonicalization"'
```
Result: **2 examples, 0 failures** (PASS)

### Summary
All 5 anchor groups green (12 total examples, 0 failures). Baseline locked — safe to proceed with IdentityBridge refactor.
