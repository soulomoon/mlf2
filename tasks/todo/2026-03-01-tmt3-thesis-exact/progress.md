# Progress — 2026-03-01 Pod A Wave 1

## Log
- Initialized task folder and planning files.
- Scanned owned Phi modules and targeted specs.
- Identified fallback-sensitive locations:
  - `Translate.computeTraceBinderReplayBridge` canonical domain checks.
  - `Omega` local canonical fallbacks in replay target/domain checks.
  - `IdentityBridge.lookupBinderIndex` canonical alias match class.
- Updated tests first (RED):
  - `test/Phi/IdentityBridgeSpec.hs`
  - `test/ElaborationSpec.hs`
- Implemented strict-source changes (GREEN):
  - `IdentityBridge`: removed canonical-alias matching from source key and binder-index resolution.
  - `Translate`: replay-map codomain/domain validation now raw replay key-space only.
  - `Omega`: removed canonical-based binder acceptance in replay-target resolution.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi alignment" --match "fails fast when replay-map source domain mismatches trace binder sources" --match "fails fast when replay-map codomain target is outside replay binder domain" --match "IdentityBridge"'` => PASS (30 examples).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "fails fast when replay-map codomain only matches replay domain via canonical alias"'` => PASS (1 example).
  - `rg -n "Solved\\.canonical" src/MLF/Elab/Phi` executed for required audit output.
- Follow-up hard-check #1:
  - `rg -n "Solved\\.canonical" src/MLF/Elab/Phi` => no matches.
  - Targeted command with added canonical-alias fail-fast matcher => PASS (31 examples).
