# Findings

## Starting Point

- The repository already has a live top-level `orchestrator/` contract, so
  this is a successor-loop scaffold, not a first-time scaffold.
- `orchestrator/state.json` was parked at:
  - `roadmap_id`: `2026-03-29-03-non-local-proxy-phi-translation-and-reclassification`
  - `stage`: `done`
  - `controller_stage`: `done`
  - `last_completed_round`: `round-155`
- The top-level pointer stubs were stale and still pointed at the March 27
  roadmap family instead of the live March 29 family.

## Current CI / Verification Baseline

- Existing GitHub Actions coverage is a single workflow:
  `.github/workflows/thesis-conformance.yml`
- That workflow currently runs only on `ubuntu-latest` with `ghc-version:
  9.12.2`; there is no GitHub Actions matrix yet.
- Local repo gate:
  - `cabal build all && cabal test`: PASS (`1177 examples, 0 failures`)
- Local thesis gate:
  - `./scripts/thesis-conformance-gate.sh`: FAIL
  - exact blocker:
    `thesis-obligations render: markdown is out of date: /Volumes/src/mlf4/docs/thesis-obligations.md`

## Matrix-Scope Constraint

- The current conformance gate is driven by POSIX shell scripts
  (`scripts/thesis-conformance-gate.sh`, `scripts/check-thesis-obligations-ledger.sh`,
  `scripts/check-thesis-claims.sh`).
- That means a bounded Unix-first matrix is honest immediately; a Windows lane
  should not be promised until the supporting commands are intentionally made
  Windows-compatible.
