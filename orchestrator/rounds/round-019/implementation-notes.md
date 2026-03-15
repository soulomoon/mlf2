# Round 019 Implementation Notes (`P4`)

Date: 2026-03-15  
Round: `round-019`  
Scope: roadmap item 4 only (`P4-prototype-decision-gate`)

## Summary

Implemented bounded `P4` routing and execution under the shared research entrypoint `uri-r2-c1-prototype-entrypoint-v1` with scenario fixed to `uri-r2-c1-only-v1`.

`P4` now consumes only authoritative stage records from:

- `orchestrator/rounds/round-016/review-record.json` (`P1`)
- `orchestrator/rounds/round-017/review-record.json` (`P2`)
- `orchestrator/rounds/round-018/review-record.json` (`P3`)

Decision gate rule implemented exactly as planned:

- `reopen-handoff-track` only if all consumed stage results are `pass` and `terminal_reason` is `none`
- otherwise `hard-stop`

For the inherited authoritative vector (`P1=pass`, `P2=semantic-negative`, `P3=semantic-negative`), `P4` resolves to `hard-stop`.

## Code Slice

- Added `P4` selector/path constants and routing:
  - `src/MLF/Research/URI/R2/C1/Prototype/Types.hs`
  - `src/MLF/Research/URI/R2/C1/Prototype/Entrypoint.hs`
- Added dedicated `P4` executor module:
  - `src/MLF/Research/URI/R2/C1/Prototype/P4.hs`
- Added canonical `P4` artifact writer and renderer:
  - `src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs`
- Registered module in Cabal:
  - `mlf2.cabal`
- Extended prototype tests with `P4` entrypoint coverage and bounded rejection checks:
  - `test/Research/UriR2C1PrototypeP1Spec.hs`

## Produced `P4` Evidence

- Canonical artifact:
  - `docs/plans/2026-03-15-uri-r2-c1-p4-prototype-decision-gate.md`
- Attempt evidence (`attempt-1`):
  - `orchestrator/rounds/round-019/evidence/P4/attempt-1/trace-bundle.json`
  - `orchestrator/rounds/round-019/evidence/P4/attempt-1/stage-consumption.json`
  - `orchestrator/rounds/round-019/evidence/P4/attempt-1/decision-verdict.json`

## Verification Run

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 P4 prototype entrypoint"'` (pass)
- `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P4-prototype-decision-gate --scenario-id uri-r2-c1-only-v1 --attempt-id 1` (pass, terminal decision `hard-stop`)
- `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P4-prototype-decision-gate --scenario-id wrong-scenario --attempt-id 2` (expected reject)
- `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector wrong-stage --scenario-id uri-r2-c1-only-v1 --attempt-id 2` (expected reject)
- `test ! -d orchestrator/rounds/round-019/evidence/P4/attempt-2` (pass; rejection path did not create attempt output)
- `python3 -m json.tool orchestrator/rounds/round-019/evidence/P4/attempt-1/decision-verdict.json >/dev/null` (pass)
- `python3 -m json.tool orchestrator/rounds/round-019/evidence/P4/attempt-1/stage-consumption.json >/dev/null` (pass)
- `python3 -m json.tool orchestrator/rounds/round-019/evidence/P4/attempt-1/trace-bundle.json >/dev/null` (pass)
- `cabal run mlf2` (pass; default production path unchanged)
- `git diff --check` (pass)
- `python3 -m json.tool orchestrator/state.json >/dev/null` (pass)
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` (pass)
- `test -f docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md` (pass)
- `cabal build all && cabal test` (pass)

## Boundary Compliance

- No edits to `orchestrator/state.json`.
- No edits to `orchestrator/roadmap.md`.
- No edits to `app/Main.hs`.
- No edits under `src-public/`.
- No rewrite of predecessor round artifacts (`round-016` through `round-018`).
