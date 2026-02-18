# Task Plan: Thesis Conformance Gate Command/Profile

## Objective
Add a single thesis-conformance gate entrypoint and CI-required job that continuously enforces key paper anchors: Φ/Ω translatability matrix, A6 parity, and representative theorem examples.

## Scope
- Add one canonical gate command (`scripts/thesis-conformance-gate.sh`).
- Add GitHub Actions workflow job that runs `cabal build all` plus the gate command.
- Document the gate in repo docs (`README.md`, `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`).
- Run focused + full verification.

## Phases
1. Baseline anchor confirmation and matcher shape validation. (completed)
2. Implement gate command script with fail-fast stale-matcher checks. (completed)
3. Add CI workflow wiring to run the gate command. (completed)
4. Update docs/tracker files (`README.md`, `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`). (completed)
5. Verification + commit. (completed)

## Decisions
- Use substring `--match` invocations per anchor group because Hspec matcher does not support regex alternation as needed for grouped anchors.
- Enforce minimum matched-example counts in the gate script to prevent false green on stale matcher strings.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Parallel `cabal test` invocations hit `package.cache.lock` removal race while probing matcher counts. | 1 | Re-ran the same probes sequentially; all focused anchor runs passed. |

## 2026-02-18 implementation status
- Added `scripts/thesis-conformance-gate.sh` and made it executable.
- Added `.github/workflows/thesis-conformance.yml` for required CI execution.
- Updated `README.md`, `TODO.md`, `implementation_notes.md`, and `CHANGELOG.md` with gate command/profile references.
- Local verification completed:
  - `./scripts/thesis-conformance-gate.sh` (PASS)
  - `cabal build all && cabal test` (PASS)
- Task closure: all phases complete; folder ready to move from `tasks/todo/` to `tasks/archive/`.
