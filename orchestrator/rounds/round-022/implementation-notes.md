# Round 022 Implementer Notes (`D3` attempt 1)

## Scope implemented

- Added bounded `D3` stage routing for the replay root-cause entrypoint:
  - `research_entrypoint_id = uri-r2-c1-p2-replay-root-cause-v1`
  - `stage_selector = D3-fixability-probe`
  - `scenario_id = uri-r2-c1-only-v1`
  - `attempt_id` range `1..100`
- Added `D3` execution module at `src/MLF/Research/URI/R2/C1/Prototype/D3.hs`.
  - Consumes only authoritative inherited artifacts in-scope for D3:
    - round-016/P1 subject token (attempt-2)
    - round-017/P2 accepted replay boundary (`check-P2-W`, `stage-verdict`, attempt-2)
    - round-020 authoritative D1 (`stage-verdict`, `review-record.json`)
    - round-021 authoritative D2 (`check-D2-L`, `check-D2-O`, `stage-verdict`, `trace-bundle`, `review-record.json`)
  - Implements `D3-H`, `D3-B`, and `D3-V` checks for one bounded hypothesis (`H1`) at the D2-localized `applyInstantiation`/`InstBot` boundary.
  - Emits only attempt-local D3 evidence under:
    - `orchestrator/rounds/round-022/evidence/D3/attempt-1/`
    - `trace-bundle.json`
    - `check-D3-H.json`
    - `check-D3-B.json`
    - `check-D3-V.json`
    - `stage-verdict.json`
  - Does **not** emit `subject-token.json`.
- Extended artifact writer support with `writeD3Artifact`, generating:
  - `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md`
- Extended focused prototype tests in `test/Research/UriR2C1PrototypeP1Spec.hs`:
  - D3 acceptance path (evidence schema + artifact generation)
  - D3 tuple rejection path (wrong scenario/stage/entrypoint/attempt)
- Registered the new library module:
  - `MLF.Research.URI.R2.C1.Prototype.D3` in `mlf2.cabal`.
- Preserved default no-arg `cabal run mlf2` behavior (no second executable interface, no default-path behavior change).

## Verification run

- `cabal build all` ✅
- `cabal test` ✅ (suite output: `1121 examples, 0 failures`)
  - Environment caveat: command exits non-zero after success because Cabal attempts to write `~/.cache/cabal/logs/build.log` and receives `permission denied`.
- D3 execution via shared research entrypoint:
  - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-p2-replay-root-cause-v1 --stage-selector D3-fixability-probe --scenario-id uri-r2-c1-only-v1 --attempt-id 1` ✅ (`Prototype result: pass`)
- Baseline contract checks in round-022 worktree ✅
  - `git diff --check`
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - existence checks for the roadmap and retry-subloop control files
- D3-specific checks ✅
  - evidence file set is exact and JSON-valid
  - `subject-token.json` absent in D3 attempt lane
  - stage verdict encodes:
    - `attempt_verdict = repair-supporting`
    - `stage_result = pass`
  - canonical D3 artifact records `D3-H`, `D3-B`, `D3-V`, bounded hypothesis `H1`, and probe-only repair-direction support.

## Residual risk

- The D3 fixability classification is intentionally evidence-driven from inherited D1/D2 diagnostics and trace strings. If those upstream diagnostic strings change format without semantic metadata replacements, D3 may degrade to `semantic-negative` or `inconclusive` even when the underlying boundary remains the same.
