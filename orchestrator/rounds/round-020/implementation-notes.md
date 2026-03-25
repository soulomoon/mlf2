# Round 020 Implementer Notes (`D1` attempt 1)

## Scope implemented

- Added root-cause routing for `research_entrypoint_id = uri-r2-c1-p2-replay-root-cause-v1` with strict stage selector `D1-replay-reproduction`, scenario `uri-r2-c1-only-v1`, and attempt range `1..100`.
- Added bounded `D1` execution module at `src/MLF/Research/URI/R2/C1/Prototype/D1.hs`.
  - `D1-I` verifies continuity against inherited authoritative inputs:
    - `round-016` `P1` attempt-2 subject token
    - `round-017` `P2` attempt-2 `check-P2-W`, `stage-verdict`, `trace-bundle`
  - `D1-R` reruns the bounded replay lane by executing the real `P2` replay workflow in an isolated sandbox and classifies replay outcome (`exact-bounded-replay-failure` / drift / bounded inability).
  - `D1-M` compares observed replay signature against the accepted `P2-W` boundary (`partial-replay`, `InstBot expects ⊥, got: t9 -> t9`).
  - Emits only:
    - `trace-bundle.json`
    - `check-D1-I.json`
    - `check-D1-R.json`
    - `check-D1-M.json`
    - `stage-verdict.json`
  - Does **not** emit `subject-token.json`.
- Added D1 artifact support in `Artifact.hs` and generated canonical artifact:
  - `docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md`
- Extended focused research tests in `test/Research/UriR2C1PrototypeP1Spec.hs` for D1 acceptance/rejection/evidence/non-mutation requirements.
- Registered `MLF.Research.URI.R2.C1.Prototype.D1` in `mlf2.cabal`.
- Preserved default no-arg executable path (`mlf2` still returns the demo `Type: ...` output).

## Verification run

- `git diff --check` ✅
- `python3 -m json.tool orchestrator/rounds/round-020/state-snapshot.json >/dev/null` ✅
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-020/state-snapshot.json` ✅
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/roadmap.md` ✅
- Required spec/control files existence checks ✅
- `cabal build all` ✅
- `cabal test` ✅ test suite passed (`1117 examples, 0 failures`)
  - Environment caveat: Cabal exits non-zero after successful runs due `~/.cache/cabal/logs/build.log` permission denial.
- D1 execution command (required tuple) run via built binary due same Cabal log-file issue:
  - `./dist-newstyle/.../mlf2 --research-entrypoint uri-r2-c1-p2-replay-root-cause-v1 --stage-selector D1-replay-reproduction --scenario-id uri-r2-c1-only-v1 --attempt-id 1` ✅ (`Prototype result: pass`)
- Round-specific evidence checks ✅
  - attempt directory file set matches planned outputs
  - all attempt outputs parse as JSON
  - `subject-token.json` absent
  - `check-D1-I` continuity fields reference authoritative subject/signature
  - `check-D1-R` records replay classification + replay diagnostic
  - `check-D1-M` compares explicit target/observed signatures
  - `stage-verdict.json` stage result is in allowed set (`pass`)
  - wrong-stage and wrong-scenario invocations do not mutate attempt-1 evidence

## Residual risk

- D1 replay rerun reuses the production-bounded `P2` replay implementation through a temporary sandbox call. This keeps semantics aligned with accepted replay behavior, but future refactors to `P2` internals must preserve this coupling for D1 reproducibility guarantees.
