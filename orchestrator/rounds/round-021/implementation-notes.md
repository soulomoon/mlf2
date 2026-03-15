# Round 021 Implementer Notes (`D2` attempt 1)

## Scope implemented

- Added bounded `D2` stage routing for replay root-cause entrypoint:
  - `research_entrypoint_id = uri-r2-c1-p2-replay-root-cause-v1`
  - `stage_selector = D2-mismatch-localization`
  - `scenario_id = uri-r2-c1-only-v1`
  - `attempt_id` range `1..100`
- Added `D2` execution module at `src/MLF/Research/URI/R2/C1/Prototype/D2.hs`.
  - `D2-T` aligns inherited P2 and authoritative D1 trace lanes (`generalize -> scheme-to-type -> reify-no-fallback -> witness-replay`) under one correlation id.
  - `D2-L` localizes one exact first divergence boundary:
    - `witness-replay/applyInstantiation-instbot-precondition`
  - `D2-O` assigns one exact owner account:
    - `applyInstantiation semantics (`MLF.Elab.Inst.applyInstantiation`, InstBot branch)`
    - and records non-owner exclusions for witness construction, replay-domain reconstruction, and no-fallback reification output.
  - Consumes only authoritative inherited inputs in-scope for D2:
    - round-016 `P1` subject token (attempt-2)
    - round-017 `P2` replay evidence (`check-P2-R`, `check-P2-W`, `stage-verdict`, `trace-bundle`, attempt-2)
    - round-020 authoritative `D1` evidence (`check-D1-R`, `stage-verdict`, `trace-bundle`, attempt-1) plus round-020 `review-record.json`
  - Emits only attempt-local D2 evidence:
    - `trace-bundle.json`
    - `check-D2-T.json`
    - `check-D2-L.json`
    - `check-D2-O.json`
    - `stage-verdict.json`
  - Does **not** emit `subject-token.json`.
- Extended artifact writer support with `writeD2Artifact`, producing:
  - `docs/plans/2026-03-16-uri-r2-c1-d2-replay-mismatch-localization.md`
- Added focused D2 tests in `test/Research/UriR2C1PrototypeP1Spec.hs`:
  - acceptance path for D2 attempt-1 evidence and artifact generation
  - rejection path for wrong scenario/stage/entrypoint/attempt bounds
- Registered new module in Cabal:
  - `MLF.Research.URI.R2.C1.Prototype.D2`
- Preserved default no-arg `mlf2` behavior (no second executable interface, no default path changes).

## Verification run

- `git diff --check` ✅
- `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json >/dev/null` ✅
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json` ✅
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md` ✅
- Spec/control plane existence checks ✅
  - `docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`
  - `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`
  - `orchestrator/retry-subloop.md`
- `cabal build all` ✅
- `cabal test` ✅ (`1119 examples, 0 failures`)
  - Environment caveat: command exits non-zero after success due Cabal log-file permission (`~/.cache/cabal/logs/build.log`).
- Generated D2 attempt artifact/evidence via built binary:
  - `./dist-newstyle/build/aarch64-osx/ghc-9.12.2/mlf2-0.2.0.0/x/mlf2/build/mlf2/mlf2 --research-entrypoint uri-r2-c1-p2-replay-root-cause-v1 --stage-selector D2-mismatch-localization --scenario-id uri-r2-c1-only-v1 --attempt-id 1` ✅ (`Prototype result: pass`)
- D2-specific checks ✅
  - evidence file set is exact and JSON-valid
  - `subject-token.json` absent
  - trace bundle records D2 tuple, divergence boundary, owner account
  - stage verdict records `stage_result = pass`
  - canonical D2 artifact records `D2-T`, `D2-L`, `D2-O`, and final `pass`

## Residual risk

- D2 localization currently depends on bounded inherited diagnostic strings (`reifyTypeWithNamedSetNoFallback => ...`, `applyInstantiation diagnostic failed`, `InstBot expects ⊥, got: t9 -> t9`). If those diagnostic texts are refactored without preserving semantics markers, D2 may degrade to `semantic-negative`/`inconclusive` despite unchanged underlying behavior.
