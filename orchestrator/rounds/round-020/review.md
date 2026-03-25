# Round `round-020` Attempt `1`

- Baseline checks:
  - `git diff --check` -> pass.
  - `python3 -m json.tool orchestrator/rounds/round-020/state-snapshot.json >/dev/null` -> pass.
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-020/state-snapshot.json` -> pass (`contract_version: 2`, `retry: null` present).
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/roadmap.md` -> pass (ordered parseable roadmap items present).
  - `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md` -> pass.
  - `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md` -> pass.
  - `test -f orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/retry-subloop.md` -> pass.
  - `cabal build all && cabal test` -> pass (`1117 examples, 0 failures`; no post-success Cabal log-permission failure occurred in this reviewer run).
  - Continuity guards:
    - `git diff --name-only | rg '^orchestrator/rounds/round-(00[1-9]|01[0-9])/' || true` -> pass (no matches).
    - `git ls-files --others --exclude-standard | rg '^orchestrator/rounds/round-(00[1-9]|01[0-9])/' || true` -> pass (no matches).
    - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/' || true` -> pass (no matches).
    - `git ls-files --others --exclude-standard | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/' || true` -> pass (no matches).

- Task-specific checks:
  - Required D1 invocation:
    - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-p2-replay-root-cause-v1 --stage-selector D1-replay-reproduction --scenario-id uri-r2-c1-only-v1 --attempt-id 1` -> pass (`Prototype result: pass`).
  - Default path stability:
    - `cabal run mlf2` -> pass (`Type: ∀(a ⩾ ⊥) a -> a`).
  - Attempt-1 output set and schema:
    - `ls -la orchestrator/rounds/round-020/evidence/D1/attempt-1` -> pass (exactly `check-D1-I.json`, `check-D1-R.json`, `check-D1-M.json`, `stage-verdict.json`, `trace-bundle.json`).
    - JSON parse sweep over all attempt-1 `*.json` -> pass.
    - `test ! -f orchestrator/rounds/round-020/evidence/D1/attempt-1/subject-token.json` -> pass.
  - D1 continuity and signature boundary:
    - `check-D1-I.json` confirms inherited continuity fields for authoritative `round-016/P1 attempt-2` subject and `round-017/P2 attempt-2` failure boundary (`partial-replay`, `InstBot expects ⊥, got: t9 -> t9`).
    - `check-D1-R.json` records `classification=exact-bounded-replay-failure` with replay diagnostic details.
    - `check-D1-M.json` explicitly compares target vs observed trigger/mismatch and reports continuity `ok`.
    - `stage-verdict.json` stage result is `pass` (allowed set: `pass|semantic-negative|inconclusive`).
  - Rejection-path non-mutation checks:
    - Wrong scenario:
      - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-p2-replay-root-cause-v1 --stage-selector D1-replay-reproduction --scenario-id uri-r2-c1-other-v1 --attempt-id 1` -> `UnsupportedScenario "uri-r2-c1-other-v1"`.
    - Wrong stage:
      - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-p2-replay-root-cause-v1 --stage-selector D2-mismatch-localization --scenario-id uri-r2-c1-only-v1 --attempt-id 1` -> `UnsupportedStageSelector "D2-mismatch-localization"`.
    - Wrong entrypoint:
      - `cabal run mlf2 -- --research-entrypoint wrong-entrypoint --stage-selector D1-replay-reproduction --scenario-id uri-r2-c1-only-v1 --attempt-id 1` -> `UnsupportedResearchEntrypoint "wrong-entrypoint"`.
    - Out-of-range attempt:
      - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-p2-replay-root-cause-v1 --stage-selector D1-replay-reproduction --scenario-id uri-r2-c1-only-v1 --attempt-id 101` -> `UnsupportedAttemptId 101`.
    - Evidence immutability proof:
      - SHA-256 hashes for all attempt-1 JSON files were identical before and after rejection commands.
  - Forbidden-widening and controller-file checks:
    - `git diff --name-only` and `git status --short --untracked-files=all` show no edits to `orchestrator/rounds/round-020/state-snapshot.json`, `orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/roadmap.md`, `src-public/`, or historical rounds `round-016` through `round-019`.

- Implemented stage result: `pass`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `none`

- Decision summary:
  - `D1` attempt 1 satisfies the bounded replay-reproduction contract under the required entrypoint/stage/scenario tuple, reproduces the authoritative `P2-W` mismatch in the bounded replay lane, and preserves scope/authority boundaries with no forbidden widening.

- Evidence summary:
  - Attempt-local evidence is complete and valid, rejection paths fail fast without mutating `attempt-1`, no `subject-token.json` was emitted, default runtime behavior is unchanged, and reviewer verification supports `accepted + finalize`.
