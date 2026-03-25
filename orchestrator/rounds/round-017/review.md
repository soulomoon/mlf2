# Round `round-017`

## Findings

- No blocking findings. The repaired `P2` round now matches the round delta and approved prototype-evidence design: witness replay diagnostics are recorded as bounded non-pass, `attempt-1` remains preserved as rejected history, and the admitted diff/isolation boundary holds.

## Baseline Checks

- `git diff --check`
  - pass.
- `python3 -m json.tool orchestrator/rounds/round-017/state-snapshot.json >/dev/null`
  - pass.
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-15-00-uri-r2-c1-prototype-evidence-successor-roadmap/rev-002/roadmap.md`
  - pass.
  - Evidence: roadmap items `1` through `4` remain parseable with statuses `done`, `pending`, `pending`, `pending`.
- `test -f docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
  - pass.
- `cabal build all && cabal test`
  - pass.
  - Evidence: `1110 examples, 0 failures`.

## Continuity Checks

- `git diff --name-only | rg '^orchestrator/rounds/round-(00[1-9]|01[0-6])/'`
  - pass.
  - Evidence: no matches.
- `git ls-files --others --exclude-standard | rg '^orchestrator/rounds/round-(00[1-9]|01[0-6])/'`
  - pass.
  - Evidence: no matches.
- `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'`
  - pass.
  - Evidence: no matches.
- `sed -n '1,160p' orchestrator/rounds/round-016/review-record.json`
  - pass.
  - Evidence: `P1` remains authoritative at attempt `2` with result `pass`; `P2`, `P3`, and `P4` remain `not-yet-run`.
- `rg -n 'not-yet-reopen|remain-stop|No rewriting of accepted \`RE1\` through \`RE5\` artifacts or predecessor packet history|controlling inherited artifact' docs/plans/2026-03-14-uri-r2-c1-re5-final-successor-recommendation.md docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
  - pass.
  - Evidence:
    - accepted `RE4` remains `not-yet-reopen`;
    - accepted `RE5` remains `remain-stop`;
    - the design still forbids rewriting accepted `RE1` through `RE5` or predecessor packet history;
    - the invariant audit remains the controlling inherited artifact for acyclicity, binding, reconstruction, reification, witness replay, and termination boundaries.

## Round-Specific Checks

- Audit trail preservation and rerun attempt:
  - Commands:
    - `find orchestrator/rounds/round-017/evidence/P2/attempt-1 -maxdepth 1 -type f | sort`
    - `shasum -a 256 orchestrator/rounds/round-017/evidence/P2/attempt-1/*`
    - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P2-provenance-preservation --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
    - `shasum -a 256 orchestrator/rounds/round-017/evidence/P2/attempt-1/*`
    - `find orchestrator/rounds/round-017/evidence/P2/attempt-2 -maxdepth 1 -type f | sort`
  - Result: pass.
  - Evidence:
    - `attempt-1` still contains the seven historical files `check-P2-G.json`, `check-P2-S.json`, `check-P2-R.json`, `check-P2-W.json`, `stage-verdict.json`, `subject-token.json`, and `trace-bundle.json`;
    - the seven `attempt-1` SHA-256 digests were identical before and after the rerun, so the rejected audit trail remained byte-stable;
    - the bounded rerun returned `Prototype result: semantic-negative`;
    - `attempt-2` now contains exactly the six allowed non-pass outputs `check-P2-G.json`, `check-P2-S.json`, `check-P2-R.json`, `check-P2-W.json`, `stage-verdict.json`, and `trace-bundle.json`, with no `subject-token.json`.

- Diff boundary and admitted `mlf2.cabal` change:
  - Commands:
    - `git diff --name-only`
    - `git diff -- src/MLF/Research/URI/R2/C1/Prototype/P1.hs`
    - `git diff --name-only | rg '^(src/MLF/Research/URI/R2/C1/Prototype/P1\.hs|orchestrator/state\.json|orchestrator/roadmap\.md|app/Main\.hs|src-public/|orchestrator/rounds/round-016/)'`
    - `nl -ba mlf2.cabal | sed -n '90,100p'`
  - Result: pass.
  - Evidence:
    - tracked diff paths are exactly `mlf2.cabal`, `src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs`, `src/MLF/Research/URI/R2/C1/Prototype/Entrypoint.hs`, `src/MLF/Research/URI/R2/C1/Prototype/Types.hs`, and `test/Research/UriR2C1PrototypeP1Spec.hs`;
    - `src/MLF/Research/URI/R2/C1/Prototype/P1.hs` has no diff;
    - no forbidden tracked path appears for `orchestrator/rounds/round-017/state-snapshot.json`, `orchestrator/roadmaps/2026-03-15-00-uri-r2-c1-prototype-evidence-successor-roadmap/rev-002/roadmap.md`, `orchestrator/rounds/round-016/`, `app/Main.hs`, or `src-public/`;
    - `mlf2.cabal` remains limited to the admitted one-line module registration at line `96`: `MLF.Research.URI.R2.C1.Prototype.P2`.

- `P2-W` classification honesty and design conformance:
  - Commands:
    - `python3 -m json.tool orchestrator/rounds/round-017/evidence/P2/attempt-1/check-P2-W.json`
    - `python3 -m json.tool orchestrator/rounds/round-017/evidence/P2/attempt-1/stage-verdict.json`
    - `python3 -m json.tool orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-W.json`
    - `python3 -m json.tool orchestrator/rounds/round-017/evidence/P2/attempt-2/stage-verdict.json`
    - `python3 -m json.tool orchestrator/rounds/round-017/evidence/P2/attempt-2/trace-bundle.json`
    - `python3 - <<'PY' ... shared subject/correlation audit ... PY`
    - `nl -ba src/MLF/Research/URI/R2/C1/Prototype/P2.hs | sed -n '288,312p'`
    - `nl -ba docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md | sed -n '1,80p'`
    - `nl -ba docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md | sed -n '178,235p'`
    - `nl -ba docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md | sed -n '520,549p'`
  - Result: pass.
  - Evidence:
    - `attempt-1` remains historical rejected evidence with `check-P2-W.verdict = "pass"`, `rejection_trigger = "none"`, and `stage_result = "pass"`, preserving the old mistaken classification as history only;
    - `attempt-2` now records the same `applyInstantiation` diagnostic as `check-P2-W.verdict = "semantic-negative"` with `rejection_trigger = "partial-replay"`;
    - `stage-verdict.json` for `attempt-2` records `stage_result = "semantic-negative"` and `subject_token_ref = null`;
    - `src/MLF/Research/URI/R2/C1/Prototype/P2.hs:293-308` now maps `applyInstantiation` failure to `semantic-negative` / `partial-replay`, which matches the round delta at `orchestrator/rounds/round-017/plan.md:63-68` and the design requirement that `P2-W` reject partial or repair-requiring replay at `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md:538-549`;
    - `trace-bundle.json` plus all four `check-P2-*` files repeat the inherited `subject_id` `uri-r2-c1/cluster-1` and the shared `correlation_id` `uri-r2-c1-only-v1-p2-attempt-2`, matching the authoritative `P1` token from `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`;
    - the canonical `P2` artifact now says `Attempt: 2`, reports `P2-W` as `semantic-negative`, records observed trigger `partial-replay`, and omits any `Next-Stage Handoff` section, satisfying the artifact versioning and non-pass requirements at `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md:178-191`.

- Shared-entrypoint isolation and unchanged outer behavior:
  - Commands:
    - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P3-safety-validation --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
    - `python3 - <<'PY' ... wrong-scenario no-write audit ... PY`
    - `cabal run mlf2`
    - `nl -ba src/MLF/Research/URI/R2/C1/Prototype/Entrypoint.hs | sed -n '44,68p'`
  - Result: pass.
  - Evidence:
    - `P3` still rejects with `UnsupportedStageSelector "P3-safety-validation"`;
    - the wrong-scenario run exits with return code `1`, reports `UnsupportedScenario "wrong-scenario"`, and leaves all `attempt-2` file digests unchanged;
    - the default executable path remains unchanged and returns `Type: ∀(a ⩾ ⊥) a -> a`;
    - the only prototype execution interface remains the shared research entrypoint dispatch in `src/MLF/Research/URI/R2/C1/Prototype/Entrypoint.hs:45-67`, with no second executable surface introduced.

## Stage Result

- Implemented stage result: `semantic-negative`.
- Reviewer-authoritative round result: approve.
- Reviewer-authoritative `P2` status: authoritative bounded non-pass at attempt `2`.
- Inherited continuity: `P1` remains authoritative from round `016`; `P3` and `P4` remain `not-yet-run`.

## Decision

- `approve`

## Evidence Summary

- The round satisfies the repaired round contract in `orchestrator/rounds/round-017/plan.md:21-23`, `orchestrator/rounds/round-017/plan.md:63-68`, and `orchestrator/rounds/round-017/plan.md:91-96`.
- It also remains consistent with the approved prototype-evidence design in `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md:178-191` and `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md:524-549`.
- `review-record.json` was written to mark `P1` authoritative from round `016`, `P2` authoritative at attempt `2` with result `semantic-negative`, and later stages as `not-yet-run`.
