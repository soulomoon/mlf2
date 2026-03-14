# Round `round-018`

## Findings

- No blocking findings. The repaired `P3` round now matches the repair delta: direct `P1` token fallback is gone, `attempt-1` remains preserved as rejected history, and `attempt-2` records bounded non-pass strictly from authoritative `P2` no-token continuity.

## Baseline Checks

- `git diff --check`
  - pass.
- `python3 -m json.tool orchestrator/state.json >/dev/null`
  - pass.
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - pass.
  - Evidence: roadmap items `1` through `4` remain parseable with statuses `done`, `done`, `pending`, `pending`.
- `test -f docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
  - pass.
- `cabal build all && cabal test`
  - pass.
  - Evidence: `1112 examples, 0 failures`.

## Continuity Checks

- `git diff --name-only | rg '^orchestrator/rounds/round-(00[1-9]|01[0-7])/'`
  - pass.
  - Evidence: no matches.
- `git ls-files --others --exclude-standard | rg '^orchestrator/rounds/round-(00[1-9]|01[0-7])/'`
  - pass.
  - Evidence: no matches.
- `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'`
  - pass.
  - Evidence: no matches.
- `rg -n 'not-yet-reopen|remain-stop|No rewriting of accepted \`RE1\` through \`RE5\` artifacts or predecessor packet history' docs/plans/2026-03-14-uri-r2-c1-re5-final-successor-recommendation.md docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
  - pass.
  - Evidence: accepted `RE4` remains `not-yet-reopen`, accepted `RE5` remains `remain-stop`, and the design still forbids rewriting accepted predecessor artifacts.

## Round-Specific Checks

- Repair-delta contract removal of direct `P1` token fallback:
  - Commands:
    - `rg -n 'p1AuthoritativeSubjectTokenRelativePath|round-016/evidence/P1/attempt-2/subject-token.json' src/MLF/Research/URI/R2/C1/Prototype/P3.hs src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs`
    - `nl -ba src/MLF/Research/URI/R2/C1/Prototype/P3.hs | sed -n '44,203p'`
    - `nl -ba src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs | sed -n '154,223p'`
    - `nl -ba docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md | sed -n '386,394p'`
    - `nl -ba docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md | sed -n '519,522p'`
  - Result: pass.
  - Evidence:
    - the contract-violating `P1` token path no longer appears in `P3.hs` or `Artifact.hs`;
    - `P3.hs` now validates only the authoritative `P2` review record plus the absence of `orchestrator/rounds/round-017/evidence/P2/attempt-2/subject-token.json`;
    - the artifact now states that `P3` consumes only authoritative `P2` handoff continuity and records bounded non-pass without fallback to `P1`;
    - this matches the design rule that `P3` may consume only the token emitted or reaffirmed by `P2`, and that no `P3` token handoff exists when authoritative `P2` is non-pass.

- Attempt preservation and repaired rerun:
  - Commands:
    - `find orchestrator/rounds/round-018/evidence/P3/attempt-1 -maxdepth 1 -type f | sort`
    - `find orchestrator/rounds/round-018/evidence/P3/attempt-2 -maxdepth 1 -type f | sort`
    - `python3 -m json.tool` over all `attempt-2` JSON outputs
    - `test ! -f orchestrator/rounds/round-018/evidence/P3/attempt-2/subject-token.json`
    - `python3 -m json.tool orchestrator/rounds/round-018/evidence/P3/attempt-2/stage-verdict.json`
    - `python3 -m json.tool orchestrator/rounds/round-018/evidence/P3/attempt-2/trace-bundle.json`
  - Result: pass.
  - Evidence:
    - `attempt-1` remains present as rejected history with the same six non-pass files;
    - `attempt-2` contains exactly `check-P3-S.json`, `check-P3-A.json`, `check-P3-B.json`, `check-P3-C.json`, `stage-verdict.json`, and `trace-bundle.json`;
    - all `attempt-2` machine-readable outputs parse as JSON;
    - `attempt-2` emits no `subject-token.json`;
    - `stage-verdict.json` records `stage_result = "semantic-negative"` with `subject_token_ref = null`;
    - `trace-bundle.json` records `subject_id = null` and shared `correlation_id = "uri-r2-c1-only-v1-p3-attempt-2"`, matching the repair delta for no-token continuity.

- Shared-entrypoint isolation and no-write rejection audit:
  - Commands:
    - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P3-safety-validation --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
    - wrong-scenario rerun with pre/post SHA-256 digest comparison of `attempt-2`
    - wrong-stage rerun with pre/post SHA-256 digest comparison of `attempt-2`
    - `cabal run mlf2`
  - Result: pass.
  - Evidence:
    - bounded `P3` rerun returns `Prototype result: semantic-negative`;
    - wrong-scenario rejects with `UnsupportedScenario "wrong-scenario"` and leaves `attempt-2` digests unchanged;
    - wrong-stage rejects with `UnsupportedStageSelector "wrong-stage"` and leaves `attempt-2` digests unchanged;
    - default executable path remains `Type: ∀(a ⩾ ⊥) a -> a`.

- Diff boundary and inherited-authority audit:
  - Commands:
    - `git diff --name-only`
    - `python3 -m json.tool orchestrator/rounds/round-016/review-record.json`
    - `python3 -m json.tool orchestrator/rounds/round-017/review-record.json`
    - `sed -n '1,220p' docs/plans/2026-03-15-uri-r2-c1-p3-safety-validation-prototype.md`
  - Result: pass.
  - Evidence:
    - tracked diff paths remain `mlf2.cabal`, `src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs`, `src/MLF/Research/URI/R2/C1/Prototype/Entrypoint.hs`, `src/MLF/Research/URI/R2/C1/Prototype/Types.hs`, and `test/Research/UriR2C1PrototypeP1Spec.hs`, with the expected new `P3` files and round artifacts untracked in the round worktree;
    - no forbidden tracked path appears for `orchestrator/state.json`, `orchestrator/roadmap.md`, `app/Main.hs`, or `src-public/*`;
    - inherited authority remains `P1` authoritative at attempt `2` with result `pass` and `P2` authoritative at attempt `2` with result `semantic-negative`;
    - the repaired `P3` artifact now cites only `P2` handoff continuity in its stage input interface and records bounded non-pass without widening beyond the approved lane.

## Stage Result

- Implemented stage result: `semantic-negative`.
- Reviewer-authoritative round result: approve.
- Reviewer-authoritative `P3` status: authoritative bounded non-pass at attempt `2`.
- Inherited continuity: `P1` remains authoritative from round `016`; `P2` remains authoritative from round `017`; `P4` remains `not-yet-run`.

## Decision

- `approve`

## Evidence Summary

- The repaired round satisfies the round delta in `orchestrator/rounds/round-018/plan.md`, especially the removal of direct `P1` token consumption and the requirement that `P3` derive its bounded non-pass only from authoritative `P2` no-token continuity.
- It remains consistent with the approved prototype-evidence design in `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`.
- `review-record.json` is written to mark `P1` authoritative from round `016`, `P2` authoritative from round `017`, `P3` authoritative at attempt `2` with result `semantic-negative`, and `P4` as `not-yet-run`.
