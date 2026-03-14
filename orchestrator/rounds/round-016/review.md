# Round `round-016`

## Findings

- No blocking findings.
- Residual scope note: only `P1` ran in this round. `P2`, `P3`, and `P4` remain not yet run.

## Baseline Checks

- `git diff --check`
  - Result: pass.
- `python3 -m json.tool orchestrator/state.json >/dev/null`
  - Result: pass.
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - Result: pass.
  - Evidence: roadmap items `1` through `4` remain parseable and pending.
- `test -f docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
  - Result: pass.
- `cabal build all && cabal test`
  - Result: pass.
  - Evidence: `1108 examples, 0 failures`.
- Continuity check against inherited evidence
  - Commands:
    - `git diff --name-only | rg '^orchestrator/state\.json$|^orchestrator/roadmap\.md$|^orchestrator/rounds/round-00(1|2|3|4|5|6|7|8|9|10|11|12|13|14|15)/|^src-public/|^tasks/todo/2026-03-11-recursive-types-orchestration/'`
    - `rg -n 'not-yet-reopen|remain-stop|Completed rounds \`001\` through \`014\`|predecessor recursive-types packet' docs/plans/2026-03-14-uri-r2-c1-re5-final-successor-recommendation.md docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md orchestrator/rounds/round-015/review.md`
  - Result: pass.
  - Evidence:
    - the diff-continuity command returned no matches;
    - `RE4` remains `not-yet-reopen`;
    - `RE5` remains `remain-stop`;
    - the design spec still states the prototype lane does not rewrite accepted `RE1` through `RE5` or predecessor packet history.

## Task-Specific Checks

- Diff versus round plan and repair delta
  - Commands:
    - `git diff --name-only`
    - `git ls-files --others --exclude-standard`
  - Result: pass.
  - Evidence:
    - tracked edits remain confined to `app/Main.hs`, `mlf2.cabal`, `test/Main.hs`, and `test/RepoGuardSpec.hs`;
    - untracked additions remain confined to the round-016 packet, the bounded `P1` prototype modules, the `P1` spec, and the canonical `P1` artifact;
    - no `src-public/` change, no `orchestrator/state.json` change, no `orchestrator/roadmap.md` change, and no prior-round or predecessor-packet rewrite.

- Shared-entrypoint isolation and bounded scenario
  - Commands:
    - `rg -n '^executable ' mlf2.cabal`
    - `git diff --name-only | rg '^src-public/'`
    - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P1-subject-discovery --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
    - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P2-provenance-preservation --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
    - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P1-subject-discovery --scenario-id wrong-scenario --attempt-id 2`
    - `cabal run mlf2`
  - Result: pass.
  - Evidence:
    - `mlf2.cabal` still declares only `executable mlf2` and `executable frozen-parity-gen`;
    - the `src-public/` diff check returned no matches;
    - the bounded positive path returned `Prototype result: pass`;
    - the wrong-selector path failed with `UnsupportedStageSelector "P2-provenance-preservation"`;
    - the wrong-scenario path failed with `UnsupportedScenario "wrong-scenario"`;
    - the default path still returned `Type: ∀(a ⩾ ⊥) a -> a`.

- Attempt continuity and repair target
  - Commands:
    - `test -d orchestrator/rounds/round-016/evidence/P1/attempt-1 && find orchestrator/rounds/round-016/evidence/P1/attempt-1 -maxdepth 1 -type f | sort`
    - `find orchestrator/rounds/round-016/evidence/P1/attempt-2 -maxdepth 1 -type f | sort`
    - `sed -n '1,80p' docs/plans/2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md`
  - Result: pass.
  - Evidence:
    - `attempt-1/` still exists with the original eight JSON files and remains historical evidence;
    - `attempt-2/` contains the repaired eight JSON files and is the active rerun target;
    - the canonical artifact records `Attempt: 2` and points its evidence directory and handoff token at `attempt-2`.

- `P1` machine-readable schema and metadata contract
  - Commands:
    - `for f in orchestrator/rounds/round-016/evidence/P1/attempt-2/*.json; do python3 -m json.tool "$f" >/dev/null || exit 1; done`
    - `python3 - <<'PY' ... attempt-2 schema audit ... PY`
  - Result: pass.
  - Evidence:
    - every `attempt-2` JSON file parses successfully;
    - the schema audit confirmed the required repeated metadata on every machine-readable file:
      - `research_entrypoint_id: uri-r2-c1-prototype-entrypoint-v1`
      - `stage_selector: P1-subject-discovery`
      - `scenario_id: uri-r2-c1-only-v1`
      - `attempt_id: 2`
    - `trace-bundle.json` now carries `stage`, `correlation_id`, `subject_id`, and `trace_refs`;
    - each `check-P1-*.json` now carries `check_id`, `subject_id`, `evidence_ref`, `verdict`, and `rejection_trigger`;
    - `stage-verdict.json` now carries `stage`, `subject_token_ref`, `checker_results`, `stage_result`, and `terminal_reason`;
    - `subject-token.json` now carries the full canonical token contract, including `subject_scope`, complete `provenance_anchor`, `owner_family_status`, and `trace_handles`;
    - the canonical subject token remains `uri-r2-c1/cluster-1`, with `candidate_inventory_ref` bound to `attempt-2`, `normalization_basis` fixed to `cluster-equivalence-v1`, and discovery trace `trace://uri-r2-c1/p1/discovery-cluster-1`;
    - all observed rejection triggers remained inside the approved normalized vocabulary.

- Design and round-plan alignment
  - Commands:
    - `sed -n '250,390p' docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
    - `sed -n '460,520p' docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
    - `sed -n '1,260p' orchestrator/rounds/round-016/plan.md`
    - `sed -n '1,260p' orchestrator/rounds/round-016/implementation-notes.md`
  - Result: pass.
  - Evidence:
    - the repaired `attempt-2` outputs match the design spec’s canonical token, trace bundle, checker-result, stage-verdict, candidate-selection-rule, and candidate-inventory requirements for `P1`;
    - the round stayed at `P1`, kept `URI-R2-C1` and `uri-r2-c1-only-v1` fixed, and did not widen into `P2`, `P3`, or `P4`;
    - the repair remained a schema-and-tests rerun rather than a broadened prototype feature.

## Stage Result

- Implemented stage result: `pass`.
- Reviewer-authoritative stage result: `P1` is approved with authoritative attempt `2`.
- Later stages: `P2`, `P3`, and `P4` not yet run.

## Decision

- `approve`

## Evidence Summary

- The full baseline gate passed, including `cabal build all && cabal test`.
- The repaired `attempt-2` evidence now satisfies the approved `P1` schema and repeated-metadata contract that attempt `1` failed.
- Prototype execution remains isolated behind the shared research entrypoint, bounded to `URI-R2-C1` and `uri-r2-c1-only-v1`, with the default executable path unchanged and inherited history preserved.
