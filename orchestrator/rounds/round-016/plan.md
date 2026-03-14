# Round 016 Plan Delta (Roadmap Item 1: `P1` Subject-Discovery Prototype)

## Delta Status

This file is a same-round replan after the rejected review in `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/orchestrator/rounds/round-016/review.md`.

It replaces the earlier round plan wherever they conflict.

## Delta Objective

Repair only the review-blocking contract gaps in the existing `P1` implementation so that a rerun of the same bounded `URI-R2-C1` / `uri-r2-c1-only-v1` prototype lane emits design-conformant machine-readable evidence and a canonical subject token with the required repeated invocation metadata.

The repair target is one new authoritative rerun for the same round at:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/orchestrator/rounds/round-016/evidence/P1/attempt-2/`

Attempt `1` is historical evidence now. Do not overwrite or reinterpret it.

## Locked Boundaries

- Keep the round at `P1` only. Do not broaden into `P2`, `P3`, or `P4`.
- Keep the active subject fixed to `URI-R2-C1` only.
- Keep the active scenario fixed to `uri-r2-c1-only-v1` only.
- Keep the prototype path behind the same shared research entrypoint `uri-r2-c1-prototype-entrypoint-v1` only.
- Keep the same single-SCC, single-binder-family, non-equi-recursive, non-cyclic-graph, shared-entrypoint-isolation boundaries.
- Do not add a new executable, a new public API, a second research entrypoint, a new scenario, or any production-path dependency on research metadata.
- Do not revisit the already-passing isolation, execution-path, or default-path behavior except as required to keep them passing after the schema repair.
- Do not edit `orchestrator/state.json` or `orchestrator/roadmap.md`.

Rationale: the review rejected round `016` only because the emitted machine-readable evidence and canonical subject token are contract-incomplete. The next slice is therefore a schema-and-tests repair, not a new prototype feature.

## Primary Repair Files

Modify only these files unless a compile-only type ripple makes one additional local edit unavoidable:

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/src/MLF/Research/URI/R2/C1/Prototype/Types.hs`
2. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/src/MLF/Research/URI/R2/C1/Prototype/P1.hs`
3. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/test/Research/UriR2C1PrototypeP1Spec.hs`

Conditional-only edits:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs` only if the repaired types force a compile-alignment change.
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/docs/plans/2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md` only if the accepted rerun must update `Attempt: 2` or a file path reference. Its narrative shape already passed review; do not redesign it.

Files expected to remain untouched:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/app/Main.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/mlf2.cabal`
- every file under `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/src-public/`

## Sequential Repair Tasks

### Task 1 - Repair the internal `P1` evidence types to match the approved contract exactly

Target files:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/src/MLF/Research/URI/R2/C1/Prototype/Types.hs`

Required type changes:

- Expand the canonical subject token type so it can serialize every required field from the design spec:
  - `subject_id`
  - `subject_kind`
  - `subject_scope`
  - `provenance_anchor.origin_stage`
  - `provenance_anchor.candidate_id`
  - `provenance_anchor.candidate_inventory_ref`
  - `provenance_anchor.normalization_basis`
  - `provenance_anchor.discovery_trace_ref`
  - `owner_family_status.kind`
  - `owner_family_status.family_id`
  - `trace_handles`
- Add explicit internal types for the emitted machine-readable outputs that are currently ad hoc:
  - trace bundle
  - checker result
  - aggregated stage verdict
  - file-level metadata envelope for repeated invocation fields
- The repeated invocation metadata for every machine-readable output in this round must be:
  - `research_entrypoint_id: uri-r2-c1-prototype-entrypoint-v1`
  - `stage_selector: P1-subject-discovery`
  - `scenario_id: uri-r2-c1-only-v1`
  - `attempt_id: 2`
- Where the approved schema also requires `stage`, keep that field too and set it to `P1`. Do not substitute `stage_selector` for `stage`.

Out of scope:

- Do not change the candidate-discovery algorithm, normalization rule, admissibility rule, or stage-result logic unless a compile or serialization fix makes a tiny local adjustment unavoidable.

### Task 2 - Re-emit the `P1` machine-readable outputs with exact schema fields and exact repeated metadata

Target files:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/src/MLF/Research/URI/R2/C1/Prototype/P1.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/src/MLF/Research/URI/R2/C1/Prototype/Types.hs`

Required output contract for `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/orchestrator/rounds/round-016/evidence/P1/attempt-2/`:

- `trace-bundle.json`
  - must contain the approved trace-bundle fields:
    - `research_entrypoint_id`
    - `scenario_id`
    - `stage`
    - `attempt_id`
    - `correlation_id`
    - `subject_id`
    - `trace_refs`
  - must also repeat `stage_selector: P1-subject-discovery`
- `candidate-inventory.json`
  - must carry the repeated invocation metadata fields above
  - must serialize the normalized candidate inventory with one record per candidate using:
    - `candidate_id`
    - `candidate_kind`
    - `normalization_basis`
    - `admissibility_verdict`
    - `rejection_trigger`
- `candidate-selection-rule.json`
  - must carry the repeated invocation metadata fields above
  - must serialize `candidate_selection_rule` with:
    - `candidate_universe`
    - `normalization`
    - `admissibility_test`
    - `outcome`
- `check-P1-C.json`, `check-P1-N.json`, `check-P1-U.json`
  - each must carry the repeated invocation metadata fields above
  - each must satisfy the checker-result schema:
    - `check_id`
    - `subject_id`
    - `evidence_ref`
    - `verdict`
    - `rejection_trigger`
- `stage-verdict.json`
  - must carry the approved aggregated stage-verdict fields:
    - `research_entrypoint_id`
    - `scenario_id`
    - `stage`
    - `attempt_id`
    - `subject_token_ref`
    - `checker_results`
    - `stage_result`
    - `terminal_reason`
  - must also repeat `stage_selector: P1-subject-discovery`
- `subject-token.json`
  - present only when `stage_result` is `pass`
  - must serialize the full canonical token object, including:
    - `subject_id`
    - `subject_kind`
    - `subject_scope`
    - `provenance_anchor`
    - `owner_family_status`
    - `trace_handles`
  - must also repeat the round-required invocation metadata fields above

Required token discipline:

- `subject_id` must stay `uri-r2-c1/<candidate_id>`.
- `provenance_anchor.origin_stage` must stay `P1`.
- `provenance_anchor.candidate_inventory_ref` must point at the exact `attempt-2` inventory file.
- `provenance_anchor.normalization_basis` must be `cluster-equivalence-v1`.
- `owner_family_status.kind` at `P1` may remain `unknown`; if so, `family_id` must be `null`.
- If a discovery trace handle exists in `trace-bundle.json`, it must be repeated in `subject-token.json.trace_handles` and in `provenance_anchor.discovery_trace_ref`.
- If no discovery trace handle is persisted, `trace_refs` and `trace_handles` may be empty, but both fields must still exist.

Required rejection-vocabulary discipline:

- Do not invent new rejection-trigger values.
- Keep `P1-C`, `P1-N`, and `P1-U` limited to the allowed normalized vocabulary already required by the approved design and the current round.

Preservation rule:

- The repair should preserve the current `P1` semantic outcome if the bounded candidate analysis is unchanged.
- Do not change a current `pass` into a different result just to avoid emitting the missing fields.

### Task 3 - Tighten the tests so the rejected schema gaps cannot pass again

Target files:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/test/Research/UriR2C1PrototypeP1Spec.hs`

Required test additions:

- Add exact assertions for every previously missing field called out in review finding 1:
  - `trace-bundle.json` must include `correlation_id`, `subject_id`, and `trace_refs`
  - each `check-P1-*.json` must include `subject_id`, `evidence_ref`, and `verdict`
  - `stage-verdict.json` must include `attempt_id`, `subject_token_ref`, `checker_results`, and `terminal_reason`
- Add exact assertions for every previously missing field called out in review finding 2:
  - `subject-token.json` must include `owner_family_status` and `trace_handles`
  - `provenance_anchor` must include `candidate_id`, `normalization_basis`, and `discovery_trace_ref`
- Add exact assertions for repeated invocation metadata on every emitted machine-readable file:
  - `research_entrypoint_id`
  - `stage_selector`
  - `scenario_id`
  - `attempt_id`
  - for the two design-schema files that also require `stage`, assert `stage == "P1"`
- Make the tests fail if any emitted rejection trigger falls outside the approved normalized vocabulary for the relevant `P1` file.
- Keep the existing isolation and wrong-scenario or wrong-selector tests intact. They already protect the boundary and must keep passing.

Test-scope guardrail:

- Do not add new stage selectors, new scenario cases, or broader prototype coverage to the test suite.
- This is a contract-locking expansion of the existing `P1` tests only.

### Task 4 - Rerun `P1` as attempt `2` and keep the review surface narrow

Execution target:

- `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P1-subject-discovery --scenario-id uri-r2-c1-only-v1 --attempt-id 2`

Required rerun discipline:

- Preserve `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/orchestrator/rounds/round-016/evidence/P1/attempt-1/` unchanged as the rejected attempt.
- Write the repaired authoritative candidate files only under `attempt-2/`.
- If the docs artifact is updated, limit the change to `Attempt: 2` and the `attempt-2` evidence references only.
- Do not rewrite the round selection, the round review, or any predecessor artifact.

## Acceptance Criteria (All Required)

1. The only intended behavioral change is that the emitted `P1` machine-readable outputs and canonical subject token now satisfy the approved schema and repeated-metadata contract.
2. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/orchestrator/rounds/round-016/evidence/P1/attempt-1/` remains intact and `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/orchestrator/rounds/round-016/evidence/P1/attempt-2/` becomes the repair rerun target.
3. `trace-bundle.json`, every `check-P1-*.json`, `stage-verdict.json`, and `subject-token.json` include the fields that were missing in the rejected review.
4. Every machine-readable output produced for `attempt-2` repeats:
   - `research_entrypoint_id: uri-r2-c1-prototype-entrypoint-v1`
   - `stage_selector: P1-subject-discovery`
   - `scenario_id: uri-r2-c1-only-v1`
   - `attempt_id: 2`
5. `subject-token.json`, when present, matches the canonical token contract for `P1`, including `owner_family_status`, `trace_handles`, and the full `provenance_anchor`.
6. The repair does not widen candidate discovery, change the bounded scenario, introduce a second entrypoint, or alter the default no-argument `mlf2` path.
7. The test suite now fails if the machine-readable outputs regress to the incomplete shapes identified in the rejected review.
8. Diff stays narrowly focused on the schema-repair files plus the new `attempt-2` evidence and any minimal artifact pointer update.

## Reviewer And Verification Checks

Baseline checks from `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/orchestrator/verification.md` still apply:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '^\\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
- `cabal build all && cabal test`

Round-specific re-review checks:

- confirm the plan delta stayed narrow:
  - `git diff --name-only`
  - reviewer should expect no broadening beyond the primary repair files, optional compile-alignment edits, the `attempt-2` evidence directory, and any minimal `Attempt: 2` artifact update
- rerun the repaired prototype path:
  - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P1-subject-discovery --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
- verify the rejected attempt is preserved and the rerun is isolated:
  - `test -d orchestrator/rounds/round-016/evidence/P1/attempt-1`
  - `find orchestrator/rounds/round-016/evidence/P1/attempt-2 -maxdepth 1 -type f | sort`
- JSON-parse every repaired machine-readable file under `attempt-2` with `python3 -m json.tool`
- confirm the repaired files now expose the missing review fields:
  - reviewer should explicitly audit `trace-bundle.json`, `check-P1-C.json`, `check-P1-N.json`, `check-P1-U.json`, `stage-verdict.json`, and `subject-token.json` against the finding list in `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-016/orchestrator/rounds/round-016/review.md`
- confirm unchanged boundaries still hold:
  - wrong selector still rejects without writing evidence
  - wrong scenario still rejects without writing evidence
  - default `cabal run mlf2` path still works
  - no new executable or `src-public/` export was added

Reviewer decision rule:

- Approve only if the review blockers are resolved without widening the round beyond this schema-and-tests repair.
- Reject again if the repair changes the stage scope, rewrites attempt `1`, omits any required schema field, or leaves the repeated invocation metadata inconsistent across the emitted files.
