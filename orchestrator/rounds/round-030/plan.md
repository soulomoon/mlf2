# Round 030 Plan (`U3` Uniqueness And Owner-Stability Clearance)

## Objective

Execute only roadmap item `U3` and produce one bounded uniqueness/owner-stability artifact at:
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md`.

This round must record, for repaired `URI-R2-C1` only, whether there is exactly one admissible local root/cluster with stable ownership and no heuristic ranking.

## Locked Round Context

- Round id: `round-030`
- Stage: `plan` for roadmap item `U3`
- Active attempt: `attempt-1` (fresh attempt; `retry: null`)
- Active subject (fixed): repaired `URI-R2-C1`
- Mandatory inherited boundary (fixed): `explicit-only / non-equi-recursive / non-cyclic-graph`
- Stage mode: bounded docs/evidence uniqueness-owner analysis only (no solver/pipeline implementation slice)

Inherited carry-forward facts that must remain unchanged:

- `U1` finalized in `round-028` and bound the successor lane to repaired `URI-R2-C1`.
- `U2` finalized in `round-029` with bounded result token `authority-narrowed`.
- No widening is authorized to multi-SCC, cross-family search, equi-recursive reasoning, cyclic structural graph encoding, second interfaces, fallback paths, compatibility shims, or default-on broad automatic recursive inference.

## Authoritative Inputs To Preserve

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-030/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-028/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-029/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` (continuity-only reference)

## Files Expected In Scope

Primary writable file:

1. `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md`

Optional bounded note file:

1. `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-030/implementation-notes.md` (only if needed for reviewer clarity)

Files that must remain untouched in this round:

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-030/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`
- production/test surfaces under `/Users/ares/.codex/worktrees/d432/mlf4/src/`, `/Users/ares/.codex/worktrees/d432/mlf4/src-public/`, `/Users/ares/.codex/worktrees/d432/mlf4/app/`, `/Users/ares/.codex/worktrees/d432/mlf4/test/`, `/Users/ares/.codex/worktrees/d432/mlf4/mlf2.cabal`
- prior round artifacts under `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-001/` through `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-029/`

## Sequential Tasks

### Task 1 - Freeze `U3` attempt-1 contract and inherited boundary

- In the `U3` artifact, state explicitly: `attempt-1`, `retry: null`, and `U3` scope only.
- Reassert fixed live subject as repaired `URI-R2-C1` with no subject widening.
- Reassert inherited boundary unchanged: explicit-only baseline, non-equi-recursive semantics, non-cyclic structural graph encoding.
- Mark `U3` as uniqueness/owner-stability evidence only, not implementation authorization.

### Task 2 - Define the bounded `U3` admissibility and decision rubric

- Consolidate only the evidence needed from `U1` and `U2` to evaluate uniqueness and ownership stability for repaired `URI-R2-C1`.
- Define admissibility criteria for a local root/cluster candidate that are reviewer-auditable and non-heuristic.
- Require an explicit no-ranking rule: if multiple competing candidates remain, the result must be treated as not cleared rather than ranked heuristically.
- Require one final bounded `U3` result token in the artifact:
  - `uniqueness-owner-stable-cleared`, or
  - `uniqueness-owner-stable-refuted`.

### Task 3 - Author the canonical `U3` uniqueness/owner artifact

- Write `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md` with:
  - stage metadata (`Round`, `Roadmap item`, `Attempt`, `Retry state`, `Live subject`);
  - inherited-boundary restatement from `U1`/`U2`;
  - bounded uniqueness/owner analysis for repaired `URI-R2-C1` only;
  - explicit statement of whether one admissible local root/cluster exists with stable ownership and no heuristic ranking;
  - exactly one final `U3` result token (`uniqueness-owner-stable-cleared` or `uniqueness-owner-stable-refuted`) plus rationale;
  - carry-forward implications for `U4` only, without preempting `U4` through `U6`.

### Task 4 - Run and record bounded verification for `U3`

- Run applicable baseline checks from `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f orchestrator/retry-subloop.md`
- Add `U3`-specific verification notes proving:
  - live subject remains repaired `URI-R2-C1` only;
  - inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary remains intact;
  - the artifact records exactly one admissible uniqueness/owner result and uses no heuristic ranking;
  - no fallback/compatibility path or hidden widening is introduced.
- Full Cabal gate handling:
  - do not run `cabal build all && cabal test` unless code/test surfaces are edited;
  - if docs-only scope holds, record explicit skip justification.

### Task 5 - Prepare reviewer handoff with retry-contract completeness

- Ensure reviewer can emit one legal `U3` retry-contract combination:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure reviewer record fields are fully supported by evidence:
  - `Implemented stage result`
  - `Attempt verdict`
  - `Stage action`
  - `Retry reason`
  - `Fix hypothesis`
- Preserve immutability rule: if later `U3` retries occur, attempt-1 artifacts remain unchanged and later attempts are additive.

## Non-Goals

- No widening beyond repaired `URI-R2-C1`.
- No broad automatic recursive inference authorization or implementation.
- No equi-recursive semantics, implicit unfolding success path, or cyclic structural graph encoding.
- No `U4` feasibility/termination clearance work, no `U5` implementation planning, and no `U6` aggregate decision work.
- No edits to `orchestrator/state.json`, `orchestrator/roadmap.md`, `Bugs.md`, or prior-round history.
- No second executable interface, compatibility fallback, convenience shim, or default-on widening path.

## Reviewer Checks

Baseline checks from `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md` still apply.

Round-specific checks:

1. `plan.md` and produced `U3` artifact explicitly name `attempt-1` with `retry: null`.
2. The artifact keeps the live subject fixed to repaired `URI-R2-C1` and does not widen to other subjects/families.
3. The artifact preserves inherited boundary constraints: explicit-only, non-equi-recursive, non-cyclic-graph.
4. The artifact records whether exactly one admissible local root/cluster exists with stable ownership and no heuristic ranking.
5. The artifact records exactly one bounded `U3` result token (`uniqueness-owner-stable-cleared` or `uniqueness-owner-stable-refuted`) with evidence-backed rationale.
6. The artifact introduces no fallback, compatibility shim, second interface, or default-on widening behavior.
7. The round remains `U3`-only and does not preempt `U4` through `U6`.
8. Verification evidence includes baseline command outcomes and a clear full-gate skip note when scope remains docs-only.
