# Round 029 Plan (`U2` Provenance-Stable Unannotated Authority Clearance)

## Objective

Execute only roadmap item `U2` and produce one bounded authority artifact at:
`docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md`.

This round must decide, for repaired `URI-R2-C1` only, whether provenance-stable unannotated authority is cleared or sharply narrowed, without widening the live subject or relaxing inherited boundaries.

## Locked Round Context

- Round id: `round-029`
- Stage: `plan` for roadmap item `U2`
- Active attempt: `attempt-1` (fresh attempt; `retry: null`)
- Active subject (fixed): repaired `URI-R2-C1`
- Mandatory inherited boundary (fixed): `explicit-only / non-equi-recursive / non-cyclic-graph`
- Stage mode: bounded docs/evidence authority analysis only (no solver/pipeline implementation slice)

Inherited carry-forward facts that must remain unchanged:

- `U1` is finalized as accepted in `round-028` (`accepted + finalize`) and binds the successor lane to repaired `URI-R2-C1`.
- Broad automatic recursive inference remains unresolved/disabled under inherited baseline contracts.
- No widening to multi-SCC, cross-family search, equi-recursive reasoning, cyclic graph encoding, fallback paths, or second interfaces is authorized in `U2`.

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-029/selection.md`
- `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-007/verification.md`
- `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-007/retry-subloop.md`
- `orchestrator/rounds/round-028/review-record.json`
- `docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`
- `Bugs.md` (continuity-only reference)

## Files Expected In Scope

Primary writable file:

1. `docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md`

Optional bounded note file:

1. `orchestrator/rounds/round-029/implementation-notes.md` (only if needed for reviewer clarity)

Files that must remain untouched in this round:

- `orchestrator/rounds/round-029/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-007/roadmap.md`
- `orchestrator/rounds/round-029/selection.md`
- `Bugs.md`
- production/test surfaces under `src/`, `src-public/`, `app/`, `test/`, `mlf2.cabal`
- prior round artifacts under `orchestrator/rounds/round-001/` through `orchestrator/rounds/round-028/`

## Sequential Tasks

### Task 1 - Freeze `U2` attempt-1 contract and inherited boundaries

- In the `U2` artifact, state explicitly: `attempt-1`, `retry: null`, and `U2` scope only.
- Reassert fixed live subject as repaired `URI-R2-C1` with no subject widening.
- Reassert inherited boundary unchanged: explicit-only baseline, non-equi-recursive semantics, non-cyclic structural graph encoding.
- Mark `U2` as authority-clearance evidence only, not implementation authorization.

### Task 2 - Consolidate controlling provenance obligations for repaired `URI-R2-C1`

- Synthesize only the provenance-related blocker chain from inherited documents:
  - `R5` unresolved `URI-R3-O4` authority gap,
  - repaired-lane continuity from `R4`,
  - successor `U1` bind constraints from `round-028`.
- Define reviewer-auditable acceptance criteria for `U2` as exactly one bounded result:
  - `authority-cleared` (for repaired `URI-R2-C1` only), or
  - `authority-narrowed` (precise remaining blocker for repaired `URI-R2-C1` only).
- Require explicit evidence that no authority is manufactured via fallback, compatibility shim, heuristic ranking, or late repair outside accepted provenance.

### Task 3 - Author the canonical `U2` authority artifact

- Write `docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md` with:
  - stage metadata (`Round`, `Roadmap item`, `Attempt`, `Retry state`, `Live subject`);
  - inherited-boundary restatement from `U1` and predecessor decisions;
  - bounded provenance chain for the repaired live subject only;
  - final `U2` result token (`authority-cleared` or `authority-narrowed`) plus rationale;
  - explicit carry-forward implications for `U3` without preempting `U3`.
- Keep the document docs-only and evidence-only; do not include production patch planning.

### Task 4 - Run and record bounded verification for `U2`

- Run applicable baseline checks from `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-007/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-029/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-029/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-007/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-007/retry-subloop.md`
- Add `U2`-specific verification notes proving:
  - live subject remains repaired `URI-R2-C1` only;
  - inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary remains intact;
  - the `U2` outcome is bounded (`authority-cleared` or `authority-narrowed`) and does not invent authority via fallback or late repair.
- Full Cabal gate handling:
  - do not run `cabal build all && cabal test` unless code/test surfaces are edited;
  - if docs-only scope holds, record explicit skip justification.

### Task 5 - Prepare reviewer handoff with retry-contract completeness

- Ensure reviewer can emit one legal `U2` retry-contract combination:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure reviewer record fields are fully supported by evidence:
  - `Implemented stage result`
  - `Attempt verdict`
  - `Stage action`
  - `Retry reason`
  - `Fix hypothesis`
- Preserve immutability rule: if later `U2` retries occur, attempt-1 artifacts remain unchanged and later attempts are additive.

## Non-Goals

- No widening beyond repaired `URI-R2-C1`.
- No broad automatic recursive inference authorization or implementation.
- No equi-recursive semantics, implicit unfolding success path, or cyclic structural graph encoding.
- No uniqueness/owner-stability clearance (`U3`) and no feasibility/termination clearance (`U4`) beyond strict carry-forward notes.
- No `U5` implementation planning or `U6` aggregate decision work.
- No edits to `orchestrator/rounds/round-029/state-snapshot.json`, `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-007/roadmap.md`, `Bugs.md`, or prior-round history.
- No second executable interface, compatibility fallback, convenience shim, or default-on widening path.

## Reviewer Checks

Baseline checks from `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-007/verification.md` still apply.

Round-specific checks:

1. `plan.md` and produced `U2` artifact explicitly name `attempt-1` with `retry: null`.
2. The artifact keeps the live subject fixed to repaired `URI-R2-C1` and does not widen to other subjects/families.
3. The artifact preserves inherited boundary constraints: explicit-only, non-equi-recursive, non-cyclic-graph.
4. The artifact records exactly one bounded `U2` result (`authority-cleared` or `authority-narrowed`) with evidence-backed reasoning.
5. The artifact demonstrates no manufactured authority via fallback path, compatibility shim, heuristic ranking, or late repair.
6. The round remains `U2`-only and does not preempt `U3` through `U6`.
7. Verification evidence includes baseline command outcomes and a clear full-gate skip note when scope remains docs-only.
