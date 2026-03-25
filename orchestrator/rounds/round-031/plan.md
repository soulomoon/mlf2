# Round 031 Plan (`U4` Constructor-Directed / Acyclicity / Termination Clearance)

## Objective

Execute only roadmap item `U4` and produce one bounded feasibility-clearance artifact at:
`docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md`.

This round must record, for repaired `URI-R2-C1` only, whether the current constructor-directed, acyclic, terminating solver discipline can admit the live subject without equi-recursive reasoning, cyclic structural graphs, or weakened termination guarantees.

## Locked Round Context

- Round id: `round-031`
- Stage: `plan` for roadmap item `U4`
- Active attempt: `attempt-1` (fresh attempt; `retry: null`)
- Active subject (fixed): repaired `URI-R2-C1`
- Mandatory inherited boundary (fixed): `explicit-only / non-equi-recursive / non-cyclic-graph`
- Stage mode: bounded docs/evidence feasibility analysis only (no solver/pipeline implementation slice)

Inherited carry-forward facts that must remain unchanged:

- `U1` finalized in `round-028` and bound the successor lane to repaired `URI-R2-C1`.
- `U2` finalized in `round-029` with bounded result token `authority-narrowed`.
- `U3` finalized in `round-030` with bounded result token `uniqueness-owner-stable-refuted`.
- No widening is authorized to multi-SCC, cross-family search, broad automatic recursive inference, equi-recursive reasoning, cyclic structural encoding, second interfaces, fallback paths, compatibility shims, or default-on widening.

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-031/selection.md`
- `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-009/verification.md`
- `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-009/retry-subloop.md`
- `docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
- `docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`
- `docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md`
- `docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md`
- `orchestrator/rounds/round-028/review-record.json`
- `orchestrator/rounds/round-029/review-record.json`
- `orchestrator/rounds/round-030/review-record.json`
- `Bugs.md` (continuity-only reference)

## Files Expected In Scope

Primary writable file:

1. `docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md`

Optional bounded note file:

1. `orchestrator/rounds/round-031/implementation-notes.md` (only if needed for reviewer clarity)

Files that must remain untouched in this round:

- `orchestrator/rounds/round-031/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-009/roadmap.md`
- `orchestrator/rounds/round-031/selection.md`
- `Bugs.md`
- production/test surfaces under `src/`, `src-public/`, `app/`, `test/`, `mlf2.cabal`
- prior round artifacts under `orchestrator/rounds/round-001/` through `orchestrator/rounds/round-030/`

## Sequential Tasks

### Task 1 - Freeze `U4` attempt-1 contract and inherited boundary

- In the `U4` artifact, state explicitly: `attempt-1`, `retry: null`, and `U4` scope only.
- Reassert fixed live subject as repaired `URI-R2-C1` with no subject widening.
- Reassert inherited boundary unchanged: explicit-only baseline, non-equi-recursive semantics, non-cyclic structural graph encoding.
- Mark `U4` as feasibility/clearance evidence only, not implementation authorization.

### Task 2 - Define bounded `U4` admissibility rubric

- Build a reviewer-auditable `U4` rubric for repaired `URI-R2-C1` that requires all of:
  - constructor-directed local reasoning only;
  - acyclic structural graph requirements preserved;
  - no implicit unfolding and no equi-recursive equality/search;
  - no termination weakening, recursive chase broadening, or occurs-check relaxation;
  - no widened ownership/search scope beyond the bound subject.
- Make explicit that `U2` (`authority-narrowed`) and `U3` (`uniqueness-owner-stable-refuted`) are inherited facts, not rewrite targets in this round.
- Require one final bounded result token in the artifact:
  - `constructor-acyclic-termination-cleared`, or
  - `constructor-acyclic-termination-refuted`.

### Task 3 - Author the canonical `U4` feasibility artifact

- Write `docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md` with:
  - stage metadata (`Round`, `Roadmap item`, `Attempt`, `Retry state`, `Live subject`);
  - inherited boundary restatement and predecessor-result carry-forward (`U1`/`U2`/`U3`);
  - explicit `U4` admissibility rubric and bounded analysis for repaired `URI-R2-C1` only;
  - exactly one final `U4` result token (`constructor-acyclic-termination-cleared` or `constructor-acyclic-termination-refuted`) with rationale;
  - carry-forward implications for `U5` only, without preempting `U5`/`U6`.

### Task 4 - Run and record bounded verification for `U4`

- Run applicable baseline checks from `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-009/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-031/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-031/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-009/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-009/retry-subloop.md`
- Add `U4`-specific verification notes proving:
  - live subject remains repaired `URI-R2-C1` only;
  - inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary remains intact;
  - the artifact records one bounded constructor/acyclic/termination result and does not rely on implicit unfolding, equi-recursive reasoning, cyclic structural encoding, or termination weakening;
  - no second interface, compatibility shim, or fallback/default widening is introduced.
- Full Cabal gate handling:
  - do not run `cabal build all && cabal test` unless code/test surfaces are edited;
  - if docs-only scope holds, record explicit skip justification.

### Task 5 - Prepare reviewer handoff with retry-contract completeness

- Ensure reviewer can emit one legal `U4` retry-contract combination:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure reviewer record fields are fully supported by evidence:
  - `Implemented stage result`
  - `Attempt verdict`
  - `Stage action`
  - `Retry reason`
  - `Fix hypothesis`
- Preserve immutability rule: if later `U4` retries occur, attempt-1 artifacts remain unchanged and later attempts are additive.

## Non-Goals

- No widening beyond repaired `URI-R2-C1`.
- No broad automatic recursive inference authorization or implementation.
- No equi-recursive semantics, implicit unfolding success path, cyclic structural graph encoding, or termination-weakening allowance.
- No `U5` solver/pipeline implementation work and no `U6` aggregate decision work.
- No edits to `orchestrator/rounds/round-031/state-snapshot.json`, `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-009/roadmap.md`, `Bugs.md`, or prior-round history.
- No second executable interface, compatibility fallback, convenience shim, or default-on widening path.

## Reviewer Checks

Baseline checks from `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-009/verification.md` still apply.

Round-specific checks:

1. `plan.md` and produced `U4` artifact explicitly name `attempt-1` with `retry: null`.
2. The artifact keeps the live subject fixed to repaired `URI-R2-C1` and does not widen to other subjects/families.
3. The artifact preserves inherited boundary constraints: explicit-only, non-equi-recursive, non-cyclic-graph.
4. The artifact defines and applies a bounded constructor-directed / acyclicity / termination rubric without implicit unfolding, equi-recursive reasoning, cyclic encoding, or termination weakening.
5. The artifact records exactly one bounded `U4` result token (`constructor-acyclic-termination-cleared` or `constructor-acyclic-termination-refuted`) with evidence-backed rationale.
6. The artifact introduces no fallback, compatibility shim, second interface, or default-on widening behavior.
7. The round remains `U4`-only and does not preempt `U5` or `U6`.
8. Verification evidence includes baseline command outcomes and a clear full-gate skip note when scope remains docs-only.
