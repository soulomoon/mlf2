# Round 007 Plan (Roadmap Item 2: `R2` Bounded Subset Selection and Admissibility Contract)

## Round Objective

Deliver the smallest docs-only research slice that closes successor roadmap item 2 by writing one concrete `R2` selection artifact that chooses exactly one bounded unannotated candidate subset and states its admissibility contract, while preserving the approved `R1` -> `R5` staging and keeping the inherited item-2 invariant audit authoritative.

## Scope Mode (Strictly Docs-Only Research Artifact)

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not edit `orchestrator/rounds/round-007/state-snapshot.json` or `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-002/roadmap.md`.
- Do not rewrite predecessor packet history or prior round artifacts.
- Do not write the `R3` inference-obligation contract, the `R4` feasibility decision, or the `R5` implementation-handoff / research-stop artifact.

Rationale: the selected roadmap item is `R2` only, and the approved successor design requires one exact bounded-subset choice plus admissibility contract before any obligation-writing or feasibility work can proceed.

## Scope Guardrails

- Preserve the fixed boundary model throughout:
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - fail closed if the subset would require forbidden widening.
- Treat `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited invariant audit for acyclicity, binding-tree discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality risk boundaries.
- Treat `docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` as the narrowing input for this round, especially `G1` and `G2`; do not claim that `G3` through `G6` are solved in `R2`.
- Use completed rounds `001` through `006`, the approved successor design spec, and the predecessor recursive-types packet as inherited evidence only; do not reopen them as pending work.
- Keep the round bounded to docs/orchestrator/task-surface edits only.

## Exact Target Files

1. Create `docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md`
2. Create `orchestrator/rounds/round-007/implementation-notes.md`

## Sequential Tasks

### Task 1 - Write the `R2` bounded-subset selection artifact

Target file:
- `docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md`

Required contents:
- A short inherited-baseline section that cites:
  - accepted `R0` / `ARI-C1` as the starting point;
  - `docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` as the immediate predecessor artifact;
  - `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited invariant audit;
  - `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md` as the roadmap-design source.
- One explicit selection section that chooses exactly one bounded unannotated candidate subset and gives:
  - a stable `Candidate ID`;
  - a short human-readable name;
  - a concise problem statement explaining which unannotated `single-SCC` / `single-binder-family` cases the candidate is meant to admit.
- One admissibility-contract section, preferably as a table, that records for the chosen candidate:
  - the positive admission rules for the subset;
  - which `R1` gaps the rule is intended to narrow (`G1`, `G2`, and any carried-forward relation to `G3` through `G6`);
  - which inherited invariant classes remain authoritative constraints for that rule;
  - which cases stay outside the chosen subset even if they are still theoretically plausible later.
- The admissibility contract must make the following subset boundaries explicit:
  - obligation discovery must stay bounded enough to yield one stable candidate root or one stable local obligation cluster without explicit anchors;
  - every admitted obligation must remain inside one `single-SCC`;
  - every admitted obligation must remain inside one `single-binder-family`;
  - no cross-family SCC linking is admitted;
  - no implicit unfolding, equi-recursive reasoning, or cyclic structural graph encoding is admitted;
  - ambiguous, multi-root, multi-cluster, provenance-unstable, or heuristic-search-dependent cases fail closed.
- A deferred-alternatives section that names plausible but not-yet-selected unannotated designs that remain future possibilities without claiming they are admitted now.
- A rejected-alternatives section that names alternatives already disallowed by the fixed boundary model, including at minimum:
  - multi-SCC subsets;
  - cross-family SCC linking;
  - equi-recursive / implicit-unfolding designs;
  - cyclic structural graph encodings.
- A stage-discipline section that explicitly says:
  - this document selects the `R2` subset only;
  - this document does not write the `R3` obligation contract;
  - this document does not record an `R4` `feasible-continue` or `not-yet-go` outcome;
  - this document does not draft the `R5` handoff / research-stop artifact.
- A continuity section that states completed rounds and the predecessor recursive-types packet are cited as evidence only and are not rewritten.

### Task 2 - Record round implementation notes

Target file:
- `orchestrator/rounds/round-007/implementation-notes.md`

Required contents:
- Short summary of the selected `R2` subset artifact and why it satisfies successor roadmap item 2.
- Explicit statement that exactly one candidate subset was selected and all other alternatives were classified as deferred or rejected.
- Explicit statement that the inherited item-2 invariant audit remains authoritative and was not reopened.
- Explicit statement that this round is docs-only and introduced no production behavior changes.
- Explicit statement that `R3` through `R5` remain future stages and were not executed here.

## Acceptance Criteria (All Required)

1. `docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md` exists and selects exactly one bounded unannotated candidate subset.
2. The `R2` artifact gives that subset one stable identifier and one bounded admissibility contract rather than a vague design family.
3. The artifact explicitly preserves the fixed boundary model:
   - `single-SCC` only;
   - `single-binder-family` only;
   - no cross-family SCC linking;
   - non-equi-recursive only;
   - non-cyclic structural graph only;
   - fail-closed on ambiguous or widening-dependent cases.
4. The artifact explicitly records deferred alternatives separately from rejected alternatives.
5. The artifact keeps the inherited invariant audit authoritative by mapping contract clauses back to inherited invariant classes instead of re-auditing them from scratch.
6. The artifact explicitly preserves the approved `R1` -> `R5` staging by stopping at subset selection and avoiding any `R3`, `R4`, or `R5` output.
7. `orchestrator/rounds/round-007/implementation-notes.md` exists and records docs-only execution plus staging continuity.
8. Diff remains limited to planned docs/round artifacts and does not modify:
   - `orchestrator/rounds/round-007/state-snapshot.json`,
   - `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-002/roadmap.md`,
   - `src/`, `src-public/`, `app/`, `test/`,
   - `mlf2.cabal`,
   - predecessor packet/task history,
   - prior round artifacts.

## Reviewer And Verification Checks

Baseline checks (from `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-002/verification.md`):
- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-007/state-snapshot.json >/dev/null`
- `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-002/roadmap.md`
- `cabal build all && cabal test` is not required this round if the diff excludes `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`; reviewer must record that skip rationale explicitly.
- Reviewer-recorded continuity check against inherited evidence from completed rounds `001` through `006`, the approved successor design spec, and the predecessor recursive-types packet.

Round-specific checks:
- Docs-only boundary:
  - `git diff --name-only`
  - `git status --short`
  - `git ls-files --others --exclude-standard`
  - `git diff --name-only | rg '^(src/|src-public/|app/|test/|mlf2\\.cabal$)'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/state\\.json$'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/roadmap\\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/rounds/round-00[1-6]/'` (expected: no matches)
- Selection-artifact content markers present:
  - `rg -n 'R0|ARI-C1|R1|G1|G2|Candidate ID|admissibility|single-SCC|single-binder-family|cross-family|deferred|rejected|authoritative|R3|R4|R5' docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md orchestrator/rounds/round-007/implementation-notes.md`
- Exactly-one-candidate checks:
  - `rg -n '^Candidate ID:' docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md | wc -l` (expected: `1`)
  - `rg -n 'deferred|rejected' docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md`
- Stage-preservation checks:
  - `rg -n 'feasible-continue|not-yet-go|implementation-handoff' docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md` (expected: no matches)
- Continuity-reference checks:
  - `rg -n '2026-03-14-unannotated-iso-recursive-r1-gap-map|2026-03-14-automatic-recursive-inference-invariant-audit|2026-03-14-unannotated-iso-recursive-roadmap-design' docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md`

Review output requirements:
- Reviewer writes `orchestrator/rounds/round-007/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - explicit confirmation that this round stayed at `R2`, preserved the inherited invariant-audit authority, and did not widen beyond the fixed boundary model.
