# Round 008 Plan (Roadmap Item 3: `R3` Inference-Obligation Contract)

## Round Objective

Deliver the smallest docs-only research slice that closes successor roadmap item 3 by writing one concrete `R3` inference-obligation contract for the already-selected subset `URI-R2-C1`, while preserving the approved `R1` -> `R5` staging and keeping the inherited item-2 invariant audit authoritative.

## Scope Mode (Strictly Docs-Only Research Artifact)

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not edit `orchestrator/rounds/round-008/state-snapshot.json` or `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-003/roadmap.md`.
- Do not rewrite predecessor packet history or prior round artifacts.
- Do not reselect the `R2` subset, do not execute the `R4` feasibility decision, and do not draft the `R5` implementation-handoff / research-stop artifact.

Rationale: the selected roadmap item is `R3` only, and the approved successor design requires the chosen `R2` subset to be turned into an explicit obligation contract before any bounded feasibility judgment can be made.

## Scope Guardrails

- Preserve the fixed boundary model throughout:
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - fail closed if any obligation would require forbidden widening.
- Treat `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited invariant audit for acyclicity, binding-tree discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality risk boundaries.
- Treat `docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` and `docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md` as authoritative narrowing inputs; do not claim that `R3` reopens `R1` gap discovery or `R2` subset selection.
- Keep the chosen subject fixed to `URI-R2-C1` and preserve its inherited bounded meaning: unique local obligation root or equivalent local obligation cluster, one closed binder-family cluster, no heuristic tie-breaking, and no provenance-unstable admission.
- Use completed rounds `001` through `007`, the approved successor design spec, and the predecessor recursive-types packet as inherited evidence only; do not reopen them as pending work.
- Keep the round bounded to docs/orchestrator/task-surface edits only.

## Exact Target Files

1. Create `docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`
2. Create `orchestrator/rounds/round-008/implementation-notes.md`

## Sequential Tasks

### Task 1 - Write the `R3` inference-obligation contract artifact

Target file:
- `docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`

Required contents:
- A short inherited-baseline section that cites:
  - accepted `R0` / `ARI-C1` as the starting point;
  - `docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` as the gap-map predecessor;
  - `docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md` as the chosen-subset predecessor, naming `URI-R2-C1` explicitly;
  - `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited invariant audit;
  - `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md` as the roadmap-design source.
- A fixed-boundary section that restates, without weakening, the mandatory boundary phrases:
  - `URI-R2-C1` remains `single-SCC` and `single-binder-family` only;
  - recursion remains obligation-level rather than cyclic structural-graph encoding;
  - no cross-family SCC linking is admitted;
  - no equi-recursive reasoning or implicit unfolding is admitted;
  - no default-on widening is admitted.
- One obligation matrix or equivalent structured list that defines the exact obligations `URI-R2-C1` must satisfy. Each obligation entry must include:
  - a stable obligation ID;
  - the bounded subject inside `URI-R2-C1`;
  - the hard contract statement that must hold;
  - the authoritative inherited invariant class or classes that constrain it;
  - the fail-closed rejection condition if the obligation is not met;
  - the future `R4` evidence shape needed to judge that obligation.
- The obligation contract must cover, at minimum:
  - acyclicity obligations proving the structural `TyNode` / constraint graph stays acyclic even though obligation-level recursion may form one SCC;
  - binder ownership and scope-discipline obligations proving every admitted obligation stays inside one stable binder family with deterministic parent-chain ownership;
  - occurs-check and termination obligations proving constructor-directed reasoning remains sufficient and no implicit unfolding or speculative recursive equality search is introduced;
  - reconstruction, reification, and witness replay obligations proving one provenance-stable root/cluster can flow through generalization, reification, and replay without widening or ambiguity;
  - principality-risk boundary obligations proving admission remains unique, bounded, and fail-closed rather than heuristic or silently broadened.
- One explicit fail-closed rejection section that groups the classes of cases that must remain rejected at `R3`, including at minimum:
  - ambiguous or multi-root obligation discovery;
  - multi-cluster or multi-SCC dependency shapes;
  - mixed-owner or cross-family ownership claims;
  - implicit-unfolding or equi-recursive pressure;
  - cyclic structural-graph encodings;
  - provenance-unstable reconstruction or replay subjects;
  - any case that would require widening beyond the fixed boundary model.
- One future-feasibility handoff section that states, in words rather than implementation steps, what `R4` will have to check for each obligation class:
  - positive example classes;
  - negative example classes;
  - no-go triggers;
  - immediate stop conditions.
- A stage-discipline section that explicitly says:
  - this document writes the `R3` obligation contract only;
  - this document does not reselect `URI-R2-C1` or admit a new candidate subset;
  - this document does not record an `R4` `feasible-continue` or `not-yet-go` outcome;
  - this document does not draft the `R5` handoff / research-stop artifact.
- A continuity section that states completed rounds and the predecessor recursive-types packet are cited as evidence only and are not rewritten.

### Task 2 - Record round implementation notes

Target file:
- `orchestrator/rounds/round-008/implementation-notes.md`

Required contents:
- Short summary of the `R3` obligation-contract artifact and why it satisfies successor roadmap item 3.
- Explicit statement that `URI-R2-C1` remained the fixed chosen subset and was not reselected or widened.
- Explicit statement that the inherited item-2 invariant audit remains authoritative and was not reopened.
- Explicit statement that this round is docs-only and introduced no production behavior changes.
- Explicit statement that `R4` and `R5` remain future stages and were not executed here.

## Acceptance Criteria (All Required)

1. `docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md` exists and defines an explicit obligation contract for `URI-R2-C1`.
2. The `R3` artifact preserves the fixed boundary model:
   - `single-SCC` obligation-level recursion only;
   - `single-binder-family` ownership only;
   - no cross-family SCC linking;
   - non-equi-recursive only;
   - non-cyclic structural graph only;
   - fail-closed treatment of ambiguous or widening-dependent cases.
3. The artifact maps every obligation back to inherited invariant classes instead of re-auditing them from scratch.
4. The artifact distinguishes:
   - what must hold now as the `R3` contract,
   - what must still fail closed,
   - what `R4` must later verify as evidence.
5. The artifact explicitly preserves the approved `R1` -> `R5` staging by stopping at the obligation-contract stage and avoiding any `R4` decision or `R5` handoff output.
6. `orchestrator/rounds/round-008/implementation-notes.md` exists and records docs-only execution plus staging continuity.
7. Diff remains limited to planned docs/round artifacts and does not modify:
   - `orchestrator/rounds/round-008/state-snapshot.json`,
   - `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-003/roadmap.md`,
   - `src/`, `src-public/`, `app/`, `test/`,
   - `mlf2.cabal`,
   - predecessor packet/task history,
   - prior round artifacts.

## Reviewer And Verification Checks

Baseline checks (from `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-003/verification.md`):
- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-008/state-snapshot.json >/dev/null`
- `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-003/roadmap.md`
- `cabal build all && cabal test` is not required this round if the diff excludes `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`; reviewer must record that skip rationale explicitly.
- Reviewer-recorded continuity check against inherited evidence from completed rounds `001` through `007`, the approved successor design spec, and the predecessor recursive-types packet.

Round-specific checks:
- Docs-only boundary:
  - `git diff --name-only`
  - `git status --short`
  - `git ls-files --others --exclude-standard`
  - `git diff --name-only | rg '^(src/|src-public/|app/|test/|mlf2\\.cabal$)'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/state\\.json$'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/roadmap\\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/rounds/round-00[1-7]/'` (expected: no matches)
- Obligation-contract content markers present:
  - `rg -n 'R0|ARI-C1|R1|R2|URI-R2-C1|obligation|acyclic|binding|scope|occurs-check|termination|reconstruction|reification|witness|principality|fail-closed|R4|R5' docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md orchestrator/rounds/round-008/implementation-notes.md`
- Selected-subset continuity checks:
  - `rg -n 'URI-R2-C1|single-SCC|single-binder-family|cross-family|non-cyclic|implicit unfolding|equi-recursive' docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`
  - `rg -n '^Candidate ID:' docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md` (expected: no matches)
- Stage-preservation checks:
  - `rg -n 'feasible-continue|not-yet-go|implementation-handoff' docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md` (expected: no matches)
- Continuity-reference checks:
  - `rg -n '2026-03-14-unannotated-iso-recursive-r1-gap-map|2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection|2026-03-14-automatic-recursive-inference-invariant-audit|2026-03-14-unannotated-iso-recursive-roadmap-design' docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`

Review output requirements:
- Reviewer writes `orchestrator/rounds/round-008/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - explicit confirmation that this round stayed at `R3`, preserved inherited invariant-audit authority, kept `URI-R2-C1` fixed, and did not widen beyond the fixed boundary model.
