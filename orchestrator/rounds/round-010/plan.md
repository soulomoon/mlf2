# Round 010 Plan (Roadmap Item 5: Explicit `R5` Research-Stop Decision)

## Round Objective

Deliver the smallest bounded item-5 artifact that converts the accepted `R4` outcome `not-yet-go` for `URI-R2-C1` into one explicit reviewer-visible **research-stop** decision, while preserving the approved `R1` -> `R5` staging and keeping the inherited item-2 invariant audit authoritative.

## Scope Mode (Strictly Docs-Only Stop Decision)

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not edit `orchestrator/state.json` or `orchestrator/roadmap.md`.
- Do not rewrite predecessor packet history or prior round artifacts.
- Do not draft an implementation handoff unless inherited accepted evidence inside the fixed boundary actually contradicts the accepted `R4` `not-yet-go` record.
- Do not write `review.md`, `merge.md`, or `implementation-notes.md` in this round.

Rationale: roadmap item 5 is now the terminal decision point for this successor track. Because accepted `round-009` concluded `not-yet-go` for `URI-R2-C1`, the default bounded outcome is an explicit research-stop artifact, not an implementation-handoff spec. The round must fail closed unless inherited evidence already accepted in the track truly supports reversing that decision without widening.

## Scope Guardrails

- Preserve the fixed boundary model throughout:
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - fail closed if any claimed progress would require forbidden widening, prototype-backed invention, or reopening earlier accepted stages.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited item-2 invariant audit for acyclicity, binding-tree discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality-risk boundaries.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`, and `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md` as authoritative inherited stages; do not reopen `R1`, reselect `R2`, rewrite `R3`, or rerun `R4`.
- Keep the chosen subject fixed to `URI-R2-C1` and carry forward the inherited `URI-R3-O1` through `URI-R3-O5` obligation classes as the reasoned basis for the item-5 decision.
- Use completed rounds `001` through `009`, the approved successor design spec, and the predecessor recursive-types packet as inherited evidence only; do not reopen them as pending work.
- If any inherited-evidence review suggests contradictory feasibility, the round may only record that contradiction explicitly and stop for reviewer scrutiny; it must not silently switch to implementation-handoff by reinterpretation.

## Exact Target File

1. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`

## Sequential Tasks

### Task 1 - Reconfirm the inherited evidence chain and lock the decision direction

Required inputs to cite directly in the artifact:

- accepted `R0` / `ARI-C1` baseline from the completed automatic-recursive-inference chain;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md`;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md`;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md`, naming `URI-R2-C1`;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`, naming `URI-R3-O1` through `URI-R3-O5`;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`, naming the accepted outcome `not-yet-go`;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited invariant audit;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/orchestrator/rounds/round-010/selection.md` as the current round-selection authority.

Decision-direction rule:

- Default direction is `research-stop`.
- Switch away from `research-stop` only if inherited accepted evidence inside the fixed boundary already clears the contradiction explicitly and reviewer-visibly; absence of such evidence is itself the reason to keep the stop decision.

### Task 2 - Write the explicit `R5` research-stop decision artifact

Target file:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`

Required contents:

- A title and metadata block that identify roadmap item 5, `URI-R2-C1`, and the final bounded direction `research-stop`.
- One decision statement section that says item 5 resolves to **research-stop** because accepted `R4` concluded `not-yet-go` and no inherited contradictory evidence inside the fixed boundary overrules that result.
- One evidence-chain section that summarizes items `R1` through `R4` sequentially and explicitly preserves the approved `R1` -> `R5` staging rather than collapsing earlier gates.
- One unresolved-obligations section that ties the stop decision to the inherited `URI-R3-O1` through `URI-R3-O5` contract, including:
  - the decisive inherited gap around provenance-stable unannotated replay authority for `URI-R3-O4`;
  - the missing docs-only uniqueness clearance for `URI-R3-O5`;
  - the remaining prototype-free evidence gap for positive unannotated satisfaction of `URI-R3-O1`, `URI-R3-O2`, and `URI-R3-O3`;
  - an explicit statement that these gaps remain bounded to `URI-R2-C1` and do not authorize widening.
- One fixed-boundary section that restates, without weakening, the mandatory boundary phrases:
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - no default-on widening;
  - no prototype-backed reinterpretation of the accepted evidence chain.
- One explicit non-handoff section that says this round does **not** authorize implementation planning, production implementation, first-touch module selection, or test-authoring work for the unannotated target.
- One future-research re-entry section that records what would have to exist before a later roadmap could revisit handoff, phrased as bounded evidence requirements rather than as an implementation plan. At minimum include:
  - authoritative prototype-free evidence for one provenance-stable unannotated root/cluster;
  - bounded uniqueness evidence that clears `URI-R3-O5` without heuristic ranking;
  - bounded positive evidence that `URI-R3-O1` through `URI-R3-O3` can be satisfied without implicit unfolding, cyclic graphs, or widened ownership/search.
- One continuity section that states completed rounds `001` through `009`, the approved successor design, and the predecessor recursive-types packet are cited as inherited evidence only and are not rewritten.
- One stage-discipline section that explicitly says:
  - this document executes the item-5 final decision only;
  - this document does not reopen `R1`, reselect `R2`, rewrite `R3`, or rerun `R4`;
  - this document does not convert `not-yet-go` into implementation clearance;
  - this document does not authorize production implementation work.

### Task 3 - Self-check the artifact against docs-only and decision-direction constraints

Required checks before review:

- Confirm the artifact records exactly one final item-5 direction and that it is `research-stop`.
- Confirm the artifact never claims `implementation-handoff`, first-touch module boundaries, or implementation readiness.
- Confirm all reasoning is inherited-evidence-based and keeps the invariant audit authoritative.
- Confirm the diff stays limited to the planned docs artifact plus this round plan.

## Acceptance Criteria (All Required)

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` exists.
2. The artifact records item 5 as an explicit bounded `research-stop` decision rather than an implementation-handoff spec.
3. The artifact cites the accepted `R4` result `not-yet-go` for `URI-R2-C1` as the controlling inherited decision input.
4. The artifact preserves the approved `R1` -> `R5` staging and the inherited item-2 invariant audit authority.
5. The artifact ties the stop decision to the inherited `URI-R3-O1` through `URI-R3-O5` obligations, with explicit mention of the unresolved `URI-R3-O4` and `URI-R3-O5` gaps and the remaining prototype-free evidence gap for `URI-R3-O1` through `URI-R3-O3`.
6. The artifact explicitly states that the decision remains bounded to `URI-R2-C1` and does not generalize to broader unannotated recursive inference.
7. The artifact explicitly states that no implementation handoff is authorized from the current evidence.
8. Diff remains limited to planned docs/round artifacts and does not modify:
   - `orchestrator/state.json`,
   - `orchestrator/roadmap.md`,
   - `src/`, `src-public/`, `app/`, `test/`,
   - `mlf2.cabal`,
   - predecessor packet/task history,
   - prior round artifacts.

## Reviewer And Verification Checks

Baseline checks (from `orchestrator/verification.md`):

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '^\\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmap.md`
- `cabal build all && cabal test` is not required this round if the diff excludes `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`; reviewer must record that skip rationale explicitly.
- Reviewer-recorded continuity check against inherited evidence from completed rounds `001` through `009`, the approved successor design spec, and the predecessor recursive-types packet.

Round-specific checks:

- Docs-only boundary:
  - `git diff --name-only`
  - `git status --short`
  - `git ls-files --others --exclude-standard`
  - `git diff --name-only | rg '^(src/|src-public/|app/|test/|mlf2\\.cabal$)'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/state\\.json$'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/roadmap\\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/rounds/round-00[1-9]/'` (expected: no matches)
- Research-stop artifact content markers present:
  - `rg -n 'R0|ARI-C1|R1|R2|R3|R4|R5|URI-R2-C1|URI-R3-O1|URI-R3-O2|URI-R3-O3|URI-R3-O4|URI-R3-O5|Decision outcome|research-stop|not-yet-go|implementation-handoff|authoritative inherited invariant audit|single-SCC|single-binder-family|non-cyclic|implicit unfolding|equi-recursive' docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- Exactly-one-direction checks:
  - `rg -n '^Decision outcome:' docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md | wc -l` (expected: `1`)
  - `rg -n '^Decision outcome: research-stop$' docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- Non-handoff checks:
  - `rg -n 'First-Touch Implementation File Set|implementation planner|implementation-ready|authorized target for the next implementation round' docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` (expected: no matches)
- Continuity-reference checks:
  - `rg -n '2026-03-14-unannotated-iso-recursive-r1-gap-map|2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection|2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract|2026-03-14-unannotated-iso-recursive-r4-feasibility-decision|2026-03-14-automatic-recursive-inference-invariant-audit|2026-03-14-unannotated-iso-recursive-roadmap-design' docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`

Review output requirements:

- Reviewer writes `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-010/orchestrator/rounds/round-010/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - explicit confirmation that this round stayed at item 5, preserved the inherited invariant-audit authority, kept `URI-R2-C1` fixed, remained docs-only, and resolved the current evidence to `research-stop` without widening.
