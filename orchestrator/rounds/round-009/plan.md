# Round 009 Plan (Roadmap Item 4: `R4` Bounded Feasibility Decision)

## Round Objective

Deliver the smallest bounded `R4` research slice that judges feasibility for the already-selected subset `URI-R2-C1` and records one explicit reviewer-visible outcome, while preserving the approved `R1` -> `R5` staging and keeping the inherited item-2 invariant audit authoritative.

## Scope Mode (Strictly Docs-Only Feasibility Artifact)

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not edit `orchestrator/state.json` or `orchestrator/roadmap.md`.
- Do not rewrite predecessor packet history or prior round artifacts.
- Do not draft the `R5` implementation-handoff / research-stop artifact in this round.

Rationale: roadmap item 4 requires a bounded feasibility decision, not a production implementation attempt. The approved successor design and `round-009/selection.md` allow prototype evidence only as a non-default last resort; this plan keeps the round docs-only and requires the decision to fail closed to `not-yet-go` if inherited evidence is insufficient.

## Scope Guardrails

- Preserve the fixed boundary model throughout:
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - fail closed if any claimed feasibility would require forbidden widening.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited item-2 invariant audit for acyclicity, binding-tree discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality-risk boundaries.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md`, and `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md` as authoritative narrowing inputs; do not reopen `R1`, reselect `R2`, or rewrite the `R3` contract.
- Keep the chosen subject fixed to `URI-R2-C1` and judge it only through the inherited `URI-R3-O1` through `URI-R3-O5` obligation classes.
- Use completed rounds `001` through `008`, the approved successor design spec, and the predecessor recursive-types packet as inherited evidence only; do not reopen them as pending work.
- If the bounded evidence cannot clear every required obligation without new prototype work or widening, record `not-yet-go` rather than stretching scope.

## Exact Target Files

1. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`
2. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/orchestrator/rounds/round-009/implementation-notes.md`

## Sequential Tasks

### Task 1 - Write the `R4` bounded feasibility-decision artifact

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`

Required contents:
- A short inherited-baseline section that cites:
  - accepted `R0` / `ARI-C1` as the starting point;
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` as the gap-map predecessor;
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md` as the chosen-subset predecessor, naming `URI-R2-C1` explicitly;
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md` as the obligation-contract predecessor, naming `URI-R3-O1` through `URI-R3-O5` explicitly;
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited invariant audit;
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md` as the roadmap-design source;
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/orchestrator/rounds/round-009/selection.md` as the current round-selection authority.
- A fixed-boundary section that restates, without weakening, the mandatory boundary phrases:
  - `URI-R2-C1` remains `single-SCC` and `single-binder-family` only;
  - recursion remains obligation-level rather than cyclic structural-graph encoding;
  - no cross-family SCC linking is admitted;
  - no equi-recursive reasoning or implicit unfolding is admitted;
  - no default-on widening is admitted;
  - this round uses docs-only evidence and fails closed if that evidence is insufficient.
- One feasibility-evidence matrix, or equivalent structured list, that evaluates the chosen subset against each inherited `R3` obligation class. Each entry must include:
  - the stable obligation ID (`URI-R3-O1` through `URI-R3-O5`);
  - the bounded subject inside `URI-R2-C1`;
  - the positive example classes to be treated as in-scope evidence for that obligation;
  - the negative example classes that must still fail for that obligation;
  - the inherited evidence available from prior accepted artifacts;
  - the no-go trigger for that obligation;
  - the immediate stop condition for that obligation;
  - the per-obligation judgment contribution to the final round outcome.
- The feasibility evaluation must cover, at minimum:
  - structural acyclicity evidence proving the structural `TyNode` / constraint graph remains acyclic even though obligation-level recursion may form one SCC;
  - binder ownership and scope-discipline evidence proving every admitted obligation stays inside one stable binder family with deterministic parent-chain ownership;
  - occurs-check and termination evidence proving constructor-directed reasoning remains sufficient and no implicit unfolding or speculative recursive-equality search is introduced;
  - reconstruction, reification, and witness replay evidence proving one provenance-stable root/cluster can flow through generalization, reification, and replay without widening or ambiguity;
  - principality-risk boundary evidence proving admission remains unique, bounded, and fail-closed rather than heuristic or silently broadened.
- One explicit evidence-source section that makes the round's bounded method reviewer-visible:
  - identify which conclusions come from inherited docs and accepted-round evidence;
  - state that no new production code, test, or Cabal changes were used to manufacture feasibility evidence in this round;
  - state that if prototype evidence would be required to clear an obligation, the artifact must resolve to `not-yet-go` rather than widening the round.
- One explicit no-go and stop section that groups the classes of failures that force `not-yet-go`, including at minimum:
  - any obligation that cannot be cleared from inherited evidence without new prototype work;
  - any pressure toward ambiguous or multi-root obligation discovery;
  - any pressure toward multi-cluster or multi-SCC dependency shapes;
  - any mixed-owner or cross-family ownership claim;
  - any implicit-unfolding, equi-recursive, or cyclic-graph pressure;
  - any provenance-unstable reconstruction, reification, or replay subject;
  - any principality or termination claim that would depend on heuristic search or widened solver behavior.
- One final decision section that records exactly one reviewer-visible outcome:
  - `Decision outcome: feasible-continue`; or
  - `Decision outcome: not-yet-go`.
- The final decision section must also include:
  - a concise explanation of why the evidence does or does not clear all `R3` obligations under the fixed boundary model;
  - an explicit statement that the decision is bounded to `URI-R2-C1` only and does not generalize to broader unannotated recursive inference;
  - an explicit statement that `R5` is unlocked only by `feasible-continue`, while `not-yet-go` preserves fail-closed stopping behavior.
- A stage-discipline section that explicitly says:
  - this document executes the `R4` bounded feasibility decision only;
  - this document does not reopen `R1`, reselect `R2`, or rewrite the `R3` obligation contract;
  - this document does not draft the `R5` handoff / research-stop artifact itself;
  - this document does not authorize production implementation work.
- A continuity section that states completed rounds and the predecessor recursive-types packet are cited as evidence only and are not rewritten.

### Task 2 - Record round implementation notes

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/orchestrator/rounds/round-009/implementation-notes.md`

Required contents:
- Short summary of the `R4` feasibility-decision artifact and why it satisfies successor roadmap item 4.
- Explicit statement that `URI-R2-C1` remained the fixed chosen subset and was not reselected or widened.
- Explicit statement that the inherited item-2 invariant audit remains authoritative and was not reopened.
- Explicit statement that this round is docs-only and introduced no production behavior changes or prototype evidence.
- Explicit statement of the recorded round outcome (`feasible-continue` or `not-yet-go`) and why that outcome is still bounded to `URI-R2-C1`.
- Explicit statement that `R5` remains future work and was not drafted in this round.

## Acceptance Criteria (All Required)

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md` exists and evaluates `URI-R2-C1` against inherited `URI-R3-O1` through `URI-R3-O5`.
2. The `R4` artifact preserves the fixed boundary model:
   - `single-SCC` obligation-level recursion only;
   - `single-binder-family` ownership only;
   - no cross-family SCC linking;
   - non-equi-recursive only;
   - non-cyclic structural graph only;
   - fail-closed treatment of ambiguous, widening-dependent, or evidence-insufficient cases.
3. The artifact records positive example classes, negative example classes, inherited evidence, no-go triggers, and immediate stop conditions for every `R3` obligation class instead of skipping any inherited invariant area.
4. The artifact records exactly one explicit bounded decision outcome, either `feasible-continue` or `not-yet-go`, with reviewer-visible reasoning tied to the inherited evidence base.
5. The artifact explicitly states that lack of docs-only evidence resolves to `not-yet-go` rather than adding prototype evidence or widening scope in this round.
6. The artifact explicitly preserves the approved `R1` -> `R5` staging by stopping at the bounded feasibility decision and avoiding any `R5` handoff artifact.
7. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/orchestrator/rounds/round-009/implementation-notes.md` exists and records docs-only execution plus staging continuity.
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
- `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmap.md`
- `cabal build all && cabal test` is not required this round if the diff excludes `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`; reviewer must record that skip rationale explicitly.
- Reviewer-recorded continuity check against inherited evidence from completed rounds `001` through `008`, the approved successor design spec, and the predecessor recursive-types packet.

Round-specific checks:
- Docs-only boundary:
  - `git diff --name-only`
  - `git status --short`
  - `git ls-files --others --exclude-standard`
  - `git diff --name-only | rg '^(src/|src-public/|app/|test/|mlf2\\.cabal$)'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/state\\.json$'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/roadmap\\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/rounds/round-00[1-8]/'` (expected: no matches)
- Feasibility-artifact content markers present:
  - `rg -n 'R0|ARI-C1|R1|R2|R3|URI-R2-C1|URI-R3-O1|URI-R3-O2|URI-R3-O3|URI-R3-O4|URI-R3-O5|positive example|negative example|no-go|stop condition|Decision outcome|feasible-continue|not-yet-go|fail-closed|R5' docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md orchestrator/rounds/round-009/implementation-notes.md`
- Exactly-one-decision checks:
  - `rg -n '^Decision outcome:' docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md | wc -l` (expected: `1`)
  - `rg -n '^Decision outcome: (feasible-continue|not-yet-go)$' docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`
- Fixed-boundary continuity checks:
  - `rg -n 'URI-R2-C1|single-SCC|single-binder-family|cross-family|non-cyclic|implicit unfolding|equi-recursive|authoritative inherited invariant audit' docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`
  - `rg -n '^Candidate ID:' docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md` (expected: no matches)
- Docs-only evidence-source checks:
  - `rg -n 'docs-only|no production behavior changes|no prototype evidence|inherited evidence|fail closed' docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md orchestrator/rounds/round-009/implementation-notes.md`
- Continuity-reference checks:
  - `rg -n '2026-03-14-unannotated-iso-recursive-r1-gap-map|2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection|2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract|2026-03-14-automatic-recursive-inference-invariant-audit|2026-03-14-unannotated-iso-recursive-roadmap-design' docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`

Review output requirements:
- Reviewer writes `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-009/orchestrator/rounds/round-009/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - explicit confirmation that this round stayed at `R4`, preserved the inherited invariant-audit authority, kept `URI-R2-C1` fixed, stayed docs-only, and did not widen beyond the fixed boundary model.
