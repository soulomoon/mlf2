# Round 012 Plan (Roadmap Item 2: `RE2` Uniqueness-Evidence Contract)

## Round Objective

Deliver the smallest bounded `RE2` research artifact that defines reviewer-checkable uniqueness evidence for `URI-R3-O5` inside `URI-R2-C1`, while preserving the approved `RE1` -> `RE5` staging, keeping the inherited invariant-audit authority controlling, and remaining prototype-free and fail-closed.

## Scope Mode (Strictly Docs-Only Evidence Contract)

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not edit `orchestrator/state.json` or `orchestrator/roadmap.md`.
- Do not rewrite predecessor packet history or prior round artifacts.
- Do not draft `RE3`, `RE4`, or `RE5` outputs in this round.
- Do not weaken the accepted `R5` stop or the accepted `RE1` provenance-authority contract.
- Do not write `implementation-notes.md`, `review.md`, or `merge.md` in this round.

Rationale: roadmap item 2 is the next re-entry evidence-contract stage only. The accepted `R5` stop and the current round selection both identify `URI-R3-O5` as a separate unresolved blocker after provenance authority, so this round must define what later review may count as bounded uniqueness evidence and what evidence remains inadmissible, without widening into positive-evidence, gate, recommendation, or implementation work.

## Scope Guardrails

- Preserve the fixed boundary model throughout:
  - `URI-R2-C1` only;
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - no default-on widening;
  - fail closed if uniqueness would require heuristic ranking, competing-root comparison, multi-cluster comparison, widened search, prototype-backed evidence, experiment-backed evidence, or implementation drift.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited invariant audit for principality-risk, occurs-check/termination, reconstruction/reification/witness replay, binding-tree discipline, and acyclicity boundaries.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`, and `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract.md` as authoritative inherited statements of the bounded subject, the `URI-R3-O5` obligation, the prior no-go result, the stop decision, and the already-approved provenance-authority contract; do not reinterpret them as already-cleared uniqueness evidence.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md` as the staging authority for `RE1` through `RE5`; preserve that ordering exactly.
- Use completed rounds `001` through `011`, the accepted `R5` stop decision, the approved re-entry design, and the predecessor recursive-types packet as inherited evidence only; do not reopen or rewrite them.
- Keep the artifact prototype-free: if the uniqueness contract cannot be stated from inherited docs and accepted evidence alone, the artifact must say so explicitly and fail closed rather than authorizing experiments or implementation work.

## Exact Target File

1. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`

## Sequential Tasks

### Task 1 - Reconfirm the inherited authority chain for `URI-R3-O5`

Required inherited inputs to cite directly in the artifact:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md`, naming `RE2` as the uniqueness-evidence stage and preserving the approved `RE1` -> `RE5` ladder;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`, naming `URI-R3-O5` as an unresolved blocker after provenance authority and preserving the bounded stop;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`, naming the missing docs-only uniqueness evidence and its no-go triggers;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`, naming the formal `URI-R3-O5` hard contract and fail-closed rejection condition;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection.md`, naming `URI-R2-C1` and its original “one stable candidate recursive root, or one stable local obligation cluster” subset boundary;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract.md`, preserving the separate `RE1` authority contract without collapsing uniqueness into provenance;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`, naming the authoritative inherited principality-risk and safety boundaries;
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/orchestrator/rounds/round-012/selection.md` as the current round-selection authority.

Task output requirements:

- State explicitly that `RE2` does not decide whether `URI-R3-O5` is cleared; it defines what evidence would count as bounded uniqueness evidence and what evidence remains rejected.
- Lock the bounded subject to the same local root or equivalent local cluster already admitted by `URI-R2-C1` and constrained further by `RE1`; `RE2` may judge whether that subject is uniquely admissible, but may not replace it with a widened comparison class.
- Carry forward the inherited invariant-audit authority and the accepted `RE1` provenance-authority contract instead of inventing new principality, replay, search, or ownership semantics.

### Task 2 - Write the `RE2` uniqueness-evidence contract artifact

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`

Required contents:

- A title and metadata block that identify roadmap item 2, `RE2`, `URI-R3-O5`, and `URI-R2-C1`, and that describe the artifact as an evidence contract rather than a feasibility decision, re-entry gate, or implementation handoff.
- One inherited-authority section that cites the accepted `R2`, `R3`, `R4`, `R5`, `RE1`, the approved re-entry design, the authoritative invariant audit, and the current selection file as the direct authority chain for this round.
- One fixed-boundary section that restates, without weakening, the mandatory boundary phrases:
  - `URI-R2-C1` only;
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - no default-on widening;
  - prototype-free evidence only.
- One contract-subject section that defines the exact bounded subject for uniqueness:
  - the already-bounded local root or equivalent local cluster inside `URI-R2-C1`;
  - uniqueness is judged only among admissible bounded interpretations already inside that subject boundary;
  - no new candidate may be introduced by widened search, replay-domain expansion, owner-family broadening, or structural reinterpretation;
  - `RE2` consumes `RE1` as a prerequisite authority shape but does not restate or settle `RE1`.
- One admissible-evidence section that defines what counts as bounded uniqueness evidence for `URI-R3-O5`. It must include reviewer-checkable requirements for:
  - exactly one live admissible local root or equivalent local cluster inside `URI-R2-C1`;
  - no second bounded interpretation that remains simultaneously admissible under the same fixed boundary;
  - uniqueness derived from bounded exclusion rules, not preference ordering;
  - no dependence on heuristic ranking, solver-choice sensitivity, search-order sensitivity, or “best candidate” narratives;
  - no dependence on widened comparison classes such as multi-cluster, multi-SCC, cross-family, or non-local propagation cases;
  - compatibility with the inherited principality-risk, occurs-check/termination, binding-tree, and replay/reification boundaries.
- One rejection section that explicitly lists evidence that must remain inadmissible, including at minimum:
  - heuristic ranking or tie-breaking between bounded candidates;
  - competing roots or competing clusters, even if one appears preferable;
  - multi-cluster or multi-SCC comparison used to salvage uniqueness;
  - uniqueness claims that appear only after widened search, non-local propagation, owner repair, replay-domain widening, or structural reinterpretation;
  - prototype-backed, experiment-backed, or implementation-drift-backed claims.
- One reviewer gate section that says exactly what a later reviewer would need to point to in order to mark `URI-R3-O5` as cleared, and exactly what findings force it to remain uncleared.
- One fail-closed evidence-source section that states:
  - this round uses inherited docs and accepted evidence only;
  - no production code, test, or Cabal change may be used to manufacture uniqueness evidence;
  - if uniqueness cannot be shown without heuristic ranking, widened comparison, or prototype-backed evidence, the later gate must remain closed.
- One stage-discipline section that explicitly says:
  - this document executes `RE2` only;
  - this document does not settle `RE3` positive evidence, `RE4` re-entry, or `RE5` recommendation;
  - this document does not reopen or rewrite `RE1`;
  - this document does not authorize implementation planning or production work.
- One continuity section that states completed rounds `001` through `011` and the predecessor recursive-types packet remain inherited evidence only and are not rewritten.

### Task 3 - Self-check the contract against fail-closed, continuity, and staging constraints

Required checks before review:

- Confirm the artifact defines an evidence contract, not a verdict, recommendation, prototype request, or implementation handoff.
- Confirm the artifact preserves the approved `RE1` -> `RE5` staging, leaves `RE3` through `RE5` unsettled, and keeps the inherited invariant audit authoritative.
- Confirm the artifact treats `RE1` as prerequisite authority, but does not collapse uniqueness into provenance or revise the `RE1` contract.
- Confirm every admissible-evidence condition is bounded to `URI-R2-C1` and one local root/cluster only.
- Confirm the rejection list explicitly covers heuristic ranking, competing roots, competing clusters, multi-SCC or multi-cluster salvage, widened search, replay-domain widening, owner repair, and prototype-backed evidence.
- Confirm the diff stays limited to the planned docs artifact plus this round plan.

## Acceptance Criteria (All Required)

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md` exists.
2. The artifact defines `RE2` as a uniqueness-evidence contract for `URI-R3-O5`, not as a feasibility decision, re-entry verdict, or implementation-handoff document.
3. The artifact explicitly preserves the approved `RE1` -> `RE5` staging, preserves the inherited invariant-audit authority, and treats the accepted `RE1` contract as prerequisite authority rather than rewritten content.
4. The artifact keeps the active subject fixed to `URI-R2-C1`, `single-SCC`, `single-binder-family`, non-equi-recursive, non-cyclic, prototype-free boundaries.
5. The artifact defines one reviewer-checkable bounded subject for uniqueness and requires exactly one admissible local root or equivalent local cluster inside that fixed boundary.
6. The artifact explicitly defines admissible uniqueness evidence and explicitly rejects heuristic ranking, competing roots/clusters, multi-cluster or multi-SCC salvage, widened comparison, owner or provenance repair, and prototype-backed evidence.
7. The artifact states exactly what would count as clearing `URI-R3-O5` later and what would force it to remain uncleared.
8. The artifact explicitly states that this round does not settle `RE3`, `RE4`, or `RE5`, does not reopen `RE1`, and does not authorize production implementation work.
9. Diff remains limited to the planned docs/round artifacts and does not modify:
   - `orchestrator/state.json`,
   - `orchestrator/roadmap.md`,
   - `src/`, `src-public/`, `app/`, `test/`,
   - `mlf2.cabal`,
   - predecessor packet/task history,
   - prior round artifacts outside `round-012/plan.md`.

## Reviewer And Verification Checks

Baseline checks (from `orchestrator/verification.md`):

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '^\\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmap.md`
- `cabal build all && cabal test` is not required this round if the diff excludes `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`; reviewer must record that skip rationale explicitly.
- Reviewer-recorded continuity check against inherited evidence from completed rounds `001` through `011`, the accepted `R5` stop decision, the approved re-entry design, the accepted `RE1` contract, the authoritative invariant audit, and the predecessor recursive-types packet.

Round-specific checks:

- Docs-only boundary:
  - `git diff --name-only`
  - `git status --short`
  - `git ls-files --others --exclude-standard`
  - `git diff --name-only | rg '^(src/|src-public/|app/|test/|mlf2\\.cabal$)'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/state\\.json$'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/roadmap\\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/rounds/round-00(1|2|3|4|5|6|7|8|9|10|11)/'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/rounds/round-012/(review|merge|implementation-notes)\\.md$'` (expected: no matches)
- Uniqueness-contract markers present:
  - `rg -n 'RE2|URI-R3-O5|URI-R2-C1|uniqueness|unique|admissible|heuristic|ranking|competing roots|competing clusters|multi-cluster|multi-SCC|prototype-free|fail closed|RE1|RE3|RE4|RE5' docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`
- Fixed-boundary checks:
  - `rg -n 'single-SCC|single-binder-family|cross-family|non-cyclic|equi-recursive|default-on widening|prototype-free' docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`
  - `rg -n '^Candidate ID:' docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md` (expected: no matches)
- Stage-preservation checks:
  - `rg -n 'reopen-handoff-track|not-yet-reopen|remain-stop|implementation-handoff|implementation-ready|feasible-continue|not-yet-go' docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md` (expected: no matches except inherited citations that are explicitly marked as inherited inputs)
  - reviewer must confirm the document does not contain a live `RE3`, `RE4`, or `RE5` decision and does not revise the accepted `RE1` contract.
- Uniqueness-rejection checks:
  - `rg -n 'heuristic ranking|tie-breaking|competing roots|competing clusters|multi-cluster|multi-SCC|widened search|non-local propagation|replay-domain widening|owner repair|prototype-backed|implementation-drift' docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`
- Continuity-reference checks:
  - `rg -n '2026-03-14-uri-r2-c1-reentry-roadmap-design|2026-03-14-unannotated-iso-recursive-r5-research-stop-decision|2026-03-14-unannotated-iso-recursive-r4-feasibility-decision|2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract|2026-03-14-unannotated-iso-recursive-r2-candidate-subset-selection|2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract|2026-03-14-automatic-recursive-inference-invariant-audit' docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`
- Reviewer logic checks:
  - reviewer must verify the admissible-evidence section is finite, bounded, and sufficient to answer the `RE2` exit question from the roadmap design: what evidence would or would not clear `URI-R3-O5`.
  - reviewer must verify the contract proves uniqueness by exclusion inside the fixed boundary rather than by ranking, preference, or widened candidate comparison.

Review output requirements:

- Reviewer writes `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-012/orchestrator/rounds/round-012/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - explicit confirmation that this round stayed at `RE2`, preserved the inherited invariant-audit authority, preserved the accepted `RE1` contract without rewriting it, kept `URI-R2-C1` fixed, remained prototype-free and fail-closed, and did not settle `RE3`, `RE4`, or `RE5`.
