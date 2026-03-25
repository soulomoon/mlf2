# Round 013 Plan (Roadmap Item 3: `RE3` Positive-Evidence Contract)

## Round Objective

Deliver the smallest bounded `RE3` research artifact that defines reviewer-checkable, prototype-free positive evidence for `URI-R3-O1` through `URI-R3-O3` inside `URI-R2-C1`, while preserving the approved `RE1` -> `RE5` staging, keeping the inherited invariant-audit authority controlling, and remaining fail-closed.

## Scope Mode (Strictly Docs-Only Evidence Contract)

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not edit `orchestrator/rounds/round-013/state-snapshot.json` or `orchestrator/roadmaps/2026-03-14-02-uri-r2-c1-re-entry-evidence-successor-roadmap/rev-003/roadmap.md`.
- Do not rewrite predecessor packet history or prior round artifacts.
- Do not draft `RE4` or `RE5` outputs in this round.
- Do not weaken the accepted `R5` stop or the accepted `RE1` / `RE2` evidence contracts.
- Do not write `implementation-notes.md`, `review.md`, or `merge.md` in this round.

Rationale: roadmap item 3 is the next re-entry evidence-contract stage only. The accepted `R5` stop and the current round selection identify `URI-R3-O1`, `URI-R3-O2`, and `URI-R3-O3` as the remaining positive-evidence blockers after provenance authority and uniqueness. This round must define what later review may count as bounded positive evidence, and what evidence remains inadmissible, without widening into re-entry, recommendation, prototype work, or implementation planning.

## Scope Guardrails

- Preserve the fixed boundary model throughout:
  - `URI-R2-C1` only;
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - no default-on widening;
  - fail closed if positive evidence would require prototype-backed evidence, experiment-backed evidence, implementation drift, cyclic structural encoding, explicit-anchor replay substitution, widened ownership, widened search, non-local salvage, implicit unfolding, equi-recursive equality search, or termination-class weakening.
- Treat `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited invariant audit for acyclicity, binding-tree discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality-risk boundaries.
- Treat `docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`, `docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`, `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`, `docs/plans/2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract.md`, and `docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md` as authoritative inherited statements of the bounded subject, unresolved blockers, and already-approved `RE1` / `RE2` prerequisites; do not reinterpret them as already-cleared positive evidence.
- Treat `docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md` as the staging authority for `RE1` through `RE5`; preserve that ordering exactly.
- Use completed rounds `001` through `012`, the accepted `R5` stop decision, the approved re-entry design, and the predecessor recursive-types packet as inherited evidence only; do not reopen or rewrite them.
- Keep the artifact prototype-free: if the positive-evidence contract cannot be stated from inherited docs and accepted evidence alone, the artifact must say so explicitly and fail closed rather than authorizing experiments or implementation work.

## Exact Target File

1. Create `docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md`

## Sequential Tasks

### Task 1 - Reconfirm the inherited authority chain for `URI-R3-O1` through `URI-R3-O3`

Required inherited inputs to cite directly in the artifact:

- `docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md`, naming `RE3` as the positive-evidence stage and preserving the approved `RE1` -> `RE5` ladder;
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`, naming `URI-R3-O1`, `URI-R3-O2`, and `URI-R3-O3` as unresolved blockers after `RE1` and `RE2`;
- `docs/plans/2026-03-14-unannotated-iso-recursive-r4-feasibility-decision.md`, naming the missing docs-only positive evidence and the no-go triggers that keep `URI-R2-C1` at `not-yet-go`;
- `docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`, naming the formal `URI-R3-O1` / `URI-R3-O2` / `URI-R3-O3` hard contracts and fail-closed rejection conditions;
- `docs/plans/2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract.md`, preserving the separate provenance-authority prerequisite without restating or relaxing it;
- `docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`, preserving the separate uniqueness prerequisite without collapsing positive evidence into uniqueness;
- `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`, naming the authoritative inherited safety boundary;
- `orchestrator/rounds/round-013/selection.md` as the current round-selection authority.

Task output requirements:

- State explicitly that `RE3` does not decide whether `URI-R3-O1` through `URI-R3-O3` are cleared; it defines what evidence would count as bounded positive evidence and what evidence remains rejected.
- Lock the bounded subject to the same local root or equivalent local cluster already fixed by `URI-R2-C1`, constrained by `RE1`, and rendered uniquely admissible by `RE2`; `RE3` may define the positive evidence burden for that subject, but may not replace it with widened ownership or search domains.
- Carry forward the inherited invariant-audit authority and the accepted `RE1` / `RE2` contracts instead of inventing new replay, ownership, search, or termination semantics.

### Task 2 - Write the `RE3` positive-evidence contract artifact

Target file:
- `docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md`

Required contents:

- A title and metadata block that identify roadmap item 3, `RE3`, `URI-R3-O1` through `URI-R3-O3`, and `URI-R2-C1`, and that describe the artifact as an evidence contract rather than a feasibility decision, re-entry gate, or implementation handoff.
- One inherited-authority section that cites the accepted `R3`, `R4`, `R5`, `RE1`, `RE2`, the approved re-entry design, the authoritative invariant audit, and the current selection file as the direct authority chain for this round.
- One fixed-boundary section that restates, without weakening, the mandatory boundary phrases:
  - `URI-R2-C1` only;
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - no default-on widening;
  - prototype-free evidence only.
- One contract-subject section that defines the exact bounded subject for `RE3`:
  - the already-bounded local root or equivalent local cluster inside `URI-R2-C1`;
  - the structural `TyNode` / constraint slice touched by that same subject for `URI-R3-O1`;
  - the deterministic owner chain for that same subject for `URI-R3-O2`;
  - the constructor-directed reasoning path for that same subject for `URI-R3-O3`;
  - no widened owner family, replay-domain expansion, or non-local search surface may enter the subject definition.
- One admissible-evidence section for `URI-R3-O1` that defines what later review may count as positive acyclicity evidence. It must require reviewer-checkable evidence for:
  - one local obligation SCC only;
  - one structural slice that remains acyclic and binder-mediated;
  - no structural back-edge, cyclic `TyNode` encoding, or second dependency component;
  - consistency with the inherited acyclicity and replay/termination boundaries.
- One admissible-evidence section for `URI-R3-O2` that defines what later review may count as positive single-family ownership evidence. It must require reviewer-checkable evidence for:
  - one closed binder family only;
  - one deterministic parent-chain owner for every obligation in the bounded subject;
  - no mixed-owner, shadow-driven, sibling-family, or cross-family ownership drift;
  - consistency with the inherited binding-tree, replay, and reification boundaries.
- One admissible-evidence section for `URI-R3-O3` that defines what later review may count as positive occurs-check/termination evidence. It must require reviewer-checkable evidence for:
  - constructor-directed local reasoning only;
  - no implicit unfolding, no equi-recursive equality search, and no speculative recursive-equality chase;
  - no termination-class weakening and no open-ended recursive search;
  - consistency with the inherited occurs-check, termination, and acyclicity boundaries.
- One explicit rejection section that lists inadmissible evidence classes, including at minimum:
  - prototype-backed, experiment-backed, or implementation-drift-backed claims;
  - explicit-anchor replay substitution used to manufacture positive evidence;
  - widened ownership, owner repair, or parent-chain repair;
  - widened search, non-local propagation, multi-cluster or multi-SCC salvage;
  - cyclic structural encoding, structural back-edge dependence, implicit unfolding, equi-recursive reasoning, or termination weakening.
- One reviewer gate section that says exactly what a later reviewer would need to point to in order to mark `URI-R3-O1` through `URI-R3-O3` cleared, and exactly what findings force one or more of them to remain uncleared.
- One fail-closed evidence-source section that states:
  - this round uses inherited docs and accepted evidence only;
  - no production code, test, or Cabal change may be used to manufacture positive evidence;
  - if positive evidence cannot be shown without prototype-backed evidence, implementation drift, explicit-anchor replay substitution, widened ownership/search, cyclic encoding, or termination weakening, the later gate must remain closed.
- One stage-discipline section that explicitly says:
  - this document executes `RE3` only;
  - this document does not settle `RE4` re-entry or `RE5` recommendation;
  - this document does not reopen or rewrite `RE1` or `RE2`;
  - this document does not authorize implementation planning or production work.
- One continuity section that states completed rounds `001` through `012` and the predecessor recursive-types packet remain inherited evidence only and are not rewritten.

### Task 3 - Self-check the contract against fail-closed, continuity, and staging constraints

Required checks before review:

- Confirm the artifact defines an evidence contract, not a verdict, recommendation, prototype request, or implementation handoff.
- Confirm the artifact preserves the approved `RE1` -> `RE5` staging, leaves `RE4` and `RE5` unsettled, and keeps the inherited invariant audit authoritative.
- Confirm the artifact treats `RE1` and `RE2` as prerequisite authority, but does not collapse positive evidence into provenance authority or uniqueness clearance.
- Confirm every admissible-evidence condition is bounded to `URI-R2-C1` and the already-bounded local root/cluster only.
- Confirm the rejection list explicitly covers prototype-backed evidence, implementation drift, explicit-anchor replay substitution, widened ownership, owner repair, widened search, non-local propagation, multi-cluster or multi-SCC salvage, cyclic structural encoding, implicit unfolding, equi-recursive reasoning, and termination weakening.
- Confirm the diff stays limited to the planned docs artifact plus this round plan.

## Acceptance Criteria (All Required)

1. `docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md` exists.
2. The artifact defines `RE3` as a positive-evidence contract for `URI-R3-O1` through `URI-R3-O3`, not as a feasibility decision, re-entry verdict, or implementation-handoff document.
3. The artifact explicitly preserves the approved `RE1` -> `RE5` staging, preserves the inherited invariant-audit authority, and treats the accepted `RE1` and `RE2` contracts as prerequisite authority rather than rewritten content.
4. The artifact keeps the active subject fixed to `URI-R2-C1`, `single-SCC`, `single-binder-family`, non-equi-recursive, non-cyclic, prototype-free boundaries.
5. The artifact defines one reviewer-checkable bounded subject for `RE3` and ties `URI-R3-O1`, `URI-R3-O2`, and `URI-R3-O3` to that same bounded subject without widened ownership or search.
6. The artifact explicitly defines admissible positive evidence for structural acyclicity, deterministic single-family ownership, and constructor-directed occurs-check/termination clearance.
7. The artifact explicitly rejects prototype-backed evidence, implementation drift, explicit-anchor replay substitution, widened ownership/search, multi-cluster or multi-SCC salvage, cyclic structural encoding, implicit unfolding, equi-recursive reasoning, and termination weakening.
8. The artifact states exactly what would count as clearing `URI-R3-O1` through `URI-R3-O3` later and what would force one or more of them to remain uncleared.
9. The artifact explicitly states that this round does not settle `RE4` or `RE5`, does not reopen `RE1` or `RE2`, and does not authorize production implementation work.
10. Diff remains limited to the planned docs/round artifacts and does not modify:
   - `orchestrator/rounds/round-013/state-snapshot.json`,
   - `orchestrator/roadmaps/2026-03-14-02-uri-r2-c1-re-entry-evidence-successor-roadmap/rev-003/roadmap.md`,
   - `src/`, `src-public/`, `app/`, `test/`,
   - `mlf2.cabal`,
   - predecessor packet/task history,
   - prior round artifacts outside `round-013/plan.md`.

## Reviewer And Verification Checks

Baseline checks (from `orchestrator/roadmaps/2026-03-14-02-uri-r2-c1-re-entry-evidence-successor-roadmap/rev-003/verification.md`):

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-013/state-snapshot.json >/dev/null`
- `rg -n '^\\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-14-02-uri-r2-c1-re-entry-evidence-successor-roadmap/rev-003/roadmap.md`
- `cabal build all && cabal test` is not required this round if the diff excludes `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`; reviewer must record that skip rationale explicitly.
- Reviewer-recorded continuity check against inherited evidence from completed rounds `001` through `012`, the accepted `R5` stop decision, the approved re-entry design, the accepted `RE1` and `RE2` contracts, the authoritative invariant audit, and the predecessor recursive-types packet.

Round-specific checks:

- Docs-only boundary:
  - `git diff --name-only`
  - `git status --short`
  - `git ls-files --others --exclude-standard`
  - `git diff --name-only | rg '^(src/|src-public/|app/|test/|mlf2\\.cabal$)'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/state\\.json$'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/roadmap\\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/rounds/round-00(1|2|3|4|5|6|7|8|9|10|11|12)/'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/rounds/round-013/(review|merge|implementation-notes)\\.md$'` (expected: no matches)
- Positive-evidence markers present:
  - `rg -n 'RE3|URI-R3-O1|URI-R3-O2|URI-R3-O3|URI-R2-C1|positive evidence|acyclic|binder|owner|occurs-check|termination|prototype-free|RE1|RE2|RE4|RE5' docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md`
- Fixed-boundary checks:
  - `rg -n 'single-SCC|single-binder-family|cross-family|non-cyclic|equi-recursive|default-on widening|prototype-free' docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md`
  - `rg -n 'explicit-anchor replay substitution|widened ownership|widened search|non-local propagation|multi-cluster|multi-SCC|cyclic structural|implicit unfolding|termination weakening' docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md`
- Stage-preservation checks:
  - `rg -n 'reopen-handoff-track|not-yet-reopen|remain-stop|implementation-handoff|implementation-ready|feasible-continue|not-yet-go' docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md` (expected: no matches except inherited citations that are explicitly marked as inherited inputs)
  - reviewer must confirm the document does not contain a live `RE4` or `RE5` decision and does not revise the accepted `RE1` or `RE2` contracts.
- Positive-evidence rejection checks:
  - `rg -n 'prototype-backed|experiment-backed|implementation-drift|explicit-anchor replay substitution|owner repair|parent-chain repair|widened search|non-local propagation|multi-cluster|multi-SCC|cyclic structural|implicit unfolding|equi-recursive|termination weakening' docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md`
- Continuity-reference checks:
  - `rg -n '2026-03-14-uri-r2-c1-reentry-roadmap-design|2026-03-14-unannotated-iso-recursive-r5-research-stop-decision|2026-03-14-unannotated-iso-recursive-r4-feasibility-decision|2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract|2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract|2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract|2026-03-14-automatic-recursive-inference-invariant-audit' docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md`
- Reviewer logic checks:
  - reviewer must verify the admissible-evidence sections are finite, bounded, and sufficient to answer the `RE3` exit question from the roadmap design: what evidence would or would not clear `URI-R3-O1` through `URI-R3-O3`.
  - reviewer must verify the contract proves positive evidence only inside the fixed `URI-R2-C1` boundary and does not rely on prototype evidence, widened ownership/search, or implementation drift.

Review output requirements:

- Reviewer writes `orchestrator/rounds/round-013/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - explicit confirmation that this round stayed at `RE3`, preserved the inherited invariant-audit authority, preserved the accepted `RE1` and `RE2` contracts without rewriting them, kept `URI-R2-C1` fixed, remained prototype-free and fail-closed, and did not settle `RE4` or `RE5`.
