# Round 014 Plan (Roadmap Item 4: `RE4` Bounded Re-Entry Gate)

## Round Objective

Deliver the smallest bounded `RE4` research artifact that evaluates inherited `RE1`, `RE2`, and `RE3` authority for `URI-R2-C1` and records one explicit re-entry decision, while preserving the approved `RE1` -> `RE5` staging, keeping the inherited invariant-audit authority controlling, remaining prototype-free and fail-closed, and not settling `RE5` beyond what the gate itself requires.

## Planned Gate Outcome

- Planned round outcome: `not-yet-reopen`.

Rationale: the inherited `RE1`, `RE2`, and `RE3` artifacts define reviewer-checkable evidence contracts, but they do not themselves manufacture new positive evidence or implementation-backed authority. This round is docs-only and fail-closed. Unless the gate can cite bounded inherited evidence that already satisfies every required `RE1` / `RE2` / `RE3` condition without prototype work, widened search, or broadened ownership, the honest `RE4` result remains `not-yet-reopen`. The plan still requires the gate artifact to state the decision explicitly and to record what exact condition would have been required for `reopen-handoff-track`.

## Scope Mode (Strictly Docs-Only Gate Decision)

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not edit `orchestrator/rounds/round-014/state-snapshot.json` or `orchestrator/roadmaps/2026-03-14-02-uri-r2-c1-re-entry-evidence-successor-roadmap/rev-004/roadmap.md`.
- Do not rewrite predecessor packet history or prior round artifacts.
- Do not draft `RE5` as a recommendation document in this round.
- Do not authorize a handoff-track design round inside this artifact.
- Do not weaken the accepted `R5` stop, the accepted `RE1` / `RE2` / `RE3` contracts, or the inherited invariant audit.
- Do not write `implementation-notes.md`, `review.md`, or `merge.md` in this round.

Rationale: roadmap item 4 is the bounded gate only. The approved re-entry design says `RE4` must judge whether the accumulated bounded evidence is sufficient to reopen a later handoff-track design, but `RE4` is not itself that design and is not implementation clearance. Because this round is prototype-free and fail-closed, it must prefer a narrow gate-decision document over any new prototype, code, or recommendation work.

## Scope Guardrails

- Preserve the fixed boundary model throughout:
  - `URI-R2-C1` only;
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - no default-on widening;
  - fail closed if a `reopen-handoff-track` case would require prototype-backed evidence, experiment-backed evidence, implementation drift, broadened provenance roots, heuristic ranking, widened ownership, widened search, multi-cluster or multi-SCC salvage, equi-recursive reasoning, cyclic structural encoding, or any other subject broadening.
- Treat `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative inherited invariant audit for acyclicity, binding-tree discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality-risk boundaries.
- Treat `docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md` as the staging authority for `RE1` through `RE5`; preserve that ordering exactly.
- Treat `docs/plans/2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract.md`, `docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`, and `docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md` as the controlling stage prerequisites; do not rewrite them, relax them, or smuggle missing evidence past them.
- Treat the accepted `R5` bounded stop and the predecessor recursive-types packet as inherited historical authority only; do not reopen or rewrite them.
- Keep the gate judgment tied to inherited evidence only. If the round cannot prove every prerequisite from accepted docs and inherited evidence alone, the gate must record `not-yet-reopen`.
- If the gate records `reopen-handoff-track`, it may only say that a later roadmap may design a bounded handoff-track. It must not draft that later roadmap, settle `RE5`, or imply implementation authorization.

## Exact Target Files

1. Create `docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md`

Filename note:

- The exact `RE4` deliverable name for this round is `2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md`.
- Do not substitute or accept the legacy shorter spelling `2026-03-14-uri-r2-c1-re4-reentry-gate.md`.

## Sequential Tasks

### Task 1 - Reconfirm the inherited authority chain and gate inputs

Required inherited inputs to cite directly in the artifact:

- `docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md`, naming `RE4` as the bounded re-entry gate and preserving the approved `RE1` -> `RE5` ladder;
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`, preserving the accepted bounded stop as the immediate predecessor authority;
- `docs/plans/2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract.md`, defining the provenance-authority gate that must already be satisfied without manufactured authority;
- `docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`, defining the uniqueness gate that must already be satisfied without heuristic ranking or widened comparison;
- `docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md`, defining the prototype-free positive-evidence gate that must already be satisfied without implementation drift or widened ownership/search;
- `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`, preserving the authoritative inherited safety boundary;
- `orchestrator/rounds/round-014/selection.md` as the current round-selection authority.

Task output requirements:

- State explicitly that `RE4` is a gate decision over inherited evidence, not a new evidence-contract stage, not a prototype request, not a handoff design, and not implementation clearance.
- State explicitly that `RE4` may record only one of `reopen-handoff-track` or `not-yet-reopen`.
- Preserve the same bounded subject already fixed by `URI-R2-C1`, `RE1`, `RE2`, and `RE3`; the gate may not broaden provenance roots, comparison sets, ownership families, SCC scope, or graph semantics.
- Carry forward the inherited invariant-audit authority and the accepted `RE1` / `RE2` / `RE3` contracts instead of inventing new replay, uniqueness, ownership, search, or termination semantics.

### Task 2 - Write the `RE4` bounded re-entry gate artifact

Target file:
- `docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md`

Required contents:

- A title and metadata block that identify roadmap item 4, `RE4`, `URI-R2-C1`, and the artifact as a bounded gate decision rather than an implementation-handoff or successor recommendation.
- One inherited-authority section that cites the approved re-entry design, accepted `R5`, accepted `RE1`, accepted `RE2`, accepted `RE3`, the authoritative invariant audit, and the current selection file as the direct authority chain for this round.
- One fixed-boundary section that restates, without weakening, the mandatory boundary phrases:
  - `URI-R2-C1` only;
  - `single-SCC` obligation-level recursion only;
  - `single-binder-family` ownership only;
  - no cross-family SCC linking;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - no default-on widening;
  - prototype-free evidence only.
- One gate-input section that enumerates the exact prerequisites under review:
  - provenance authority per `RE1`;
  - uniqueness per `RE2`;
  - positive evidence for `URI-R3-O1` through `URI-R3-O3` per `RE3`;
  - continuity with the inherited invariant audit and accepted `R5` stop.
- One evaluation section organized as a bounded checklist that records, for each prerequisite, both:
  - what inherited evidence actually exists in the repository; and
  - whether that evidence satisfies the corresponding gate condition, remains only a contract without affirmative evidence, or is blocked by a fail-closed trigger.
- One fail-closed triggers section that says the gate must remain closed if any reopening argument depends on:
  - prototype-backed or experiment-backed evidence;
  - implementation drift;
  - manufactured provenance authority, late repair, or widened replay domains;
  - heuristic ranking, tie-breaking, or widened comparison;
  - widened ownership, owner repair, parent-chain repair, or cross-family reasoning;
  - widened search, non-local propagation, multi-cluster or multi-SCC salvage;
  - cyclic structural encoding, implicit unfolding, equi-recursive reasoning, or termination weakening.
- One decision section that records exactly one explicit outcome and its justification:
  - `not-yet-reopen`, unless every prerequisite is affirmatively satisfied from inherited evidence alone; or
  - `reopen-handoff-track`, only if the artifact can cite bounded inherited evidence satisfying every `RE1` / `RE2` / `RE3` prerequisite without any fail-closed trigger.
- One unsatisfied-conditions section that names the exact conditions still missing if the decision is `not-yet-reopen`, keeping those conditions bounded to `URI-R2-C1` and not broadening the subject.
- One successor-boundary section that states:
  - if the result is `reopen-handoff-track`, the only authorized consequence is that a later roadmap may design a bounded handoff-track for `URI-R2-C1`;
  - if the result is `not-yet-reopen`, the stop remains in force until later bounded evidence exists;
  - this document does not itself write `RE5`, does not draft a handoff-track, and does not authorize implementation work.
- One continuity section that states completed rounds `001` through `013` and the predecessor recursive-types packet remain inherited evidence only and are not rewritten.

### Task 3 - Force the gate to decide fail-closed and record the bounded result

Required decision discipline:

- Compare inherited evidence against `RE1`, `RE2`, and `RE3` separately before writing the final outcome.
- If any prerequisite lacks affirmative inherited evidence and only supplies a contract or unresolved blocker, mark that prerequisite unsatisfied.
- If any prerequisite would need prototype work, implementation-backed evidence, or broadened subject boundaries to become satisfied, stop immediately and record `not-yet-reopen`.
- Do not use ambiguity language such as "maybe reopen", "lean reopen", "defer to RE5", or "implementation-ready".
- Record the decision as a single explicit label in the artifact and repeat it in the concluding paragraph.

Round expectation:

- Because this round is limited to inherited docs and accepted evidence, and because the `RE1` / `RE2` / `RE3` inputs are contract-setting artifacts rather than new positive evidence, the default and expected decision is `not-yet-reopen`.
- If the implementer believes `reopen-handoff-track` is supportable, the artifact must prove that every prerequisite is already affirmatively satisfied by inherited evidence alone and must explain why that does not collapse `RE4` into prototype-backed or broadened reasoning. Otherwise the round must stay at `not-yet-reopen`.

### Task 4 - Self-check the gate against staging, continuity, and no-broadening constraints

Required checks before review:

- Confirm the artifact is a bounded gate decision, not a new contract stage, not a recommendation document, not a prototype request, and not an implementation handoff.
- Confirm the artifact preserves the approved `RE1` -> `RE5` staging, keeps `RE5` unsettled except for the minimal successor-boundary note required by the gate, and keeps the inherited invariant audit authoritative.
- Confirm the artifact evaluates `RE1`, `RE2`, and `RE3` as prerequisites and does not rewrite them or treat their contract definitions as automatic clearance.
- Confirm the artifact keeps the active subject fixed to `URI-R2-C1` and does not widen SCC scope, binder-family scope, provenance roots, comparison classes, ownership search, or graph semantics.
- Confirm the artifact records exactly one explicit outcome: `not-yet-reopen` or `reopen-handoff-track`.
- Confirm the artifact records exact unsatisfied conditions if the outcome is `not-yet-reopen`, without drifting into `RE5`.
- Confirm the diff stays limited to the planned docs artifact plus this round plan.

## Acceptance Criteria (All Required)

1. `docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md` exists.
2. The artifact defines `RE4` as a bounded re-entry gate for `URI-R2-C1`, not as an implementation-handoff document or final successor recommendation.
3. The artifact explicitly preserves the approved `RE1` -> `RE5` staging, preserves the inherited invariant-audit authority, and treats accepted `RE1`, `RE2`, and `RE3` as prerequisite authority rather than rewritten content.
4. The artifact keeps the active subject fixed to `URI-R2-C1`, `single-SCC`, `single-binder-family`, non-equi-recursive, non-cyclic, prototype-free boundaries.
5. The artifact explicitly evaluates inherited evidence for `RE1`, `RE2`, and `RE3` separately and states whether each prerequisite is satisfied, unsatisfied, or blocked by a fail-closed trigger.
6. The artifact explicitly lists fail-closed triggers covering manufactured provenance authority, heuristic ranking, widened comparison, widened ownership/search, multi-cluster or multi-SCC salvage, cyclic structural encoding, implicit unfolding, equi-recursive reasoning, prototype-backed evidence, experiment-backed evidence, and implementation drift.
7. The artifact records exactly one explicit outcome, either `reopen-handoff-track` or `not-yet-reopen`, and justifies it against the inherited `RE1` / `RE2` / `RE3` prerequisites.
8. If the artifact records `not-yet-reopen`, it names the exact unsatisfied bounded conditions without broadening the subject or drifting into `RE5`.
9. If the artifact records `reopen-handoff-track`, it states only that a later bounded roadmap may design a handoff-track and does not itself draft that roadmap or authorize implementation.
10. The artifact explicitly states that this round does not settle `RE5` beyond the minimal successor-boundary note required by the gate and does not authorize production implementation work.
11. Diff remains limited to the planned docs/round artifacts and does not modify:
   - `orchestrator/rounds/round-014/state-snapshot.json`,
   - `orchestrator/roadmaps/2026-03-14-02-uri-r2-c1-re-entry-evidence-successor-roadmap/rev-004/roadmap.md`,
   - `src/`, `src-public/`, `app/`, `test/`,
   - `mlf2.cabal`,
   - predecessor packet/task history,
   - prior round artifacts outside `round-014/plan.md`.

## Reviewer And Verification Checks

Baseline checks (from `orchestrator/roadmaps/2026-03-14-02-uri-r2-c1-re-entry-evidence-successor-roadmap/rev-004/verification.md`):

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-014/state-snapshot.json >/dev/null`
- `rg -n '^\\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-14-02-uri-r2-c1-re-entry-evidence-successor-roadmap/rev-004/roadmap.md`
- `cabal build all && cabal test` is not required this round if the diff excludes `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`; reviewer must record that skip rationale explicitly.
- Reviewer-recorded continuity check against inherited evidence from completed rounds `001` through `013`, the accepted `R5` stop decision, the approved re-entry design, the accepted `RE1` / `RE2` / `RE3` contracts, the authoritative invariant audit, and the predecessor recursive-types packet.

Round-specific checks:

- Docs-only boundary:
  - `git diff --name-only`
  - `git status --short`
  - `git ls-files --others --exclude-standard`
  - `git diff --name-only | rg '^(src/|src-public/|app/|test/|mlf2\\.cabal$)'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/state\\.json$'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/roadmap\\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/rounds/round-00(1|2|3|4|5|6|7|8|9|10|11|12|13)/'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/rounds/round-014/(review|merge|implementation-notes)\\.md$'` (expected: no matches)
- Gate markers present:
  - `rg -n 'RE4|URI-R2-C1|RE1|RE2|RE3|reopen-handoff-track|not-yet-reopen|prototype-free|fail-closed|single-SCC|single-binder-family' docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md`
- Fixed-boundary and no-broadening checks:
  - `rg -n 'single-SCC|single-binder-family|cross-family|non-cyclic|equi-recursive|default-on widening|prototype-free' docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md`
  - `rg -n 'manufactured provenance|late repair|heuristic ranking|tie-breaking|widened comparison|widened ownership|owner repair|parent-chain repair|widened search|non-local propagation|multi-cluster|multi-SCC|cyclic structural|implicit unfolding|termination weakening|implementation drift' docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md`
- Explicit-decision checks:
  - `rg -n '^- Planned round outcome: `not-yet-reopen`\\.$' orchestrator/rounds/round-014/plan.md`
  - `rg -n 'reopen-handoff-track|not-yet-reopen' docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md`
  - reviewer must confirm the gate artifact records exactly one live decision and does not use ambiguous intermediate labels.
- Stage-preservation checks:
  - `rg -n 'RE5|handoff-track|implementation-handoff|implementation-ready|recommendation' docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md`
  - reviewer must confirm any `RE5` or handoff mention is limited to the minimal successor-boundary note required by the gate and does not become a recommendation document.
- Continuity-reference checks:
  - `rg -n '2026-03-14-uri-r2-c1-reentry-roadmap-design|2026-03-14-unannotated-iso-recursive-r5-research-stop-decision|2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract|2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract|2026-03-14-uri-r2-c1-re3-positive-evidence-contract|2026-03-14-automatic-recursive-inference-invariant-audit' docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md`
- Reviewer logic checks:
  - reviewer must verify the artifact distinguishes evidence contracts from affirmative evidence and does not treat the existence of `RE1` / `RE2` / `RE3` documents as automatic clearance.
  - reviewer must verify the artifact defaults to `not-yet-reopen` if any prerequisite lacks affirmative inherited evidence or requires prototype-backed or broadened reasoning.
  - reviewer must verify the artifact states exact bounded unsatisfied conditions for `not-yet-reopen`, or exact bounded clearance grounds for `reopen-handoff-track`, without broadening the subject.

Review output requirements:

- Reviewer writes `orchestrator/rounds/round-014/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - explicit confirmation that this round stayed at `RE4`, preserved the inherited invariant-audit authority, preserved accepted `RE1` / `RE2` / `RE3` without rewriting them, kept `URI-R2-C1` fixed, remained prototype-free and fail-closed, recorded one explicit bounded outcome, and did not settle `RE5` beyond the gate's minimal successor-boundary note.
