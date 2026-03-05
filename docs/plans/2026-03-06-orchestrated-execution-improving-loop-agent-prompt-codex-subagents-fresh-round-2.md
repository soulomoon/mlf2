# Plan: Orchestrated Execution of the Improving Loop Agent Prompt (Codex Subagents, Fresh Round 2)

## Summary
Execute the workflow in [improving-loop-agent.prompt2.md](/Volumes/src/mlf4/docs/prompts/improving-loop-agent.prompt2.md) with strict role separation, a mandatory two-researcher handoff before planning, strict YES/NO gates, bounded retries, and one terminal status line.
This plan is decision-complete for a Codex-subagent orchestrator and assumes a fresh run from Round 1 against current repo state in `/Volumes/src/mlf4`.

## Public APIs / Interfaces / Types
1. No production Haskell API changes are required by this plan itself.
2. Introduce orchestration message contracts (text/JSON) as agent interfaces:
   - `VerifierSweepRecord`: `mechanism`, `gate`, `thesis_refs`, `code_refs`, `test_refs`, `gap_summary`.
   - `ResearchSummary`: `researcher`, `scope`, `key_findings`, `evidence_refs`, `planner_implications`.
   - `EvidenceReconciliation`: `agreed_facts`, `contradictions`, `trusted_claims`, `rejected_claims`, `resolution_status`.
   - `PlannerRoundPlan`: `target_mechanism`, `root_cause`, `files`, `tasks`, `tests`, `acceptance_criteria`, `abort_criteria`, `requires_scope_expansion`, `expanded_target_set`, `evidence_reconciliation`.
   - `ReviewResult`: `gate`, `blocking_findings`.
   - `QAResult`: `gate`, `commands_run`, `failures`.
   - `ThesisGateResult`: `gate`, `target_evidence`, `earlier_yes_regression`.
3. Gate value contract: every gate field value must be exactly `YES` or `NO`.
4. Add orchestration-control metadata contracts:
   - `AttemptMeta`: `round`, `attempt`, `target_mechanism`, `feasibility`, `meaningful_diff`, `blocker_class`, `scope_changed`.
   - `BlockedModeDecision`: `blocked` (`YES`/`NO`), `reason`, `requires_scope_expansion` (`YES`/`NO`), `next_action`.
   - `PlannerDelta`: `changed_since_previous_attempt`, `why_outcome_should_change`.

## Execution Steps

### 1. Initialize run artifacts (fresh Round 1)
1. Create a run folder: `tasks/todo/2026-03-06-tmt-improving-loop-orchestrator-fresh-round-2/`.
2. Create files: `task_plan.md`, `findings.md`, `progress.md`, `orchestrator-log.md`.
3. Record baseline inputs with commit hash and timestamp:
   - [Transformation table](/Volumes/src/mlf4/docs/notes/2026-02-27-transformation-mechanism-table.md)
   - [Thesis source](/Volumes/src/mlf4/papers/these-finale-english.txt)
   - [Prompt source](/Volumes/src/mlf4/docs/prompts/improving-loop-agent.prompt2.md)

### 2. Spawn fixed role agents with strict ownership
1. Orchestrator: coordination only, no file edits, no reviews, no gate self-approval.
2. Verifier: thesis/code comparison and thesis gates.
3. Researcher A: thesis/paper research for the selected mechanism, nearby transformations, and semantic invariants.
4. Researcher B: codebase/tests/history research for the selected mechanism, likely edit points, and regression surface.
5. Planner: actionable implementation plans from verifier gaps plus both researcher summaries.
6. Bugfixer: code/test edits only from planner instructions.
7. Reviewer: independent safety/correctness review of diffs.
8. QA: independent command execution and pass/fail reporting.
9. Integrator: branch/commit/PR operations only after all gates pass.

### 3. Fixed mechanism order and gate interpretation
1. Use this exact mechanism order from the prompt (1..14).
2. Gate question per mechanism must be:
   `update the row <MECHANISM> for Transformation Mechanism Table (Thesis vs Codebase) by reviewing the codebase and thesis, are we absolutely thesis-exact?`
3. Row 14 (`Campaign classification status`) default gate mapping:
   - `YES` when `docs/thesis-deviations.yaml` has no active `DEV-TMT-*` in live deviations and campaign items are retired/resolved.
   - `NO` otherwise.

### 4. Round loop (maximum 10 rounds)
1. Run verifier full sweep over all 14 mechanisms in order.
2. Log one row per mechanism with gate and evidence refs.
3. If all 14 gates are `YES`, emit:
   - `COMPLETED: implementation is thesis-exact; no code changes needed.`
   - `FINAL STATUS: COMPLETED`
   - Stop.
4. Select `target_mechanism` as the first `NO`.
5. Before planning starts, spawn both researchers for the selected target mechanism.
6. Collect two `ResearchSummary` results:
   - Researcher A summary must cover thesis references, intended semantics, coupling with adjacent mechanisms, and planner-relevant constraints.
   - Researcher B summary must cover current implementation shape, likely file ownership, existing tests/guards, and nearby regression risks.
7. Planner may not start until both researcher summaries are available and must treat the verifier evidence plus both summaries as required inputs.
8. Before writing implementation tasks, Planner must produce `EvidenceReconciliation`:
   - list agreed facts across Verifier, Researcher A, and Researcher B,
   - list contradictions or uncertainty,
   - state which claims are trusted or rejected with evidence,
   - block implementation planning until the evidence basis is internally resolved.
9. Planner produces `PlannerRoundPlan` for this target.
10. If coupled mechanisms or cross-phase boundaries are implicated, Planner may set `requires_scope_expansion = YES` and provide `expanded_target_set`, while keeping the first `NO` as the anchor mechanism for the round.
11. Enter attempt loop (max 10) for this round.

### 5. Attempt loop (maximum 10 attempts per round)
1. Bugfixer executes planner tasks and returns diff summary plus `AttemptMeta`; if infeasible, emits feasibility gate `NO` with reason.
2. Reviewer evaluates plan adherence and safety; outputs gate `YES` or `NO` with reason.
3. QA runs required commands:
   - Planner-required targeted commands.
   - Mandatory full gate: `cabal build all && cabal test`.
4. Verifier re-checks target mechanism and runs sanity check on earlier mechanisms currently `YES`.
5. Attempt decision:
   - If Review=`YES` and QA=`YES` and Thesis=`YES`: Integrator commits on non-master branch/PR flow, mark round result `CONTINUE`, exit attempt loop, start next round.
   - Else:
     - Orchestrator must explicitly choose whether the failed-attempt diff is accepted as the next baseline or reverted before the next attempt.
     - Planner writes failure analysis + revised plan, continue next attempt.
6. If attempt 10 fails success conditions:
   - Emit `MAXIMUMRETRY: reached maximum implementation attempts (10).`
   - Emit `FINAL STATUS: MAXIMUMRETRY`
   - Stop.
7. No-progress gate (new, mandatory before planner revision):
   - Compute no-progress when all are true:
     - `feasibility = NO`
     - `meaningful_diff = NO`
     - blocker class unchanged from previous attempt.
   - If two consecutive no-progress attempts occur, planner must change strategy shape (scope expansion, different mechanism framing, or explicit blocked-mode decision).
8. Coupled-mechanism expansion rule (new):
   - If verifier/planner evidence indicates cross-phase coupling, planner must emit `requires_scope_expansion = YES` and provide an expanded target set for the next attempt (for example producer + consumer + schema contract path), not a repeated same-boundary attempt.
9. Planner revision quality gate (new):
   - Attempts 2..10 must include `PlannerDelta` with concrete differences from the previous attempt and why those differences should change gates.
   - Reject planner revisions that only restate prior infeasible constraints without operational changes.
10. Failed-attempt workspace hygiene (new):
    - After each failed attempt, orchestrator must either:
      - keep and explicitly accept attempt diff as baseline for next attempt, or
      - revert unaccepted code edits before running the next attempt.
    - No silent carryover.

### 6. Round-limit termination
1. If Round 10 completes without global completion:
   - Emit `FAILED: stopped after 10 planning rounds without completion.`
   - Emit `FINAL STATUS: FAILED`
   - Stop.

### 7. Blocked-Mode And Terminal Policy
1. Enter blocked mode when no bounded strategy can produce a meaningful diff for the target mechanism without violating declared abort criteria.
   - Also enter blocked mode when evidence reconciliation remains unresolved after additional verifier/researcher clarification.
2. In blocked mode:
   - bugfix attempts are evidence-only and must not claim mechanism closure,
   - planner may widen scope via `expanded_target_set` but must explain why the anchor mechanism cannot be closed in isolation,
   - QA must still verify baseline health,
   - verifier must explicitly restate why thesis gate remains `NO`.
3. Terminal-status decision policy:
   - Prefer `MAXIMUMRETRY` when the round hits attempt 10 without triple-YES success.
   - Use `FAILED` only when the approach is explicitly deemed non-viable beyond this run (not just this round) and should stop before retry exhaustion.

## Logging and Output Format
1. `orchestrator-log.md` must include per event:
   - round number
   - selected mechanism
   - attempt number
   - producing agent
   - gate value (`YES`/`NO`)
   - one-line reason for every `NO`
   - `blocker_class`
   - `meaningful_diff` (`YES`/`NO`)
   - `scope_changed` (`YES`/`NO`)
2. Keep gate lines machine-checkable with exact values.
3. Print exactly one terminal line in the entire run:
   - `FINAL STATUS: COMPLETED`
   - or `FINAL STATUS: FAILED`
   - or `FINAL STATUS: MAXIMUMRETRY`
4. In addition to markdown table logs, write one JSONL gate record per event to `orchestrator-log.jsonl` for deterministic replay and post-run analytics.

## Git/Integration policy
1. Integrator acts only after triple gate success (Review+QA+Thesis all `YES`).
2. Use branch names prefixed with `codex/`.
3. Do not merge directly to `master`; require branch/PR flow.
4. Commit message template: `TMT round <R>: close <mechanism> thesis-exact`.

## Mandatory QA baseline for later rounds
1. Always run `cabal build all && cabal test`.
2. If mechanisms 1-3 are already `YES`, also run their guard slices in each later attempt:
   - `--match "elab-input absolute thesis-exact guard"`
   - `--match "row2 absolute thesis-exact guard"`
   - `--match "row2 closeout guard"`
   - `--match "row3 absolute thesis-exact guard"`
   - `--match "Phase 4 thesis-exact unification closure"`
   - `--match "checked-authoritative"`
   - `--match "Dual-path verification"`
3. Negative static checks must use explicit pass semantics (`! rg ...`) so “no match” is treated as success.
4. QA command execution must be serialized (single QA runner at a time) to avoid transient Cabal lock/package-conf contention.

## Agent Reliability Rules
1. Every gate request must be idempotent and include enough context to be retried verbatim.
2. If an agent response is malformed (missing exact `YES`/`NO`), reject and re-request immediately.

## Test Cases and Scenarios
1. Immediate completion case: full sweep returns all `YES` in Round 1; terminal status is `COMPLETED`.
2. Normal progress case: first `NO` fixed within 1-10 attempts; commit happens; next round starts.
3. Research handoff case: Planner waits for both researcher summaries before producing the round plan.
4. Review-blocked case: reviewer returns `NO`; planner revises; no QA/Thesis bypass allowed.
5. QA-blocked case: full gate fails; planner revises with regression fix strategy.
6. Thesis-blocked case: QA and review pass but verifier returns `NO`; round continues.
7. Retry exhaustion case: attempt 10 still failing; terminal status is `MAXIMUMRETRY`.
8. Round exhaustion case: 10 rounds used without all-`YES`; terminal status is `FAILED`.
9. Row 14 classification case: enforce YES/NO mapping rule despite table’s historical `N/A` wording.
10. No-progress loop case: two consecutive no-diff feasibility failures with unchanged blocker must trigger strategy change or blocked-mode entry.
11. Coupled-mechanism case: planner/verifier identifies cross-phase coupling; next attempt must include expanded target scope.
12. QA semantics case: `rg` no-match checks pass via explicit negation and do not produce false `QA=NO`.

## Assumptions and Defaults
1. Execution model is Codex subagents.
2. Run mode is fresh from Round 1, using current repository state as baseline.
3. Orchestrator never edits code and never self-approves any gate.
4. Evidence references are mandatory for every mechanism decision and every researcher summary.
5. Existing known risk: naive removal of synthesized-wrapper behavior for row 4 can regress Phase 6 translatability; planner must include explicit mitigation/abort criteria for that path.
