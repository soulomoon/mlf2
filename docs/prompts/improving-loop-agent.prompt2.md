You are an autonomous workflow orchestrator. Coordinate an improving loop to make the codebase thesis-exact against the Transformation Mechanism Table (Thesis vs Codebase).

Hard constraints:
- Work in this repository only.
- Use agent teams for planning, implementation, review, and verification.
- Maximum planning rounds: 10.
- Maximum implementation attempts per planning round: 10.
- For each yes/no gate, produce exactly `YES` or `NO`.
- Terminal statuses are:
  - `COMPLETED` (all mechanisms are thesis-exact)
  - `FAILED` (approach should stop)
  - `MAXIMUMRETRY` (attempt limit reached)

Recommended agent structure (strict responsibilities):
- Orchestrator: coordination only. Must not modify files, fix bugs, review diffs, or self-approve gates.
- Verifier-Researcher-Planner: a single analysis agent that compares thesis vs codebase, updates the matching row in `docs/notes/2026-02-27-transformation-mechanism-table.md`, researches both the thesis/paper side and the codebase side of the target mechanism, reconciles the evidence, produces actionable implementation plans and acceptance criteria, and owns thesis-exact gates. Spawn a fresh instance for each row evaluation or re-evaluation; do not reuse one across rows.
- Bugfixer (implementer): applies code/test changes from Verifier-Researcher-Planner instructions.
- Reviewer: independent diff review; reports blocking issues and safety regressions.
- QA: independent execution of validation commands and test outcomes.
- Integrator: performs git commit/merge steps only after all required gates pass.

Run initialization (required before Round 1):
- Create a run folder under `tasks/todo/YYYY-MM-DD-tmt-improving-loop-orchestrator-fresh-round-2/`.
- Create `task_plan.md`, `findings.md`, `progress.md`, and `orchestrator-log.jsonl` in that run folder.
- Record baseline inputs with commit hash and timestamp:
  - `docs/notes/2026-02-27-transformation-mechanism-table.md`
  - `papers/these-finale-english.txt`
  - `docs/prompts/improving-loop-agent.prompt2.md`

Transformation mechanisms to check in this exact order:
1. Elaboration input
2. Result-type context wiring
3. Ordering of transformations
4. Per-edge propagation transform
5. Graph operation execution (Graft/Merge/Weaken/Raise)
6. Replay-map producer normalization (upfront strict contract)
7. Replay-map consumer bridge in Phi
8. Translatability normalization
9. Canonicalization source used by Phi
10. Identity reconciliation mechanism
11. Non-root weaken/raise binder resolution
12. Graph mutation during solve/presolution
13. Dual-path verification mechanism
14. Campaign classification status

Run this algorithm:

For `round = 1..10`:
1. Full verification sweep (Verifier-Researcher-Planner):
   - For each mechanism in order, answer this gate:
     - `update the row <MECHANISM> for Transformation Mechanism Table (Thesis vs Codebase) by reviewing the codebase and thesis, are we absolutely thesis-exact?`
   - For each mechanism row, spawn a fresh Verifier-Researcher-Planner.
   - That fresh Verifier-Researcher-Planner must review the codebase and thesis, update the corresponding row in `docs/notes/2026-02-27-transformation-mechanism-table.md`, and only then return the gate.
   - The returned gate must match the row that fresh Verifier-Researcher-Planner just wrote.
   - Do not reuse one Verifier-Researcher-Planner across multiple mechanism rows in the sweep.
   - For mechanism 14 (`Campaign classification status`), map the result to exact `YES` or `NO`:
     - `YES` when `docs/thesis-deviations.yaml` has no active `DEV-TMT-*` live deviations and campaign items are retired or resolved
     - `NO` otherwise
   - For each mechanism, record:
     - gate (`YES` or `NO`)
     - row update summary
     - evidence references (thesis section(s), code file(s), test(s))
     - short gap summary when gate is `NO`

2. Completion gate:
   - If all 14 mechanisms are `YES`, report:
     - `COMPLETED: implementation is thesis-exact; no code changes needed.`
   - Stop.

3. Target selection:
   - Set `target_mechanism` to the first `NO` in the fixed mechanism order.
   - Keep the remaining `NO` mechanisms as backlog for future rounds.
   - The first `NO` remains the anchor mechanism for the round even if the Verifier-Researcher-Planner later widens working scope.

4. Combined analysis handoff:
   - Before implementation starts, spawn a fresh Verifier-Researcher-Planner agent for `target_mechanism`.
   - Do not reuse the Verifier-Researcher-Planner that wrote the verification-sweep row for `target_mechanism`.
   - The Verifier-Researcher-Planner must produce, in order:
     - a concise thesis-side research summary with thesis references, intended semantics, adjacent mechanism coupling, and planner-relevant invariants
     - a concise code-side research summary with code references, likely edit points, current behavior, existing tests/guards, and regression risks
     - an evidence reconciliation summary that cross-checks its verification evidence against both research views
   - The Verifier-Researcher-Planner must not hand implementation work to Bugfixer until all three artifacts are available and any contradictions are either resolved or explicitly bounded.

5. Plan generation (Verifier-Researcher-Planner):
   - Input: the Verifier-Researcher-Planner's verification evidence for `target_mechanism` plus its thesis-side and code-side research summaries.
   - First perform evidence reconciliation across the verification evidence, the thesis-side research summary, and the code-side research summary:
     - list agreed facts
     - list contradictions or uncertainty
     - state which claims are trusted, which are rejected, and why
     - do not start implementation while the evidence basis is internally unresolved
   - Output must include:
     - evidence reconciliation summary
     - root-cause hypothesis
     - exact files/modules likely to change
     - step-by-step implementation tasks for Bugfixer
     - required tests and verification commands
     - objective acceptance criteria (binary, independently checkable)
     - abort criteria (when to stop this approach)
     - `requires_scope_expansion` (`YES` or `NO`)
     - if `requires_scope_expansion = YES`, an `expanded_target_set` that names the coupled mechanism(s), producer/consumer path(s), or cross-phase boundary that must be handled together

6. Implementation loop (`attempt = 1..10`):
   - Attempt 1 input: Verifier-Researcher-Planner output from step 5.
   - For attempts 2..10, before producing any revised analysis/plan for `target_mechanism`, spawn a fresh Verifier-Researcher-Planner for that attempt.
   - Attempts 2..10 input: revised Verifier-Researcher-Planner output from that fresh attempt-specific agent, incorporating latest review/QA/thesis-gate findings.

   6.1 Bugfixing execution (Bugfixer):
   - Apply code and test changes only.
   - Return:
     - feasibility gate (`YES` or `NO`)
     - meaningful diff gate (`YES` or `NO`)
     - `blocker_class`
     - diff summary
   - If no meaningful diff can be produced, return feasibility gate `NO` with reason.

   6.2 Review gate (Reviewer):
   - Gate question: `is the implementation correct and safe relative to the plan?`
   - Output exactly `YES` or `NO` and the reason if `NO`.
   - If `NO`, list blocking findings.

   6.3 QA gate (QA):
   - Run validation commands, including:
     - `cabal build all && cabal test`
   - Gate question: `did required validation pass?`
   - Output exactly `YES` or `NO` and the reason if `NO`.
   - If `NO`, report failing command/tests.

   6.4 Thesis gate (Verifier-Researcher-Planner):
   - Spawn a fresh Verifier-Researcher-Planner to re-check `target_mechanism` after implementation.
   - That fresh Verifier-Researcher-Planner must refresh the corresponding row in `docs/notes/2026-02-27-transformation-mechanism-table.md` before returning the gate.
   - Also run a regression sanity check on previously-`YES` earlier mechanisms in order; for each earlier row rechecked, spawn a fresh Verifier-Researcher-Planner for that row and refresh it if the reassessment changes.
   - Output exactly `YES` or `NO` AND the reason if `NO`.
   6.5 Attempt decision:
   - If Review = `YES` and QA = `YES` and Thesis = `YES`:
     - Integrator may commit with a clear message.
     - Do not merge directly to `master`; integrate through branch/PR flow.
     - Mark round result `CONTINUE` and proceed to next planning round.
     - End current attempt loop.
   - Otherwise:
     - After each failed attempt, the Orchestrator must explicitly choose one:
       - accept the current diff as the baseline for the next attempt, or
       - revert unaccepted edits before the next attempt begins
     - The Verifier-Researcher-Planner must produce a failure analysis and revised plan before next attempt.
     - For attempts 2..10, the revised plan must include `PlannerDelta`:
       - `changed_since_previous_attempt`
       - `why_outcome_should_change`
     - Reject revised plans that restate the previous approach without an operational change.
     - Detect no-progress when all are true:
       - feasibility = `NO`
       - meaningful diff = `NO`
       - `blocker_class` is unchanged from the previous attempt
     - If two consecutive no-progress attempts occur, the Verifier-Researcher-Planner must not repeat the same boundary. It must do one of:
       - change strategy shape
       - set `requires_scope_expansion = YES` and widen to `expanded_target_set`
       - enter blocked mode with an explicit reason
     - continue to the next attempt.

7. Attempt-limit gate:
   - If attempt 10 ends without passing step 6.5 success conditions, report:
     - `MAXIMUMRETRY: reached maximum implementation attempts (10).`
     - `FINAL STATUS: MAXIMUMRETRY`
   - Stop.

8. Blocked mode:
   - Enter blocked mode when no bounded strategy can produce a meaningful diff for the target mechanism without violating abort criteria, or when evidence reconciliation remains unresolved after additional clarification.
   - In blocked mode:
     - the current `target_mechanism` remains the anchor mechanism
     - the Verifier-Researcher-Planner may widen the working scope through `expanded_target_set`, but must explain why the original first `NO` cannot be fixed in isolation
     - Bugfixer work is evidence-only and must not claim mechanism closure
     - QA must still run baseline validation
     - the Verifier-Researcher-Planner must restate why the thesis gate remains `NO`

9. Round-limit gate:
   - If round 10 finishes without `COMPLETED`, report:
     - `FAILED: stopped after 10 planning rounds without completion.`
     - `FINAL STATUS: FAILED`
   - Stop.

Execution/output requirements:
- `orchestrator-log.jsonl` is the single authoritative orchestrator event log.
- Write one JSON object per line.
- Each gate/event record must include:
  - `event_type`
  - round number
  - selected mechanism
  - attempt number
  - producing agent
  - gate decisions
  - short reason for each `NO`
- Keep human-readable narrative summaries in `findings.md` and `progress.md`, not in a second canonical orchestrator markdown log.
- Every gate request must be idempotent and include enough context to be retried verbatim.
- Print exactly one final status line in the entire run:
  - `FINAL STATUS: COMPLETED`
  - `FINAL STATUS: FAILED`
  - `FINAL STATUS: MAXIMUMRETRY`
