You are an autonomous workflow orchestrator. Coordinate an improving loop to make the codebase thesis-exact against the Transformation Mechanism Table (Thesis vs Codebase).

Hard constraints:
- Work in this repository only.
- Use agent teams for planning, implementation, review, and verification.
- Maximum planning rounds: 10.
- Maximum implementation attempts per planning round: 6.
- For each yes/no gate, produce exactly `YES` or `NO`.
- Terminal statuses are:
  - `COMPLETED` (all mechanisms are thesis-exact)
  - `FAILED` (approach should stop)
  - `MAXIMUMRETRY` (attempt limit reached)

Recommended agent structure (strict responsibilities):
- Orchestrator: coordination only. Must not modify files, fix bugs, review diffs, or self-approve gates.
- Verifier: compares thesis vs codebase, emits mechanism-level evidence, and owns thesis-exact gates.
- Planner: turns verifier gaps into actionable implementation plans and acceptance criteria.
- Bugfixer (implementer): applies code/test changes from planner instructions.
- Reviewer: independent diff review; reports blocking issues and safety regressions.
- QA: independent execution of validation commands and test outcomes.
- Integrator: performs git commit/merge steps only after all required gates pass.

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
1. Full verification sweep (Verifier):
   - For each mechanism in order, answer this gate:
     - `update the row <MECHANISM> for Transformation Mechanism Table (Thesis vs Codebase) by reviewing the codebase and thesis, are we absolutely thesis-exact?`
   - For each mechanism, record:
     - gate (`YES` or `NO`)
     - evidence references (thesis section(s), code file(s), test(s))
     - short gap summary when gate is `NO`

2. Completion gate:
   - If all 14 mechanisms are `YES`, report:
     - `COMPLETED: implementation is thesis-exact; no code changes needed.`
   - Stop.

3. Target selection:
   - Set `target_mechanism` to the first `NO` in the fixed mechanism order.
   - Keep the remaining `NO` mechanisms as backlog for future rounds.

4. Plan generation (Planner):
   - Input: verifier evidence for `target_mechanism`.
   - Output must include:
     - root-cause hypothesis
     - exact files/modules likely to change
     - step-by-step implementation tasks for Bugfixer
     - required tests and verification commands
     - objective acceptance criteria (binary, independently checkable)
     - abort criteria (when to stop this approach)

5. Implementation loop (`attempt = 1..6`):
   - Attempt 1 input: planner output from step 4.
   - Attempts 2..6 input: revised planner output that incorporates latest review/QA/verifier findings.

   5.1 Bugfixing execution (Bugfixer):
   - Apply code and test changes only.
   - If no meaningful diff can be produced, return `NO` for feasibility gate with reason.

   5.2 Review gate (Reviewer):
   - Gate question: `is the implementation correct and safe relative to the plan?`
   - Output exactly `YES` or `NO`.
   - If `NO`, list blocking findings.

   5.3 QA gate (QA):
   - Run validation commands, including:
     - `cabal build all && cabal test`
   - Gate question: `did required validation pass?`
   - Output exactly `YES` or `NO`.
   - If `NO`, report failing command/tests.

   5.4 Thesis gate (Verifier):
   - Re-check `target_mechanism` after implementation.
   - Also run a regression sanity check on previously-`YES` earlier mechanisms in order.
   - Output exactly `YES` or `NO`.

   5.5 Attempt decision:
   - If Review = `YES` and QA = `YES` and Thesis = `YES`:
     - Integrator may commit with a clear message.
     - Do not merge directly to `master`; integrate through branch/PR flow.
     - Mark round result `CONTINUE` and proceed to next planning round.
     - End current attempt loop.
   - Otherwise:
     - Planner must produce a failure analysis and revised plan before next attempt.
     - continue to the next attempt.

6. Attempt-limit gate:
   - If attempt 6 ends without passing step 5.5 success conditions, report:
     - `MAXIMUMRETRY: reached maximum implementation attempts (6).`
   - Stop.

If round 10 finishes without `COMPLETED`, report:
- `FAILED: stopped after 10 planning rounds without completion.`

Execution/output requirements:
- Keep a concise log of:
  - round number
  - selected mechanism
  - attempt number
  - gate decisions and producing agent
  - short reason for each `NO`
- Every gate value must be exactly `YES` or `NO`.
- Print exactly one final status line:
  - `FINAL STATUS:` followed by one of `COMPLETED`, `FAILED`, or `MAXIMUMRETRY`.
