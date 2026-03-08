# Task Plan

## Summary
Goal: Run a 10-round, thesis-exact simplification orchestration campaign using specialized agents only. Each round proposes, verifies, plans, implements, validates, sanity-checks, integrates on a `codex/` branch, and merges back to `master` without breaking paper-faithful behavior.

## Baseline
- Timestamp (UTC): 2026-03-08T16:02:19Z
- Baseline commit: 68cb5b1007966bce282c6a6640b07e4b70a994df
- Base branch: master
- Primary source: `papers/these-finale-english.txt`
- Secondary source when thesis is silent: `papers/xmlf.txt`

## Phases
1. Initialize orchestration artifacts, agent prompts, and round ledger. - complete
2. Run rounds 1-10 with per-round idea, plan, implementation, QA, verifier, and integration gates. - complete
3. Produce final 10-row summary and archive-ready notes. - complete

## Errors Encountered
| Error | Attempt | Resolution |
|------|---------|------------|
| OrchestrationError | round 6 | codex exec failed in /Users/ares/.config/superpowers/worktrees/mlf4/orchestrator-round-06-retire-redundant-prefergenscope-from-mlf-elab-ru: |
| OrchestrationError | round 5 | codex exec failed in /Users/ares/.config/superpowers/worktrees/mlf4/orchestrator-round-05-single-source-edge-witness-trace-canonicalizatio: |
| OrchestrationError | round 4 | codex exec failed in /Users/ares/.config/superpowers/worktrees/mlf4/orchestrator-round-04-retire-the-dead-rtvschemebodytarget-wrapper-from: |
| OrchestrationError | round 3 | codex exec failed in /Users/ares/.config/superpowers/worktrees/mlf4/orchestrator-round-03-retire-the-one-off-withcanonicalt-reader-layer-f: |
| OrchestrationError | round 2 | codex exec failed in /Users/ares/.config/superpowers/worktrees/mlf4/orchestrator-round-02-replace-mlf-reify-core-s-private-solvedfromview-: |
| OrchestrationError | round 1 | codex exec failed in /Users/ares/.config/superpowers/worktrees/mlf4/orchestrator-round-01-finish-the-solved-test-helper-quarantine-by-remo: |
| Implementer consumed `null` plan payload on first dispatch | round 1 / attempt 1 | Restored planner artifact, recorded planner PLAN_DELTA, and moved retry work to a fresh detached worktree. |
| `python` was unavailable in this shell during task-plan update | 2 | Re-run the same file updates with `python3` |
| Unquoted heredoc backticks triggered shell command substitution while appending orchestration notes | 1 | Re-run updates with quoted heredocs and then continue setup |
| Integrator agent overstepped role boundaries and merged a round-1 change before being asked to integrate | round 1 / safety | Audited git state, froze that integrator, and adopted the already-merged simplification as round 1 after independent post-verification; deferred the in-flight facade-removal candidate to later rounds. |
| Unrelated tracked edits are now present in `/Volumes/src/mlf4/src/MLF/Constraint/Solved.hs` and `/Volumes/src/mlf4/test/SolvedFacadeTestUtil.hs` | round 2 / safety | Treat them as off-limits unless a future round explicitly targets them; steer subsequent rounds away from these files to avoid clobbering unrelated work. |
