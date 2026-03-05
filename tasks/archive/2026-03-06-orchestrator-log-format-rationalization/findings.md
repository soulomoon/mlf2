# Findings: 2026-03-06 Orchestrator Log Format Rationalization

- The live round-2 plan currently requires both `orchestrator-log.md` and `orchestrator-log.jsonl`.
- The markdown log requirement duplicates machine-checkable gate content already required elsewhere.
- The strongest justification in the current docs is for JSONL: deterministic replay, post-run analytics, and exact gate fields.
- The reusable goal-loop skill and scaffold still model the third artifact as a markdown round log, so changing only the live plan would leave the generator contract stale.
- After the update, live docs point only to `orchestrator-log.jsonl`; residual `orchestrator-log.md` mentions remain only inside the new design/implementation-plan docs that describe the migration.
- `python3 -m py_compile .codex/skills/goal-table-orchestrator-loop/scripts/scaffold_goal_loop_docs.py` passed.
- Scaffold smoke test passed and now emits a default `.jsonl` event-log artifact.
