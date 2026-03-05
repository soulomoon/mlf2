---
name: goal-table-orchestrator-loop
description: Create or update a goal-status mechanism table and paired multi-agent improving-loop orchestrator artifacts (prompt plus round log). Use when asked to document current-vs-target behavior for a goal and drive iterative YES/NO-gated improvement rounds.
---

# Goal Table Orchestrator Loop

Create three coordinated artifacts for a goal:
- mechanism status table (current vs target)
- orchestrator prompt (role-based improving loop)
- JSONL orchestrator event log template (attempt-by-attempt gate evidence)

## Inputs

Collect or infer:
- goal statement
- source-of-truth reference(s)
- ordered mechanism list
- preferred output paths (or use defaults)

If mechanism list is missing, derive a first-pass list from the goal decomposition and label it as provisional.

## Workflow

1. Inspect existing artifacts first.
- Reuse existing mechanism names/order when continuing an active campaign.
- Do not overwrite existing files unless explicitly asked.

2. Generate or refresh the mechanism table.
- Use the template in `references/table-template.md`.
- Include, per row: current behavior, target behavior, evidence, gate, and next action.
- Gate values must be exactly `YES` or `NO`.

3. Generate or refresh the orchestrator prompt.
- Use the template in `references/orchestrator-prompt-template.md`.
- Keep role separation strict (orchestrator, verifier, researcher A, researcher B, planner, implementer, reviewer, QA, integrator).
- Keep limits explicit (default: 10 rounds, 10 attempts unless the goal says otherwise).
- Require verifier-owned row refreshes before returning gates.
- Require the planner to wait for both researcher summaries and reconcile evidence before implementation begins.
- Require failed-attempt accept-or-revert hygiene plus `PlannerDelta` revisions on attempts 2..N.
- Require one terminal status line.

4. Generate or refresh the JSONL orchestrator event log template.
- Use `references/orchestrated-round-template.md`.
- Record one JSON object per line with `event_type`, round, selected mechanism, attempt, producing agent, gate outputs, reason for each `NO`, and the retry/scope metadata fields.
- Treat `orchestrator-log.jsonl` as the single authoritative orchestrator log; keep human-readable narrative in `findings.md` / `progress.md`.

5. Cross-check consistency.
- Mechanism order must match across table, prompt, and log.
- Gate vocabulary must be exactly `YES` or `NO` everywhere.
- Final status vocabulary must be exactly `COMPLETED`, `FAILED`, or `MAXIMUMRETRY`.

## Optional Scaffold Script

Use the script when you need fast, repeatable scaffolding:

```bash
python3 scripts/scaffold_goal_loop_docs.py --goal "<goal>" --date YYYY-MM-DD
```

Important flags:
- `--source` source-of-truth anchor text
- `--mechanism` repeatable; specify ordered mechanisms
- `--table-path`, `--prompt-path`, `--round-path` for explicit output locations
- `--overwrite` to replace existing files intentionally

## Output Contract

When done, report:
- absolute paths of generated/updated artifacts
- whether each file was created or updated
- any assumptions used (especially provisional mechanism list)
