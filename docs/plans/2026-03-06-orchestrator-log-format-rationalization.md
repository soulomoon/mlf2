# Orchestrator Log Format Rationalization Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Standardize orchestrator workflow logging on `orchestrator-log.jsonl` as the single authoritative execution log.

**Architecture:** Update the live orchestration contract first, then align the reusable goal-loop skill and scaffold so new artifacts inherit the same JSONL-only policy. Keep human-readable narrative in the existing planning files instead of a second orchestrator markdown log.

**Tech Stack:** Markdown docs, Python scaffold script.

---

### Task 1: Update live orchestration docs

**Files:**
- Modify: `docs/plans/2026-03-06-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-2.md`
- Modify: `docs/prompts/improving-loop-agent.prompt2.md`
- Modify: `docs/prompts/goal-improving-loop-agent.prompt.md`

**Step 1: Remove the duplicate markdown orchestrator-log requirement**

Edit the round-2 plan so run initialization requires `orchestrator-log.jsonl` rather than `orchestrator-log.md`, and rewrite the logging section so JSONL is the single authoritative event log.

**Step 2: Make the prompt-level contract explicit**

Update the live prompts so they instruct the orchestrator to append machine-checkable JSONL event records and keep narrative summaries in `findings.md` / `progress.md`.

**Step 3: Verify wording consistency**

Run: `rg -n 'orchestrator-log\\.(md|jsonl)|JSONL|findings.md|progress.md' docs/plans/2026-03-06-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-2.md docs/prompts/improving-loop-agent.prompt2.md docs/prompts/goal-improving-loop-agent.prompt.md`

Expected: live docs mention only `orchestrator-log.jsonl` as the required orchestrator log artifact.

### Task 2: Update reusable skill/scaffold assets

**Files:**
- Modify: `.codex/skills/goal-table-orchestrator-loop/SKILL.md`
- Modify: `.codex/skills/goal-table-orchestrator-loop/references/orchestrated-round-template.md`
- Modify: `.codex/skills/goal-table-orchestrator-loop/references/orchestrator-prompt-template.md`
- Modify: `.codex/skills/goal-table-orchestrator-loop/scripts/scaffold_goal_loop_docs.py`

**Step 1: Rewrite the skill contract**

Change the skill language from "markdown round execution log" to "JSONL orchestrator event log" and explain that human-readable summaries live elsewhere.

**Step 2: Update the reference template**

Replace the markdown table example with JSONL sample records that show header/setup, gate events, and final status.

**Step 3: Update scaffold output**

Make the scaffold script generate a `.jsonl` log artifact by default and emit JSONL template lines instead of markdown.

**Step 4: Verify the scaffold text paths**

Run: `python3 -m py_compile .codex/skills/goal-table-orchestrator-loop/scripts/scaffold_goal_loop_docs.py`

Expected: command succeeds with no syntax errors.

### Task 3: Record the workflow change

**Files:**
- Modify: `CHANGELOG.md`

**Step 1: Add a concise unreleased entry**

Document that orchestrator logging now uses JSONL as the single authoritative execution log and that markdown summary responsibility moved to `findings.md` / `progress.md`.

### Task 4: Final verification

**Files:**
- Inspect only

**Step 1: Search for stale live dual-log references**

Run: `rg -n 'orchestrator-log\\.md|orchestrator-log\\.jsonl' docs .codex`

Expected: live docs/scaffolds no longer require both formats; archived references may remain if they are clearly historical.

**Step 2: Inspect working tree summary**

Run: `git diff --stat`

Expected: only the targeted docs/scaffold files plus planning artifacts are changed.
