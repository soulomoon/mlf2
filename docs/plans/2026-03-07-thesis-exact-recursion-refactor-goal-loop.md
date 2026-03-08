# Thesis-Exact Recursion Refactor Goal Loop Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Produce the thesis-exact mechanism table, simplified improving-loop prompt, and JSONL log template for the recursion-refactor campaign.

**Architecture:** Reuse the repo’s goal-loop templates, but replace placeholders with an 8-row mechanism set derived from the thesis and the current code/test anchors. Keep the prompt aligned with the approved simplified role model and keep `orchestrator-log.jsonl` as the only authoritative execution log.

**Tech Stack:** Markdown, JSONL, `docs/thesis-obligations.yaml`, Haskell module/test anchors, Python scaffold script at `.codex/skills/goal-table-orchestrator-loop/scripts/scaffold_goal_loop_docs.py`.

---

### Task 1: Freeze the approved design contract

**Files:**
- Modify: `docs/plans/2026-03-07-thesis-exact-recursion-refactor-goal-loop-design.md`
- Reference: `papers/these-finale-english.txt`
- Reference: `docs/thesis-obligations.yaml`

**Step 1: Copy the approved scope into the design doc**

Write the approved source-of-truth policy, broad scope decision, and 8-row mechanism list into the design doc.

**Step 2: Verify the design doc mentions the simplified role set**

Run: `rg -n 'Researcher A|Researcher B|Planner|Verifier|Integrator' docs/plans/2026-03-07-thesis-exact-recursion-refactor-goal-loop-design.md`
Expected: only `Planner`, `Verifier`, and `Integrator` appear; `Researcher A` / `Researcher B` do not.

**Step 3: Record the artifact contract**

List the table, prompt, JSONL template, and implementation-plan paths explicitly.

**Step 4: Commit**

```bash
git add docs/plans/2026-03-07-thesis-exact-recursion-refactor-goal-loop-design.md
git commit -m "docs: record thesis-exact recursion-refactor goal-loop design"
```

### Task 2: Populate the mechanism table

**Files:**
- Modify: `docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md`
- Reference: `docs/thesis-obligations.yaml`
- Reference: `src/MLF/Frontend/Normalize.hs`
- Reference: `src/MLF/Elab/Sigma.hs`
- Test: `test/ElaborationSpec.hs`
- Test: `test/Presolution/EnforcementSpec.hs`
- Test: `test/ScopeSpec.hs`

**Step 1: Scaffold the table from the goal-loop script**

Run:
```bash
python3 .codex/skills/goal-table-orchestrator-loop/scripts/scaffold_goal_loop_docs.py \
  --goal "Thesis-exact Haskell refactoring and recursion-schemes simplification without violating gMLF/xMLF graph semantics" \
  --date 2026-03-07 \
  --source "papers/these-finale-english.txt (primary); papers/xmlf.txt only if the thesis is silent" \
  --mechanism "Surface Preprocessing Exactness" \
  --mechanism "Leftmost-Lowermost Quantifier Ordering" \
  --mechanism "Let-Scope Translation Discipline" \
  --mechanism "Translatable Presolution Boundary" \
  --mechanism "Typing Environment Construction" \
  --mechanism "Computation Context Construction" \
  --mechanism "Binder-Safe Tree Recursion Coverage" \
  --mechanism "Graph-Phase Explicitness Guardrail" \
  --table-path docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md \
  --overwrite
```
Expected: a table file exists with the fixed 8-row order.

**Step 2: Replace placeholders with thesis-first row content**

For each row, fill `Current`, `Target`, `Gap`, `Evidence`, and `Next action` using thesis section labels plus concrete code/test anchors.

**Step 3: Verify gate vocabulary and row order**

Run: `rg -n '\| (YES|NO) \|' docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md`
Expected: every row gate is `NO` initially and no other gate vocabulary appears.

**Step 4: Commit**

```bash
git add docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md
git commit -m "docs: add thesis-exact recursion-refactor mechanism table"
```

### Task 3: Tailor the improving-loop prompt

**Files:**
- Modify: `docs/prompts/thesis-exact-recursion-refactor-improving-loop-agent.prompt.md`
- Reference: `docs/prompts/goal-improving-loop-agent.prompt.md`
- Reference: `docs/prompts/improving-loop-agent.prompt2.md`

**Step 1: Scaffold the prompt**

Run the same scaffold command as Task 2 with `--prompt-path docs/prompts/thesis-exact-recursion-refactor-improving-loop-agent.prompt.md`.

**Step 2: Replace the default role model**

Remove `Researcher A` / `Researcher B` and make `Planner` own thesis/code research plus evidence reconciliation.

**Step 3: Add campaign-specific planner rules**

Require the planner to classify the anchor row as `positive_refactor` or `guardrail`, and spell out that guardrail rows may close via explicit-code preservation plus tests/docs.

**Step 4: Verify the prompt contract**

Run:
```bash
rg -n 'Researcher A|Researcher B|positive_refactor|guardrail|FINAL STATUS: COMPLETED|FINAL STATUS: FAILED|FINAL STATUS: MAXIMUMRETRY' docs/prompts/thesis-exact-recursion-refactor-improving-loop-agent.prompt.md
```
Expected: no `Researcher A` / `Researcher B`; all three final statuses and both row kinds appear.

**Step 5: Commit**

```bash
git add docs/prompts/thesis-exact-recursion-refactor-improving-loop-agent.prompt.md
git commit -m "docs: add thesis-exact recursion-refactor improving-loop prompt"
```

### Task 4: Define the JSONL event contract

**Files:**
- Modify: `docs/prompts/2026-03-07-orchestrated-execution-thesis-exact-recursion-refactor-codex-subagents-fresh-round-2.jsonl`
- Reference: `docs/prompts/improving-loop-agent.prompt2.md`

**Step 1: Scaffold the JSONL template**

Run the scaffold command with `--round-path docs/prompts/2026-03-07-orchestrated-execution-thesis-exact-recursion-refactor-codex-subagents-fresh-round-2.jsonl`.

**Step 2: Expand the event fields**

Add `row_kind`, planner retry metadata, and scope-expansion fields while preserving exact `YES` / `NO` gates and exact terminal statuses.

**Step 3: Validate JSONL syntax**

Run:
```bash
python3 - <<'PY'
import json, pathlib
path = pathlib.Path('docs/prompts/2026-03-07-orchestrated-execution-thesis-exact-recursion-refactor-codex-subagents-fresh-round-2.jsonl')
for i, line in enumerate(path.read_text().splitlines(), 1):
    json.loads(line)
print('ok')
PY
```
Expected: `ok`.

**Step 4: Commit**

```bash
git add docs/prompts/2026-03-07-orchestrated-execution-thesis-exact-recursion-refactor-codex-subagents-fresh-round-2.jsonl
git commit -m "docs: add thesis-exact recursion-refactor JSONL log template"
```

### Task 5: Sync tracker docs and validate consistency

**Files:**
- Modify: `TODO.md`
- Modify: `CHANGELOG.md`
- Reference: `tasks/archive/2026-03-07-thesis-exact-recursion-refactor-goal-loop/`

**Step 1: Add the in-progress / completed tracker note**

Update `TODO.md` with the campaign’s next priorities.

**Step 2: Add the changelog entry**

Summarize the new design/table/prompt/log artifacts in `CHANGELOG.md`.

**Step 3: Run a consistency sweep**

Run:
```bash
rg -n 'Surface Preprocessing Exactness|Leftmost-Lowermost Quantifier Ordering|Let-Scope Translation Discipline|Translatable Presolution Boundary|Typing Environment Construction|Computation Context Construction|Binder-Safe Tree Recursion Coverage|Graph-Phase Explicitness Guardrail' \
  docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md \
  docs/prompts/thesis-exact-recursion-refactor-improving-loop-agent.prompt.md \
  docs/prompts/2026-03-07-orchestrated-execution-thesis-exact-recursion-refactor-codex-subagents-fresh-round-2.jsonl
```
Expected: the same fixed mechanism order appears across all three artifacts.

**Step 4: Commit**

```bash
git add TODO.md CHANGELOG.md tasks/archive/2026-03-07-thesis-exact-recursion-refactor-goal-loop/
git commit -m "docs: sync thesis-exact recursion-refactor campaign trackers"
```
