# TMT Closeout Doc Sync Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Sync the live Transformation Mechanism Table note and directly coupled trackers to the current fully-closed thesis-exact state.

**Architecture:** Treat the archived row6 blocked run and recovery/orchestrator artifacts as historical evidence, then rewrite only the live summaries that still present row6 as open. Keep the note’s row table and archived execution logs intact, but add superseding closeout context and move directly related completed task folders out of `tasks/todo/`.

**Tech Stack:** Markdown trackers, repository task folders, lightweight shell validation.

---

### Task 1: Rewrite the live TMT note tail

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`

**Step 1: Write the failing test**

Use a content check: the note should no longer end with row6 as the current live status.

**Step 2: Run test to verify it fails**

Run: `rg -n 'orchestrated runner row6 status|Treat row6 as cross-phase work' docs/notes/2026-02-27-transformation-mechanism-table.md`
Expected: matches still exist before the edit.

**Step 3: Write minimal implementation**

Replace the stale tail with a superseding 2026-03-07 closeout section that records the row6 recovery and fresh full sweep, then restate only steady-state regression priorities.

**Step 4: Run test to verify it passes**

Run: `rg -n '2026-03-07 fresh round-2 full sweep closeout|steady-state regression priorities' docs/notes/2026-02-27-transformation-mechanism-table.md`
Expected: both new markers exist.

### Task 2: Sync coupled trackers

**Files:**
- Modify: `TODO.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`

**Step 1: Write the failing test**

Use content checks: live trackers should stop treating row6 rerun/audit as future work.

**Step 2: Run test to verify it fails**

Run: `rg -n 'Re-run the row6 goal-table/orchestrator verification|Audit row6 docs/prompts/task artifacts' TODO.md implementation_notes.md`
Expected: stale future-work text still matches before the edit.

**Step 3: Write minimal implementation**

Add a concise closeout-sync entry, rewrite stale next steps into maintenance guardrails, and mark the historical row6 blocked run as superseded.

**Step 4: Run test to verify it passes**

Run: `rg -n 'superseded|all 14 mechanisms|steady-state' TODO.md implementation_notes.md CHANGELOG.md`
Expected: new closeout wording appears.

### Task 3: Archive directly related completed task folders

**Files:**
- Move: `tasks/todo/2026-03-06-tmt-identity-row-update`
- Move: `tasks/todo/2026-03-05-goal-table-orchestrator-loop-skill`

**Step 1: Write the failing test**

Use presence checks: the completed folders still live under `tasks/todo/`.

**Step 2: Run test to verify it fails**

Run: `find tasks/todo -maxdepth 1 -mindepth 1 -type d | rg '2026-03-06-tmt-identity-row-update|2026-03-05-goal-table-orchestrator-loop-skill'`
Expected: both folders appear before the move.

**Step 3: Write minimal implementation**

Move the completed folders to `tasks/archive/` unchanged.

**Step 4: Run test to verify it passes**

Run: `find tasks/archive -maxdepth 1 -mindepth 1 -type d | rg '2026-03-06-tmt-identity-row-update|2026-03-05-goal-table-orchestrator-loop-skill'`
Expected: both folders appear under archive.

### Task 4: Validate the closeout sync

**Files:**
- Verify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Verify: `TODO.md`
- Verify: `implementation_notes.md`
- Verify: `CHANGELOG.md`

**Step 1: Run focused validation**

Run: `git diff --check && rg -n 'MAXIMUMRETRY' docs/notes/2026-02-27-transformation-mechanism-table.md TODO.md implementation_notes.md`
Expected: no diff-format issues; any remaining `MAXIMUMRETRY` references are explicitly historical/superseded.

**Step 2: Review final task placement**

Run: `find tasks/todo -maxdepth 1 -mindepth 1 -type d | sort`
Expected: the archived completed task folders no longer appear.
