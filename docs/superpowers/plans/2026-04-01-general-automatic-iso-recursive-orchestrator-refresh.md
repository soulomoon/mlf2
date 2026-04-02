# General Automatic Iso-Recursive Orchestrator Refresh Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Retarget the live repo-local orchestrator control plane so the active roadmap bundle again drives bounded progress toward general automatic iso-recursive inference.

**Architecture:** Create one fresh successor roadmap family under `orchestrator/roadmaps/`, point `orchestrator/state.json` and the top-level pointer stubs at it, and retune the repo-local role prompts plus verification/retry contracts to the inherited automatic-iso-recursive boundary. Preserve all March families as predecessor evidence only and stop after the checkpoint commit.

**Tech Stack:** Markdown control-plane files, JSON state, git, existing repo-local orchestrator contract

---

### Task 1: Create The New Active Roadmap Bundle

**Files:**
- Create: `docs/superpowers/plans/2026-04-01-general-automatic-iso-recursive-orchestrator-refresh.md`
- Create: `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`
- Create: `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/verification.md`
- Create: `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/retry-subloop.md`

- [ ] **Step 1: Draft the roadmap family**

Write four serial items:
1. docs-only successor freeze
2. one bounded implementation/validation slice
3. one settlement/repo-impact read
4. one successor decision/handoff

Run: `sed -n '1,260p' orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`
Expected: the file exists and each item includes `Item id:`, `Depends on:`, `Parallel safe:`, `Parallel group:`, and `Merge after:`.

- [ ] **Step 2: Tailor the verification contract**

Make the baseline checks conditional on scope:
- pointer/state identity checks and `git diff --check` for every round
- `cabal build all && cabal test` when production/test files change
- `./scripts/thesis-conformance-gate.sh` when thesis-facing docs or scripts change

Run: `sed -n '1,260p' orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/verification.md`
Expected: the file contains `## Baseline Checks`, `## Task-Specific Checks`, `## Approval Criteria`, and `## Reviewer Record Format`.

- [ ] **Step 3: Tailor the retry contract**

Bind retry behavior to the four new items, keep retries bounded to the same round, and require escalation after three rejected attempts.

Run: `sed -n '1,260p' orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/retry-subloop.md`
Expected: the file references the new roadmap family and describes item-specific retry boundaries.

### Task 2: Retarget The Live Control Plane

**Files:**
- Modify: `orchestrator/state.json`
- Modify: `orchestrator/roadmap.md`
- Modify: `orchestrator/verification.md`
- Modify: `orchestrator/retry-subloop.md`

- [ ] **Step 1: Point state.json at the new active family**

Preserve the base branch and global round continuity, clear active-round fields, and set safe serial defaults.

Run: `python3 -m json.tool orchestrator/state.json`
Expected: valid JSON with `roadmap_id`, `roadmap_revision`, and `roadmap_dir` set to the new family.

- [ ] **Step 2: Repair the top-level pointer stubs**

Make all three stubs match the values in `state.json` exactly.

Run: `sed -n '1,40p' orchestrator/roadmap.md && printf '\n@@\n' && sed -n '1,40p' orchestrator/verification.md && printf '\n@@\n' && sed -n '1,40p' orchestrator/retry-subloop.md`
Expected: all three files point at the same new roadmap bundle.

### Task 3: Retune The Repo-Local Roles

**Files:**
- Modify: `orchestrator/roles/guider.md`
- Modify: `orchestrator/roles/planner.md`
- Modify: `orchestrator/roles/implementer.md`
- Modify: `orchestrator/roles/reviewer.md`
- Modify: `orchestrator/roles/merger.md`
- Modify: `orchestrator/roles/recovery-investigator.md`

- [ ] **Step 1: Replace stale code-quality wording**

Reframe each role around the automatic-iso-recursive successor loop instead of CI matrix work.

Run: `for f in guider planner implementer reviewer merger recovery-investigator; do echo "@@ $f @@"; sed -n '1,120p' orchestrator/roles/$f.md; done`
Expected: none of the role files still describe the completed code-quality roadmap as the active subject.

- [ ] **Step 2: Preserve the inherited boundary in the role guidance**

State explicitly that the live goal is bounded current-architecture progress under the inherited explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback boundary unless a later accepted roadmap revision changes it.

Run: `rg -n "explicit-only|iso-recursive|non-equi-recursive|non-cyclic-graph|no-fallback" orchestrator/roles`
Expected: the updated role set mentions the inherited boundary where relevant.

### Task 4: Verify The Scaffold And Create The Checkpoint Commit

**Files:**
- Modify: `orchestrator/**`

- [ ] **Step 1: Run control-plane verification**

Run:
- `python3 -m json.tool orchestrator/state.json`
- `test -f orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`
- `test -f orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/verification.md`
- `test -f orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/retry-subloop.md`
- `git diff --check -- orchestrator`

Expected: all commands succeed with no diff-check errors.

- [ ] **Step 2: Stage only orchestrator files and commit**

Run:
- `git add orchestrator`
- `git commit -m "Retarget orchestrator to auto iso-recursive successor"`

Expected: one checkpoint commit containing only orchestrator refresh files.
