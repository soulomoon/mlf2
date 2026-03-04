# TMT Row3 Ordering Absolute Thesis-Exact Agent-Team Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move TMT row `Ordering of transformations` from loop-final delayed-weaken flushing toward strict thesis-shaped ordering boundaries (per `SolveConstraint` semantics), while preserving current regression/parity behavior.

**Architecture:** Current Phase-4 flow already has topological edge traversal plus unification-closure draining, but delayed weakens are still flushed at loop-final boundary. Prior per-edge flush attempts regressed on locked-node paths (`BUG-2026-03-05-001`). This plan introduces owner-aware pending-weaken scheduling so weakens flush at safe edge boundaries, not only at loop end, with explicit contracts and RED->GREEN evidence.

**Tech Stack:** Haskell (`cabal`, `hspec`), presolution modules under `src/MLF/Constraint/Presolution/*`, gates in `test/PipelineSpec.hs`, `test/Presolution/*`, `test/ElaborationSpec.hs`, and `test/FrozenParitySpec.hs`.

**Execution Discipline:** `@haskell-pro`, `@test-driven-development`, `@subagent-driven-development`, `@dispatching-parallel-agents`, `@systematic-debugging`, `@verification-before-completion`.

---

## Thesis Anchors

- `papers/these-finale-english.txt` §12.1.3 Figure 12.1.1 (`SolveConstraint`):
  - solve unification edges,
  - traverse instantiation edges in dependency order,
  - per-edge propagation + unification.
- `papers/these-finale-english.txt` §15.2.1:
  - delayed weaken semantics in normalized propagation witnesses must remain valid.
- `papers/these-finale-english.txt` Def. 15.2.10:
  - translatability obligations are constructive runtime obligations, not optional post-hoc checks.

## Current Gap Snapshot

- `runPresolutionLoop` currently flushes delayed weakens only once after processing all instantiation edges.
- Row3 remains `Thesis-exact = No` because boundary weakening is still loop-final, not strict edge-boundary.
- Historical risk: naive per-edge flush caused `OperationOnLockedNode`, `make const`, `BUG-002-V1`, and frozen parity regressions (`BUG-2026-03-05-001`).

---

## Team Topology (Agent Teams)

| Team | Scope | File Ownership |
|---|---|---|
| Team A (`contracts`) | RED->GREEN ordering contracts and semantic characterization tests | `test/PipelineSpec.hs`, `test/Presolution/UnificationClosureSpec.hs`, optional `test/Presolution/OrderingTransformationsSpec.hs` |
| Team B (`pending-weakens-model`) | Owner-aware pending weaken state model | `src/MLF/Constraint/Presolution/Base.hs`, `src/MLF/Constraint/Presolution/EdgeUnify.hs`, `src/MLF/Constraint/Presolution/StateAccess.hs` |
| Team C (`scheduler`) | Edge-loop flush scheduling and boundary policy | `src/MLF/Constraint/Presolution/EdgeProcessing.hs`, optional `src/MLF/Constraint/Presolution/EdgeProcessing/Schedule.hs` |
| Team D (`regression-shield`) | Locked-node regression hardening and translatability/finalization safety | `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Constraint/Presolution/EdgeUnify.hs`, targeted tests |
| Team E (`integration-docs`) | Gate execution, table/docs/ledger closeout | `docs/notes/2026-02-27-transformation-mechanism-table.md`, `implementation_notes.md`, `CHANGELOG.md`, `TODO.md`, task tracker files |

### Wave Order

- **Wave 0 (serial):** Team A RED contract baseline
- **Wave 1 (parallel):** Team B + Team C model/scheduler implementation
- **Wave 2 (serial):** Team D regression hardening and integration
- **Wave 3 (serial):** Team E verification gates
- **Wave 4 (serial):** Team E docs/table closeout

---

## Merge Gates

1. **Gate A (RED baseline must fail before Wave 1):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 absolute thesis-exact guard"'`
   - Expected before Wave 1: FAIL (non-empty matcher output).

2. **Gate B (ordering core GREEN):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 absolute thesis-exact guard"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Translatable presolution"'`

3. **Gate C (regression shield GREEN):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "generalizes reused constructors via make const"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V1"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`

4. **Final Gate:**
   - `cabal build all && cabal test`

> If any matcher returns `0 examples`, rerun with narrower matcher and log non-empty evidence in task tracker files.

---

### Task 1: Add Absolute Row3 Contracts (RED First)

**Agent Owner:** Team A (`contracts`)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/Presolution/UnificationClosureSpec.hs`
- Optional create: `test/Presolution/OrderingTransformationsSpec.hs`
- If created, wire in: `test/Main.hs`, `mlf2.cabal`

**Step 1: Add new strict guard matcher**
- Add tests under matcher label `row3 absolute thesis-exact guard`.
- Assert loop scheduling contract is not loop-final-only (source-level contract check).
- Assert edge-loop boundary scheduling API/hook is present (not just `flushPendingWeakens` substring).

**Step 2: Add semantic characterization tests**
- Add one fixture where weakens from a closed edge owner must flush at boundary.
- Add one fixture where dependent-owner weakens must remain deferred until their safe boundary (prevents premature locked-node failures).
- Keep assertions semantic, not only source-string checks.

**Step 3: Verify RED baseline**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 absolute thesis-exact guard"'`

Expected: FAIL.

**Step 4: Commit**
```bash
git add test/PipelineSpec.hs test/Presolution/UnificationClosureSpec.hs test/Presolution/OrderingTransformationsSpec.hs test/Main.hs mlf2.cabal
git commit -m "test: add row3 absolute thesis-exact RED contracts"
```

### Task 2: Introduce Owner-Aware Pending Weaken Model

**Agent Owner:** Team B (`pending-weakens-model`)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Base.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeUnify.hs`
- Modify: `src/MLF/Constraint/Presolution/StateAccess.hs`
- Optional: `src/MLF/Constraint/Presolution/Ops.hs`

**Step 1: Replace monolithic pending weaken set with owner-aware model**
- Replace `psPendingWeakens :: IntSet` with a keyed structure that preserves weaken owner/provenance.
- Add helper APIs for:
  - queue-by-owner,
  - flush subset of owners,
  - inspect pending owners for assertions/debugging.

**Step 2: Keep backward compatibility wrappers while migrating call sites**
- Keep temporary compatibility helpers to avoid broad breakage during Wave 1.
- Ensure no behavior change until scheduler migration (Task 3).

**Step 3: Focused verification**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`

Expected: still green except intentional RED guard.

**Step 4: Commit**
```bash
git add src/MLF/Constraint/Presolution/Base.hs src/MLF/Constraint/Presolution/EdgeUnify.hs src/MLF/Constraint/Presolution/StateAccess.hs src/MLF/Constraint/Presolution/Ops.hs
git commit -m "refactor: add owner-aware pending weaken model"
```

### Task 3: Implement Safe Edge-Boundary Scheduler

**Agent Owner:** Team C (`scheduler`)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`
- Optional create: `src/MLF/Constraint/Presolution/EdgeProcessing/Schedule.hs`
- Optional modify: `src/MLF/Constraint/Acyclicity.hs` (only if extra dependency metadata is required)

**Step 1: Add safe-boundary flush policy**
- Compute closable weaken owners at each edge boundary.
- Flush only closable owners at the boundary; never rely on unconditional loop-final-only flush.
- Preserve strict no-pending-unify boundary assertions.

**Step 2: Preserve delayed-weaken witness intent**
- Keep weaken ordering constraints relative to edge-local operations (no early semantic reordering).
- Document the boundary policy with a note block in module.

**Step 3: Remove residual loop-final-only scheduling path**
- Eliminate unconditional loop-final flush branch if boundary policy covers all owners.
- Keep final invariant assertion that pending owners are empty by loop completion.

**Step 4: Focused verification**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 absolute thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`

**Step 5: Commit**
```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing.hs src/MLF/Constraint/Presolution/EdgeProcessing/Schedule.hs src/MLF/Constraint/Acyclicity.hs
git commit -m "refactor: enforce safe edge-boundary weaken scheduling"
```

### Task 4: Regression Shield and Finalization Safety

**Agent Owner:** Team D (`regression-shield`)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Driver.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeUnify.hs`
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`
- Modify: `test/FrozenParitySpec.hs`

**Step 1: Harden locked-node handling under new schedule**
- Ensure `OperationOnLockedNode` paths remain benign where semantically idempotent.
- Preserve fail-fast behavior for genuine invariant violations.

**Step 2: Keep constructive translatability checkpoints**
- Ensure finalization boundary checks work with owner-aware pending structures.
- Preserve existing TyExp/witness/trace invariant checks.

**Step 3: Verify known regression triad**
Run:
- `--match "generalizes reused constructors via make const"`
- `--match "BUG-002-V1"`
- `--match "Frozen parity artifact baseline"`

**Step 4: Defect protocol**
- For any new failure, add `Bugs.md` entry in the same iteration with reproducer and owning modules.

**Step 5: Commit**
```bash
git add src/MLF/Constraint/Presolution/Driver.hs src/MLF/Constraint/Presolution/EdgeUnify.hs test/PipelineSpec.hs test/ElaborationSpec.hs test/FrozenParitySpec.hs Bugs.md
git commit -m "fix: harden row3 boundary scheduling against locked-node regressions"
```

### Task 5: Verification Gates

**Agent Owner:** Team E (`integration-docs`)

**Step 1: Run Gate B and Gate C sequentially**
- Run all commands listed in Gate B and Gate C.
- Record example/failure counts in tracker files.

**Step 2: Run Final Gate**
- `cabal build all && cabal test`

**Step 3: Stop condition**
- Do not proceed to docs closeout until all gates are green and non-empty where required.

### Task 6: Table/Docs/Ledger Closeout

**Agent Owner:** Team E (`integration-docs`)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md` (row `Ordering of transformations`)
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `tasks/todo/2026-03-05-row3-ordering-absolute-thesis-exact-agent-team-replan/task_plan.md`
- Modify: `tasks/todo/2026-03-05-row3-ordering-absolute-thesis-exact-agent-team-replan/findings.md`
- Modify: `tasks/todo/2026-03-05-row3-ordering-absolute-thesis-exact-agent-team-replan/progress.md`
- Modify: `Bugs.md` (if needed)

**Step 1: Update row narrative with post-integration facts**
- Refresh `Current codebase` and `What to change` text from actual merged behavior.
- Reclassify to `Yes` only if strict criterion is satisfied by runtime + tests.
- If strict criterion is still unmet, keep `No` with explicit residual gap statement.

**Step 2: Evidence sync**
- Add concrete gate evidence counts to docs.
- Sync implementation notes and TODO next priorities.

**Step 3: Commit**
```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md tasks/todo/2026-03-05-row3-ordering-absolute-thesis-exact-agent-team-replan/task_plan.md tasks/todo/2026-03-05-row3-ordering-absolute-thesis-exact-agent-team-replan/findings.md tasks/todo/2026-03-05-row3-ordering-absolute-thesis-exact-agent-team-replan/progress.md Bugs.md
git commit -m "docs: close row3 absolute ordering follow-up evidence"
```

---

## Definition of Done

- Pending weaken scheduling is no longer loop-final-only; boundary flush is owner-aware and enforced during edge traversal.
- Row3 absolute guard matcher is non-empty and green.
- Regression shield triad (`make const`, `BUG-002-V1`, frozen parity) remains green.
- Existing safety slices remain green (`Phase 4 thesis-exact unification closure`, `Translatable presolution`, `checked-authoritative`, `Dual-path verification`).
- Final gate passes (`cabal build all && cabal test`).
- TMT row and docs are updated with concrete evidence and an honest final classification.
