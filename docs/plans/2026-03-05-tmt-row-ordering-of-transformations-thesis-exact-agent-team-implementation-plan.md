# TMT Row Ordering of Transformations Thesis-Exact Agent-Team Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the Transformation Mechanism Table row `Ordering of transformations` materially more thesis-exact by tightening Phase-4 execution order around thesis `SolveConstraint` semantics (§12.1.3), reducing non-thesis global staging, and preserving delayed-weaken/translatability obligations from §15.2.

**Architecture:** The codebase already enforces thesis-like edge order and per-edge closure in `runPresolutionLoop`, but `computePresolution` still performs broad post-loop staging (`materializeExpansions -> flushPendingWeakens -> rewrite -> rigidify -> normalize`). This plan moves weaken flushing into per-edge boundaries, extracts a named translatability-construction/finalization stage with explicit invariants, and adds RED->GREEN guards so ordering semantics are enforced continuously rather than inferred from final state only.

**Tech Stack:** Haskell (`cabal`, `hspec`), presolution core modules under `src/MLF/Constraint/Presolution/*`, regression suites under `test/Presolution/*`, plus integration guards in `test/PipelineSpec.hs`.

**Execution Discipline:** `@haskell-pro`, `@test-driven-development`, `@subagent-driven-development`, `@verification-before-completion`, `@dispatching-parallel-agents`.

**Thesis anchors:**
- `papers/these-finale-english.txt` §12.1.3 (Figure 12.1.1 `SolveConstraint` ordering: topological order, initial unify, per-edge propagation + unify).
- `papers/these-finale-english.txt` §15.2.1 (propagation witness normalization with delayed weakenings).
- `papers/these-finale-english.txt` Def. 15.2.10 (constructive translatable presolution obligations).

---

## Team Topology (Agent Teams)

| Team | Scope | File Ownership |
|---|---|---|
| Team A (`contracts`) | RED->GREEN row3 guards + characterization tests | `test/Presolution/UnificationClosureSpec.hs`, `test/PipelineSpec.hs`, optional new `test/Presolution/OrderingTransformationsSpec.hs` |
| Team B (`loop-order`) | Move weaken flush semantics to edge-loop boundaries | `src/MLF/Constraint/Presolution/EdgeProcessing.hs` |
| Team C (`edge-weaken-core`) | Harden weaken queue/flush semantics for per-edge use | `src/MLF/Constraint/Presolution/EdgeUnify.hs` |
| Team D (`finalization`) | Extract/clean finalization stage, preserve constructive obligations | `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Constraint/Presolution/Materialization.hs`, `src/MLF/Constraint/Presolution/Validation.hs`, optional new `src/MLF/Constraint/Presolution/Finalization.hs` |
| Team E (`integration-docs`) | Integration gates, TMT/docs/task closeout | docs + task tracker files |

### Wave Order

- **Wave 0 (serial):** Task 1 (Team A RED baseline)
- **Wave 1 (parallel):** Task 2 (Team B) + Task 3 (Team C)
- **Wave 2 (serial):** Task 4 (Team D integration/finalization)
- **Wave 3 (serial):** Task 5 (Team E verification gates)
- **Wave 4 (serial):** Task 6 (Team E docs + ledger closeout)

## Merge Gates

1. **Gate A (RED baseline):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 ordering thesis-exact guard"'`
   - Expected: FAIL before Wave 1.
2. **Gate B (ordering core GREEN):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 ordering thesis-exact guard"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
   - Expected: PASS.
3. **Gate C (translatability and runtime parity):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Translatable presolution"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
   - Expected: PASS.
4. **Final Gate:**
   - `cabal build all && cabal test`

> If any matcher returns `0 examples`, run narrower fallback matchers and record non-empty evidence before advancing waves.

---

### Task 1: Add Row3 Ordering Guard (RED First)

**Agent Owner:** Team A (`contracts`)

**Files:**
- Modify: `test/Presolution/UnificationClosureSpec.hs`
- Modify: `test/PipelineSpec.hs`
- Optional create: `test/Presolution/OrderingTransformationsSpec.hs`
- If created, wire in: `test/Main.hs`, `mlf2.cabal`

**Step 1: Add strict row guard assertions**

Add `row3 ordering thesis-exact guard` checks that currently fail:
- `src/MLF/Constraint/Presolution/Driver.hs` does not perform global `flushPendingWeakens` in post-loop finalization path.
- `src/MLF/Constraint/Presolution/EdgeProcessing.hs` performs weaken flush at per-edge boundary (after edge processing and closure drain).
- Preserve existing boundary invariant expectations (no pending unify edges across edge boundaries).

**Step 2: Add behavior characterization tests**

- Extend `Phase 4 thesis-exact unification closure` tests to assert edge-boundary ordering semantics on a crafted constraint where delayed weaken would be observable.
- Keep assertions semantic (queue drained by edge boundary), not only source-string checks.

**Step 3: Confirm RED baseline**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 ordering thesis-exact guard"'`

Expected: FAIL.

**Step 4: Commit**

```bash
git add test/Presolution/UnificationClosureSpec.hs test/PipelineSpec.hs test/Presolution/OrderingTransformationsSpec.hs test/Main.hs mlf2.cabal
git commit -m "test: add row3 ordering thesis-exact RED guard"
```

### Task 2: Move Weaken Flush to Edge-Loop Boundary

**Agent Owner:** Team B (`loop-order`)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`

**Step 1: Introduce explicit per-edge boundary sequence**

Refactor loop ordering to an explicit boundary model aligned with §12.1.3 + delayed-weaken requirement:
- initial closure drain before first edge
- for each edge: `processInstEdge -> drain closure -> flush pending weakens -> (if needed) closure drain -> assert boundary`

**Step 2: Keep fail-fast boundary assertions**

- Preserve and tighten `assertNoPendingUnifyEdges` semantics before/after each edge boundary.
- Keep edge-context diagnostics for reproducibility.

**Step 3: Focused verification**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing.hs
git commit -m "refactor: enforce per-edge weaken flush boundary in presolution loop"
```

### Task 3: Harden Pending-Weaken Core for Boundary Flushes

**Agent Owner:** Team C (`edge-weaken-core`)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeUnify.hs`

**Step 1: Make boundary flush semantics explicit and idempotent**

- Ensure `flushPendingWeakens` is safe for repeated per-edge invocation.
- Keep no-op semantics for already-rigid/missing-parent nodes and preserve existing error surfaces.

**Step 2: Preserve witness-order intent**

- Keep note-level/documented invariant: weaken remains delayed relative to other edge-local operations, but is no longer deferred to global post-loop flush.
- Do not alter operation witness meaning.

**Step 3: Focused verification**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure|Translatable presolution"'`

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeUnify.hs
git commit -m "refactor: harden edge-local pending weaken flush semantics"
```

### Task 4: Extract Thesis-Named Finalization Stage and Remove Redundant Global Ordering Work

**Agent Owner:** Team D (`finalization`)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Driver.hs`
- Modify: `src/MLF/Constraint/Presolution/Materialization.hs`
- Modify: `src/MLF/Constraint/Presolution/Validation.hs`
- Optional create: `src/MLF/Constraint/Presolution/Finalization.hs`
- If created, wire in: `mlf2.cabal`

**Step 1: Extract post-loop actions into explicit finalization stage**

Create a named stage (inline or module) that performs only non-ordering artifact finalization:
- expansion materialization
- rewrite/canonicalization
- rigidification for translatability construction
- witness normalization

**Step 2: Remove global weaken-flush from Driver**

- Delete post-loop `flushPendingWeakens` invocation from `computePresolution`.
- Rely on per-edge boundary flush (Tasks 2/3).

**Step 3: Add constructive invariant checkpoints**

- Add fail-fast checkpoints at stage boundaries so translatability obligations are treated as construction invariants, not only final validation.
- Keep/extend existing checks for residual `TyExp`, residual edge queues, and witness/trace key contracts.

**Step 4: Focused verification**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 ordering thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Translatable presolution"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/Driver.hs src/MLF/Constraint/Presolution/Materialization.hs src/MLF/Constraint/Presolution/Validation.hs src/MLF/Constraint/Presolution/Finalization.hs mlf2.cabal
git commit -m "refactor: align row3 ordering with per-edge boundary and explicit finalization stage"
```

### Task 5: Verification Gates

**Agent Owner:** Team E (`integration-docs`)

**Step 1: Execute Gate B/C and full gate in order**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 ordering thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Translatable presolution"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- `cabal build all && cabal test`

**Step 2: Defect handling protocol**

- On any failure, add/update `Bugs.md` entry with reproducer, expected/actual, and owning modules.
- Do not advance to docs closeout until all required gates are green.

### Task 6: TMT/Docs/Ledger Closeout

**Agent Owner:** Team E (`integration-docs`)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md` (row `Ordering of transformations`)
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `tasks/todo/2026-03-05-tmt-ordering-of-transformations-thesis-exact-agent-team-plan/task_plan.md`
- Modify: `tasks/todo/2026-03-05-tmt-ordering-of-transformations-thesis-exact-agent-team-plan/findings.md`
- Modify: `tasks/todo/2026-03-05-tmt-ordering-of-transformations-thesis-exact-agent-team-plan/progress.md`
- Modify: `Bugs.md` (if applicable)

**Step 1: Update row narrative with evidence**

- Refresh `Current codebase` and `What to change` based on actual integrated behavior.
- Reclassify `Thesis-exact` only if strict criteria are met by code and gates; otherwise keep `No` with narrowed gap statement.

**Step 2: Update notes/changelog/TODO**

- Record exact gate outputs (example/failure counts).
- Update next priorities based on remaining divergence.

**Step 3: Commit**

```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md tasks/todo/2026-03-05-tmt-ordering-of-transformations-thesis-exact-agent-team-plan/task_plan.md tasks/todo/2026-03-05-tmt-ordering-of-transformations-thesis-exact-agent-team-plan/findings.md tasks/todo/2026-03-05-tmt-ordering-of-transformations-thesis-exact-agent-team-plan/progress.md Bugs.md
git commit -m "docs: close row3 ordering thesis-exact alignment evidence"
```

---

## Definition of Done

- Edge-loop execution order is explicit and thesis-shaped: initial closure, per-edge propagation/unify, edge-boundary delayed-weaken flush, boundary invariants.
- Driver no longer relies on global post-loop weaken flush for ordering semantics.
- Finalization stage is explicit and focused on artifact construction, with translatability checks treated as construction checkpoints.
- `row3 ordering thesis-exact guard` exists and passes with non-empty evidence.
- Existing presolution and runtime parity slices remain green (`Phase 4 thesis-exact unification closure`, `Translatable presolution`, `checked-authoritative`, `Dual-path verification`).
- Full validation gate passes (`cabal build all && cabal test`).
- TMT/docs/task trackers updated with concrete evidence and residual risks (if any).
