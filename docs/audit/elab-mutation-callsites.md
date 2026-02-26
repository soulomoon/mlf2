# Audit: Graph Mutation Call Sites in Elaboration Path

**Date:** 2026-02-27
**Scope:** `src/MLF/Elab/` only (presolution path excluded)
**Purpose:** Identify all places where the elaboration path mutates or rebuilds the constraint graph, to support Phase D pipeline boundary enforcement.

## Mutation Functions Searched

- `rebuildWithConstraint`
- `rebuildWithNodes`
- `rebuildWithBindParents`
- `rebuildWithGenNodes`
- `patchNode`
- `pruneBindParentsSolved`
- Direct record updates: `{ cNodes = }`, `{ cBindParents = }`, `{ cGenNodes = }`

## Functions Not Found in Elab Path

- `rebuildWithNodes` — no hits
- `rebuildWithBindParents` — no hits
- `rebuildWithGenNodes` — no hits
- `{ cGenNodes = }` — no standalone hits (only appears inside multi-field record construction, covered below)

## Call Site Inventory

### 1. `rebuildWithConstraint` — Pipeline.hs (pipeline setup)

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Run/Pipeline.hs` |
| **Line** | 104 |
| **Context** | Inside `setSolvedConstraint` helper, which canonicalizes a constraint via `rewriteConstraintWithUF` then rebuilds. Called at line 129 to produce `solvedForGen`. |
| **Persists?** | Yes — produces the `Solved` value used for the rest of the pipeline. |
| **Classification** | **Pipeline setup** — runs before elaboration begins; constructs the Solved handle that elaboration will read from. |
| **Elimination plan** | Low priority. This is pre-elaboration setup, not mutation during elaboration. Will be naturally replaced when the projection-first pipeline (Task 17) removes the need for constraint rewriting. |

### 2. `rebuildWithConstraint` — Generalize.hs (alias insertion for reification)

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Generalize.hs` |
| **Line** | 413 |
| **Context** | Creates `resAlias` by rebuilding with `constraintAlias`, a locally-constructed constraint that adds alias nodes for reification. The rebuilt Solved is passed to `reifyWithOrig` and `inlineRigidOrig` but never written back to the pipeline's handle. |
| **Persists?** | No — local to the reification block. |
| **Classification** | **Local projection rebuild** — creates a new Solved value scoped to the reification call; does not mutate the pipeline's Solved handle. |
| **Elimination plan** | Medium priority. Can be replaced by projection-first queries once available (Task 17). The alias nodes are only needed for pretty-printing, so this rebuild can be deferred to a read-only projection. |

### 3. `pruneBindParentsSolved` — Pipeline.hs (pipeline cleanup)

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Run/Pipeline.hs` |
| **Line** | 105 |
| **Context** | Produces `solvedClean` by pruning stale bind-parent entries from the freshly-built Solved. Used immediately after `buildSolved`. |
| **Persists?** | Yes — `solvedClean` becomes the base Solved for the rest of the pipeline. |
| **Classification** | **Pipeline setup** — runs before elaboration begins; cleans up the Solved handle. |
| **Elimination plan** | Low priority. Same as call site 1: pre-elaboration setup. Will be absorbed into the projection-first pipeline. |

### 4. `patchNode` — Fallback.hs (TyVar bound patching)

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Run/ResultType/Fallback.hs` |
| **Line** | 487 |
| **Context** | When `boundTarget` is `Just baseN`, patches the node at `rootC` to add a `tnBound` field. Produces `resFinalBounded`, which is then used for scope resolution and passed to `generalizeWithPlan`. |
| **Persists?** | Yes — the patched Solved (`resFinalBounded`) is used for downstream scope resolution and generalization within the fallback path. |
| **Classification** | **Graph mutation in elab** — actual mutation of the Solved graph during elaboration. This is the primary target for elimination. |
| **Elimination plan** | **High priority (Task 14).** Replace with a local projection that overlays the bound information without mutating the underlying Solved. The bound is only needed for scope resolution, so a read-only view suffices. |

### 5. Direct `{ cNodes = }` — Fallback.hs line 700 (base constraint adjustment)

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Run/ResultType/Fallback.hs` |
| **Line** | 700 |
| **Context** | Inside the bind-parents generalization-args setup. Creates `baseConstraint'` by adjusting nodes in `baseConstraint` (adding a bound to the base root node), then stores it in `gaBaseConstraint`. This is a local Constraint value used for generalization, not a Solved mutation. |
| **Persists?** | No — local to the `bindParentsGaFinal` computation. |
| **Classification** | **Read-only temporary** — constructs a local Constraint for generalization-args; never fed back into the pipeline's Solved. |
| **Elimination plan** | None needed. Already a local temporary. |

### 6. Direct `{ cNodes = }` — Phase3.hs line 44

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Run/Generalize/Phase3.hs` |
| **Line** | 44 |
| **Context** | Creates `upperConstraint` from `solvedConstraint` with solved-domain nodes. Used only for `mkIsUpperRef` predicate. |
| **Persists?** | No — local binding for a predicate function. |
| **Classification** | **Read-only temporary** — creates a local Constraint to define the `isUpperRef` predicate. Never persisted. |
| **Elimination plan** | None needed. Already a local temporary. |

### 7. Direct `{ cNodes = }` — Phase4.hs line 77

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Run/Generalize/Phase4.hs` |
| **Line** | 77 |
| **Context** | Creates `upperConstraint` from `solvedConstraint` with solved-domain nodes. Used only for `mkIsUpperRef` predicate. Identical pattern to Phase3. |
| **Persists?** | No — local binding for a predicate function. |
| **Classification** | **Read-only temporary** — creates a local Constraint to define the `isUpperRef` predicate. Never persisted. |
| **Elimination plan** | None needed. Already a local temporary. |

### 8. Direct `{ cNodes = , cGenNodes = , cBindParents = }` — Phase4.hs line 622–626

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Run/Generalize/Phase4.hs` |
| **Line** | 622–626 |
| **Context** | Inside `attachOrphans`. Creates `constraint0` from `solvedConstraint` with all three fields set, used to compute `bindingRoots` for orphan attachment. |
| **Persists?** | No — local to `attachOrphans` helper. |
| **Classification** | **Read-only temporary** — constructs a local Constraint for bind-parent computation. Never persisted. |
| **Elimination plan** | None needed. Already a local temporary. |

### 9. Direct `{ cNodes = }` — Finalize.hs line 65

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Run/Generalize/Finalize.hs` |
| **Line** | 65 |
| **Context** | Creates `upperConstraint` from `solvedConstraint` with solved-domain nodes. Used only for `mkIsUpperRef` predicate. Same pattern as Phase3 and Phase4. |
| **Persists?** | No — local binding for a predicate function. |
| **Classification** | **Read-only temporary** — creates a local Constraint to define the `isUpperRef` predicate. Never persisted. |
| **Elimination plan** | None needed. Already a local temporary. |

### 10. Direct `{ cNodes = , cBindParents = , cGenNodes = }` — Finalize.hs line 109–113

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Run/Generalize/Finalize.hs` |
| **Line** | 109–113 |
| **Context** | Creates `constraint0` from `solvedConstraint` with all three fields, then immediately passed to `pruneBindParentsConstraint`. Result used for final generalization constraint. |
| **Persists?** | No — local to the finalization computation. |
| **Classification** | **Read-only temporary** — constructs a local Constraint for the final generalization step. Never persisted to the pipeline's Solved. |
| **Elimination plan** | None needed. Already a local temporary. |

### 11. Direct `{ cBindParents = }` — Constraint.hs line 46

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Run/Generalize/Constraint.hs` |
| **Line** | 46 |
| **Context** | Inside `pruneBindParentsConstraint`. Filters stale bind-parent entries and returns a new Constraint with the pruned map. Pure function operating on a Constraint value. |
| **Persists?** | Depends on caller — when called from Pipeline.hs (via `pruneBindParentsSolved`), the result becomes the pipeline's Solved. When called from Finalize.hs, it's local. |
| **Classification** | **Local projection rebuild** — pure function that returns a new Constraint. The mutation semantics depend on the caller. |
| **Elimination plan** | Low priority. The function itself is pure; the Pipeline.hs caller is pipeline setup (pre-elaboration). |

### 12. Direct `{ cNodes = }` — Generalize.hs lines 402, 409

| Field | Value |
|-------|-------|
| **File** | `src/MLF/Elab/Generalize.hs` |
| **Line** | 402, 409 |
| **Context** | Creates `constraintAlias` and `originalConstraintAlias` by adding alias nodes to existing constraints. These are local values used for reification. |
| **Persists?** | No — local to the alias-insertion block. |
| **Classification** | **Read-only temporary** — constructs local Constraints with alias nodes for reification. Never persisted. |
| **Elimination plan** | None needed. Already local temporaries. |

## Summary by Classification

| Classification | Count | Call Sites |
|----------------|-------|------------|
| **Pipeline setup** | 2 | #1 (Pipeline.hs:104), #3 (Pipeline.hs:105) |
| **Local projection rebuild** | 2 | #2 (Generalize.hs:413), #11 (Constraint.hs:46) |
| **Graph mutation in elab** | 1 | #4 (Fallback.hs:487) |
| **Read-only temporary** | 7 | #5, #6, #7, #8, #9, #10, #12 |

## Key Finding

There is exactly **one** true graph mutation during elaboration: `patchNode` at `Fallback.hs:487`. This is the target for Task 14. All other call sites are either pre-elaboration pipeline setup, local projection rebuilds, or read-only temporaries that never feed back into the pipeline's Solved handle.
