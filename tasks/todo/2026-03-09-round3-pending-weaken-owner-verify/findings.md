# Findings

## Repository / docs
- Pending.

## Code
- Pending.

## Thesis
- Pending.

## Repository / docs
- `TODO.md` shows a live campaign of removing residual helper/fallback surfaces; recent completed tasks on 2026-03-08 already retired facade/helper aliases when they were non-thesis-core.
- `tasks/readme` confirms the active task-folder layout used for this verifier pass.
- No deeper `AGENTS.md` files were found under the repository root, so root guidance governs the inspected files.
- Need targeted follow-up in `implementation_notes.md`, `CHANGELOG.md`, and `Bugs.md` for any row3 / pending-weaken-owner specific references because the first broad read was truncated.

## Code
- `src/MLF/Constraint/Presolution/EdgeUnify.hs` still exports `pendingWeakenOwners`, `pendingWeakenOwnerForNode`, and `pendingWeakenOwnerForEdge` alongside `flushPendingWeakensAtOwnerBoundary`, even though the first two owner-specific functions are direct aliases to `StateAccess.pendingWeakenOwnerM` / `StateAccess.instEdgeOwnerM`.
- `pendingWeakenOwners` is a small aggregation helper over `psPendingWeakens` + `psPendingWeakenOwners`; its only live production consumers are `EdgeProcessing` and `Driver` diagnostics/boundary checks.
- `src/MLF/Constraint/Presolution/StateAccess.hs` already owns the underlying owner-resolution semantics (`pendingWeakenOwnerM`, `instEdgeOwnerM`, `pendingWeakenOwnerUnder`), so the aliases in `EdgeUnify` do not add behavior.
- `src/MLF/Constraint/Presolution/EdgeProcessing.hs` imports `pendingWeakenOwners` from `EdgeUnify` only for residual-owner diagnostics after boundary flushing; `src/MLF/Constraint/Presolution/Driver.hs` imports the same helper only for finalization-boundary diagnostics.
- Current source guards remove stale `Driver`/`EdgeProcessing` re-exports and the legacy flush-all helper, but they do not guard the owner-query helpers staying in `EdgeUnify`.
- `implementation_notes.md` currently describes the owner lookup helpers as part of the authoritative pending-weaken API surface, so this proposal would require a doc update if accepted.

## Thesis
- `papers/these-finale-english.txt` constrains presolution behavior through stability/translation properties, not internal module placement: Lemma 13.1.6 covers weakening/merging stability for presolutions, and Chapter 15 translates normalized instance operations by their effect/order.
- `papers/these-finale-english.txt` around Figure 15.3.4 / translation text requires the operation ordering and effect (`Raise`, `Merge`, `RaiseMerge`, `Weaken`) to be preserved for translation correctness, but does not prescribe where helper queries live inside an implementation.
- `papers/xmlf.txt` gives the same semantic shape more readably: instantiation witnesses decompose into interior `Graft`/`Weaken`, interior `Merge`, `Raise`, and `RaiseMerge`, and weakening must appear after other operations below the node. This constrains queue/flush behavior, not module ownership of read-only owner queries.

## Validation
- Attempted targeted guard tests, but the current workspace/test suite fails earlier on an unrelated `Solved.fromPreRewriteState` export mismatch in `test/Parity/FrozenArtifacts.hs` and `test/SpecUtil.hs`.
- The repository has unrelated uncommitted changes in `src/MLF/Constraint/Solved.hs` and `test/SolvedFacadeTestUtil.hs`; this verifier pass did not modify them.
