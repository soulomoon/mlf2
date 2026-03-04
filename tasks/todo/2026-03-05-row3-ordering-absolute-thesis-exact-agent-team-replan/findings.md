# Findings: Row3 Ordering Absolute Thesis-Exact Execution

## Baseline (before Wave 0)
- `runPresolutionLoop` currently calls `flushPendingWeakens` only once after all instantiation edges.
- Existing row3 guard text in `PipelineSpec` is too weak: it only checks for substring presence and does not enforce per-edge boundary scheduling semantics.
- `Driver` already has no global post-loop `flushPendingWeakens` call; boundary behavior is owned by `EdgeProcessing`.

## Thesis/Runtime Alignment Anchors
- Thesis `SolveConstraint` ordering requires edge traversal semantics with propagation and unification in dependency order (`papers/these-finale-english.txt` §12.1.3).
- Delayed weaken witness intent must remain valid (`§15.2.1`).
- Translatability obligations remain constructive and enforced (`Def. 15.2.10`).

## Key Risk Surface
- Historical bug `BUG-2026-03-05-001`: naive earlier flush sequencing caused locked-node failures (`OperationOnLockedNode`) and parity drift.
- Sensitive regressions to protect during integration:
  - `generalizes reused constructors via make const`
  - `BUG-002-V1`
  - `Frozen parity artifact baseline`

## Execution Notes
- Wave order must remain: 0 -> 1(B+C parallel) -> 2 -> 3 -> 4.
- Gate commands must run sequentially and record non-empty matcher evidence.

## Gate A RED Evidence (2026-03-05)
- Command:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 absolute thesis-exact guard"'`
- Result:
  - `4 examples, 4 failures` (expected RED baseline, non-empty).
- Contract failure markers:
  - Loop-final weaken flush fallback still present in `EdgeProcessing` sequence.
  - Owner-aware boundary scheduler markers absent:
    - `scheduleWeakensByOwnerBoundary`
    - `flushPendingWeakensAtOwnerBoundary`
    - `assertNoPendingWeakensOutsideOwnerBoundary`

## Wave 1 Integration Findings (2026-03-05)
- Team B introduced owner-aware weaken ownership APIs in:
  - `Base` (`PendingWeakenOwner`)
  - `StateAccess` (`pendingWeakenOwnerM`, `instEdgeOwnerM`)
  - `EdgeUnify` (`flushPendingWeakensAtOwnerBoundary` and owner inspection helpers)
- Team C rewired `EdgeProcessing` loop to boundary scheduling markers:
  - `scheduleWeakensByOwnerBoundary`
  - `assertNoPendingWeakensOutsideOwnerBoundary`
- Loop-final fallback shape (`flushPendingWeakens` followed by `drainPendingUnifyClosureIfNeeded`) is removed.
- Gate B verification after merge:
  - `row3 absolute thesis-exact guard`: `4 examples, 0 failures`
  - `Phase 4 thesis-exact unification closure`: `10 examples, 0 failures`
  - `Translatable presolution`: `8 examples, 0 failures`

## Wave 2/3 Regression Finding and Resolution
- During Wave 3 sequential verification, `Phase 4 thesis-exact unification closure` regressed (`10 examples, 3 failures`) with:
  - `InternalError "presolution boundary violation (after-inst-edge-closure): pending unify edges = [], pending weakens = [...]"`
- Root cause:
  - Owner-boundary flush keyed to planner owner could miss pending weaken buckets keyed by observed pending-node owners, leaving residual queue entries at boundary invariant checks.
- Resolution:
  - Edge-boundary scheduler now flushes all currently pending owner buckets at each owner-boundary transition.
  - Boundary postcondition rechecks that no pending owner buckets remain after boundary flush.
- Post-fix required gates:
  - `row3 absolute thesis-exact guard`: `4 examples, 0 failures`
  - `Phase 4 thesis-exact unification closure`: `10 examples, 0 failures`
  - `Translatable presolution`: `8 examples, 0 failures`
  - `generalizes reused constructors via make const`: `1 example, 0 failures`
  - `BUG-002-V1`: `1 example, 0 failures`
  - `Frozen parity artifact baseline`: `1 example, 0 failures`
  - `checked-authoritative`: `8 examples, 0 failures`
  - `Dual-path verification`: `4 examples, 0 failures`
  - `cabal build all && cabal test`: PASS (`942 examples, 0 failures`)
