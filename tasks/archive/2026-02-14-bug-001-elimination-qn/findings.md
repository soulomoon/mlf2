# Findings: BUG-2026-02-14-001

## 2026-02-14
- Reproducer failure was stable with seed `1715612721`:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 1 — Constraint generation/Binding edges/elimination rewrite removes eliminated binders from Q(n)/" --seed 1715612721'`
  - Failure message: `predicate failed on: fromList []`.
- Temporary instrumentation showed the failing path had:
  - `eliminated=[]`
  - `qn=[18]`
  - witness ops `[OpGraft ...]` only (no `OpMerge`/`OpRaiseMerge`)
  - alias-bound/store lookup for relevant nodes as `Nothing`.
- Pattern check against a known passing test confirmed frontend normalization intentionally inlines alias bounds before constraint generation:
  - `alias bound ∀(b ⩾ a) inlined by normalization before constraint gen`.
- Root-cause conclusion:
  - BUG-001 was a stale test assumption, not a production elimination bug.
  - This path should not assert non-empty eliminated binders after normalization.
- Minimal fix:
  - Updated `ConstraintGenSpec` test name/expectation to assert `eliminated == IntSet.empty` for this normalized path.
- Verification outcomes:
  - Targeted renamed case: `1 example, 0 failures`.
  - `--match "Phase 1 — Constraint generation"`: `48 examples, 0 failures`.
  - Full suite snapshot after fix: `652 examples, 41 failures` (down from `42`).
