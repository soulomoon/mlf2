# Findings

## Wave 3 Team E
- Verification run initialized on 2026-03-05.
- No code edits planned; execution-only unless a gate failure forces escalation.
- Gate 1 (`row3 absolute thesis-exact guard`) passed with `4 examples, 0 failures`.
- Gate 2 (`Phase 4 thesis-exact unification closure`) failed with `10 examples, 3 failures`.
- Key failure signature: `InternalError "presolution boundary violation (after-inst-edge-closure): pending unify edges = [], pending weakens = [13]"` (also `[11]` in one example).
