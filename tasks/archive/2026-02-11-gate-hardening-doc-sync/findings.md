# Findings

## 2026-02-11
- `Phase 3 atomic wrapping equivalence gates` previously allowed an error fallback for a now-resolved path.
- Strengthened gate now requires checked success to `Int` for the `make` path.
- Strengthened gate now verifies `\y. let id = (\x. x) in id y` as `forall a. a -> a` shape explicitly.
- Targeted and full validation after hardening are green:
  - gates match: `7 examples, 0 failures`
  - `BUG-2026-02-06-002` match: `10 examples, 0 failures`
  - `BUG-2026-02-08-004` match: `1 example, 0 failures`
  - full gate: `633 examples, 0 failures`
