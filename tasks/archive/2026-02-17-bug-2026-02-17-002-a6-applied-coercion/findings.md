# Findings: BUG-2026-02-17-002 A6 applied-coercion let mismatch

## Key discoveries
- Pending.

## 2026-02-17 findings
- The applied bounded/coercion-heavy variant remains a deterministic mismatch bucket on unchecked elaboration (`TCLetTypeMismatch`), while existing A6 parity regressions stay green.
- A non-flaky sentinel was added to preserve suite greenness while explicitly tracking this expected-pass gap:
  - `/Volumes/src/mlf4/test/PipelineSpec.hs`
  - test name: `BUG-2026-02-17-002 sentinel: applied bounded-coercion path is expected-pass but currently reports let mismatch`.
- Canonical bug tracker now contains a new open entry with reproducer and ownership pointers:
  - `/Volumes/src/mlf4/Bugs.md` (`BUG-2026-02-17-002`).
