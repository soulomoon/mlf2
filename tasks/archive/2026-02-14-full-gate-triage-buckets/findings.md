# Findings: Full-Gate Failure Triage Buckets

## 2026-02-14
- Baseline full-suite status on current dirty branch: `652 examples, 42 failures`.
- Durable audit artifact copied: `/Volumes/src/mlf4/tasks/todo/2026-02-14-full-gate-triage-buckets/mlf4-fullgate-triage-2026-02-14.log`.
- Final triage split uses 4 minimal buckets:
  - `BUG-2026-02-14-001`: Phase-1 elimination rewrite/Q(n) bookkeeping (`#1`)
  - `BUG-2026-02-14-002`: OpRaise witness emission + replay divergence (`#2-#5`)
  - `BUG-2026-02-14-003`: Binder provenance/copy-map + PhiReorder identity drift (`#6, #7, #10, #11, #13, #22, #31, #33, #35, #39, #40`)
  - `BUG-2026-02-14-004`: Checked-authoritative polymorphism/instantiation collapse (remaining 26 failures)
- Mapping invariant: `1 + 4 + 11 + 26 = 42`.
- Boundary rationale:
  - Keep structural presolution bugs (`#1`, `#2-#5`) isolated from elaboration/runtime regressions.
  - Split elaboration regressions into provenance failures vs. generalized polymorphism collapse to avoid a single non-actionable mega-bucket.
