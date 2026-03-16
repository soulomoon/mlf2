# Round 024 Implementation Notes

- Added an implementation-facing `URI-R2-C1` replay reproducer in `test/ElaborationSpec.hs` via `uriR2C1ReplayFixture`, keeping the lane fixed to `uri-r2-c1-only-v1`.
- The new test proves the accepted no-fallback shape (`t5 -> t5`) and witness replay instantiation reach `MLF.Elab.Inst.applyInstantiation`, where the `InstBot` branch still fails with `InstBot expects ⊥, got: t9 -> t9`.
- Recorded the bounded `R1` contract in `docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md`; no production repair was implemented.
