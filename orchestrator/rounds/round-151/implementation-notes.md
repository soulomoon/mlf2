# Round 151 Implementation Notes

## Item 1: Reclassify nested-forall μ absorption

### Changes made

1. **test/Research/P5ClearBoundarySpec.hs** — Renamed two test descriptions:
   - `"fails closed once the same wrapper crosses a nested forall boundary"` → `"correctly absorbs μ when polymorphic mediation crosses a nested forall boundary"` — reflects that the non-recursive outcome is correct behavior, not a failure mode.
   - `"hits the same authoritative instantiation-translation blocker on both current pipeline entrypoints once the wrapper crosses a nested forall boundary"` → `"reports PhiTranslatabilityError at pipeline entrypoints as a downstream consequence of correct non-recursive nested-forall outcome"` — reframes the PhiTranslatabilityError as a downstream consequence of correct constraint solving, not a primary blocker.

2. **implementation_notes.md** — Reclassified the nested-forall μ absorption from a "known remaining limitation" to "known correct behavior under polymorphic mediation", with an explicit note that this is the expected outcome when a polymorphic mediator generalizes away recursive structure. The non-local proxy PhiTranslatabilityError remains as the sole "known remaining limitation".

### Verification

- `cabal build all` — clean build, no warnings.
- `cabal test` — all 1175 examples pass, 0 failures.
- No test assertions were changed; only test description strings and documentation text were updated.
