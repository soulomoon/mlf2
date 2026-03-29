# Round 154 — Implementation Notes

## Changes Made

### PipelineSpec.hs line 2336: Test description update

**Old:** `"hits elaboration blocker for non-local proxy wrapper despite open fallback at pipeline entrypoints"`

**New:** `"elaboration succeeds but produces TCArgumentMismatch for non-local proxy wrapper"`

**Rationale:** After rounds 152+153 fixed the `PhiTranslatabilityError` for non-local proxy patterns, the pipeline no longer hits an elaboration blocker. Instead, elaboration succeeds but the elaborated term fails type checking with `TCArgumentMismatch (TVar "a") (TForall "b" Nothing (TArrow TBottom (TVar "b")))`. The old description was inaccurate; the new one reflects the actual current failure mode.

No assertion logic was changed — only the `it` description string.

## ElaborationSpec PhiTranslatabilityError Survey

Surveyed all 13 `PhiTranslatabilityError` assertion sites in `test/ElaborationSpec.hs`. **None** match the non-local proxy wrapper pattern that items 2+3 fixed; all are legitimate untranslatable cases or test helpers.

| # | Line | Test Name | Error Pattern | Classification |
|---|------|-----------|---------------|----------------|
| 1 | ~2867 | "duplicate no-replay graft+weaken aligns source/spine in empty replay-domain lane" | OpGraft targets non-binder node | (b) Legitimate |
| 2 | ~2971 | "OpWeaken on an alias target fails fast under strict replay-map resolution" | OpWeaken on alias target | (b) Legitimate |
| 3 | ~3016 | "OpWeaken on a shared alias class fails fast without trace fallback search" | OpWeaken on shared alias class | (b) Legitimate |
| 4 | ~3056 | "OpWeaken on unrecoverable non-binder alias fails fast (no no-op fallback)" | OpWeaken on non-binder alias | (b) Legitimate |
| 5 | ~3107 | "OpWeaken does not repair no-replay triple-pattern targets via nearest-key fallback" | OpWeaken unresolved non-root binder target | (b) Legitimate |
| 6 | ~3156 | "OpWeaken on binder target missing from quantifier spine fails fast" | OpWeaken on binder missing from spine | (b) Legitimate |
| 7 | ~3220 | "OpGraft on binder target missing from quantifier spine still fails fast even when witness-domain matches exist" | OpGraft binder not in quantifier spine | (b) Legitimate |
| 8 | ~3345 | "OpMerge with rigid endpoint only on m fails as non-translatable" | OpMerge rigid endpoint on non-operated node | (b) Legitimate |
| 9 | ~3390 | "OpRaiseMerge with rigid endpoint only on m fails as non-translatable" | OpRaiseMerge rigid endpoint on non-operated node | (b) Legitimate |
| 10 | ~4127 | "rejects OpGraft on out-of-scheme target (no non-binder recovery)" | OpGraft targets non-binder node | (b) Legitimate |
| 11 | ~4177 | "rejects OpGraft on out-of-scheme target (no InstBot/InstApp fallback)" | OpGraft targets non-binder node | (b) Legitimate |
| 12 | ~4227 | "producer-trace OpGraft on non-binder still fails fast (no copy-map skip fallback)" | OpGraft targets non-binder node | (b) Legitimate |
| 13 | ~4610+4625 | Helper functions `expectStrictOpWeakenFailure` / `expectStrictRejection` in "Paper alignment baselines" | String match for PhiTranslatabilityError | (c) Helper — accepts PhiTranslatabilityError as one of several valid failure modes |

### Classification Key

- **(a) Same non-local proxy pattern** — would be affected by items 2+3 fixes. **None found.**
- **(b) Legitimate untranslatable case** — tests that correctly assert failure for structurally untranslatable Ω-operations (non-binder targets, missing spine entries, rigid endpoints, alias targets). These are valid negative tests. **12 found.**
- **(c) Helper** — utility functions that accept `PhiTranslatabilityError` among other failure modes. **1 found (2 helper functions).**

### Conclusion

The non-local proxy `PhiTranslatabilityError` pattern was specific to the pipeline entrypoint test (PipelineSpec:2336) and was fully resolved by rounds 152+153. No ElaborationSpec tests need reclassification or updating. The remaining blocker for the pipeline test is `TCArgumentMismatch`, a deeper elaboration correctness issue (likely missing fold/unfold coercions for μ-types) that is outside the scope of this round.

## Verification

- `cabal build all && cabal test` → **1176 examples, 0 failures**
- No regressions from baseline
