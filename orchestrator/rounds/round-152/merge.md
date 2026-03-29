# Squash-Commit Title

Fix reifyInst TyMu without binder child for non-local proxy

## Summary

Replace the PhiTranslatabilityError in the TyMu 0-binder arm of reifyInst (src/MLF/Reify/Type.hs) with a synthesized-binder fallback that uses the TyMu node itself as the binder identity. Add targeted test. Update SameLaneRetainedChildRepresentativeGapSpec error expectations. 1176 examples, 0 failures.

## Follow-up

Items 3-5 remain pending. Item-3 (OpRaise non-spine) depends on item-2 and may be partially resolved.
