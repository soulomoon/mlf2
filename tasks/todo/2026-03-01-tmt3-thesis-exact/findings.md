# Findings — 2026-03-01 Pod A Wave 1

- `Solved.canonical` is referenced in owned Phi files: `Translate.hs`, `Omega.hs`, `IdentityBridge.hs` (via stored canonical function).
- Existing tests already assert strict fail-fast for replay-map domain mismatch and codomain-outside-domain.
- `IdentityBridge` currently supports canonical alias matching in `lookupBinderIndex` (exact + alias match classes).
- Mission implies removing runtime canonical/class-member fallback policy from Phi path while retaining trace-domain adaptation.
- Added RED expectations:
  - `IdentityBridge.isBinderNode` now expected to reject canonical-alias-only binder matches.
  - `IdentityBridge.lookupBinderIndex` now expected to reject alias-only canonical identity matches.
  - New elaboration regression expects replay-map codomain validation to reject targets that only canonicalize into replay binder domain.
- Strict replay-map codomain validation now rejects targets that only canonicalize into replay binder domain.
- IdentityBridge now keeps source-key operations in witness raw key-space and drops canonical-alias lookup fallback.
- Follow-up acceptance fix: removed remaining `Solved.canonical` call-site tokens under `src/MLF/Elab/Phi` by switching to unqualified `canonical` imports in `Context`, `Translate`, and `Omega`.
- P0 fix: `OpGraft` now has strict-first resolution with constrained fallback:
  - first raw replay/spine match,
  - then replay-map-in-spine recovery,
  - then (only when replay contract is empty) binder-name recovery against current spine IDs.
- Merge/RaiseMerge no longer use canonical-equality shortcut decisions (`n==m` and root checks are replay-space/raw).
- Added focused A6 positive locks in `test/Phi/AlignmentSpec.hs` (C4) to keep bounded-alias/coercion parity covered by Pod A focused run.
