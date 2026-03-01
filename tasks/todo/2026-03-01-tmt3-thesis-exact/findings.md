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
