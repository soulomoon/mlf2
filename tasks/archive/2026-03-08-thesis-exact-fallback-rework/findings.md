# Findings — 2026-03-08 thesis-exact fallback rework

- Residual live behavior from the audit included the let-level chooser in `MLF.Elab.Elaborate`, secondary `reifyInst` refinement via `targetArgs <|> expansionArgs`, and the recursive generalization fallback callback/scheme recursion.
- The approved design docs remained the right implementation target: the final pass removed those behaviors without reviving any compatibility ladder.
- The second-pass blocker root cause was not missing expansion fallback in general; it was incomplete witness/domain authority selection.
- For empty-Ω edges, the remaining authoritative instantiation candidates live in witness-owned data: direct target nodes, trace binder args, and copied nodes from `EdgeTrace.etCopyMap`.
- Searching those witness-domain copied nodes is sufficient to recover the bounded/coercion-heavy cases that are still thesis-authoritative, while leaving genuinely expansion-only paths as strict fail-fast.
- Exact-scheme annotation reuse needed a small `AAnnF` guard so already-authoritative polymorphic subjects are not re-closed into duplicate top-level `forall`s.
- The historical `BUG-2026-02-06-002` / `BUG-2026-02-08-004` compatibility-success anchors are now intentionally documented as strict fail-fast under the final thesis-exact policy when they rely only on expansion-derived recovery.
- Final verification passed after regenerating the frozen parity artifact to the new strict semantics.
