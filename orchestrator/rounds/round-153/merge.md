# Round 153 Merge — Item 3: Fix OpRaise non-spine context

## Squash-Commit Title

Fix OpRaise non-spine missing computation context for non-local bind-parent

## Summary

Add `Just TyMu{} -> True` to the `mbRootInst` bind-parent guard in
`phiWithSchemeOmega` (src/MLF/Elab/Phi/Omega/Interpret.hs line 1099). This
treats TyMu as a binder-like node alongside TyForall, allowing OpRaise
non-spine computation to resolve for non-local proxy wrappers whose
bind-parent is a TyMu node.

Single file changed, 1 insertion, 0 deletions. 1176 examples, 0 failures.

## Follow-Up Notes

- PipelineSpec:2303 blocker test still expects failure (case a: additional
  downstream blocker beyond mbRootInst). Test upgrade deferred to item-4.
- No new dedicated test added; existing pipeline and OpRaise tests exercise
  the path. Item-4 will provide definitive coverage.
