# Findings — TODO Next-Step Audit Refresh

## 2026-03-08 audit outcome

- The highest-priority live TODO item is `Task 46` in `TODO.md`; newer tasks `58`, `52`, and `36` are already closed or superseded in the current TODO.
- `Task 46` is no longer a broad fallback-removal campaign. `TODO.md` already narrows it to one residual cleanup: `src/MLF/Elab/Elaborate.hs:110` still swallows base binding-path failures via `Left _ -> typeRef root`.
- The remaining fallback is still live in code:
  - `scopeRootFromBase` maps solved roots back to base roots, then falls back to `typeRef root` when `bindingPathToRootLocal` fails.
  - This path feeds `scopeRootForNode` / `generalizeAtNode`, so it still affects annotation generalization behavior.
- The stricter generalization path already fail-fasts on the analogous condition: `test/ElaborationSpec.hs` contains `resolveContext propagates ga base binding-path failures instead of falling back`.
- The planned witness-authoritative guard has not been implemented yet:
  - `rg` finds the string only in `TODO.md`, task files, and the plan doc.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input witness-authoritative guard"'` runs `0 examples, 0 failures`.
- The older strictness guard still exists and passes:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input absolute thesis-exact guard"'` runs `1 example, 0 failures`.

## Recommendation

- If we want the next real implementation task, do a small `Task 46` strictness follow-up only:
  - remove the `Left _ -> typeRef root` swallow in `src/MLF/Elab/Elaborate.hs`;
  - add the missing `elab-input witness-authoritative guard` (or equivalent focused regression);
  - verify with the existing row-1 slices plus the full Cabal gate.
- If we do not want stricter fail-fast behavior for malformed base binding trees, then `Task 46` is optional and can be explicitly closed as a conscious non-goal.
