# Findings

- `pendingWeakenOwners` is implemented in `MLF.Constraint.Presolution.EdgeUnify.Omega` and merely re-exported by the `MLF.Constraint.Presolution.EdgeUnify` façade.
- Live production call sites are bounded to `MLF.Constraint.Presolution.Driver` and `MLF.Constraint.Presolution.EdgeProcessing`.
- The nearby owner-query aliases and legacy flush-all helper are already retired, so this is the remaining small façade-boundary cleanup in the area.
- The cleanup is thesis-safe because the helper is read-only introspection; owner resolution, queue stamping, and boundary flushing stay in their existing owner modules.
- A source-guard TDD test now fails for the intended reason: `MLF.Constraint.Presolution.EdgeUnify` still mentions/re-exports `pendingWeakenOwners`, while the desired boundary is direct `Omega` ownership for the two remaining consumers.
- The minimal code patch was sufficient: only `EdgeUnify`, `Driver`, `EdgeProcessing`, and the new source guard changed; no thesis-facing presolution logic moved.
