# Findings

- Pending.
- Live source search shows `instEdgeOwnerM` is only mentioned in `MLF.Constraint.Presolution.StateAccess` plus rolling backlog notes; there are no production or test call sites.
- The new `PipelineSpec` source guard fails for the intended reason: `StateAccess` still exports and defines `instEdgeOwnerM`.
- The minimal patch was sufficient: only `StateAccess` and one `PipelineSpec` guard changed; no consumers needed migration because the helper was already dead.
