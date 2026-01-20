# Design Document

## Overview
We remove the scheme free-var closure fallback in `generalizeAt`. Instead, we enforce a binding-structure invariant so that all named nodes reachable from a scheme root are bound under the scope gen node (Q(g)). If not, we fail fast with a structured error. This aligns with the thesis translation: Γa lists named nodes bound on g, and Sχ′p is used without post-hoc binder insertion.

## Architecture
- Generalization (`MLF.Elab.Generalize`): remove binder insertion for missing free vars; compute free vars and error if any remain.
- Binding Tree Invariant (`MLF.Binding.Tree`): add a validator that checks scheme roots against their gen nodes to ensure named-node closure.
- Error Types (`MLF.Constraint.Types` or `MLF.Elab.Types`): add structured errors for free-var closure violations.
- Wiring: invoke the binding invariant where scheme emission occurs (e.g. before `generalizeAt` in the pipeline), and keep a local check in `generalizeAt` for safety.

## Components and Interfaces
- `MLF.Elab.Generalize`
  - Remove the "close missing free vars" step.
  - Add a check:
    - Input: `SolveResult`, `scopeRoot`, `typeRoot`.
    - Output: `Either ElabError` that fails if `freeNames(type)` ⊄ binders.
  - Error: `SchemeFreeVars { schemeRoot :: NodeId, freeNames :: [String] }`.

- `MLF.Binding.Tree`
  - Add `checkSchemeClosure` (or similar):
    - For each `GenNode` and each `gnSchemes` root, compute named nodes reachable from the root (flex-bound nodes); ensure each is bound under the gen node.
    - If not, error `GenSchemeFreeVars { schemeRoot :: NodeId, schemeGen :: GenNodeId, freeNodes :: [NodeId] }`.

- Wiring
  - Call `checkSchemeClosure` in the same place as `checkNoGenFallback` (e.g. `MLF.Elab.Phi` or `MLF.Elab.Run` before generalization).
  - Keep `SchemeFreeVars` check in `generalizeAt` as a guardrail.

## Data Models
- Add `GenSchemeFreeVars` to `BindingError` (or new error type if preferred).
- Add `SchemeFreeVars` to `ElabError` with scheme root and free variable list.
- Use existing `freeNamesOf` and named-node logic (no new core data types).

## Error Handling
- Binding tree invariant failures surface as `BindingTreeError GenSchemeFreeVars`.
- Generalization failures surface as `ElabError SchemeFreeVars`.
- Errors must include enough context to locate the offending scheme root and nodes.

## Testing Strategy
- Add a regression test that previously depended on free-var closure and now fails with `SchemeFreeVars` or `GenSchemeFreeVars`.
- Ensure existing Q(g)/Q(n) and invariant tests remain green.
- Run `cabal test --test-show-details=direct`.
