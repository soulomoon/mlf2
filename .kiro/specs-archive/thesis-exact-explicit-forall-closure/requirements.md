# Requirements

## Goal
Make scheme closure thesis-exact by ensuring any type variables referenced by a scheme body are either named nodes bound on the scheme's gen node or structurally inlined by Sχ′p, per `papers/these-finale-english.txt` §15.3.1–§15.3.2. Remove the current explicit-forall allowance by fixing the binding structure instead.

## User Stories
- As a developer, I want generalization to fail only when a scheme body mentions named nodes outside its gen node, so that Sχ′p aligns with the thesis definition of Typ(a′).
- As a maintainer, I want explicit foralls in annotations to be scoped so their binders are part of the scheme’s Γ (or otherwise inlined), so no ad hoc free-name exemptions are needed.

## Acceptance Criteria (EARS)
1.1 When a scheme root is generalized, the implementation shall only accept free names that correspond to named nodes bound under the scheme’s gen node.
1.2 When a scheme root is formed from an annotated scheme that introduces explicit foralls, the implementation shall bind those forall variables under the scheme’s gen node (or rebind them so they become named nodes under that gen), so Sχ′p does not introduce free names outside Γ.
1.3 The implementation shall remove the explicit-forall allowance in generalization (the additional “type-bound outside” free-name exemption).
1.4 The bounded-aliasing annotated-let test shall continue to pass without any free-name exemptions.
1.5 The full test suite shall pass.

## Non-goals
- Do not change the semantics of instantiation witnesses or presolution witness normalization.
- Do not weaken the SchemeFreeVars/GenSchemeFreeVars checks.

## References
- `papers/these-finale-english.txt` §15.3.1–§15.3.2 (named nodes, Sχ′p, Typ(a′)).
