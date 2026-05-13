# ADR: Retire Legacy Compatibility Surfaces

Date: 2026-05-14
Status: Accepted

## Context

The repository prioritizes thesis-faithful implementation over backwards
compatibility. Several remaining surfaces still preserve older internal or
syntax shapes:

- solved/view/raw adapters around `PresolutionView`, `Solved`, `Finalize`, and
  `ChiQuery`;
- parser acceptance for non-canonical legacy eMLF and xMLF syntax;
- transition documentation that describes legacy spellings as accepted parser
  behavior.

Keeping those surfaces turns compatibility into an implicit architectural
requirement. That conflicts with the project rule that compatibility layers,
fallbacks, and shims need a paper-backed reason.

## Decision

Adopt **Legacy Surface Retirement** as the framing for cleanup that reaches
outdated compatibility surfaces. When a cleanup touches a legacy adapter,
parser alias, or non-canonical syntax path, the default action is to delete the
surface rather than move it to a new compatibility home.

**Snapshot Finalization** is one sub-slice of this broader decision. It should
centralize canonical read-model and solved-handle construction in
`MLF.Constraint.Finalize`, while `MLF.Constraint.Presolution.View` remains the
read-model surface. It is not a reason to preserve solved/view/raw adapters.

Parser support for legacy syntax and ASCII aliases is also in scope for
retirement. The accepted language should follow the paper-aligned canonical
syntax exactly unless a specific thesis-faithfulness reason keeps a spelling.
This applies uniformly to frontend eMLF and explicit xMLF parsers; keeping
aliases in one parser family while retiring them in the other is not the target
language contract.

## Consequences

- Cleanup slices may broaden when deleting legacy syntax support or old
  solved/view adapters is necessary to remove the compatibility surface.
- Tests should assert canonical behavior directly and should not rely on legacy
  grammar shapes or ASCII aliases unless the test documents a paper-backed
  exception.
- Parser cleanup should retire aliases and legacy grammar shapes consistently in
  both frontend eMLF and explicit xMLF parser tests.
- Parser cleanup should add focused rejection tests for retired aliases and
  legacy grammar shapes, alongside positive tests for the canonical Unicode
  syntax.
- Rejection tests should assert parse failure, not exact parser diagnostic text.
- `docs/syntax.md` should describe the parser after the implementation slice
  removes legacy forms; it should not keep a transition compatibility section as
  a standing contract.
- Any retained legacy spelling, alias, adapter, or raw-view bridge needs an
  explicit justification tied to the thesis or to a documented implementation
  invariant.
