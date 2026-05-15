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
`MLF.Constraint.Presolution.View` should not expose broad raw-view adapters;
when a remaining migration or research check needs a raw comparison, keep that
bridge local and constraint-level instead of converting an entire read model.

Reification should treat `PresolutionView` as the canonical read model rather
than reconstructing `Solved` from a view. The reification core should consume
`PresolutionView` directly, or a narrower read-model interface derived from it,
and read the original constraint, canonical constraint, canonical mapping, and
variable bounds from that surface. Any remaining `Solved`-typed helper should be
an edge wrapper that first builds a `PresolutionView` from `Solved`; the core
path should not convert `PresolutionView` back into `Solved` merely to reify.

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
- Reification cleanup should delete the view-to-solved round trip from the core
  reify path. Rebuilding `Solved` from `PresolutionView` to recover canonical
  graph semantics is a compatibility workaround, not the target architecture.
- Snapshot cleanup should delete broad raw-view adapters from
  `MLF.Constraint.Presolution.View`; tests that still compare against raw
  solved handles should consume typed views directly and make any raw constraint
  projection explicit at the comparison point.
- Constraint phase cleanup should prefer directional phase transition helpers.
  Generic graph-level phase casts and legacy raw-constraint bridges should be
  deleted; if a raw internal backend still exists, phase erasure should stay
  private to that owner and be named for that owner's invariant.
- Solve cleanup should keep result-shaped `SolveResult` construction out of
  the `MLF.Constraint.Solve` facade. Snapshot-bearing solve output should carry
  the minimum data needed for staged solved construction, while low-level
  white-box tests use a named test-support seam.
- Frontend syntax cleanup should delete stale raw aliases and normalized
  pattern synonyms that only preserve older API spellings. Staged syntax
  synonyms may remain when they are the canonical names used by the current
  pipeline.
- Elaboration cleanup should delete legacy conversion modules when their only
  live behavior can be owned by the concrete elaboration step that uses it.
  Dead solved-typed conversion exports are not a protected public surface.
- Public facades and internal pipeline modules should not keep
  checker-authoritative aliases once the underlying implementation has one
  canonical entrypoint. Parity tests should call that canonical entrypoint
  directly. Semantically distinct entrypoints may remain when they expose a real
  capability, such as the detailed unchecked pipeline path used by `.mlfp`
  finalization.
- Public import shims should be retired once their owned capabilities are
  available from canonical public modules. For `.mlfp`, parsing/pretty-printing
  belongs to `MLF.API`, while checking/runtime belongs to `MLF.Pipeline`; a
  separate `MLF.Program` re-export is compatibility surface, not architecture.
- Any retained legacy spelling, alias, adapter, or raw-view bridge needs an
  explicit justification tied to the thesis or to a documented implementation
  invariant.
