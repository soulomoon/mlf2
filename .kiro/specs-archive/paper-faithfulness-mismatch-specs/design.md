# Design Document

## Overview
This spec fans out the remaining paper-faithfulness mismatches into separate
Kiro specs. Each mismatch gets its own spec directory with requirements,
design, and tasks. The mismatch list below is the authoritative inventory for
this fan-out.

## Architecture
Specs live under `.kiro/specs/<name>` and follow the standard three-document
format. Each spec targets exactly one mismatch and links to other specs if
cross-cutting work is required.

Mismatch inventory and target spec names:
- `witness-normalization-yakobowski`
  - Gap: Full witness normalization per Yakobowski'08 (beyond current
    lightweight normalization).
  - Primary modules: `MLF.Constraint.Presolution.Witness`,
    `MLF.Constraint.Presolution.Core`, `MLF.Elab.Pipeline`.
- `instantiation-o-interleaving`
  - Gap: Interleave quantifier-introduction `O` with Omega ops instead of
    suffix-only `ewForallIntros`.
  - Primary modules: `MLF.Constraint.Presolution.Core`,
    `MLF.Constraint.Types`, `MLF.Elab.Pipeline`.
- `application-dual-instantiation`
  - Gap: Application elaboration instantiates both sides per Figure 7.
  - Primary modules: `MLF.Frontend.ConstraintGen.Emit`,
    `MLF.Elab.Elaborate`.
- `generalization-fallback-retirement`
  - Gap: Replace top-level generalization fallback when root has no direct
    binders (align to paper binder enumeration).
  - Primary modules: `MLF.Elab.Generalize`, `MLF.Binding.Tree`.
- `rigid-bound-inlining`
  - Gap: Inline rigidly bound variables (R(n)=T(n)) and keep non-Forall
    generalization aligned with rigid binding edges.
  - Primary modules: `MLF.Elab.Generalize`, `MLF.Elab.Reify`.
- `omega-elimination-graph-rewrite`
  - Gap: Elimination is tracked via `cEliminatedVars` instead of rewriting the
    graph (ω operations should transform χe into χ with eliminated binders
    removed from Q(n) and inlined by bounds).
  - Primary modules: `MLF.Constraint.Presolution.Driver`,
    `MLF.Constraint.Types`, `MLF.Elab.Reify`.
- `xmlf-phase7-semantics`
  - Gap: Implement xMLF typechecking (Figure 4) and reduction semantics
    (Figure 5).
  - Primary modules: new `MLF.Elab.TypeCheck`, `MLF.Elab.Reduce` (or similar),
    plus wiring in `MLF.Elab.Pipeline`.

Rooted binding-tree behavior (single gen root, no-LCA errors) is already
covered by the existing `binding-lca-totality` spec and is considered
paper-faithful, so it is not listed as a remaining mismatch.

## Components and Interfaces
- Spec directories under `.kiro/specs/<name>` with:
  - `requirements.md` (EARS acceptance criteria).
  - `design.md` (architecture, interfaces, testing plan).
  - `tasks.md` (small, verifiable steps with requirement references).
- Each mismatch spec should include references to `papers/xmlf.txt` sections
  it targets.

## Data Models
No runtime data model changes are defined in this meta-spec. Each mismatch
spec will define its own data model updates, if needed.

## Error Handling
- If a mismatch spec is missing or bundled with another mismatch, this
  meta-spec is considered incomplete.
- If a mismatch spec omits test strategy or EARS criteria, it fails review.

## Testing Strategy
- This meta-spec is validated by checking that each mismatch has a spec
  directory with the required files and headings.
- Each mismatch spec must define its own testing strategy and include at
  least one property test or regression test when algorithmic changes are
  involved.
