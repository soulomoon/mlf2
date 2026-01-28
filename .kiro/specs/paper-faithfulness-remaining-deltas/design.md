# Design Document

## Overview
This spec aligns the current implementation with the remaining thesis-defined constructs in xMLF (Ch. 14) and the presolution‑to‑xMLF translation (Ch. 15). The main deltas are: missing constructor types `Cσ`, incomplete integration of quantifier reordering ϕR, and insufficient enforcement of “translatable presolution” invariants for Φ translation. Evidence for the paper requirements is in `papers/these-finale-english.txt` (Fig. 14.2.1, 15.3.4, 15.3.4–15.3.5), and the current code locations are in `src/MLF/Types/Elab.hs`, `src/MLF/Elab/Phi.hs`, and `src/MLF/Constraint/Types.hs`.

## Architecture
Pipeline (paper-aligned):
1) Frontend → constraints
2) Normalize / acyclicity / presolution
3) Witness extraction (Ω, Φ) + expansion recipes
4) Elaborate to xMLF, including ϕR and Φ(e)

Alignment changes target steps (3) and (4), plus the xMLF type AST.

## Components and Interfaces
- **`MLF.Types.Elab`**: add `TCon` (or similar) for constructor types `Cσ` and update folds/pretty/printers.
- **`MLF.Elab.Inst` / `MLF.Elab.TypeCheck`**: ensure instantiation and typing support `TCon` bounds where relevant.
- **`MLF.Elab.Sigma`**: keep `sigmaReorder` as the implementation of ϕR, and expose it for use by elaboration.
- **`MLF.Elab.Phi` / `MLF.Elab.Elaborate`**: integrate ϕR application when `Typ` vs `Typexp` differ; add explicit validation of translatable-presolution invariants (e.g., operations must target flexibly-bound interior nodes; computation contexts must exist).
- **`MLF.Constraint.Types` / presolution pipeline**: strengthen witness normalization so Ω includes Raise/Weaken/Merge/Graft/RaiseMerge as per Fig. 15.3.4, or document any intentional restriction.

## Data Models
- **xMLF Types (Fig. 14.2.1)**: extend `Ty` with a constructor type (e.g., `TCon String ElabType` or `TCon String [ElabType]`) to represent `Cσ`. This must be reflected in `TyIF`, `Pretty`, type substitutions, and equality.
- **Quantifier reordering ϕR (Def. 15.3.4)**: model as `Instantiation` produced by `sigmaReorder`, composed with Φ(e) for each subterm. This should be explicit in the elaboration path.
- **Translatable-presolution invariants**: introduce a validation report type (e.g., `TranslatabilityError`) that lists operations targeting rigid/non‑interior nodes or missing computation contexts, so errors are explicit rather than silently skipped.

## Error Handling
- Add explicit error variants for:
  - Missing computation context for a Φ operation (currently a generic `InstantiationError`).
  - Instance ops on non‑interior or rigid nodes when translatability is required.
  - Missing Raise normalization support (if Ω is non‑normalized).
- Keep current behavior for rigid nodes translating to identity, but enforce that this occurs only when permitted by the paper’s assumptions.

## Testing Strategy
- **Unit tests**
  - `sigmaReorder` used in elaboration when Typ/Typexp differ: construct a case where quantifier order differs and assert that elaboration composes ϕR before Φ.
  - Witness validation: supply a witness op that targets a non‑interior node and assert a dedicated translatability error.
- **Integration tests**
  - End‑to‑end elaboration on a term that triggers reordering and Raise ops; check `applyInstantiation` matches expected target type.
- **Regression tests**
  - Ensure existing Φ soundness tests continue to pass after reordering is integrated.

