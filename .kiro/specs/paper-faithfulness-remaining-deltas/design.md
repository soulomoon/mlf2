# Design Document

## Overview
This spec aligns the current implementation with the remaining thesis-defined constructs in xMLF (Ch. 14) and the presolution‑to‑xMLF translation (Ch. 15). The main remaining deltas are: insufficient enforcement of “translatable presolution” invariants for Φ translation (operations should not be silently skipped), and witness normalization/validation alignment with the assumptions of Fig. 15.3.4. Quantifier reordering ϕR (Def. 15.3.4) is integrated into Φ translation, including the `Typ` vs `Typexp` mismatch case with no Raise ops. Constructor types `Cσ` are implemented (`TCon` in `src/MLF/Types/Elab.hs`).

Evidence for the paper requirements is in `papers/these-finale-english.txt` (Fig. 14.2.1, 15.3.4, 15.3.4–15.3.5), and the current code locations are primarily `src/MLF/Types/Elab.hs`, `src/MLF/Elab/Phi/`, and `src/MLF/Constraint/Presolution/`.

## Architecture
Pipeline (paper-aligned):
1) Frontend → constraints
2) Normalize / acyclicity / presolution
3) Witness extraction (Ω, Φ) + expansion recipes
4) Elaborate to xMLF, including ϕR and Φ(e)

Alignment changes target steps (3) and (4), plus the xMLF type AST.

## Components and Interfaces
- **`MLF.Types.Elab`**: already includes `TCon` for constructor types `Cσ`; ensure all traversals remain total as new constructors are added.
- **`MLF.Elab.Inst` / `MLF.Elab.TypeCheck` / `MLF.Elab.Reduce`**: ensure instantiation, typing, and reduction remain total and treat `TCon` structurally where relevant.
- **`MLF.Elab.Sigma`**: keep `sigmaReorder` as the implementation of ϕR, and expose it for use by elaboration.
- **`MLF.Elab.Phi.*` / `MLF.Elab.Elaborate`**: ϕR application is integrated when `Typ` vs `Typexp` differ; remaining work is to add explicit validation of translatable-presolution invariants (e.g., operations must target flexibly-bound interior nodes; computation contexts must exist).
- **Presolution witness pipeline**: strengthen witness normalization so Ω matches the assumptions of Fig. 15.3.4 (or document intentional deviations).

## Data Models
- **xMLF Types (Fig. 14.2.1)**: `Ty` includes constructor application via `TCon`; this must be reflected in `TyIF`, `Pretty`, type substitutions, and equality.
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
  - `sigmaReorder` used in elaboration when Typ/Typexp differ: covered by `test/ElaborationSpec.hs` (“applies Σ reordering even without Raise when Typ/Typexp differ”).
  - Witness validation: supply a witness op that targets a non‑interior node and assert a dedicated translatability error.
- **Integration tests**
  - End‑to‑end elaboration on a term that triggers reordering and Raise ops; check `applyInstantiation` matches expected target type.
- **Regression tests**
  - Ensure existing Φ soundness tests continue to pass after reordering is integrated.
