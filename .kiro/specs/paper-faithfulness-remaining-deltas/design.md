# Design Document

## Overview
This spec records the closure of the thesis-alignment deltas for xMLF core syntax (Ch. 14) and presolution-to-xMLF translation (Ch. 15). Constructor types `Cσ`, quantifier reordering ϕR (including Typ vs Typexp mismatch with no Raise), strict translatable-presolution Φ behavior, and Fig. 15.3.4 witness normalization coverage are implemented and regression-covered.

Evidence for the paper requirements is in `papers/these-finale-english.txt` (Fig. 14.2.1, 15.3.4, 15.3.4–15.3.5), and the current code locations are primarily `src/MLF/Types/Elab.hs`, `src/MLF/Elab/Phi/`, and `src/MLF/Constraint/Presolution/`.

## Architecture
Pipeline (paper-aligned):
1) Frontend → constraints
2) Normalize / acyclicity / presolution
3) Witness extraction (Ω, Φ) + expansion recipes
4) Elaborate to xMLF, including ϕR and Φ(e)

Alignment work for steps (3) and (4), plus the xMLF type AST, is now complete.

## Components and Interfaces
- **`MLF.Types.Elab`**: already includes `TCon` for constructor types `Cσ`; ensure all traversals remain total as new constructors are added.
- **`MLF.Elab.Inst` / `MLF.Elab.TypeCheck` / `MLF.Elab.Reduce`**: ensure instantiation, typing, and reduction remain total and treat `TCon` structurally where relevant.
- **`MLF.Elab.Sigma`**: keep `sigmaReorder` as the implementation of ϕR, and expose it for use by elaboration.
- **`MLF.Elab.Phi.*` / `MLF.Elab.Elaborate`**: ϕR application is integrated when `Typ` vs `Typexp` differ and Φ translation fails fast on non-translatable witness operations.
- **Presolution witness pipeline**: witness normalization/validation aligns with Fig. 15.3.4 assumptions and is regression-covered by row-labeled tests.

## Data Models
- **xMLF Types (Fig. 14.2.1)**: `Ty` includes constructor application via `TCon`; this must be reflected in `TyIF`, `Pretty`, type substitutions, and equality.
- **Quantifier reordering ϕR (Def. 15.3.4)**: model as `Instantiation` produced by `sigmaReorder`, composed with Φ(e) for each subterm. This should be explicit in the elaboration path.
- **Translatable-presolution invariants**: validation reports explicit failures for rigid/non-interior/missing-context non-translatable operations; no silent skipping.

## Error Handling
- Explicit non-translatable paths are surfaced as Φ errors (`PhiTranslatabilityError` / `PhiInvariantError`) and normalization/validation errors (for malformed witnesses).
- Rigid-node identity behavior is conditioned by the paper’s operated-node semantics; invalid rigid placements are rejected.

## Testing Strategy
- **Unit tests**
  - `sigmaReorder` used in elaboration when Typ/Typexp differ: covered by `test/ElaborationSpec.hs` (“applies Σ reordering even without Raise when Typ/Typexp differ”).
  - Witness validation: supply a witness op that targets a non‑interior node and assert a dedicated translatability error.
- **Integration tests**
  - End‑to‑end elaboration on a term that triggers reordering and Raise ops; check `applyInstantiation` matches expected target type.
- **Regression tests**
  - Ensure existing Φ soundness tests continue to pass after reordering is integrated.

## Remaining Deltas (Non-semantic)
- Proof/formalization debt remains out of scope: the implementation is tested and behavior-aligned, but not mechanically proved against the thesis metatheory.
- Phase-7 formal linkage debt remains: xMLF typecheck/reduce is implemented and tested, but not fully formalized as a proof artifact against all thesis obligations.
