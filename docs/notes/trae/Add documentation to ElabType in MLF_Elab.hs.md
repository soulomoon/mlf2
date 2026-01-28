I have analyzed `papers/xmlf.txt` and `src/MLF/Elab.hs`.

**Comparison Analysis:**

1.  **Type Structure (`ElabType`)**:
    *   **xMLF (Paper)**:
        *   Variables: $\alpha$
        *   Arrow: $\tau \to \tau$
        *   Flexible Quantification: $\forall(\alpha \ge \tau) \tau'$
        *   Bottom: $\perp$
    *   **MLF.Elab (Implementation)**:
        *   `TVar String`: Matches $\alpha$.
        *   `TArrow ElabType ElabType`: Matches $\tau \to \tau$.
        *   `TBase BaseTy`: Adds base types (Int, Bool, etc.), which are standard extensions mentioned in the paper ("Other type constructors will be added later").
        *   `TForall String (Maybe ElabType) ElabType`: Matches $\forall(\alpha \ge \tau) \tau'$. The `Maybe` allows representing standard System F unbounded quantification (equivalent to $\ge \perp$ in the paper).
        *   `TBottom`: Matches $\perp$.

    **Conclusion**: `ElabType` accurately reflects the xMLF design, including the key feature of flexible quantification.

2.  **Detailed Note Content**:
    I will add a note to `ElabType` explaining:
    *   Its correspondence to the xMLF types defined in Figure 1 of the paper.
    *   The meaning of `TForall` with `Nothing` vs `Just` (unbounded vs flexible).
    *   The role of `TBottom`.

**Plan:**

1.  Update `src/MLF/Elab.hs` to add the detailed documentation note to `ElabType`.

**Proposed Note:**
```haskell
-- | Explicitly typed types for elaboration (xMLF).
-- Corresponds to Figure 1 in "A Church-Style Intermediate Language for MLF".
--
-- xMLF extends System F with instance-bounded polymorphism (flexible quantification):
--   ∀(α ⩾ τ). σ
--
-- This restricts the variable α to range only over instances of τ.
--
-- Constructors:
--   * TVar: Type variables (α)
--   * TArrow: Function types (τ -> τ)
--   * TBase: Base types (Int, Bool, etc.) - extension of the paper's calculus
--   * TForall: Flexible quantification ∀(α ⩾ τ). σ.
--       - Nothing bound implies ⩾ ⊥ (standard System F unbounded quantification)
--       - Just bound implies explicit instance bound
--   * TBottom: The bottom type ⊥ (minimal type), used as the default bound.
```