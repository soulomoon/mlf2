# Requirements

## Goal
Verify that the implementation of thesis-exact scheme closure aligns with the thesis description of named nodes, environments, and scheme translation, and identify any remaining deviations.

## Requirements

R1. Named nodes definition
When translating presolutions, the implementation must treat named nodes as the type nodes flexibly bound on gen nodes, matching thesis section 15.3.1.

R2. Scheme closure under gen scope
For each scheme root bound under a gen node g, all named nodes reachable from the scheme root must be bound under g. Violations must be rejected (no fallback binder insertion), consistent with the thesis assumptions used by Typ and Gamma_a (15.3.1–15.3.2.1).

R3. No post-hoc closure in generalizeAt
generalizeAt must not insert missing binders for free names; it must fail if any free names remain outside the quantified binders (Gamma_a), aligning with the use of S_chi'p and Typ (15.3.1–15.3.2).

R4. Scope-root selection
When a scheme root belongs to multiple gen nodes, the implementation must pick a gen scope that actually binds the reachable named nodes, to preserve the intended Gamma_a (15.3.2.1).
