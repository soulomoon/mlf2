# Design

## Evidence crosswalk

Paper references:
- Section 15.3.1 defines named nodes as type nodes flexibly bound on gen nodes and introduces S_chi'p (papers/these-finale-english.txt around 17470–17530).
- Section 15.3.2.1 defines Gamma_a as the ordered named nodes bound on gen node g (papers/these-finale-english.txt around 17548–17575).
- Section 15.3.2 uses S_chi'p (not S_chi p) for Typ, so named nodes must be in scope (papers/these-finale-english.txt around 17586–17605).

Code alignment:
- Named nodes are defined as flexibly bound type variables under gen nodes, excluding scheme roots: src/MLF/Elab/Reify.hs:601-628.
- Scheme closure invariant rejects scheme roots that reach named nodes not bound under their gen node: src/MLF/Binding/Tree.hs:894-969.
- Scheme free variables are rejected (no binder insertion): src/MLF/Constraint/Presolution/Plan/Finalize.hs:280-363 (SchemeFreeVars).
- Scheme closure check is enforced before Phi translation: src/MLF/Elab/Phi.hs:299-309.
- Scope selection prefers gen nodes that bind reachable named variables: src/MLF/Elab/Generalize.hs:286-313.

Tests:
- Regression for scheme closure exists: test/ElaborationSpec.hs:1306-1314.
- Full suite currently passes (per latest run).

## Assessment
The implementation matches the thesis model for named nodes and scheme closure at the binding-structure level (R1, R2), and it removes fallback closure at generalization time (R3). The scope-root selection now chooses a gen owner that actually binds reachable named variables, which is consistent with Gamma_a (R4).

## Residual risk / deviation note
Lemma 15.3.5 only assumes an ambient environment Gamma that binds the free variables of Typ(a'), where Typ(a') is built from S_chi'p and named nodes (papers/these-finale-english.txt around 17632-17640). This suggests that free variables should correspond to named nodes, while rigidly bound nodes are inlined by S_chi p (Figure 15.3.2). The scheme finalizer only reports missing free names that are under the chosen scope gen; names outside the scope are allowed (src/MLF/Constraint/Presolution/Plan/Finalize.hs:280-300). If those non-scope variables can appear in Typ(a') (e.g., rigid nodes or flex nodes outside the gen subtree), this is a potential deviation from the thesis translation. No failing tests were observed, but this should be verified against concrete cases; otherwise document as an intentional deviation.
