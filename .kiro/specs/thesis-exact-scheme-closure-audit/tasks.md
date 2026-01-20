# Tasks

- [x] 1. Verify named-node definition against thesis 15.3.1
  - Confirm named nodes are exactly flexibly bound type nodes under gen nodes and exclude scheme roots.
  - Evidence: src/MLF/Elab/Reify.hs:601-628.

- [x] 2. Verify scheme closure invariant against thesis 15.3.2
  - Confirm reachable named nodes from a scheme root must be bound under its gen node.
  - Evidence: src/MLF/Binding/Tree.hs:894-969, src/MLF/Elab/Phi.hs:299-309.

- [x] 3. Validate no fallback generalization
  - Confirm generalizeAt fails on free names instead of inserting binders.
  - Evidence: src/MLF/Elab/Generalize.hs:520-602.

- [x] 4. Review allowed free-name exceptions vs Gamma_a
  - Re-check thesis Lemma 15.3.5 and surrounding text to confirm whether rigid/out-of-scope names may remain free.
  - If mismatch, adjust allowedNames logic or document the deviation.

- [x] 5. Re-run verification
  - cabal test mlf2-test --test-show-details=direct
