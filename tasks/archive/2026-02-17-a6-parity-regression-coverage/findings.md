# Findings: A6 (P2) Parity and Regression Coverage

## Key discoveries
- Pending coverage audit.

## 2026-02-17 coverage audit
- `test/ElaborationSpec.hs` already contains checked-vs-unchecked parity assertions across bounded/coercion-heavy matrix cases (`BUG-002/003/004` variants, bounded aliasing, explicit forall coercions).
- `test/PipelineSpec.hs` has checked-authoritative and selected checked/unchecked comparisons, but bounded/coercion-heavy parity is not yet covered by a dedicated matrix case that also validates `typeCheck` parity against elaborated terms.
- `test/TypeCheckSpec.hs` currently focuses on direct typechecker rules and strict InstBot behavior; it does not yet include regressions that assert parity for elaborated bounded/coercion-heavy terms.
- `test/ReduceSpec.hs` currently covers core reduction rules and simple preservation sanity only; it lacks bounded/coercion-heavy normalization regressions tied to elaboration parity expectations.

## 2026-02-17 implementation findings
- Added dedicated bounded/coercion-heavy parity gate in `test/PipelineSpec.hs` asserting agreement across:
  - `runPipelineElab` (unchecked)
  - `runPipelineElabChecked` (checked)
  - `typeCheck` on both elaborated terms.
- Added TypeCheck regression block in `test/TypeCheckSpec.hs` for:
  - bounded-alias coercion parity (`∀a. a -> a -> a` shape)
  - dual annotated coercion consumers (`Bool` result parity).
- Added Reduce regression block in `test/ReduceSpec.hs` confirming normalization preserves parity/typecheck soundness for both bounded and coercion-heavy elaborated terms.
- `test/ElaborationSpec.hs` already had the requested bounded/coercion checked-vs-unchecked parity coverage; no new case was required after audit.
- Verification results:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "A6"'` => PASS (5 examples, 0 failures).
  - `cabal build all && cabal test` => PASS.
