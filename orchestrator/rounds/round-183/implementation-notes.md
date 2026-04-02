## Summary

- Outcome:
  `sameLaneDoubleAliasFrameClearBoundaryExpr remains honest only via an exact one-extra-alias-shell TermClosure rule`.
- Investigation:
  rerunning the selected packet and the adjacent one-alias control stayed
  green on the current baseline, and the current authoritative output still
  collapses the double-alias packet back to the same retained-child `k`
  surface as the one-alias packet. Combined with the surviving
  `hasRetainedChildAliasBoundary v body 1` recursion in
  `src/MLF/Elab/TermClosure.hs`, that localizes the current success to the
  accepted one-extra-alias-shell path rather than to a new packet-local
  rescue.
- Change:
  synchronized the selected packet tests to the exact two-forall
  recursive-arrow authoritative result and added a focused `TermClosure`
  source/mechanism guard that requires the round-182 clear-boundary shape plus
  the bounded one-extra-alias-shell recursion. `src/MLF/Elab/TermClosure.hs`
  already matched that exact bounded rule, so no production edit was needed.

## Commands

- Focused packet reruns:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr preserves recursive output"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr double-alias clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
- Adjacent read-only control:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
- Full gate:
  - `cabal build all && cabal test`
