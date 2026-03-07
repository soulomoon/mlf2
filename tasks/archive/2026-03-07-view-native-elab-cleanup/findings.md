# Findings

- Remaining non-test `fromSolved` uses are concentrated in `MLF.Elab.Run.Scope`, `MLF.Elab.Run.TypeOps`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.ResultType.Util`, and `MLF.Reify.Core`.
- The runtime/result-type path is already mostly χp-native; the remaining wrappers are thin adapters and internal facade leaks.
- `MLF.Elab.Legacy` is the clear compatibility zone allowed to retain explicit `fromSolved` usage.
- The test suite already carried most χp-native helpers, so the remaining work was largely signature cleanup plus explicit view threading at a handful of test/internal facade call sites.
- `fromSolved` now remains only in `MLF.Constraint.Presolution.View`, `MLF.Elab.Legacy`, and tests.
