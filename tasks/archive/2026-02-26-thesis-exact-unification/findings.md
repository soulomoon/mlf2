# Findings

## 2026-02-26
- Execution started from plan `docs/plans/2026-02-26-thesis-exact-unification-plan.md`.
- No critical preflight blockers found; current branch/worktree already isolated.
- Presolution now carries equivalence metadata as `prUnionFind` so the raw presolution graph can remain translation input.
- Shared closure semantics now live in `MLF.Constraint.Unify.Closure` and are reused by both Solve and Presolution.
- Thesis ordering is now enforced in presolution:
  - drain initial pending unify queue before inst-edge traversal,
  - process inst edges with propagation,
  - drain closure after each traversal effect set.
- Canonicalization continuity requires composing old and new UF maps after rewrite; otherwise node representatives can drift.
- Running closure after canonicalization without parent repair can raise `ParentNotUpper`; repairing bind parents before closure drain avoids this.
- `Solved.fromPresolutionResult` can be implemented without module cycles by introducing a lightweight snapshot class (`PresolutionSnapshot`) in `MLF.Constraint.Types.Presolution` and implementing the instance where `PresolutionResult` is defined.
- Production pipeline switch to presolution-native solved construction currently breaks elaboration invariants (e.g., alias-bound finalization), so production should stay on legacy replay until native parity is broader than anchor tests.
- Presolution closure drainage must avoid forcing `runUnifyClosure` when no unify edges are pending; invoking closure on such intermediate states can surface transient binding-tree-root shape errors unrelated to pending unification work.
- Exact canonical-map equality between no-replay and legacy replay solved conversions is not guaranteed even on simple anchors; stronger parity checks should compare canonical/original constraints and elaborated-type outcomes.
- After regression fixes, full-suite failures dropped from 38 to 21, with remaining issues concentrated in presolution merge/expansion behavior and downstream elaboration baselines.
- Final stabilization pass cleared remaining failures: full suite is now green (`838 examples, 0 failures`).
- `./scripts/thesis-conformance-gate.sh` is green end-to-end, including obligations ledger, claims validation, Phi/Omega matrix, theorem/property gates, and expansion minimality checks.
- This repository tracks remote default as `origin/master` (not `origin/main`); release diffstat should use `origin/master...HEAD`.
