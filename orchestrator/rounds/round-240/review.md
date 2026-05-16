### Checks Run
- Command: `pwd && git status --short --branch && git rev-parse --abbrev-ref HEAD && git rev-parse --show-toplevel`
  Result: pass; confirmed assigned worktree `/Volumes/src/mlf4/orchestrator/worktrees/round-240` on branch `orchestrator/round-240-backend-callable-shape-module`.
- Command: `python3 -m json.tool orchestrator/rounds/round-240/selection-record.json`
  Result: pass; JSON parses and names roadmap `2026-05-16-00-architecture-deepening-roadmap`, `rev-001`, `milestone-2`, `direction-2a-backend-callable-shape-module`, and `item-2a-private-callable-shape-owner-module`.
- Command: `python3 -m json.tool orchestrator/rounds/round-240/round-plan-record.json`
  Result: pass; JSON parses and matches the selected round lineage with `worker_mode: none`.
- Command: `python3 -m json.tool orchestrator/roadmaps/2026-05-16-00-architecture-deepening-roadmap/rev-001/roadmap-view.json`
  Result: pass; JSON parses, `milestone-2` is currently `pending`, and anchors `milestone-2`, `milestone-2-completion`, and `roadmap-history-completed-rounds` resolve.
- Command: `git diff --check`
  Result: pass; no whitespace or conflict-marker issues, including after writing review artifacts.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "validates explicit closure construction and indirect closure calls"'`
  Result: pass; 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "clears shadowed closure locals when classifying let RHS values"'`
  Result: pass; 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "classifies function-valued case pattern fields as closure locals"'`
  Result: pass; 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "lowers case-selected closure callees through the explicit closure ABI"'`
  Result: pass; 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "rejects BackendApp heads that select closure values through let or case"'`
  Result: pass; 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "callable-shape contract stays explicit and direct-vs-closure call heads stay unambiguous"'`
  Result: pass; 1 example, 0 failures.
- Command: `cabal build all && cabal test`
  Result: pass; full suite completed with 2581 examples, 0 failures.
- Command: `python3 -m json.tool orchestrator/rounds/round-240/review-record.json`
  Result: pass; approving reviewer record parses as `review-record-v3` with status-only closeout selectors for `milestone-2`.

### Plan Compliance
- Audit current callable-shape authorities and freeze a minimal owner API: met. `MLF.Backend.CallableShape` owns the shared callable-head datatypes, expression view, adapter class, alternatives, and `backendCallableHead`; closure ABI and validation context stay out of the owner.
- Add private owner module and register it in Cabal: met. `src/MLF/Backend/CallableShape.hs` exists and `mlf2.cabal` registers it in the private `mlf2-internal` library, while the public library remains limited to `MLF.API`, `MLF.Pipeline`, and `MLF.XMLF`.
- Rewire `MLF.Backend.IR`: met. IR imports `MLF.Backend.CallableShape`, provides the `BackendExpr` adapter instance, and uses the owner through `backendCallableHeadInContext`; validation errors, validation context, and executable IR nodes remain in `MLF.Backend.IR`.
- Rewire `MLF.Backend.Convert`: met. Conversion imports `MLF.Backend.CallableShape` directly and routes closure-value and closure-head classification through `backendCallableHead` plus conversion-local scope resolution.
- Rewire `MLF.Backend.LLVM.Lower`: met. Lowering imports `MLF.Backend.CallableShape` directly and uses it for closure-call-path detection and malformed direct-call rejection; closure ABI layout, runtime wrappers, and native emission details remain lowerer-local.
- Update docs and RepoGuard: met. `docs/architecture.md` names `MLF.Backend.CallableShape` as the private callable-head owner, and `test/RepoGuardSpec.hs` now guards the owner and updated wording.
- Preserve focused behavior and diagnostics: met. Direct-call, closure-call, shadowing, case-field, lowering, and callable-contract selectors all pass, followed by the full Cabal gate.

### Decision
**APPROVED**

### Evidence
The diff stays in the selected milestone-2 scope: `docs/architecture.md`, `mlf2.cabal`, `src/MLF/Backend/CallableShape.hs`, `src/MLF/Backend/Convert.hs`, `src/MLF/Backend/IR.hs`, `src/MLF/Backend/LLVM/Lower.hs`, and `test/RepoGuardSpec.hs`. There is no `src-public/` diff.

Manual import and text scans confirm `MLF.Backend.Convert` and `MLF.Backend.LLVM.Lower` consume `MLF.Backend.CallableShape` directly, `MLF.Backend.IR` supplies the executable IR adapter/validation context, and the docs/RepoGuard wording does not claim milestone-3 native parity policy or milestone-4 CLI-emission work.

The round fully satisfies the milestone-2 completion signal. The closeout is status-only: mark `milestone-2` from `pending` to `done`, add a compact completion pointer, and add a compact history entry through existing `roadmap-view.json` anchors. No semantic roadmap update is required.
