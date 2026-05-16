### Selected Extraction
- Milestone: Backend Callable-Shape Module
- Milestone id: milestone-2
- Direction id: direction-2a-backend-callable-shape-module
- Extracted item id: item-2a-private-callable-shape-owner-module
- Roadmap id: 2026-05-16-00-architecture-deepening-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-16-00-architecture-deepening-roadmap/rev-001

### Goal

Create one private backend owner for callable-shape facts so direct-vs-closure head classification stops living partly in `MLF.Backend.IR`, partly in `MLF.Backend.Convert`, and partly in `MLF.Backend.LLVM.Lower`, while preserving the current `BackendApp` versus `BackendClosureCall` behavior and keeping `MLF.Backend.IR` as the single executable backend IR seam.

### Approach

`milestone-2` is the first lawful dependency-ready unfinished milestone at HEAD: `roadmap-view.json` marks `milestone-1` done, `milestone-2` pending, and later milestones still depend on it. Current repo evidence also shows the ownership gap is still live:

- `src/MLF/Backend/IR.hs` currently exports `BackendCallableBindingKind`, `BackendCallableHead`, and `backendCallableHead`, then uses them in validation and closure-global discovery.
- `src/MLF/Backend/Convert.hs` still carries separate callable-shape policy through `backendExprIsClosureValue`, `convertCallableBindingKind`, and `closureScopeNameIsClosure`.
- `src/MLF/Backend/LLVM/Lower.hs` still makes direct-vs-closure rejection decisions from callable-head analysis at lowering entry points, even though closure ABI layout and indirect-call lowering mechanics must remain lowerer-local.
- `docs/architecture.md` and `test/RepoGuardSpec.hs` still describe the callable contract, but they do not yet name one private owner module for that shared classification seam.

Keep the round serial and bounded to callable-shape ownership only. The intended implementation is a private backend module such as `MLF.Backend.CallableShape` plus the minimal cabal/docs/test updates that let IR validation, checked-program conversion, and LLVM lowering import one owner. Do not create a second public backend IR, do not widen `src-public/`, and do not move closure ABI/runtime wrapper details out of `MLF.Backend.LLVM.Lower`.

### Steps

1. Audit the current callable-shape authorities in `src/MLF/Backend/IR.hs`, `src/MLF/Backend/Convert.hs`, `src/MLF/Backend/LLVM/Lower.hs`, `docs/architecture.md`, and `test/RepoGuardSpec.hs`, and freeze the minimal owner API needed for direct-vs-closure head classification. The owner should cover shared callable-head datatypes and classification helpers, not lowering ABI details or backend program validation state assembly.
2. Add the private owner module under `src/MLF/Backend/` and register it in `mlf2.cabal`. Move `BackendCallableBindingKind`, `BackendCallableHead`, `backendCallableHead`, and the shared head-collapsing/binding-kind extension helpers into that module, or re-home equivalent logic there if the final names change.
3. Rewire `MLF.Backend.IR` to import the new owner for callable-head analysis in validation and closure-global discovery. Keep `BackendValidationError`, `BackendValidationContext`, lexical/global environment construction, and the executable IR node definitions in `MLF.Backend.IR`; the new owner must not become a second IR surface.
4. Rewire `MLF.Backend.Convert` so closure-value detection and call-shape classification flow through the owner module instead of separate local policy. Replace the current duplicated authority around `backendExprIsClosureValue`, `convertCallableBindingKind`, and `closureScopeNameIsClosure`, while keeping conversion-local scope/evidence bookkeeping and closure-capture recovery local to conversion.
5. Rewire `MLF.Backend.LLVM.Lower` to consume the same owner for direct-vs-closure rejection and callable-head detection at lowering entry points. Keep closure ABI layout, environment-record storage, wrapper/runtime symbol emission, and indirect-call lowering mechanics lowerer-local.
6. Update `docs/architecture.md` to name the callable-shape owner explicitly, update `test/RepoGuardSpec.hs` so the contract guards include that owner module, and preserve the current contract language that `BackendApp` stays direct while `BackendClosureCall` stays the explicit closure-call path.
7. Tighten or refresh focused backend tests only where the moved owner seam changes imports or helper visibility. Preserve the existing direct-call/closure-call behavior, diagnostics, shadowing behavior, and case/let-selected closure handling.

### Verification

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "validates explicit closure construction and indirect closure calls"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "clears shadowed closure locals when classifying let RHS values"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "classifies function-valued case pattern fields as closure locals"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "lowers case-selected closure callees through the explicit closure ABI"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "rejects BackendApp heads that select closure values through let or case"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "callable-shape contract stays explicit and direct-vs-closure call heads stay unambiguous"'`
- Manual audit: no second public backend IR or public lowering API is introduced, and `MLF.Backend.LLVM.Lower` still owns closure ABI/runtime implementation details.
- Manual audit: `docs/architecture.md`, `test/RepoGuardSpec.hs`, and backend module comments agree on the private callable-shape owner and do not overclaim milestone-3 parity policy or milestone-4 CLI work.
- `git diff --check`
- `cabal build all && cabal test`

### Round Plan Record

Also written beside this plan:

- `orchestrator/rounds/round-240/selection-record.json`
- `orchestrator/rounds/round-240/round-plan-record.json`
