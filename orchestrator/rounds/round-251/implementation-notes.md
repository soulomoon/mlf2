### Changes Made

- Preservation: attached the dirty detached handoff checkout at `/Users/ares/.codex/worktrees/b648/mlf4` to preservation branch `codex/full-type-level-mlfp` at starting HEAD `ca8f8e02` before transplant work. The preserved checkout still contains the handoff tracked changes plus the three handoff-untracked files.
- Starting canonical status: `/Volumes/src/mlf4/orchestrator/worktrees/round-251` was on branch `orchestrator/round-251-full-type-level-handoff` at `c8a6822f` with untracked round controller artifacts only.
- Reconciliation: transplanted the preserved handoff diff onto the canonical round-251 branch, resolving current-base conflicts in `docs/architecture.md`, `docs/syntax.md`, and `mlf2.cabal` by keeping package-substrate owners/search-path/build-graph wording and adding the type-level handoff modules/contracts.
- `src/MLF/Frontend/TypeLevel.hs`: added the internal type-level AST and normalizer for kinded forms, type lambdas, closed families, simultaneous substitution, fuel, cycle, and stuck diagnostics.
- `src/MLF/Frontend/Program/TypeFamilies.hs`: added `.mlfp` closed type-family normalization and erasure before resolver/checker core boundaries.
- `src/MLF/Frontend/Syntax.hs`, `src/MLF/Frontend/Syntax/Program.hs`, `src/MLF/Frontend/Parse.hs`, `src/MLF/Frontend/Parse/Program.hs`, `src/MLF/Frontend/Pretty.hs`, `src/MLF/Frontend/Pretty/Program.hs`, `src/MLF/Parse/Type.hs`: reconciled parser, syntax, and pretty-printer support for Unicode type lambdas, closed type families, kind annotations, multi-parameter classes, superclasses, functional dependencies, and variable-headed type applications.
- `src/MLF/Frontend/Program/Check.hs`, `src/MLF/Frontend/Program/Elaborate.hs`, `src/MLF/Frontend/Program/Finalize.hs`, `src/MLF/Frontend/Program/Prelude.hs`, `src/MLF/Frontend/Program/Resolve.hs`, `src/MLF/Frontend/Program/Run.hs`, `src/MLF/Frontend/Program/Types.hs`: reconciled program checking/elaboration/finalization/runtime preparation for type-family erasure, multi-parameter typeclasses, superclasses, functional dependencies, and package-owned visibility.
- `src/MLF/Constraint/*`, `src/MLF/Elab/*`, `src/MLF/Reify/*`, `src/MLF/Types/Elab.hs`, `src/MLF/XMLF/*`: reconciled variable-headed type application through constraint generation, unification, reification, xMLF syntax/checking, and fail-closed unsupported paths.
- `src/MLF/Backend/Convert.hs`, `src/MLF/Backend/Emission/Prepare.hs`, `src/MLF/Backend/IR.hs`, `src/MLF/Backend/LLVM/Lower.hs`, `src/MLF/Primitive/Inventory.hs`: reconciled backend/native support, including package emission preparation, variable-headed type applications where supported, explicit fail-closed cases, and native IO wrapper coverage for supported `IO` methods.
- `test/FrontendTypeLevelSpec.hs`, `test/ProgramSpec.hs`, `test/FrontendParseSpec.hs`, `test/FrontendPrettySpec.hs`, `test/FrontendNormalizeSpec.hs`, `test/BackendLLVMSpec.hs`, `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, `test/ResolvedSymbolSpec.hs`, `test/GoldenSpec.hs`, `test/Parity/ProgramMatrix.hs`, `test/Main.hs`: added and wired focused assertions for type-level normalization, parser/pretty behavior, diagnostics, program runtime parity, backend/native behavior, and type-level/public surface regressions.
- `test/SolvedFacadeTestUtil.hs`: updated the test-support node classifier for `TyVarApp` so the full build stays warning-free under `-Wall`.
- `mlf2.cabal`: registered `MLF.Frontend.Program.TypeFamilies`, `MLF.Frontend.TypeLevel`, and `FrontendTypeLevelSpec` while preserving current package build graph registration.
- `README.md`, `CHANGELOG.md`, `docs/architecture.md`, `docs/mlfp-language-reference.md`, `docs/syntax.md`: updated support-layer and public contract wording without claiming separate compilation, stable ABI, package-manager support, remote dependencies, or full compiler-in-`.mlfp` self-hosting.
- Retry after rejected review: updated `docs/mlfp-language-reference.md` to remove stale blanket backend IO rejection wording, document the supported `emit-backend`/`emit-native` native IO wrapper paths, record the remaining fail-closed `Applicative IO.ap`/`__io_ap` native-lowering boundary, and refresh the Prelude IO/IORef inventory without changing implementation code.

### Tests

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Frontend.TypeLevel"'`: passed, 12 examples, 0 failures.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program parse/pretty"'`: passed, 45 examples, 0 failures.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program diagnostics"'`: passed, 49 examples, 0 failures.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program shared runtime-success parity surface"'`: passed, 123 examples, 0 failures.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.LLVM"'`: passed, 312 examples, 0 failures.
- `git diff --check`: passed.
- Conflict-marker scan over `AGENTS.md CHANGELOG.md README.md docs src src-public test mlf2.cabal runtime scripts`: passed, no conflict markers found.
- `cabal build all`: passed after adding the `TyVarApp` test-support classifier above.
- `cabal test`: passed, 2560 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`: passed, thesis conformance anchors are green.
- Retry docs support-layer stale-wording check over `docs/mlfp-language-reference.md`: passed, no stale blanket backend IO rejection wording remains.
- Retry docs support-layer positive-marker check over `docs/mlfp-language-reference.md`: passed, the language reference now names the supported native IO wrapper paths and the `__io_ap` native-lowering boundary.
- Retry `git diff --check`: passed.
- Retry conflict-marker scan over `AGENTS.md CHANGELOG.md README.md docs src src-public test mlf2.cabal runtime scripts`: passed, no conflict markers found.
- Full Cabal gates were not rerun in this retry because the reviewer rejected only stale public support-layer documentation, the retry changed only `docs/mlfp-language-reference.md` and this notes file, and the previous review immediately recorded green `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh` on the reconciled implementation.

### Notes

The three handoff-untracked files are present in the canonical worktree:
`src/MLF/Frontend/Program/TypeFamilies.hs`, `src/MLF/Frontend/TypeLevel.hs`,
and `test/FrontendTypeLevelSpec.hs`.

Generated depfile churn in `runtime/mlfp_io/target/release/libmlfp_io.d` was
restored out of the canonical diff after focused LLVM validation and the full
required gates.

Publication state: ready for review/merge/publish from the implementer side.
This role did not merge, publish, or update controller state.
