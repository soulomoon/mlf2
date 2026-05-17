### Selected Extraction
- Milestone: Full Type-Level MLFP Handoff Merge And Publication
- Milestone id: milestone-7
- Direction id: direction-7a-full-type-level-handoff-merge
- Extracted item id: round-251-full-type-level-handoff
- Roadmap id: 2026-05-17-00-mlfp-package-substrate-roadmap
- Roadmap revision: rev-002
- Roadmap dir: orchestrator/roadmaps/2026-05-17-00-mlfp-package-substrate-roadmap/rev-002

### Goal
Consume the completed full type-level `.mlfp` handoff, preserve the dirty detached checkout before any staging, reconcile the implementation with the current package-substrate base, include the handoff-untracked files, rerun current validation, and leave the result in an explicit publication-ready state for the controller and later roles.

### Approach
Work serially. The controller state has `max_parallel_rounds=1`, and the handoff surface is broad: parser, pretty-printer, program checker, type-family normalization, typeclass/fundep handling, elaboration, constraint solving, reification, backend conversion/lowering, native IO support, docs, Cabal registration, and tests all move together. Splitting this before the preserved branch and current-base conflict set are known would create overlapping ownership and stale validation claims.

Treat the handoff validation as useful historical evidence only. Approval for this round must rest on validation rerun after reconciliation onto the current `orchestrator/round-251-full-type-level-handoff` worktree. Preserve the detached handoff checkout first, then use it as the source artifact to transplant/reconcile into the canonical round worktree. Do not discard dirty changes, do not drop untracked files, and do not update controller state or roadmap coordination.

Stay inside milestone-7. This round may reconcile the already implemented full type-level language support described by the handoff. It must not start a fresh compiler-in-`.mlfp` implementation, add a package manager, introduce remote dependencies, promise a stable ABI/linker, add a duplicate public backend IR, or claim full self-hosting.

### Steps
1. Reconfirm the canonical round worktree is `/Volumes/src/mlf4/orchestrator/worktrees/round-251` on branch `orchestrator/round-251-full-type-level-handoff`, and record the starting status in `orchestrator/rounds/round-251/implementation-notes.md`.
2. Re-read the handoff file `/var/folders/36/_743psv11gv2wrj9dclrpd500000gn/T/handoff-XXXXXX.md.obimvbtKjv` and the current rev-002 roadmap/verification files before touching the handoff checkout.
3. Inspect `/Users/ares/.codex/worktrees/b648/mlf4` with `git status --short --branch`, `git rev-parse --short HEAD`, `git diff --stat`, `git diff --name-only`, and `git ls-files --others --exclude-standard`.
4. Before staging or resetting anything in the handoff checkout, attach it to an intentional preservation branch. Prefer `codex/full-type-level-mlfp` if absent; if that branch already exists, create a clearly named preservation branch such as `codex/full-type-level-mlfp-round-251-preserve`. Record the chosen branch name and starting HEAD.
5. Verify the three handoff-untracked files are present after preservation: `src/MLF/Frontend/Program/TypeFamilies.hs`, `src/MLF/Frontend/TypeLevel.hs`, and `test/FrontendTypeLevelSpec.hs`.
6. Inventory the handoff diff by area before transplanting: syntax/parser/pretty, type-level normalizer, program type families, program check/elaborate/finalize/run/resolve/prelude/types, constraint/elab/reify type-application support, backend conversion/emission/LLVM/native IO support, docs, tests, `mlf2.cabal`, and `test/Main.hs`.
7. Transplant the preserved handoff implementation into the canonical round-251 worktree. Preserve current package-substrate behavior from milestones 1-6 when resolving conflicts; do not revive a separate one-file `.mlfp` semantic path.
8. Include the three previously untracked files in the canonical round-251 diff and confirm `mlf2.cabal` registers `MLF.Frontend.Program.TypeFamilies`, `MLF.Frontend.TypeLevel`, and `FrontendTypeLevelSpec`, with `test/Main.hs` importing and running `FrontendTypeLevelSpec.spec`.
9. Reconcile syntax/parser/pretty changes so Unicode type lambdas, closed type-family declarations, kind annotations, multi-parameter classes, superclasses, and functional dependencies are parsed and pretty-printed through the current `.mlfp` program surface.
10. Reconcile the type-level normalization and erasure path so closed families are module/import/export scoped, RHS variables are limited to equation pattern variables and local lambda binders, substitution is simultaneous and capture avoiding, lambda-body normalization is delayed where required, cycles/fuel/stuck cases fail closed, and type-family declarations/imports/exports are erased before the core boundary.
11. Reconcile program checking, elaboration, finalization, Prelude, and run preparation so multi-parameter typeclasses, fundeps, superclasses, and kind-erased variable-headed type application use the current package/module owner and do not bypass interface or visibility policy.
12. Reconcile lower core, xMLF, backend IR, conversion, reification, and LLVM/native changes so supported variable-headed type application reaches supported paths and unsupported native/backend cases fail closed with explicit diagnostics.
13. Reconcile native IO method support to match the handoff contract: `Functor IO.map` may be supported where renderable, while `Applicative IO.ap` may remain fail-closed for non-renderable function-valued `IO` results.
14. Reconcile docs and public contracts in `README.md`, `CHANGELOG.md`, `docs/mlfp-language-reference.md`, `docs/syntax.md`, and `docs/architecture.md`. Classify support by source checking, interpreter/runtime, backend/native, object code, and package build mode; do not overclaim separate compilation, a stable ABI, a package manager, remote dependencies, or full compiler-in-`.mlfp` self-hosting.
15. Confirm generated depfile churn, especially `runtime/mlfp_io/target/release/libmlfp_io.d`, is absent from the final diff unless there is an intentional source-backed reason.
16. If reconciliation exposes a hard blocker that would require widening beyond the handoff, record the blocker in implementation notes and stop. Do not silently expand milestone-7 into new compiler-in-`.mlfp`, primitives, stdlib, package-manager, or native-driver scope.
17. Leave the publication state explicit in implementation notes: name the preservation branch, summarize the reconciled canonical diff, list validation results, and state whether the round branch is ready for review/merge/publish or blocked.

### Verification
- Focused handoff surface:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Frontend.TypeLevel"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program parse/pretty"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program diagnostics"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program shared runtime-success parity surface"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.LLVM"'`
- Diff and conflict hygiene:
  - `git diff --check`
  - `! rg -n "^(<<<<<<<|=======|>>>>>>>)" AGENTS.md CHANGELOG.md README.md docs src src-public test mlf2.cabal runtime scripts`
  - `git status --short`
- Required full gates after reconciliation:
  - `cabal build all`
  - `cabal test`
  - `./scripts/thesis-conformance-gate.sh`

Manual checks:

- Confirm the handoff checkout was preserved on an intentional branch before staging, reset, or transplant work.
- Confirm the three handoff-untracked files are present in the final canonical diff and registered where needed.
- Confirm current package-substrate semantics from milestones 1-6 remain the durable `.mlfp` package model.
- Confirm type-level feature support is tested with real assertions for kinded type-level frontend support, Unicode type lambdas, closed families, multi-parameter typeclasses, superclasses, functional dependencies, module/import/export scoping, RHS variable scope, simultaneous substitution, delayed lambda-body normalization, cycle/fuel/stuck diagnostics, and variable-headed type application.
- Confirm backend/native behavior is accurately classified, including fail-closed cases.
- Confirm docs do not claim separate compilation, stable ABI, package-manager support, remote dependencies, or full compiler-in-`.mlfp` self-hosting.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this file. They are the machine authority for lineage and worker scheduling.
