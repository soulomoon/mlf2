### Selected Extraction
- Milestone: Fixture Migration And Self-Boot Readiness Evidence
- Milestone id: milestone-6
- Direction id: direction-6a-fixture-migration-readiness-ledger
- Extracted item id: round-250-fixture-migration-readiness
- Roadmap id: 2026-05-17-00-mlfp-package-substrate-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-17-00-mlfp-package-substrate-roadmap/rev-001

### Goal
Close the package-substrate family by making the `.mlfp` fixture evidence run through package mode, aligning docs so one-file inputs are described only as trivial package inputs, and adding a self-boot readiness ledger that records remaining gaps before any compiler-in-`.mlfp` roadmap begins.

### Approach
Work serially. This is the final family closeout and touches fixture ownership, parity tests, public docs, and readiness evidence; splitting it would create overlapping claims that must be checked together.

Use the current package-mode surfaces from prior milestones:

- `MLF.Frontend.Program.Package` owns file-as-trivial-package and local root/search-path package discovery.
- `MLF.Frontend.Program.Check`, `MLF.Frontend.Program.Run`, and `MLF.Backend.Emission.Prepare` now have package-aware check/run/backend preparation adapters.
- `MLF.Program.CLI` and `MLF.Pipeline` expose local package-mode check/run entrypoints.
- Existing fixture files under `test/programs/recursive-adt/` and `test/programs/unified/` are still ordinary `.mlfp` files; keep them valid as trivial package inputs, but stop presenting one-file programs as the durable semantic model.

Do not implement compiler components in `.mlfp`, broaden primitives/stdlib, add a package manager, add remote dependencies, add a stable ABI/linker, or claim full self-boot completion.

### Steps
1. Inspect the current fixture and parity surfaces before editing: `test/Parity/ProgramMatrix.hs`, `test/ProgramSpec.hs`, `test/ProgramCliPackageSpec.hs`, `test/PublicSurfaceSpec.hs`, `test/BackendLLVMSpec.hs`, `test/programs/recursive-adt/`, `test/programs/unified/`, `README.md`, `docs/mlfp-language-reference.md`, `docs/syntax.md`, and `docs/architecture.md`.
2. Add a focused package fixture spec, preferably `test/ProgramFixturePackageSpec.hs`, under the Hspec description `MLF.Program fixture package migration`.
3. In that spec, run every path from `fixturePaths` and `unifiedFixtureExpectations` through package-mode file handling, not raw `checkProgram` only. Use the public/package adapters or CLI file helpers so each file is checked/run as a trivial `LocatedProgramPackage`.
4. Add at least one durable multi-file package fixture under `test/programs/packages/` that is not a temp-directory-only test. It should split a current cross-module example into package root files and prove package root discovery, check, run, and backend emission over the static fixture.
5. Add one static ordered-search-path fixture pair under `test/programs/packages/` if not covered by the package-root fixture, so the fixture corpus includes a real file-backed search-path example outside temp tests.
6. Update `Parity.ProgramMatrix` only as needed so package fixture paths and expectations are owned in one reusable place. Avoid a broad rewrite of all matrix rows unless it removes real duplication; parser/pretty syntax tests may still load raw files when they are testing syntax rather than package behavior.
7. Keep existing single-file fixtures present unless a test-specific split is necessary. Their role after this round is trivial-package compatibility evidence, not a separate semantic mode.
8. Extend focused CLI/API tests only where needed to consume the new static package fixtures. Do not duplicate every existing temp-package assertion.
9. Update `README.md`, `docs/mlfp-language-reference.md`, and `docs/syntax.md` so the durable model is local package mode. File examples must be worded as trivial package inputs; directory root and `--search-path` package examples should be first-class examples.
10. Add a readiness ledger, preferably `docs/mlfp-self-boot-readiness.md`, with a concise matrix by layer: source checking, interpreter/runtime, backend/native, object code, package build mode, compiler-in-`.mlfp` implementation, primitives, stdlib, parser/lexer, diagnostics, and fixture evidence.
11. In the ledger, record what this package-substrate family proves and what it does not prove. It should explicitly say the repo is not yet self-hosting and that no compiler implementation in `.mlfp` exists yet.
12. In the ledger, record the next-family recommendation. Keep it evidence-based: propose the next roadmap around compiler-in-`.mlfp` prerequisites, such as lexer/parser seed, stdlib/primitive gaps, backend lowerability/native-driver gaps, and package build artifact policy, instead of starting that work here.
13. Update `docs/architecture.md` only if fixture/readiness ownership needs a durable note. Do not change package coordination or roadmap semantics.
14. Wire any new spec module into both `mlf2.cabal` and `test/Main.hs`; keep source/test modules warning-free.
15. Review for overclaims: docs must not say the project is self-booted, has a stable `.mlfp` ABI, has separate compilation, has a linker/package manager, or can compile the compiler in `.mlfp`.

### Verification
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program fixture package migration"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program CLI package migration"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program shared runtime-success parity surface"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program eMLF surface parity matrix"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program eMLF boundary matrix"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`
- If backend fixture coverage changes: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.LLVM shared ProgramSpec first-order parity"'`
- `git diff --check`
- Before approval for this behavior-changing/docs closeout round: `cabal build all && cabal test`

Manual checks:

- Confirm every fixture corpus claim is backed by tests that enter through package mode or is explicitly syntax-only.
- Confirm file inputs are described as trivial package inputs, not the durable `.mlfp` program model.
- Confirm the readiness ledger classifies remaining gaps by source checking, interpreter/runtime, backend/native, object code, package build mode, and compiler-in-`.mlfp` implementation.
- Confirm the next-family recommendation is explicit and does not claim full self-boot completion.
- Confirm no package manager, remote dependency system, stable ABI, linker, or compiler implementation was added.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this file. They are the machine authority for lineage and worker scheduling.
