# Review: round-256

### Checks Run

- `git diff --check` - passed before and after the full verification gate.
- `jq empty orchestrator/state.json orchestrator/rounds/round-256/selection-record.json orchestrator/rounds/round-256/round-plan-record.json orchestrator/roadmaps/2026-05-17-01-mlfp-compiler-frontend-seed-roadmap/rev-001/roadmap-view.json` - passed.
- `jq -e '.anchors["milestone-5-status"] and .anchors["milestone-5-completion"] and (.milestones[] | select(.milestone_id == "milestone-5" and .status == "pending"))' orchestrator/roadmaps/2026-05-17-01-mlfp-compiler-frontend-seed-roadmap/rev-001/roadmap-view.json` - passed; closeout anchors resolve and milestone-5 is pending before closeout.
- `test -z "$(git diff --name-only -- src src-public app mlf2.cabal orchestrator/roadmaps)"` - passed; the payload does not change production source, public facade, Cabal stanzas, or roadmap files.
- `cabal run mlf2 -- check-program test/programs/compiler-seed/frontend-contract` - passed with `OK`.
- `cabal run mlf2 -- run-program test/programs/compiler-seed/frontend-contract` - passed and emitted the lexer/parser evidence lines:
  - `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`
  - `parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true`
- `cabal run mlf2 -- emit-backend test/programs/compiler-seed/frontend-contract > /tmp/round256-review-emit-backend.ll && wc -l /tmp/round256-review-emit-backend.ll && rg -n 'define .*@"Main__main"|define .*@"SeedLexer__lexSeedInput"|define .*@"SeedParser__parseSeedTokens"|__io_putStrLn\.wrapper|__io_bind\.wrapper|SeedContract' /tmp/round256-review-emit-backend.ll | head -30` - passed; output was 3008 LLVM lines and included `SeedLexer__lexSeedInput`, `SeedParser__parseSeedTokens`, `Main__main`, `__io_bind.wrapper`, and `__io_putStrLn.wrapper`.
- `rg -n 'Seed(Source|Token|Diagnostic|Ast)|define .*@"Seed' /tmp/round256-review-emit-backend.ll | head -80` - passed; the backend output contains reachable seed source/lexer/parser symbols and calls.
- `cabal run mlf2 -- emit-native test/programs/compiler-seed/frontend-contract > /tmp/round256-review-emit-native.ll && wc -l /tmp/round256-review-emit-native.ll && rg -n 'define i32 @"main"|define ptr @"Main__main"|__io_putStrLn\.wrapper|__io_bind\.wrapper|__mlfp_native_render|SeedContract' /tmp/round256-review-emit-native.ll | head -40` - passed; output was 3033 LLVM lines and included C ABI `main`, `Main__main`, `__io_bind.wrapper`, and `__io_putStrLn.wrapper`.
- `rg -n 'Seed(Source|Token|Diagnostic|Ast)|define .*@"Seed' /tmp/round256-review-emit-native.ll | head -80` - passed; the native output contains reachable seed source/lexer/parser symbols and calls.
- `rg -n 'validateLLVMObjectCode|runLLVMNativeExecutable|NativeRunResult|compiler-seed emits backend' test/ProgramCompilerSeedSpec.hs` - passed; the focused spec validates native object generation and linked native execution.
- `cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'` - passed; `3 examples, 0 failures`.
- `cabal build all` - passed.
- `cabal test` - passed; `2563 examples, 0 failures`.
- `./scripts/thesis-conformance-gate.sh` - passed; obligations, claims, and thesis conformance anchors are green.

### Plan Compliance

- Seed evidence: met. The reviewed docs classify every compiler-seed module and the package root across source checking, interpreter/runtime, backend/native, object code, and package build mode.
- Package entrypoints: met. The package root passes `check-program`, `run-program`, `emit-backend`, and `emit-native`.
- Native/object evidence: met. `ProgramCompilerSeedSpec` validates LLVM assembly, native object generation, and linked native execution with the same evidence output as `run-program`.
- Backend/native unsupported boundary: met. The current seed fits the existing supported subset; unsupported future compiler shapes remain documented as fail-closed in `docs/backend-native-pipeline.md`.
- Scope control: met. The payload changes only `docs/mlfp-self-boot-readiness.md` and `test/ProgramCompilerSeedSpec.hs`, plus controller-owned `orchestrator/state.json` and round artifacts. There are no production source, public facade, Cabal, roadmap, seed grammar, parser contract, primitive/stdlib surface, backend IR, native runtime, package manager, ABI/linker, or separate-compilation changes.

### Decision

**APPROVED**

Milestone-5 can close as status-only. The implementation satisfies the milestone completion signal, and the closeout does not change future coordination, sequencing, verification meaning, or roadmap semantics.

### Evidence

The docs now provide the durable layer-classification artifact requested by the plan. The focused compiler-seed test covers the risky behavior path by asserting reachable backend/native symbols, assembly validation, object-code validation, and linked native execution. Full Cabal and thesis gates passed after the focused seed checks. The local verification run rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` with the worktree path; that generated path-only churn was restored before final review status.
