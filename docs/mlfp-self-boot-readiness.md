# `.mlfp` Self-Boot Readiness Ledger

This ledger records what the package-substrate and compiler frontend seed
roadmaps prove and what remains before self-boot can advance. It is evidence
for the current repository only; it is not a self-hosting claim.

## Current Evidence

| Layer | Current evidence | Remaining gap |
|-------|------------------|---------------|
| Source checking | `.mlfp` modules, imports, exports, ADTs, cases, typeclasses, deriving, higher-kinded parameters, opaque builtins, and explicit Prelude imports are checked through the program layer. The compiler frontend seed fixture at `test/programs/compiler-seed/frontend-contract/` now includes package-source modules for bounded symbolic input, source span labels, token stream, lexer diagnostic, lexer result, and lexer evidence. | The source checker is an implementation in Haskell, not a checked `.mlfp` compiler component. The lexer seed does not lex raw source text yet; parser, checker, optimizer, backend, package manager, linker, and driver modules are still absent. |
| Interpreter/runtime | `run-program` checks local package inputs, executes pure mains, and executes checked `main : IO Unit` through the reserved IO primitive boundary. `ProgramCompilerSeedSpec` proves the compiler frontend seed fixture runs through `runLocatedProgramPackageOutput` and the public CLI `run-program` path, including asserted positive tokenization and negative diagnostic evidence. | Runtime effects are intentionally narrow; there is no broad filesystem/process/runtime library for a compiler written in `.mlfp`. |
| Backend/native | `emit-backend` and `emit-native` use package-mode inputs and lower the supported checked subset through the private backend IR and LLVM path. | Backend lowerability is partial. Unsupported higher-order, polymorphic, IO, closure, and data shapes must remain explicit fail-closed cases until compiler workloads define the needed subset. |
| Object code | LLVM assembly/object-code smoke and native-run tests cover selected lowerable rows. | There is no stable object format contract, linker model, separate object build, or `.mlfp` ABI guarantee. |
| Package build mode | Local package roots and ordered search paths discover `.mlfp` files deterministically; package/interface/build-graph policy tests cover source metadata and interface metadata invalidation. | The package build graph is an in-repo policy and test surface, not a package manager, remote dependency solver, persisted interface format, or separate compilation pipeline. |
| Compiler-in-`.mlfp` implementation | A minimal compiler frontend seed contract and bounded symbolic-input lexer seed now exist as ordinary `.mlfp` package source under `test/programs/compiler-seed/frontend-contract/`. | The fixture is a runnable frontend seed only. It does not implement a source-text lexer, parser, checker, optimizer, backend, package manager, linker, or driver for this compiler in `.mlfp`. |
| Primitives | The primitive inventory centralizes builtin type and operation metadata for the current checker/runtime/backend. | Compiler workloads need a reviewed primitive budget for text/bytes, files, process exit, diagnostics, collections, and backend artifact generation. |
| Standard library | The built-in Prelude supplies `Nat`, `Option`, `List`, `Eq`, `Unit`, opaque `IO`, minimal `Monad IO`, `pure`, `bind`, `putStrLn`, `and`, and `id`. The lexer seed deliberately uses seed-owned monomorphic symbolic input and token-stream ADTs instead of pretending a general source-text or collection library exists. | A compiler needs a larger source library for strings/characters or bytes, maps/sets, arrays/lists with practical operations, error accumulation, parser combinators or lexer support, and IO helpers. |
| Parser/lexer | The Haskell parser accepts the current `.mlfp`, eMLF, and xMLF syntax and rejects retired compatibility spellings. The `.mlfp` lexer seed tokenizes one bounded symbolic `def main = true` input and returns an unknown-symbol diagnostic for a negative symbolic input. | There is no `.mlfp` source-text lexer/parser implementation, no character or byte stream abstraction, and no parser bootstrap corpus beyond the package-mode lexer seed. |
| Diagnostics | Located package parsing and checking preserve file paths/spans for common package, import, and visibility failures. The lexer seed has a seed-owned diagnostic ADT carrying a symbolic source span for the negative input. | Compiler-grade diagnostics need real source ranges, recovery strategy, structured error payloads, and golden diagnostics for package builds. |
| Fixture evidence | Static fixture files cover trivial file-as-package inputs, multi-file package roots, ordered search paths, runtime parity rows, backend emission over package mode, the compiler frontend seed contract fixture, and the bounded lexer evidence fixture. | Fixtures prove package-mode source, checking, and interpreter entrypoint evidence only. They do not prove source-text lexing, parsing, separate compilation, stable ABI/linking, compiler workload lowerability, native execution, or self-hosting. |

## What Current Evidence Proves

- `.mlfp` file inputs are now trivial local package source units, not a second
  durable program model.
- Local package roots and ordered search paths are the first-class package-mode
  entrypoints for checking, running, and backend emission.
- Package discovery, interface metadata, and build-graph/cache policy have
  deterministic private owners.
- The existing fixture corpus can be checked and run through package-mode
  entrypoints, and static package fixtures exercise root and search-path
  discovery outside temp-directory tests.
- The compiler frontend seed contract fixture at
  `test/programs/compiler-seed/frontend-contract/` is an ordinary local package
  root, and `ProgramCompilerSeedSpec` asserts discovery, graph order, checking,
  interpreter output, and CLI `run-program` output for that root.
- The same fixture now contains `.mlfp` source-position/span, token, diagnostic,
  input-symbol, lexer-result, and lexer-evidence ADTs. Its `Main` prints
  `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`
  through `run-program`, proving both a positive token path and a negative
  diagnostic path through the interpreter.

## What Current Evidence Does Not Prove

- The repository is not self-hosting.
- No source-text lexer, parser, checker, optimizer, backend, package manager,
  linker, or driver implementation is written in `.mlfp`.
- There is no package manager, remote dependency system, stable `.mlfp` ABI,
  linker, persisted interface file format, or separate compilation mode.
- Backend/native support remains a selected subset, not a complete compiler
  workload guarantee.

## Next-Stage Recommendation

The next stage should stay inside the compiler-in-`.mlfp` prerequisites lane,
not a self-boot claim. With the package-mode lexer seed in place, recommended
next directions are:

- define the minimal parser seed and AST/result contract over the seed token
  stream;
- inventory the stdlib and primitive gaps needed by that seed;
- classify which compiler source modules must run only in the interpreter and
  which must be backend-lowerable;
- extend backend lowerability and native-driver policy only for shapes used by
  the selected compiler slice;
- decide package build artifact policy for compiler sources before adding any
  persisted interface or object artifact format.
