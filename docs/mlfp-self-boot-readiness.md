# `.mlfp` Self-Boot Readiness Ledger

This ledger records what the package-substrate roadmap proves and what remains
before a separate compiler-in-`.mlfp` roadmap can start. It is evidence for the
current repository only; it is not a self-hosting claim.

## Current Evidence

| Layer | Current evidence | Remaining gap |
|-------|------------------|---------------|
| Source checking | `.mlfp` modules, imports, exports, ADTs, cases, typeclasses, deriving, higher-kinded parameters, opaque builtins, and explicit Prelude imports are checked through the program layer. | The source checker is an implementation in Haskell, not a checked `.mlfp` compiler component. Compiler-oriented source modules do not exist yet. |
| Interpreter/runtime | `run-program` checks local package inputs, executes pure mains, and executes checked `main : IO Unit` through the reserved IO primitive boundary. | Runtime effects are intentionally narrow; there is no broad filesystem/process/runtime library for a compiler written in `.mlfp`. |
| Backend/native | `emit-backend` and `emit-native` use package-mode inputs and lower the supported checked subset through the private backend IR and LLVM path. | Backend lowerability is partial. Unsupported higher-order, polymorphic, IO, closure, and data shapes must remain explicit fail-closed cases until compiler workloads define the needed subset. |
| Object code | LLVM assembly/object-code smoke and native-run tests cover selected lowerable rows. | There is no stable object format contract, linker model, separate object build, or `.mlfp` ABI guarantee. |
| Package build mode | Local package roots and ordered search paths discover `.mlfp` files deterministically; package/interface/build-graph policy tests cover source metadata and interface metadata invalidation. | The package build graph is an in-repo policy and test surface, not a package manager, remote dependency solver, persisted interface format, or separate compilation pipeline. |
| Compiler-in-`.mlfp` implementation | None. No lexer, parser, checker, optimizer, backend, or driver for this compiler is implemented in `.mlfp`. | A future roadmap must create compiler source modules in `.mlfp` and choose a staged bootstrap strategy before any self-hosting milestone can be claimed. |
| Primitives | The primitive inventory centralizes builtin type and operation metadata for the current checker/runtime/backend. | Compiler workloads need a reviewed primitive budget for text/bytes, files, process exit, diagnostics, collections, and backend artifact generation. |
| Standard library | The built-in Prelude supplies `Nat`, `Option`, `List`, `Eq`, `Unit`, opaque `IO`, minimal `Monad IO`, `pure`, `bind`, `putStrLn`, `and`, and `id`. | A compiler needs a larger source library for strings/characters or bytes, maps/sets, arrays/lists with practical operations, error accumulation, parser combinators or lexer support, and IO helpers. |
| Parser/lexer | The Haskell parser accepts the current `.mlfp`, eMLF, and xMLF syntax and rejects retired compatibility spellings. | There is no `.mlfp` lexer/parser implementation written in `.mlfp`, no token stream abstraction in the source language, and no parser bootstrap corpus for compiler sources. |
| Diagnostics | Located package parsing and checking preserve file paths/spans for common package, import, and visibility failures. | Compiler-grade diagnostics need richer source ranges, recovery strategy, structured error payloads, and golden diagnostics for package builds. |
| Fixture evidence | Static fixture files cover trivial file-as-package inputs, multi-file package roots, ordered search paths, runtime parity rows, and backend emission over package mode. | Fixtures prove package-substrate readiness only. They do not prove separate compilation, stable ABI/linking, compiler workload lowerability, or self-hosting. |

## What This Family Proves

- `.mlfp` file inputs are now trivial local package source units, not a second
  durable program model.
- Local package roots and ordered search paths are the first-class package-mode
  entrypoints for checking, running, and backend emission.
- Package discovery, interface metadata, and build-graph/cache policy have
  deterministic private owners.
- The existing fixture corpus can be checked and run through package-mode
  entrypoints, and static package fixtures exercise root and search-path
  discovery outside temp-directory tests.

## What This Family Does Not Prove

- The repository is not self-hosting.
- No compiler implementation is written in `.mlfp`.
- There is no package manager, remote dependency system, stable `.mlfp` ABI,
  linker, persisted interface file format, or separate compilation mode.
- Backend/native support remains a selected subset, not a complete compiler
  workload guarantee.

## Next-Family Recommendation

The next roadmap should be a compiler-in-`.mlfp` prerequisites roadmap, not a
self-boot claim. Start by selecting a bootstrap slice that can be checked,
interpreted, and, where required, lowered natively with current package mode.
Recommended first directions:

- define the minimal lexer/parser seed and token/source-span data model in
  `.mlfp`;
- inventory the stdlib and primitive gaps needed by that seed;
- classify which compiler source modules must run only in the interpreter and
  which must be backend-lowerable;
- extend backend lowerability and native-driver policy only for shapes used by
  the selected compiler slice;
- decide package build artifact policy for compiler sources before adding any
  persisted interface or object artifact format.
