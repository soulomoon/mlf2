# `.mlfp` Self-Boot Readiness Ledger

This ledger records what the package-substrate and compiler frontend seed
roadmaps prove and what remains before self-boot can advance. It is evidence
for the current repository only; it is not a self-hosting claim.

## Current Evidence

| Layer | Current evidence | Remaining gap |
|-------|------------------|---------------|
| Source checking | `.mlfp` modules, imports, exports, ADTs, cases, typeclasses, deriving, higher-kinded parameters, opaque builtins, and explicit Prelude imports are checked through the program layer. The compiler frontend seed fixture at `test/programs/compiler-seed/frontend-contract/` now includes package-source modules for bounded symbolic input, source span labels, token stream, lexer diagnostic, lexer result/evidence, AST, parser diagnostic, parser result, and parser evidence. | The source checker is an implementation in Haskell, not a checked `.mlfp` compiler component. The lexer/parser seed does not consume raw source text yet; checker, optimizer, backend, package manager, linker, and driver modules are still absent. |
| Interpreter/runtime | `run-program` checks local package inputs, executes pure mains, and executes checked `main : IO Unit` through the reserved IO primitive boundary. `ProgramCompilerSeedSpec` proves the compiler frontend seed fixture runs through `runLocatedProgramPackageOutput` and the public CLI `run-program` path, including asserted positive tokenization, negative lexer diagnostic, accepted parser AST, and rejected parser diagnostic evidence. | Runtime effects are intentionally narrow; there is no broad filesystem/process/runtime library for a compiler written in `.mlfp`. |
| Backend/native | `emit-backend` and `emit-native` use package-mode inputs and lower the supported checked subset through the private backend IR and LLVM path. The compiler frontend seed package now emits raw backend LLVM and native-entrypoint LLVM without changing the seed contract. | Backend lowerability is partial. Unsupported higher-order, polymorphic, IO, closure, and data shapes must remain explicit fail-closed cases until compiler workloads define the needed subset. The current seed's success is evidence for this bounded fixture only. |
| Object code | LLVM assembly/object-code smoke and native-run tests cover selected lowerable rows. The compiler frontend seed's emitted native LLVM validates through object-code generation and executes with the same evidence output as `run-program`. | There is no stable object format contract, linker model, separate object build, or `.mlfp` ABI guarantee. |
| Package build mode | Local package roots and ordered search paths discover `.mlfp` files deterministically; package/interface/build-graph policy tests cover source metadata and interface metadata invalidation. | The package build graph is an in-repo policy and test surface, not a package manager, remote dependency solver, persisted interface format, or separate compilation pipeline. |
| Compiler-in-`.mlfp` implementation | A minimal compiler frontend seed contract, bounded symbolic-input lexer seed, and bounded parser/AST seed now exist as ordinary `.mlfp` package source under `test/programs/compiler-seed/frontend-contract/`. | The fixture is a runnable frontend seed only. It does not implement a source-text lexer/parser, checker, optimizer, backend, package manager, linker, or driver for this compiler in `.mlfp`. |
| Primitives | The primitive inventory centralizes builtin type and operation metadata for the current checker/runtime/backend. | The seed-driven budget below identifies which primitive gaps block a larger compiler component and which remain deferred. |
| Standard library | The built-in Prelude supplies `Nat`, `Option`, `List`, `Eq`, `Unit`, opaque `IO`, minimal `Monad IO`, `pure`, `bind`, `putStrLn`, `and`, and `id`. The lexer seed deliberately uses seed-owned monomorphic symbolic input and token-stream ADTs instead of pretending a general source-text or collection library exists. | The seed-driven budget below identifies which stdlib gaps block a larger compiler component and which remain deferred. |
| Parser/lexer | The Haskell parser accepts the current `.mlfp`, eMLF, and xMLF syntax and rejects retired compatibility spellings. The `.mlfp` lexer seed tokenizes one bounded symbolic `def main = true` input and returns an unknown-symbol diagnostic for a negative symbolic input. The `.mlfp` parser seed consumes the seed token stream, returns a definition AST for the accepted token stream, and returns an expected-equals diagnostic for a malformed token stream. | There is no `.mlfp` source-text lexer/parser implementation, no character or byte stream abstraction, and no parser bootstrap corpus beyond the package-mode lexer/parser seed. |
| Diagnostics | Located package parsing and checking preserve file paths/spans for common package, import, and visibility failures. The lexer and parser seeds have seed-owned diagnostic ADTs carrying symbolic source spans for rejected inputs. | Compiler-grade diagnostics need real source ranges, recovery strategy, structured error payloads, and golden diagnostics for package builds. |
| Fixture evidence | Static fixture files cover trivial file-as-package inputs, multi-file package roots, ordered search paths, runtime parity rows, backend emission over package mode, the compiler frontend seed contract fixture, the bounded lexer evidence fixture, the bounded parser/AST evidence fixture, and bounded compiler-seed backend/native execution for the current package entrypoint. | Fixtures do not prove source-text lexing/parsing, separate compilation, stable ABI/linking, arbitrary compiler workload lowerability, or self-hosting. |

## Seed-Driven Primitive And Standard-Library Gap Budget

This budget is tied to the merged compiler frontend seed at
`test/programs/compiler-seed/frontend-contract/` and the asserted
interpreter/CLI evidence:

- `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`
- `parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true`

| Category | Seed evidence | Current support and layer owner | Classification | Next action |
|----------|---------------|----------------------------------|----------------|-------------|
| Text, characters, or bytes | `SeedSource` models source as closed `SeedInputSymbol` values and symbolic `SourceSpan` labels. The lexer never inspects raw `String`, `Char`, bytes, substrings, or offsets. | Source checking knows `String` literals and the Prelude exposes string-valued IO, but there is no source-level character, byte, substring, or cursor API for `.mlfp` code. | `needed-before-larger-compiler` | Add a narrow text/byte input representation with span tracking before replacing the symbolic lexer with a real source-text lexer. Do not add a broad text library without a selected lexer/parser slice. |
| Collection operations | `SeedInput` and `SeedTokenStream` are seed-owned monomorphic lists, and `SeedLexer`/`SeedParser` recurse by hand over those streams. | Prelude exposes `List` constructors and derived `Eq`, but not practical `List` operations such as append, reverse, fold, map/filter instances for `List`, indexed access, or non-empty streams. | `needed-before-larger-compiler` | Add only the collection operations required by the next compiler component, likely token/source streams and diagnostic lists, with focused seed evidence. |
| Maps and sets | The seed accepts exactly one identifier and one literal, so no symbol table, keyword table, import environment, or duplicate detection appears in `.mlfp` seed code. | Maps/sets are Haskell implementation details in the current package/checker/backend owners; no `.mlfp` `Map` or `Set` stdlib surface exists. | `deferred` | Reclassify when a `.mlfp` resolver, checker, or multi-keyword lexer/parser slice needs environments. Do not add generic maps/sets from this seed alone. |
| Parser helpers | `SeedParser` is a direct state machine over `SeedTokenStream` with one accepted definition form and one missing-equals diagnostic. | There is no parser-combinator library or reusable parser state abstraction in Prelude; the Haskell frontend parser remains outside the `.mlfp` seed. | `needed-before-larger-compiler` | Keep the current direct parser while the grammar is tiny. Add a narrow parser state/result helper only when a selected parser expansion proves repeated cursor/expectation logic. |
| Error accumulation | `LexerResult` and `ParserResult` carry exactly one diagnostic; the evidence checks one lexer rejection and one parser rejection. | `.mlfp` has ADTs and `List`, but no standard `Result`/validation abstraction or diagnostic accumulation helper. | `needed-before-larger-compiler` | Introduce structured result and diagnostic-list helpers before parser recovery, multi-error checking, or package-build diagnostics are selected. Keep single-error results for the current seed. |
| IO helpers | The seed uses only `putStrLn` plus `bind` to print evidence. It does not read compiler source files from `.mlfp`, write artifacts, inspect arguments, or exit with process status. | `run-program` interprets `putStrLn`, `putStr`, `pure`, `bind`, `map`, and `ap`; file IO, `getLine`, `IORef`, `exitWith`, and `getArgs` fail closed in `run-program` with native-execution diagnostics. The backend/native layer has selected IO wrappers, but milestone 5 owns native/backend classification. | `deferred` | Keep compiler-library work interpreter-first and pure where possible. A driver, filesystem workflow, process-exit policy, or native runtime expansion belongs to a later layer-classification or driver roadmap, not this budget round. |
| Diagnostics | Lexer and parser diagnostics are seed-owned ADTs with symbolic spans, and the negative paths prove the failing symbolic span is inspectable. | Package parsing/checking has located diagnostics in Haskell. The `.mlfp` seed has no real source ranges, diagnostic severity, notes, recovery, golden diagnostics, or package-build diagnostic aggregation. | `needed-before-larger-compiler` | Preserve symbolic diagnostics for the bounded seed, then add real range payloads and golden diagnostic fixtures when source-text lexing or parser recovery is selected. |

Broad package-manager, persisted-interface, ABI, linker, separate-compilation,
general FFI, backend-native redesign, or full compiler-driver work remains
`roadmap-update-required`; none is authorized by this gap budget.

## Compiler Frontend Seed Layer Classification

The classification below records current support for the seed package at
`test/programs/compiler-seed/frontend-contract/`. The package entrypoint emits:

- `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`
- `parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true`

| Seed unit | Source checking | Interpreter/runtime | Backend/native | Object code | Package build mode |
|-----------|-----------------|---------------------|----------------|-------------|--------------------|
| `SeedContract.mlfp` | `source-checked`: package module with exported seed contract ADT and predicate. | `interpreter-loaded`: present in the checked package graph; the current printed evidence no longer evaluates a `SeedContract` value. | `not-entrypoint-reachable`: no raw/native LLVM symbol is emitted for this unused contract marker in the current package entrypoint. | `not-object-reachable`: no standalone or reachable object-code claim for this unused contract marker. | `package-module`: ordinary local package source unit. |
| `SeedSource.mlfp` | `source-checked`: symbolic input, span, position, identifier, and literal ADTs check in the package graph. | `interpreter-reachable`: lexer and parser evidence inspect spans/positions from this module. | `backend-lowerable`: symbolic ADTs and functions lower as part of the package. | `object-code-via-package`: covered through the package native object/executable path. | `package-module`: ordinary local package source unit. |
| `SeedToken.mlfp` | `source-checked`: token and token-stream ADTs check in the package graph. | `interpreter-reachable`: lexer produces token streams and parser consumes them. | `backend-lowerable`: token constructors and streams lower as part of the package. | `object-code-via-package`: covered through the package native object/executable path. | `package-module`: ordinary local package source unit. |
| `SeedDiagnostic.mlfp` | `source-checked`: lexer diagnostic ADTs check in the package graph. | `interpreter-reachable`: negative lexer evidence inspects diagnostic kind and span. | `backend-lowerable`: diagnostic constructors lower as part of the package. | `object-code-via-package`: covered through the package native object/executable path. | `package-module`: ordinary local package source unit. |
| `SeedLexer.mlfp` | `source-checked`: lexer result/evidence ADTs and direct lexer functions check in the package graph. | `interpreter-runnable`: `Main` renders asserted positive and negative lexer evidence. | `backend-lowerable`: raw LLVM includes `SeedLexer__lexSeedInput`; native LLVM includes the same reachable logic through `Main__main`. | `object-code-via-package`: covered through the package native object/executable path. | `package-module`: ordinary local package source unit. |
| `SeedAst.mlfp` | `source-checked`: AST expression and definition ADTs check in the package graph. | `interpreter-reachable`: parser returns and classifies AST values from this module. | `backend-lowerable`: AST constructors lower as part of the package. | `object-code-via-package`: covered through the package native object/executable path. | `package-module`: ordinary local package source unit. |
| `SeedParser.mlfp` | `source-checked`: parser diagnostic/result/evidence ADTs and parser functions check in the package graph. | `interpreter-runnable`: `Main` renders asserted accepted-AST and rejected-diagnostic parser evidence. | `backend-lowerable`: raw LLVM includes `SeedParser__parseSeedTokens`; native LLVM includes the same reachable logic through `Main__main`. | `object-code-via-package`: covered through the package native object/executable path. | `package-module`: ordinary local package source unit. |
| `Main.mlfp` | `source-checked`: package `main : IO Unit` checks with explicit Prelude imports. | `interpreter-runnable`: `run-program` prints both seed evidence lines. | `native-runnable`: `emit-backend` emits `Main__main`; `emit-native` emits a C ABI `main` plus IO wrapper calls for `__io_bind` and `__io_putStrLn`. | `object-code-smoke-and-run`: emitted native LLVM validates through object-code generation and linked native execution for this bounded seed. | `package-entrypoint`: package root entrypoint discovered from local package mode. |
| Package root | `source-checked`: `check-program test/programs/compiler-seed/frontend-contract` returns `OK`. | `interpreter-runnable`: `run-program test/programs/compiler-seed/frontend-contract` prints the asserted lexer and parser evidence. | `backend-and-native-lowerable`: `emit-backend` and `emit-native` both succeed for the current seed package. | `object-code-smoke-and-run`: object generation and native execution preserve the same evidence output; this is not a stable object ABI or separate-compilation claim. | `local-package-root`: directory package discovery, graph ordering, and CLI package entrypoints cover the fixture. |

No compiler-seed row currently exercises a backend/native unsupported case:
the bounded seed lowers because its ADTs, direct seed functions, `String`
evidence, and `IO Unit` `putStrLn`/`bind` path are inside the existing native
subset.
Unsupported future compiler shapes still fail closed under the backend-native
contract in `docs/backend-native-pipeline.md`: residual runtime polymorphism,
unsupported result shapes, symbol collisions with native-owned runtime names,
and unlowerable backend shapes must fail before emission or native execution.
This classification does not add a second backend IR, native runtime redesign,
broad FFI lane, package manager, ABI/linker contract, or separate compilation.

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
  input-symbol, lexer-result, and lexer-evidence ADTs. It also contains AST,
  parser-result, parser-diagnostic, and parser-evidence ADTs. Its `Main` prints
  `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`
  and
  `parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true`
  through `run-program`, proving positive and negative lexer/parser paths
  through the interpreter.
- The seed-driven primitive and standard-library budget above classifies the
  concrete gaps exposed by that runnable seed without adding primitives,
  Prelude helpers, parser combinators, backend/native support, package-manager
  behavior, ABI/linker support, or separate compilation.
- The current seed package is also lowerable by the existing backend/native
  subset: raw backend LLVM includes the seed lexer/parser and `Main__main`,
  native LLVM includes a C ABI `main` plus IO wrappers, and the linked native
  executable prints the same two evidence lines as `run-program`.

## What Current Evidence Does Not Prove

- The repository is not self-hosting.
- No source-text lexer/parser, checker, optimizer, backend, package manager,
  linker, or driver implementation is written in `.mlfp`.
- There is no package manager, remote dependency system, stable `.mlfp` ABI,
  linker, persisted interface file format, or separate compilation mode.
- Backend/native support remains a selected subset. The current bounded seed is
  lowerable and native-runnable, but that is not a complete compiler workload
  guarantee.

## Seed Fixture Handoff

The compiler frontend seed family closes over the existing package fixture at
`test/programs/compiler-seed/frontend-contract/`. The fixture is intentionally
small and stable:

- `SeedContract.mlfp` records the interpreter-runnable package seed marker.
- `SeedSource.mlfp`, `SeedToken.mlfp`, `SeedDiagnostic.mlfp`, and
  `SeedLexer.mlfp` own bounded symbolic source input, spans, tokens, lexer
  diagnostics/results, and rendered positive/negative lexer evidence.
- `SeedAst.mlfp` and `SeedParser.mlfp` own the tiny definition AST, parser
  diagnostics/results, one accepted parse, and one rejected missing-equals
  diagnostic.
- `Main.mlfp` prints the two evidence lines through `putStrLn`/`bind`.

`ProgramCompilerSeedSpec` is the executable evidence owner for the fixture. It
asserts package discovery, graph order, source paths, package checking,
interpreter output, public CLI `check-program`/`run-program`, backend/native
LLVM emission, LLVM assembly/object validation, and linked native execution for
the bounded package entrypoint. No additional milestone-6 fixture test is
needed unless a future round changes the fixture location, module list, or
entrypoint evidence.

## Next-Stage Recommendation

The next stage should stay inside the compiler-in-`.mlfp` prerequisites lane,
not a self-boot claim. With the package-mode lexer/parser seed, seed-driven gap
budget, layer classification, and fixture handoff in place, the next roadmap
should target a larger frontend component rather than a backend, driver, or
package-manager slice.

- Recommended next family: source-text lexer/parser expansion in `.mlfp`.
  Start by replacing the symbolic `SeedInputSymbol` model with a narrow
  text-or-byte input and source-range representation, then expand the current
  parser one grammar feature at a time with focused accepted/rejected evidence.
- Blocking prerequisites for that family: practical stream/list operations,
  narrow parser state/result helpers, diagnostic-list or error-accumulation
  support, and real source-range diagnostic payloads. These should be added
  only when the selected frontend slice proves the need.
- Deferred until demanded by the selected slice: maps/sets for environments,
  file/process IO driver helpers, stable build artifacts, persisted
  interfaces, object ABI/linker policy, and separate compilation.
- Preserve backend lowerability and native-driver policy only for shapes used
  by the selected compiler slice.
- Decide package build artifact policy for compiler sources before adding any
  persisted interface or object artifact format.
