# `.mlfp` Self-Boot Readiness Ledger

This ledger records what the package-substrate and compiler frontend seed
roadmaps prove and what remains before self-boot can advance. It is evidence
for the current repository only; it is not a self-hosting claim. The durable
end-to-end roadmap order is decided in
`docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`.

## Current Evidence

| Layer | Current evidence | Remaining gap |
|-------|------------------|---------------|
| Source checking | `.mlfp` modules, imports, exports, ADTs, cases, typeclasses, deriving, higher-kinded parameters, opaque builtins, and explicit Prelude imports are checked through the program layer. The compiler frontend seed fixture at `test/programs/compiler-seed/frontend-contract/` now includes package-source modules for bounded symbolic input, source span labels, token stream, lexer diagnostic, lexer result/evidence, AST, parser diagnostic, parser result, and parser evidence. | The source checker is an implementation in Haskell, not a checked `.mlfp` compiler component. The lexer/parser seed does not consume raw source text yet; checker, optimizer, backend, package manager, linker, and driver modules are still absent. |
| Interpreter/runtime | `run-program` checks local package inputs, executes pure mains, includes the first broad `String` operation `stringLength : String -> Int` with Unicode scalar counting, the first `String` classification operation `stringIsEmpty : String -> Bool`, the first single-character `String`/`Char` search operation `stringContainsChar : String -> Char -> Bool`, the first substring `String` search operation `stringContains : String -> String -> Bool`, and the first prefix `String` search operation `stringStartsWith : String -> String -> Bool`, and executes checked `main : IO Unit` through the reserved IO primitive boundary. `ProgramCompilerSeedSpec` proves the compiler frontend seed fixture runs through `runLocatedProgramPackageOutput` and the public CLI `run-program` path, including asserted positive tokenization, negative lexer diagnostic, accepted parser AST, and rejected parser diagnostic evidence. | Runtime effects are intentionally narrow; there is no broad filesystem/process/runtime library for a compiler written in `.mlfp`. |
| Backend/native | `emit-backend` and `emit-native` use package-mode inputs and lower the supported checked subset through the private backend IR and LLVM path. The current text-substrate evidence includes Unicode scalar `Char` and `String` literal tracers plus the first native-capable `stringLength` operation tracer, `stringIsEmpty` classification tracer, `stringContainsChar` scalar search tracer, `stringContains` substring search tracer, and `stringStartsWith` prefix search tracer through raw LLVM, object-code validation, and linked native execution. The compiler frontend seed package now emits raw backend LLVM and native-entrypoint LLVM without changing the seed contract. | Backend lowerability is partial. First self-boot now requires **Self-Boot Total Native Coverage**: every source-checked `.mlfp` program and every `.mlfp` language feature must be supported by backend/native emission. Trusted-substrate APIs callable from `.mlfp`, including filesystem, subprocess, hashing, broad string, primitive, and runtime helper operations, must also work in native execution. Native execution must satisfy the **Self-Boot Managed GC Requirement** using a **Precise Self-Boot GC Root Model** and **Non-Moving First Self-Boot GC** for heap-allocated `.mlfp` values. The runtime/GC/FFI substrate may remain a **Shared Rust Trusted Substrate** used by both the Haskell compiler implementation and the future `.mlfp` compiler implementation through a **Shared Stable Substrate ABI Path**. Deterministic native artifact contracts must include a **Target-Scoped Stable `.mlfp` ABI** with **Stable Public Heap Layout Boundary**, **General Public `.mlfp` FFI** through declared stable foreign ABI boundaries, **Stable FFI Value Shapes**, a **Stable FFI Ownership Contract**, a **Stable FFI Error Boundary**, **Stable FFI Export and Callback Wrappers**, a **Manifest-Owned FFI Link Graph**, **Explicit FFI Effects**, and an **Explicit FFI Runtime Context**; raw Rust ABI, proof-path private Rust calls, raw implementation-value crossing, hidden exported heap layout, raw closure export, raw GC pointer FFI, conservative-scanning correctness, unversioned moving-GC pointer instability, ambient linker inputs, implicit target assumptions, implicit foreign purity, hidden public-ABI runtime initialization, and implicit unwinding are not stable public `.mlfp` FFI targets. Library-only packages must still emit native-capable module artifacts, while executable linking requires a manifest entrypoint. Existing post-check native unsupported cases are self-boot blockers; narrowing the checked `.mlfp` feature surface solely to avoid native implementation is not an acceptable self-boot shortcut. The current seed's success is evidence for this bounded fixture only. |
| Object code | LLVM assembly/object-code smoke and native-run tests cover selected lowerable rows. The compiler frontend seed's emitted native LLVM validates through object-code generation and executes with the same evidence output as `run-program`. | There is no stable object format contract, linker model, separate object build, target-scoped `.mlfp` ABI guarantee, shared Rust trusted-substrate contract, shared stable substrate ABI path, stable public heap-layout boundary, managed GC runtime contract, precise GC root model, non-moving first GC contract, general user-facing FFI, stable FFI value-shape/marshalling contract, stable ownership/lifetime contract, stable error/unwinding boundary, export/callback wrapper contract, manifest-owned FFI link graph, explicit FFI effect policy, or explicit public-ABI runtime context yet. First self-boot now requires closing that gap with a **Shared Rust Trusted Substrate**, **Shared Stable Substrate ABI Path**, **Target-Scoped Stable `.mlfp` ABI**, **Stable Public Heap Layout Boundary**, **Self-Boot Managed GC Requirement**, **Precise Self-Boot GC Root Model**, and **Non-Moving First Self-Boot GC** that are stable within an explicit ABI version, target triple, and substrate fingerprint, and includes **General Public `.mlfp` FFI** with **Stable FFI Value Shapes**, a **Stable FFI Ownership Contract**, a **Stable FFI Error Boundary**, **Stable FFI Export and Callback Wrappers**, a **Manifest-Owned FFI Link Graph**, **Explicit FFI Effects**, and an **Explicit FFI Runtime Context**, rather than treating ABI/linker/FFI policy as optional. |
| Package build mode | Local package roots and ordered search paths discover `.mlfp` files deterministically; package/interface/build-graph policy tests cover source metadata and interface metadata invalidation. | The package build graph is an in-repo policy and test surface, not a package manager, remote dependency solver, persisted interface format, or separate compilation pipeline. |
| Compiler-in-`.mlfp` implementation | A minimal compiler frontend seed contract, bounded symbolic-input lexer seed, and bounded parser/AST seed now exist as ordinary `.mlfp` package source under `test/programs/compiler-seed/frontend-contract/`. | The fixture is a runnable frontend seed only. It does not implement a source-text lexer/parser, checker, optimizer, backend, package manager, linker, or driver for this compiler in `.mlfp`. |
| Primitives | The primitive inventory centralizes builtin type and operation metadata for the current checker/runtime/backend. | The seed-driven budget below identifies which primitive gaps block a larger compiler component and which remain deferred. |
| Standard library | The built-in Prelude supplies `Nat`, `Option`, `List`, `Eq`, `Unit`, opaque `IO`, minimal `Monad IO`, `pure`, `bind`, `putStrLn`, `stringLength`, `stringIsEmpty`, `stringContainsChar`, `stringContains`, `stringStartsWith`, `and`, and `id`. The lexer seed deliberately uses seed-owned monomorphic symbolic input and token-stream ADTs instead of pretending a general source-text or collection library exists. | The seed-driven budget below identifies which stdlib gaps block a larger compiler component and which remain deferred. First self-boot additionally requires a **Prelude Package/Substrate Split**: high-level Prelude/library source should live in the **Shared Local `.mlfp` Package Set** consumed by both compiler implementations where feasible, while primitive/native operations, GC/runtime hooks, and low-level FFI shims remain declared trusted substrate. |
| Parser/lexer | The Haskell parser accepts the current `.mlfp`, eMLF, and xMLF syntax and rejects retired compatibility spellings. The `.mlfp` lexer seed tokenizes one bounded symbolic `def main = true` input and returns an unknown-symbol diagnostic for a negative symbolic input. The `.mlfp` parser seed consumes the seed token stream, returns a definition AST for the accepted token stream, and returns an expected-equals diagnostic for a malformed token stream. | There is no `.mlfp` source-text lexer/parser implementation, no character or byte stream abstraction, and no parser bootstrap corpus beyond the package-mode lexer/parser seed. |
| Diagnostics | Located package parsing and checking preserve file paths/spans for common package, import, and visibility failures. The lexer and parser seeds have seed-owned diagnostic ADTs carrying symbolic source spans for rejected inputs. | Compiler-grade diagnostics need real source ranges, recovery strategy, structured error payloads, and golden diagnostics for package builds. |
| Fixture evidence | Static fixture files cover trivial file-as-package inputs, multi-file package roots, ordered search paths, runtime parity rows, backend emission over package mode, the compiler frontend seed contract fixture, the bounded lexer evidence fixture, the bounded parser/AST evidence fixture, and bounded compiler-seed backend/native execution for the current package entrypoint. | Fixtures do not prove source-text lexing/parsing, separate compilation, stable ABI/linking, **Self-Boot Total Native Coverage**, or self-hosting. |

## Seed-Driven Primitive And Standard-Library Gap Budget

This budget is tied to the merged compiler frontend seed at
`test/programs/compiler-seed/frontend-contract/` and the asserted
interpreter/CLI evidence:

- `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`
- `parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true`

| Category | Seed evidence | Current support and layer owner | Classification | Next action |
|----------|---------------|----------------------------------|----------------|-------------|
| Text, characters, or strings | `SeedSource` models source as closed `SeedInputSymbol` values and symbolic `SourceSpan` labels. The lexer never inspects raw `String`, `Char`, substrings, or offsets. | Source checking knows `String` literals, the Prelude exposes string-valued IO, and the first source-level Unicode scalar `Char` literal tracer plus the next two-byte Unicode scalar `String` literal tracer check, run, emit LLVM, validate object code, and execute natively. The first broad `String` operation, `stringLength : String -> Int`, now counts Unicode scalar values through source checking, `run-program`, raw LLVM, object validation, and linked native execution. The first broad `String` classification operation, `stringIsEmpty : String -> Bool`, classifies `""` and the non-empty Unicode scalar string `"λ"` through the same layers. The first single-character `String`/`Char` search operation, `stringContainsChar : String -> Char -> Bool`, now classifies `stringContainsChar "aλb" 'λ'` as `true` and `stringContainsChar "ab" 'λ'` as `false` through the same layers. The first substring `String` search operation, `stringContains : String -> String -> Bool`, now classifies `stringContains "aλb" "λ"` as `true` and `stringContains "ab" "λ"` as `false` through the same layers. The first prefix `String` search operation, `stringStartsWith : String -> String -> Bool`, now classifies `stringStartsWith "λab" "λ"` as `true` and `stringStartsWith "aλb" "λ"` as `false` through the same layers. There is still no `String`/`List Char` conversion, formatting, slicing, broader classification predicate family, cursor API, or parser parity for `.mlfp` code. | `needed-before-larger-compiler` | Per `docs/adr/2026-05-18-native-broad-string-library-before-parser-parity.md`, continue adding a Prelude-level broad Unicode-scalar string/`Char` library with native slicing/classification before replacing the symbolic lexer with a real source-text parser. |
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
- There is no package manager, remote dependency system, shared local `.mlfp`
  package set, locked local package closure, checked package-lock validation,
  canonical package-lock format, exact locked package identity policy, first
  compiler-driver lock responsibility, Prelude package/substrate split, stable
  `.mlfp` ABI, shared Rust trusted-substrate contract, managed GC runtime
  contract, precise GC root model, non-moving first GC contract, stable public
  heap-layout boundary, shared stable substrate ABI path, shared substrate ABI
  conformance layer, platform substrate contract package, canonical substrate
  ABI declaration source and format, checked generated substrate binding
  validation, substrate ABI versioning policy, linker, persisted interface file
  format, or separate compilation mode.
  First self-boot requires adding the stable public `.mlfp` ABI as part of
  total native coverage, scoped by explicit ABI version, target triple, and
  substrate fingerprint, including a shared local `.mlfp` package set locked by
  package identity, local root, source hash, dependency interface hashes,
  required ABI version, and substrate fingerprint, with checked-in lock files
  in the `.mlfp`-owned canonical package-lock format recomputed and validated
  during proof-path builds, and no dependency version ranges or solver
  alternatives in the proof path; the first compiler driver must consume,
  validate, and deterministically recompute checked locks, while explicit
  reviewed lock regeneration is allowed; Prelude package/substrate split; shared
  Rust trusted substrate through a shared stable substrate ABI
  path; stable public heap-layout boundary; managed GC runtime contract;
  precise GC root model; non-moving first GC contract; and shared substrate ABI
  conformance layer generated from or validated against canonical substrate ABI
  declarations in a versioned platform substrate contract package using the
  `.mlfp`-owned canonical declaration format, with checked-in generated
  Rust/Haskell/`.mlfp` bindings validated for drift and substrate ABI versioning
  rules that make breaking declaration changes require a major ABI bump and
  artifact-reuse invalidation, and including general user-facing FFI with
  stable FFI value-shape, marshalling, and
  ownership/lifetime rules, explicit export/callback wrappers, a
  manifest-owned FFI link graph, explicit FFI effects, an explicit public-ABI
  runtime context, and an explicit error/unwinding boundary, not deferring it
  as a post-self-boot compatibility feature.
- Backend/native support remains a selected subset. The current bounded seed is
  lowerable and native-runnable, but first self-boot requires total native
  coverage for all source-checked `.mlfp` programs and `.mlfp` language
  features, not merely the compiler workload or selected conformance rows.

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

The next stage should follow
`docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`: stay inside the
compiler-in-`.mlfp` prerequisites lane, not a self-boot claim. With the
package-mode lexer/parser seed, seed-driven gap budget, layer classification,
and fixture handoff in place, the next prerequisite is a shared conformance
oracle rather than immediate self-boot implementation.

- First Full Self-Boot is a compiler milestone with explicit platform
  prerequisites, not whole-platform self-hosting. The proof target is the
  `.mlfp` compiler source package, while package model, native backend,
  ABI/substrate, runtime/GC, conformance, and trusted-substrate contracts must
  be explicit enough for both compiler implementations to share.
- Compiler-language semantics belong to the `.mlfp` compiler source package:
  parser behavior, name resolution, checking, semantic-interface
  reading/writing, backend artifact-emission decisions, diagnostics, package
  validation, and driver behavior. Trusted substrate may provide runtime, GC,
  FFI, filesystem, subprocess, hashing, and linker primitives, but those
  capabilities must not decide `.mlfp` language meaning.
- Recommended next family: the **Pre-Self-Boot Test Migration** captured in
  `docs/adr/2026-05-18-file-based-conformance-before-self-boot.md`. Migrate
  behavior-level `.mlfp` tests one by one into `test/conformance/mlfp/` with
  fixture metadata, per-command expected output files, committed goldens, and
  documented normalization.
- After that corpus exists, target native-capable broad string/`Char` substrate
  plus full canonical `.mlfp` parser parity. The current `Char` and `String`
  literal tracers plus the first `stringLength` operation tracer,
  `stringIsEmpty` classification tracer, `stringContainsChar` scalar search
  tracer, `stringContains` substring search tracer, and `stringStartsWith`
  prefix search tracer are not the complete broad string library; continue
  adding the Prelude-level Unicode-scalar string library required by
  `docs/adr/2026-05-18-native-broad-string-library-before-parser-parity.md`,
  then build the parser-owned combinator core and full canonical parser on
  that substrate.
- Blocking prerequisites for that family: practical stream/list operations,
  broad string and `Char` primitives, native string slicing/classification,
  parser state/result helpers, diagnostic-list or error-accumulation support,
  and real source-range diagnostic payloads.
- Deferred until the self-boot roadmap selects them: maps/sets for resolver or
  checker environments, broader file/process IO driver helpers, stable build
  artifacts, persisted interfaces, and separate compilation. Object ABI/linker
  policy is no longer optional for first self-boot because **Self-Boot Total
  Native Coverage** requires a **Stable Public `.mlfp` ABI**.
- Before first self-boot, close the gap to **Self-Boot Total Native Coverage**:
  backend/native emission must support every source-checked `.mlfp` program and
  every `.mlfp` feature, not only selected compiler, string, parser, or
  conformance shapes. Trusted-substrate APIs callable from `.mlfp` must have
  native implementations, not just interpreter behavior. Deterministic artifact
  contracts must include the target-scoped stable public `.mlfp` ABI and
  shared local `.mlfp` package set, locked local package closure, checked
  package-lock validation, canonical package-lock format, exact locked package
  identity policy, first compiler-driver lock responsibility, Prelude
  package/substrate split, shared Rust
  trusted-substrate contract, shared stable substrate ABI path, canonical
  substrate ABI declarations in the `.mlfp`-owned canonical declaration format
  owned by a platform substrate contract package, shared substrate ABI
  conformance layer, checked generated substrate binding validation, substrate
  ABI versioning policy, host toolchain identity in the substrate fingerprint,
  stable public heap-layout boundary, managed GC runtime contract, precise GC
  root model, and non-moving first GC contract, plus general user-facing FFI
  through declared stable foreign ABI boundaries and stable FFI value shapes
  with explicit ownership/lifetime, export/callback wrapper, manifest
  link-graph, effect, runtime-context, and error/unwinding rules.
- First self-boot proof follows the **First Self-Boot Stage Sequence**: the
  Haskell compiler builds the stage-1 native `.mlfp` compiler from checked
  package locks, the platform contract, shared local `.mlfp` packages, and the
  `.mlfp` compiler source package; that stage-1 compiler rebuilds the same
  locked inputs with the same substrate fingerprint into stage-2 artifacts; the
  proof compares exact conformance outputs and normalized semantic artifacts,
  not object or executable bytes.
- First self-boot does not require deterministic object or executable bytes,
  even under a fixed host toolchain contract. Native artifacts must be
  regenerated, recorded through command/link records, runnable where the proof
  requires execution, and produced under the declared toolchain contract.
- The first proof includes **Stage-Shared Self-Boot Conformance**: both the
  Haskell stage-0 compiler and the native `.mlfp` stage-1 compiler run the same
  declared conformance suite before normalized stage artifacts are accepted.
  That suite includes both the shared file-based compiler conformance corpus and
  the shared substrate ABI conformance layer, with stage-inapplicable fixtures
  declared in metadata rather than skipped ad hoc.
- The proof gate is conformance-first: a stage that fails the shared
  conformance suite stops before normalized semantic artifact comparison and is
  classified as a compiler behavior or substrate ABI oracle failure.
- The proof uses stage-owned output directories: checked locks, source packages,
  substrate fingerprints, platform contract packages, and checked generated
  bindings may be shared declared inputs, but semantic interfaces, normalized
  semantic artifacts, backend IR, object files, executables, link records, and
  conformance outputs are regenerated into fresh stage-local outputs.
- Proof paths are normalized only through manifest-declared package, fixture,
  platform-contract, and toolchain roots for inputs, and through stage-owned
  output roots for generated paths. Symlink and case-sensitivity behavior is
  declared by the manifest/toolchain contract. Source/input path escapes fail as
  input drift; generated output path escapes fail as proof-runner failure.
- Each proof run emits a structured proof manifest in the same `.mlfp`-owned
  canonical data format family as manifests, package locks, and substrate ABI
  declarations, recording stage inputs, substrate fingerprint, package locks,
  normalized command records, canonical link records, native execution records,
  output directories, conformance result hashes, normalized semantic artifact
  hashes, stable machine-readable failure taxonomy entries, and final comparison
  status.
- Each proof-manifest command record includes a proof action ID, argv, cwd,
  normalized environment, stdin source or hash, stdout/stderr hashes or artifact
  paths, exit status, relevant tool identity, owning stage, stage-owned output
  directory, and the compiler-driver mode when the command invokes a stage
  compiler driver.
- Each proof-manifest command, link, native execution, conformance, and
  comparison record carries a stable proof action ID such as
  `stage0.check.compiler`, `stage0.emit-native.compiler`,
  `stage1.run-conformance`, or `stage1.emit-backend.compiler`; failures
  reference action IDs rather than list order.
- First self-boot compiler-driver modes recorded in proof manifests are `check`,
  `emit-backend`, `emit-native`, and `run-conformance`; later modes must be
  explicitly declared before they appear in proof manifests.
- Each proof-manifest native link record includes linker argv, target triple,
  linker mode, object inputs, resolved runtime/system/static/dynamic/framework
  library identities, library search paths, rpath or install-name data where
  relevant, output artifact paths and hashes, owning stage, stage-owned output
  directory, and exit status.
- Linked library identities are resolved where practical to the actual file or
  framework identity, path, content hash or platform package identity, and
  whether the library was linked statically or dynamically. `-l` names and
  search paths alone are not enough proof state.
- Each proof-manifest native execution record captures executable identity,
  argv, cwd, normalized environment, loader environment, resolved dynamic
  libraries/frameworks where practical, rpath or install-name resolution used at
  execution time, stdin source or hash, stdout/stderr hashes or artifact paths,
  exit status, owning stage, and stage-owned output directory.
- Loader-affecting environment variables such as `LD_LIBRARY_PATH`,
  `DYLD_LIBRARY_PATH`, `DYLD_FALLBACK_LIBRARY_PATH`, and platform equivalents
  must be explicitly declared by the manifest/toolchain contract or scrubbed
  before native execution. Native execution records capture the normalized
  loader environment; ambient inherited loader state must not influence proof
  behavior.
- Undeclared or unscrubbed loader-affecting environment state is recorded as a
  loader environment violation, separate from substrate mismatch and stage
  command failure.
- Unresolved or unrecordable dynamic loader resolution is a proof failure that
  must be fixed. The target, toolchain contract, runner, or link mode must make
  loader environment plus resolved dynamic library, framework, rpath, and
  install-name facts recordable before the proof can pass.
- Host toolchain identity is part of the stage-consumed substrate fingerprint,
  not only command records. The fingerprint declares LLVM or backend tool
  identity, assembler, linker, target triple, sysroot, relevant system
  libraries, native codegen settings, and linker mode; command records capture
  exact invocations against that declared contract.
- Toolchain proof identity uses resolved tool paths plus content hashes where
  practical for LLVM/backend tools, assembler, and linker, plus target triple,
  sysroot identity, and relevant system library identities. Version strings are
  diagnostics, not sufficient proof identity.
- Proof-affecting ambient inputs such as wall-clock time, random seeds,
  timezone, locale, and platform equivalents are declared with expected
  normalization or scrubbed before compiler stages and conformance fixtures run.
  Inherited, undeclared, unnormalized, or unscrubbed ambient state is an ambient
  input violation.
- The proof manifest failure taxonomy distinguishes input drift, lock drift,
  substrate mismatch, ambient input violation, stage command failure,
  conformance failure, normalized semantic artifact mismatch, loader environment
  violation, native loader resolution failure, proof-runner failure, and
  unsupported or inapplicable fixture metadata failure.
- The first proof may be orchestrated by a host-side proof runner. The runner
  invokes real stage compiler drivers, runs the conformance-first gate, compares
  normalized semantic artifacts, manages stage-owned outputs, and emits the
  proof manifest, but it must not perform compiler semantics itself, move those
  semantics into trusted substrate, or replace the `.mlfp` compiler-driver
  contract.
- The proof manifest records proof-runner source/version/hash, comparison
  policy, normalization policy, command templates, and runner configuration as
  proof tooling evidence. That evidence is separate from the stage-consumed
  substrate fingerprint unless runner configuration changes compiler/runtime
  inputs consumed by a stage.
- Decide package build artifact policy for compiler sources before adding any
  persisted interface or object artifact format.
