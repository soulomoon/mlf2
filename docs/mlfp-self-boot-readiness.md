# `.mlfp` Self-Boot Readiness Ledger

This ledger records what the package-substrate and compiler frontend seed
roadmaps prove and what remains before self-boot can advance. It is evidence
for the current repository only; it is not a self-hosting claim. The durable
end-to-end roadmap order is decided in
`docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`.

## Current Evidence

| Layer | Current evidence | Remaining gap |
|-------|------------------|---------------|
| Source checking | `.mlfp` modules, imports, exports, ADTs, cases, typeclasses, deriving, higher-kinded parameters, opaque builtins, and explicit Prelude imports are checked through the program layer. The compiler frontend seed fixture at `test/programs/compiler-seed/frontend-contract/` now includes package-source modules for bounded symbolic input, source span labels, token stream, lexer diagnostic, lexer result/evidence, AST, parser diagnostic, parser result, and parser evidence. The parser parity tracers under `test/programs/compiler-parser-parity/` source-check parser-owned source/token/AST/parser modules for the canonical basic Bool definition fixture, the single-import `import Prelude exposing (Bool);` fixture, the `import Prelude exposing (Int);` plus two-`Int`-definition fixture, the `let id = λx x in id 1` Int expression fixture, the typed annotation fixture `let id : ∀a. a -> a = λ(x : Int) x in (id 1 : Int)`, and the data-declaration fixture `module Main export (Nat(..), main) { data Nat = Zero : Nat | Succ : Nat -> Nat; def main : Nat = Succ Zero; }`. | The source checker is an implementation in Haskell, not a checked `.mlfp` compiler component. The only `.mlfp` source-text parser evidence is bounded positive tracers for the basic Bool module, the single import-exposing Bool module, the value-definition-list Int/reference module, the let/lambda/application Int module, the typed-annotation Int module, and the data-declaration Nat module; broad source-text lexing/parsing, checker, optimizer, backend, package manager, linker, and driver modules are still absent. |
| Interpreter/runtime | `run-program` checks local package inputs, executes pure mains, includes the first broad `String` operation `stringLength : String -> Int` with Unicode scalar counting, the first `String` classification operation `stringIsEmpty : String -> Bool`, the first single-character `String`/`Char` search operation `stringContainsChar : String -> Char -> Bool`, the first substring `String` search operation `stringContains : String -> String -> Bool`, the first exact `String` equality operation `stringEquals : String -> String -> Bool`, the first prefix `String` search operation `stringStartsWith : String -> String -> Bool`, the first suffix `String` search operation `stringEndsWith : String -> String -> Bool`, the first append `String` operation `stringAppend : String -> String -> String`, the first character replacement `String` operation `stringReplaceChar : String -> Char -> Char -> String`, the first substring replacement `String` operation `stringReplace : String -> String -> String -> String`, the first `String`/`Char` first-match index search operation `stringIndexOfChar : String -> Char -> Option Int`, the first substring index search operation `stringIndexOf : String -> String -> Option Int`, the first substring splitting operation `stringSplit : String -> String -> List String`, delimiter joining with `stringJoin : String -> List String -> String`, single-character delimiter splitting with `stringSplitChar : String -> Char -> List String`, deterministic bytewise comparison with `stringCompare : String -> String -> Int`, the first `Char` to singleton `String` construction operation `stringFromChar : Char -> String`, the first decimal `Int` to `String` conversion operation `stringFromInt : Int -> String`, the first `Bool` to `String` conversion operation `stringFromBool : Bool -> String`, the first `Nat` to `String` conversion operation `stringFromNat : Nat -> String`, the first `Unit` to `String` conversion operation `stringFromUnit : Unit -> String`, the first `List Char` to `String` conversion operation `stringFromList : List Char -> String`, the first `String` to `List Char` conversion operation `stringToList : String -> List Char`, the first drop slicing operation `stringDrop : String -> Int -> String`, the first take slicing operation `stringTake : String -> Int -> String`, the first range slicing operation `stringSlice : String -> Int -> Int -> String`, the first in-range cursor/index operation `stringCharAt : String -> Int -> Char`, the first safe optional cursor lookup operation `stringCharAtOption : String -> Int -> Option Char`, the first `Char` classification operation `charIsDigit : Char -> Bool`, the explicit ASCII lowercase `Char` classification operation `charIsAsciiLower : Char -> Bool`, the explicit ASCII uppercase `Char` classification operation `charIsAsciiUpper : Char -> Bool`, the explicit ASCII alphabetic `Char` classification operation `charIsAsciiAlpha : Char -> Bool`, the explicit ASCII alphanumeric `Char` classification operation `charIsAsciiAlphaNum : Char -> Bool`, the explicit ASCII identifier-start `Char` classification operation `charIsAsciiIdentifierStart : Char -> Bool`, the explicit ASCII identifier-continuation `Char` classification operation `charIsAsciiIdentifierContinue : Char -> Bool`, the explicit ASCII whitespace `Char` classification operation `charIsAsciiWhitespace : Char -> Bool`, the explicit ASCII punctuation `Char` classification operation `charIsAsciiPunctuation : Char -> Bool`, the explicit ASCII printable `Char` classification operation `charIsAsciiPrintable : Char -> Bool`, ASCII hex-digit, line-break, and control classification, plus ASCII-only `Char` and `String` case conversion helpers, and executes checked `main : IO Unit` through the reserved IO primitive boundary. `ProgramCompilerSeedSpec` proves the compiler frontend seed fixture runs through `runLocatedProgramPackageOutput` and the public CLI `run-program` path, including asserted positive tokenization, negative lexer diagnostic, accepted parser AST, and rejected parser diagnostic evidence. `ProgramParserParitySpec` proves the parser parity tracers run through the public CLI `run-program` path and print committed parser-program projections matching the Haskell canonical parser for the basic Bool definition, single import-exposing Bool, value-definition-list Int/reference, let/lambda/application Int, typed-annotation Int, and data-declaration Nat fixtures, including public malformed-import, malformed value-definition semicolon, malformed let-expression, malformed annotation, and malformed data-declaration constructor-colon evidence. | Runtime effects are intentionally narrow; there is no broad filesystem/process/runtime library for a compiler written in `.mlfp`. |
| Backend/native | `emit-backend` and `emit-native` use package-mode inputs and lower the supported checked subset through the private backend IR and LLVM path. The current text-substrate evidence includes Unicode scalar `Char` and `String` literal tracers plus the first native-capable `stringLength` operation tracer, `stringIsEmpty` classification tracer, `stringContainsChar` scalar search tracer, `stringContains` substring search tracer, `stringEquals` exact equality tracer, `stringStartsWith` prefix search tracer, `stringEndsWith` suffix search tracer, `stringAppend` append tracer, `stringReplaceChar` character replacement tracer, `stringReplace` substring replacement tracer, `stringIndexOfChar` first-match index search tracer, `stringIndexOf` substring index search tracer, `stringSplit` substring splitting tracer, `stringJoin` delimiter joining tracer, `stringSplitChar` single-character delimiter splitting tracer, `stringCompare` bytewise comparison tracer, `stringFromChar` singleton construction tracer, `stringFromInt` decimal integer string conversion tracer, `stringFromBool` lowercase Boolean string conversion tracer, `stringFromNat` decimal Nat string conversion tracer, `stringFromUnit` pure Prelude Unit string conversion tracer, `stringFromList` list-to-string conversion tracer, `stringToList` string-to-list conversion tracer, `stringDrop` drop slicing tracer, `stringTake` take slicing tracer, `stringSlice` range slicing tracer, `stringCharAt` in-range cursor/index tracer, `stringCharAtOption` safe optional cursor lookup tracer, `charIsDigit` decimal `Char` classification tracer, `charIsAsciiLower` explicit ASCII lowercase `Char` classification tracer, `charIsAsciiUpper` explicit ASCII uppercase `Char` classification tracer, `charIsAsciiAlpha` explicit ASCII alphabetic `Char` classification tracer, `charIsAsciiAlphaNum` explicit ASCII alphanumeric `Char` classification tracer, `charIsAsciiIdentifierStart` explicit ASCII identifier-start `Char` classification tracer, `charIsAsciiIdentifierContinue` explicit ASCII identifier-continuation `Char` classification tracer, `charIsAsciiWhitespace` explicit ASCII whitespace `Char` classification tracer, `charIsAsciiPunctuation` explicit ASCII punctuation `Char` classification tracer, `charIsAsciiPrintable` explicit ASCII printable `Char` classification tracer, ASCII hex-digit/line-break/control classifiers, and ASCII-only `Char`/`String` case conversion tracers through raw LLVM, object-code validation, and linked native execution. The compiler frontend seed package now emits raw backend LLVM and native-entrypoint LLVM without changing the seed contract. | Backend lowerability is partial. First self-boot now requires **Self-Boot Total Native Coverage**: every source-checked `.mlfp` program and every `.mlfp` language feature must be supported by backend/native emission. Trusted-substrate APIs callable from `.mlfp`, including filesystem, subprocess, hashing, broad string, primitive, and runtime helper operations, must also work in native execution. Native execution must satisfy the **Self-Boot Managed GC Requirement** using a **Precise Self-Boot GC Root Model** and **Non-Moving First Self-Boot GC** for heap-allocated `.mlfp` values. The runtime/GC/FFI substrate may remain a **Shared Rust Trusted Substrate** used by both the Haskell compiler implementation and the future `.mlfp` compiler implementation through a **Shared Stable Substrate ABI Path**. Deterministic native artifact contracts must include a **Target-Scoped Stable `.mlfp` ABI** with **Stable Public Heap Layout Boundary**, **General Public `.mlfp` FFI** through declared stable foreign ABI boundaries, **Stable FFI Value Shapes**, a **Stable FFI Ownership Contract**, **Stable FFI Error Boundary**, **Stable FFI Export and Callback Wrappers**, a **Manifest-Owned FFI Link Graph**, **Explicit FFI Effects**, and an **Explicit FFI Runtime Context**; raw Rust ABI, proof-path private Rust calls, raw implementation-value crossing, hidden exported heap layout, raw closure export, raw GC pointer FFI, conservative-scanning correctness, unversioned moving-GC pointer instability, ambient linker inputs, implicit target assumptions, implicit foreign purity, hidden public-ABI runtime initialization, and implicit unwinding are not stable public `.mlfp` FFI targets. Library-only packages must still emit native-capable module artifacts, while executable linking requires a manifest entrypoint. Existing post-check native unsupported cases are self-boot blockers; narrowing the checked `.mlfp` feature surface solely to avoid native implementation is not an acceptable self-boot shortcut. The current seed's success is evidence for this bounded fixture only. |
| Object code | LLVM assembly/object-code smoke and native-run tests cover selected lowerable rows. The compiler frontend seed's emitted native LLVM validates through object-code generation and executes with the same evidence output as `run-program`. | There is no stable object format contract, linker model, separate object build, target-scoped `.mlfp` ABI guarantee, shared Rust trusted-substrate contract, shared stable substrate ABI path, stable public heap-layout boundary, managed GC runtime contract, precise GC root model, non-moving first GC contract, general user-facing FFI, stable FFI value-shape/marshalling contract, stable ownership/lifetime contract, stable error/unwinding boundary, export/callback wrapper contract, manifest-owned FFI link graph, explicit FFI effect policy, or explicit public-ABI runtime context yet. First self-boot now requires closing that gap with a **Shared Rust Trusted Substrate**, **Shared Stable Substrate ABI Path**, **Target-Scoped Stable `.mlfp` ABI**, **Stable Public Heap Layout Boundary**, **Self-Boot Managed GC Requirement**, **Precise Self-Boot GC Root Model**, and **Non-Moving First Self-Boot GC** that are stable within an explicit ABI version, target triple, and substrate fingerprint, and includes **General Public `.mlfp` FFI** with **Stable FFI Value Shapes**, a **Stable FFI Ownership Contract**, a **Stable FFI Error Boundary**, **Stable FFI Export and Callback Wrappers**, a **Manifest-Owned FFI Link Graph**, **Explicit FFI Effects**, and an **Explicit FFI Runtime Context**, rather than treating ABI/linker/FFI policy as optional. |
| Package build mode | Local package roots and ordered search paths discover `.mlfp` files deterministically; package/interface/build-graph policy tests cover source metadata and interface metadata invalidation. | The package build graph is an in-repo policy and test surface, not a package manager, remote dependency solver, persisted interface format, or separate compilation pipeline. |
| Compiler-in-`.mlfp` implementation | A minimal compiler frontend seed contract, bounded symbolic-input lexer seed, bounded parser/AST seed, and bounded parser parity tracers now exist as ordinary `.mlfp` package source under `test/programs/compiler-seed/frontend-contract/` and `test/programs/compiler-parser-parity/`. | These fixtures are runnable frontend seeds/tracers only. They do not implement a broad source-text lexer/parser, checker, optimizer, backend, package manager, linker, or driver for this compiler in `.mlfp`. |
| Primitives | The primitive inventory centralizes builtin type and operation metadata for the current checker/runtime/backend. | The seed-driven budget below identifies which primitive gaps block a larger compiler component and which remain deferred. |
| Standard library | The built-in Prelude supplies `Nat`, `Option`, `List`, `Eq`, `Unit`, opaque `IO`, minimal `Monad IO`, `pure`, `bind`, `putStrLn`, `stringLength`, `stringIsEmpty`, `stringContainsChar`, `stringContains`, `stringEquals`, `stringStartsWith`, `stringEndsWith`, `stringAppend`, `stringReplaceChar`, `stringReplace`, `stringIndexOfChar`, `stringIndexOf`, `stringSplit`, `stringJoin`, `stringSplitChar`, `stringCompare`, `stringFromChar`, `stringFromInt`, `stringFromBool`, `stringFromNat`, `stringFromUnit`, `stringFromList`, `stringToList`, `stringDrop`, `stringTake`, `stringSlice`, `stringCharAt`, `stringCharAtOption`, `charIsDigit`, `charIsAsciiLower`, `charIsAsciiUpper`, `charIsAsciiAlpha`, `charIsAsciiAlphaNum`, `charIsAsciiIdentifierStart`, `charIsAsciiIdentifierContinue`, `charIsAsciiWhitespace`, `charIsAsciiPunctuation`, `charIsAsciiPrintable`, `charIsAsciiHexDigit`, `charIsAsciiLineBreak`, `charIsAsciiControl`, `charToAsciiLower`, `charToAsciiUpper`, `stringToAsciiLower`, `stringToAsciiUpper`, `and`, and `id`. The lexer seed deliberately uses seed-owned monomorphic symbolic input and token-stream ADTs instead of pretending a general source-text or collection library exists. | The seed-driven budget below identifies which stdlib gaps block a larger compiler component and which remain deferred. First self-boot additionally requires a **Prelude Package/Substrate Split**: high-level Prelude/library source should live in the **Shared Local `.mlfp` Package Set** consumed by both compiler implementations where feasible, while primitive/native operations, GC/runtime hooks, and low-level FFI shims remain declared trusted substrate. |
| Parser/lexer | The Haskell parser accepts the current `.mlfp`, eMLF, and xMLF syntax and rejects retired compatibility spellings. The `.mlfp` lexer seed tokenizes one bounded symbolic `def main = true` input and returns an unknown-symbol diagnostic for a negative symbolic input. The `.mlfp` parser seed consumes the seed token stream, returns a definition AST for the accepted token stream, and returns an expected-equals diagnostic for a malformed token stream. Parser parity tracers now consume fixed source-text fixtures through parser-owned source/token/AST/parser modules: the basic Bool module, a single `import Prelude exposing (Bool);` fixture with import module and exposing-item source-span evidence plus a malformed import semicolon diagnostic, an `import Prelude exposing (Int);` fixture with two `Int` value definitions plus malformed value-definition semicolon evidence, an `import Prelude exposing (Int);` fixture with one let/lambda/application definition plus malformed missing-`in` evidence, an `import Prelude exposing (Int);` fixture with typed let annotations, annotated lambda parameters, expression annotations, arrow source types, forall source types, and malformed annotation evidence, and a `Nat(..)` export/data-declaration fixture with constructor declaration spans, `Nat -> Nat` source-type rendering, constructor application rendering, and malformed constructor-colon evidence. | There is no broad `.mlfp` source-text lexer/parser implementation, no character or byte stream abstraction, and no parser bootstrap corpus beyond the seed plus these bounded parser-parity fixtures. |
| Diagnostics | Located package parsing and checking preserve file paths/spans for common package, import, and visibility failures. The lexer and parser seeds have seed-owned diagnostic ADTs carrying symbolic source spans for rejected inputs. | Compiler-grade diagnostics need real source ranges, recovery strategy, structured error payloads, and golden diagnostics for package builds. |
| Fixture evidence | Static fixture files cover trivial file-as-package inputs, multi-file package roots, ordered search paths, runtime parity rows, backend emission over package mode, the compiler frontend seed contract fixture, the bounded lexer evidence fixture, the bounded parser/AST evidence fixture, the bounded parser-program projection artifacts for the basic Bool definition, import-exposing Bool, value-definition-list Int/reference, let/lambda/application Int, typed-annotation Int, and data-declaration Nat parser parity tracers, and bounded compiler-seed backend/native execution for the current package entrypoint. | Fixtures do not prove broad source-text lexing/parsing, separate compilation, stable ABI/linking, **Self-Boot Total Native Coverage**, or self-hosting. |

Round 300 adds the first safe optional cursor lookup tracer,
`stringCharAtOption : String -> Int -> Option Char`, to the same
source-checking, `run-program`, backend/object, `emit-native`/native-object,
and linked native execution evidence chain. It indexes zero-based Unicode
scalar positions, returns `Some '\955'` for `stringCharAtOption "aλb" 1`,
returns `Some 'b'` for `stringCharAtOption "λab" 2`, and returns `None` for
end-of-input/out-of-range examples such as `stringCharAtOption "λ" 1` and
`stringCharAtOption "" 0`; it does not claim parser combinators, parser
parity, split-family APIs, formatting completion, Unicode normalization,
locale behavior, regex, platform contracts, driver work, or self-boot proof
completion.

Round 301 adds the first exact `String` equality tracer,
`stringEquals : String -> String -> Bool`, to the same source-checking,
`run-program`, backend/object, `emit-native`/native-object, and linked native
execution evidence chain. The current native evidence covers exact comparisons
for source literals and `stringAppend` outputs registered by the native append
helper: `stringEquals "aλ" "aλ"` returns `true`, `stringEquals "aλ" "a"`
returns `false`, `stringEquals "" ""` returns `true`, source-literal
`stringEquals "a\0b" "a"` returns `false`, and
`stringEquals (stringAppend "a" "\0b") "a"` returns `false` through linked
native execution; it does not claim collation, ordering, an `Eq String`
instance, Unicode default case conversion, Unicode normalization, locale
behavior, regex, parser parity, platform contracts, driver work, or self-boot
proof completion.

Round 303 completes the rev-004 initial native-capable Broad String Library
matrix as a unified slice. The same source-checking, `run-program`,
backend/object, `emit-native`/native-object, and linked native execution
evidence now covers delimiter joining, single-character delimiter splitting,
bytewise string comparison, ASCII hex-digit/line-break/control classification,
ASCII-only `Char`/`String` case conversion, negative and overlarge
slicing/cursor boundaries, carried search/split/replace/append regressions, and
exact native byte-length metadata for source literals plus string helper
outputs including embedded U+0000. The round does not claim locale collation,
regex, parser parity, platform contracts, compiler package implementation,
driver/proof completion, Unicode normalization, Unicode collation, Unicode
default case conversion, generic `List` libraries, maps/sets, filesystem,
process IO, ABI, or linker completion.

Round 304 adds the first bounded canonical parser parity tracer for a basic
source-text module definition:
`module Main export (main) { def main : Bool = true; }`. The `.mlfp` package at
`test/programs/compiler-parser-parity/basic-module-def-bool/` owns its source,
token, AST, and parser modules, and `ProgramParserParitySpec` compares its
public `run-program` output with the committed canonical parser projection at
`test/conformance/mlfp/parser-parity/basic-module-def-bool/expected/parser-program.txt`.
This does not claim full parser parity, parser combinators, broad source-text
lexing/parsing, checker/backend/compiler-package/driver/platform/proof work,
or self-boot completion.

Round 305 adds the next bounded canonical parser parity tracer for one import
declaration before the carried Bool definition:
`import Prelude exposing (Bool);`. The `.mlfp` package at
`test/programs/compiler-parser-parity/import-exposing-def-bool/` owns its
source, token, AST, and parser modules, and `ProgramParserParitySpec` compares
its public `run-program` output with the committed canonical parser projection
at
`test/conformance/mlfp/parser-parity/import-exposing-def-bool/expected/parser-program.txt`.
The same public path records malformed import-semicolon evidence. This does
not claim full parser parity, parser combinators, checker/backend/compiler
package, driver, platform, proof, or self-boot completion.

Round 284 adds the next explicit ASCII `Char` classification helper,
`charIsAsciiWhitespace : Char -> Bool`, to the same source-checking,
`run-program`, backend/object, `emit-native`/native-object, and linked native
execution evidence chain. It classifies exactly ASCII space, tab, newline,
carriage return, form feed, and vertical tab as `true`, while ASCII `a` and a
non-ASCII Unicode scalar remain `false`; it does not claim Unicode whitespace,
locale, regex, parser parity, formatting, broader String/List Char collection
APIs, or platform/proof completion.

Round 285 adds the next explicit ASCII `Char` classification helper,
`charIsAsciiPunctuation : Char -> Bool`, to the same source-checking,
`run-program`, backend/object, `emit-native`/native-object, and linked native
execution evidence chain. It classifies exactly ASCII punctuation ranges
`0x21..0x2f`, `0x3a..0x40`, `0x5b..0x60`, and `0x7b..0x7e` as `true`, while
ASCII letters, digits, space, and non-ASCII Unicode scalars remain `false`;
it does not claim Unicode punctuation categories, locale, regex, parser
parity, formatting, broader String/List Char collection APIs, or
platform/proof completion.

Round 286 adds the next explicit ASCII `Char` classification helper,
`charIsAsciiPrintable : Char -> Bool`, to the same source-checking,
`run-program`, backend/object, `emit-native`/native-object, and linked native
execution evidence chain. It classifies exactly ASCII scalar values
`0x20..0x7e` as `true`, while ASCII controls such as tab and newline plus
non-ASCII Unicode scalars remain `false`; it does not claim Unicode
printability categories, locale, regex, parser parity, formatting, broader
String/List Char collection APIs, or platform/proof completion.

Round 287 adds the next broad `String` operation tracer,
`stringAppend : String -> String -> String`, to the same source-checking,
`run-program`, backend/object, `emit-native`/native-object, and linked native
execution evidence chain. It preserves Unicode scalar strings during
concatenation, including `stringAppend "aλ" "b"` and empty-side identity cases;
it does not claim formatting, parser parity, broader String/List Char
collection APIs, platform contracts, driver work, or self-boot proof
completion.

Round 295 adds the first character replacement `String` operation tracer,
`stringReplaceChar : String -> Char -> Char -> String`, to the same
source-checking, `run-program`, backend/object, `emit-native`/native-object,
and linked native execution evidence chain. It replaces every matching
Unicode scalar `Char`, including `stringReplaceChar "aλbλ" 'λ' 'x'`, and
preserves no-match strings such as `stringReplaceChar "ab" 'λ' 'x'`; it does
not claim regex, Unicode normalization, locale behavior, parser
parity, platform contracts, driver work, or self-boot proof completion.

Round 296 adds the first first-match `String`/`Char` index search tracer,
`stringIndexOfChar : String -> Char -> Option Int`, to the same
source-checking, `run-program`, backend/object, `emit-native`/native-object,
and linked native execution evidence chain. It reports zero-based Unicode
scalar positions through `Some`, including `stringIndexOfChar "aλbλ" 'λ'` as
`Some 1`, and reports absent matches such as `stringIndexOfChar "ab" 'λ'` as
`None`; it does not claim broader substring index APIs, regex, Unicode
normalization, locale behavior, complete cursor APIs, parser parity, platform
contracts, driver work, or self-boot proof completion.

Round 297 adds the first substring index search tracer,
`stringIndexOf : String -> String -> Option Int`, to the same source-checking,
`run-program`, backend/object, `emit-native`/native-object, and linked native
execution evidence chain. It reports zero-based Unicode scalar positions
through `Some`, including `stringIndexOf "aλbcλ" "λb"` as `Some 1`, reports
absent substrings such as `stringIndexOf "abc" "λ"` as `None`, and returns
`Some 0` for the empty needle; it does not claim replacement-family completion
beyond `stringReplace`, regex, Unicode
normalization, locale behavior, complete cursor APIs, parser parity, platform
contracts, driver work, or self-boot proof completion.

Round 298 adds the first substring replacement `String` operation tracer,
`stringReplace : String -> String -> String -> String`, to the same
source-checking, `run-program`, backend/object, `emit-native`/native-object,
and linked native execution evidence chain. It replaces non-overlapping
Unicode scalar substrings left to right, including
`stringReplace "aλbλb" "λb" "WXYZ"` as `"aWXYZWXYZ"`, preserves no-match
inputs such as `stringReplace "abc" "λ" "x"` as `"abc"`, and treats the
empty needle as a no-op with `stringReplace "abc" "" "x"` as `"abc"`; it does
not claim split-family collection APIs, regex, Unicode normalization, locale
behavior, case conversion, interpolation, formatting, parser parity, platform
contracts, replacement-family completion beyond this exact operation, driver
work, or self-boot proof completion.

Round 299 adds the first substring splitting `String` operation tracer,
`stringSplit : String -> String -> List String`, to the same source-checking,
`run-program`, backend/object, `emit-native`/native-object, and linked native
execution evidence chain. It splits non-overlapping Unicode scalar substring
delimiters left to right, including `stringSplit "aλbλc" "λ"` as
`Cons "a" (Cons "b" (Cons "c" Nil))`, preserves no-match inputs such as
`stringSplit "abc" "λ"` as `Cons "abc" Nil`, treats an empty delimiter as the
same singleton case, and preserves leading/trailing empty segments for
`stringSplit "λaλ" "λ"` as `Cons "" (Cons "a" (Cons "" Nil))`; it does not
claim split-on-character aliases, `lines`/`words`/`trim`/`reverse` APIs,
regex, Unicode normalization, locale behavior, case conversion, broader `List
String` APIs, parser parity, platform contracts, driver work, or self-boot
proof completion.

Round 288 adds the first `Char` to singleton `String` construction tracer,
`stringFromChar : Char -> String`, to the same source-checking,
`run-program`, backend/object, `emit-native`/native-object, and linked native
execution evidence chain. It preserves Unicode scalar values for singleton
strings, including `stringFromChar 'λ'` and `stringFromChar 'A'`; it does not
claim formatting, parser parity, broader String/List Char collection APIs,
platform contracts, driver work, or self-boot proof completion.

Round 289 adds the first `List Char` to `String` conversion tracer,
`stringFromList : List Char -> String`, to the same source-checking,
`run-program`, backend/object, `emit-native`/native-object, and linked native
execution evidence chain. The Prelude definition is high-level over `List`,
`stringFromChar`, and `stringAppend`, while `run-program` has a narrow public
Prelude binding handler because its top-level recursive binding guard rejects
the direct recursive Prelude function. It preserves Unicode scalar list
contents, including `stringFromList (Cons 'a' (Cons 'λ' Nil))` and
`stringFromList Nil`; it does not claim formatting, parser parity, broader
String/List Char collection APIs, platform contracts, driver work, or self-boot
proof completion.

Round 290 adds the first `String` to `List Char` conversion tracer,
`stringToList : String -> List Char`, to the same source-checking,
`run-program`, backend/object, `emit-native`/native-object, and linked native
execution evidence chain. It preserves Unicode scalar order, including
`stringToList "aλ"` as `Cons 'a' (Cons '\955' Nil)` and `stringToList ""` as
`Nil`; it does not claim formatting, parser parity, broader String/List Char
collection APIs, platform contracts, driver work, or self-boot proof
completion.

Round 291 adds the first decimal `Int` to `String` conversion tracer,
`stringFromInt : Int -> String`, to the same source-checking, `run-program`,
backend/object, `emit-native`/native-object, and linked native execution
evidence chain. It formats `stringFromInt 42` as `"42"` and
`stringFromInt 0` as `"0"`; it does not claim general Show support,
interpolation, printf-style format strings, locale behavior, parser parity,
platform contracts, driver work, or self-boot proof completion.

Round 292 adds the first `Bool` to `String` conversion tracer,
`stringFromBool : Bool -> String`, to the same source-checking, `run-program`,
backend/object, `emit-native`/native-object, and linked native execution
evidence chain. It formats `stringFromBool true` as `"true"` and
`stringFromBool false` as `"false"`; it does not claim general Show support,
interpolation, printf-style format strings, locale behavior, parser parity,
platform contracts, driver work, or self-boot proof completion.

Round 293 adds the first `Nat` to `String` conversion tracer,
`stringFromNat : Nat -> String`, to the same source-checking, `run-program`,
backend/object, `emit-native`/native-object, and linked native execution
evidence chain. It formats `stringFromNat Zero` as `"0"` and
`stringFromNat (Succ (Succ Zero))` as `"2"` by walking canonical Prelude
`Zero`/`Succ` values; it does not claim general Show support, generic ADT
rendering, interpolation, printf-style format strings, locale behavior, parser
parity, platform contracts, driver work, or self-boot proof completion.

Round 294 adds the first `Unit` to `String` conversion tracer,
`stringFromUnit : Unit -> String`, to the same source-checking, `run-program`,
backend/object, `emit-native`/native-object, and linked native execution
evidence chain. It formats `stringFromUnit Unit` as `"Unit"` through a pure
Prelude definition returning a string literal; it does not add a reserved
primitive and does not claim general Show support, generic ADT rendering,
interpolation, printf-style format strings, locale behavior, parser parity,
platform contracts, driver work, or self-boot proof completion.

## Seed-Driven Primitive And Standard-Library Gap Budget

This budget is tied to the merged compiler frontend seed at
`test/programs/compiler-seed/frontend-contract/` and the asserted
interpreter/CLI evidence:

- `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`
- `parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true`

| Category | Seed evidence | Current support and layer owner | Classification | Next action |
|----------|---------------|----------------------------------|----------------|-------------|
| Text, characters, or strings | `SeedSource` models source as closed `SeedInputSymbol` values and symbolic `SourceSpan` labels. The parser parity tracers record fixed source-text string fixtures for the basic Bool module, the import-exposing Bool module, the value-definition-list Int/reference module, the let/lambda/application Int module, the typed-annotation Int module, and the data-declaration Nat module. | Source checking knows `String` literals, the Prelude exposes string-valued IO, and the rev-004 initial Broad String Library matrix is native-capable through source checking, `run-program`, raw LLVM, object validation, and linked native execution. That matrix includes Unicode-scalar length/search/equality/prefix/suffix/append/replace/index/split/join/compare/conversion/slicing/cursor operations, ASCII classification and ASCII-only case helpers, and exact embedded-U+0000 native byte-length metadata for source literals plus string helper outputs. Formatting/interpolation beyond the listed conversions, generic String/List collection libraries, regex, Unicode normalization/collation/default case behavior, locale behavior, parser combinators, full parser parity, platform/compiler package/driver/proof work, ABI, and linker policy remain open. | `needed-before-larger-compiler` | Extend parser-owned source-text coverage only when the next parser expansion requires it; do not add generic text/parser libraries from these tracers alone. |
| Collection operations | `SeedInput` and `SeedTokenStream` are seed-owned monomorphic lists, and `SeedLexer`/`SeedParser` recurse by hand over those streams. | Prelude exposes `List` constructors and derived `Eq`, but not practical `List` operations such as append, reverse, fold, map/filter instances for `List`, indexed access, or non-empty streams. | `needed-before-larger-compiler` | Add only the collection operations required by the next compiler component, likely token/source streams and diagnostic lists, with focused seed evidence. |
| Maps and sets | The seed accepts exactly one identifier and one literal, so no symbol table, keyword table, import environment, or duplicate detection appears in `.mlfp` seed code. | Maps/sets are Haskell implementation details in the current package/checker/backend owners; no `.mlfp` `Map` or `Set` stdlib surface exists. | `deferred` | Reclassify when a `.mlfp` resolver, checker, or multi-keyword lexer/parser slice needs environments. Do not add generic maps/sets from this seed alone. |
| Parser helpers | `SeedParser` is a direct state machine over `SeedTokenStream` with one accepted definition form and one missing-equals diagnostic. The parser parity tracers are also direct and fixture-scoped. | There is no parser-combinator library or reusable parser state abstraction in Prelude; the Haskell frontend parser remains the broad parser owner. | `needed-before-larger-compiler` | Keep the current direct parser while the grammar is tiny. Add a narrow parser state/result helper only when a selected parser expansion proves repeated cursor/expectation logic. |
| Error accumulation | `LexerResult` and `ParserResult` carry exactly one diagnostic; the evidence checks one lexer rejection and one parser rejection. | `.mlfp` has ADTs and `List`, but no standard `Result`/validation abstraction or diagnostic accumulation helper. | `needed-before-larger-compiler` | Introduce structured result and diagnostic-list helpers before parser recovery, multi-error checking, or package-build diagnostics are selected. Keep single-error results for the current seed. |
| IO helpers | The seed uses only `putStrLn` plus `bind` to print evidence. It does not read compiler source files from `.mlfp`, write artifacts, inspect arguments, or exit with process status. | `run-program` interprets `putStrLn`, `putStr`, `pure`, `bind`, `map`, and `ap`; file IO, `getLine`, `IORef`, `exitWith`, and `getArgs` fail closed in `run-program` with native-execution diagnostics. The backend/native layer has selected IO wrappers, but milestone 5 owns native/backend classification. | `deferred` | Keep compiler-library work interpreter-first and pure where possible. A driver, filesystem workflow, process-exit policy, or native runtime expansion belongs to a later layer-classification or driver roadmap, not this budget round. |
| Diagnostics | Lexer and parser diagnostics are seed-owned ADTs with symbolic spans, and the negative paths prove the failing symbolic span is inspectable. | Package parsing/checking has located diagnostics in Haskell. The `.mlfp` seed has no real source ranges, diagnostic severity, notes, recovery, golden diagnostics, or package-build diagnostic aggregation. | `needed-before-larger-compiler` | Preserve symbolic diagnostics for the bounded seed, then add real range payloads and golden diagnostic fixtures when source-text lexing or parser recovery is selected. |

In the text budget row, `Formatting` means broader formatting surfaces; the
narrow decimal `stringFromInt`, lowercase Boolean `stringFromBool`,
canonical decimal `stringFromNat`, and pure Prelude `stringFromUnit` tracers are
limited to the current evidence above.
Round 300 also extends that text budget with only the safe optional
`stringCharAtOption : String -> Int -> Option Char` cursor lookup tracer;
complete cursor APIs, parser combinators, parser parity, split-family APIs,
Unicode normalization, locale behavior, regex, platform contracts, driver
work, and proof records remain open.
Round 301 extends that text budget with only
`stringEquals : String -> String -> Bool` exact equality evidence for source
literals and native `stringAppend` outputs, including embedded-U+0000
source-literal and `stringAppend`-created inequality tracers; collation,
ordering, `Eq String`, Unicode default case conversion, Unicode normalization,
locale behavior, regex, parser parity, platform contracts, driver work, and
proof records remain open.
Round 303 extends that text budget to the rev-004 initial Broad String Library
matrix named above. Remaining text/library gaps are larger surfaces:
locale-sensitive behavior, regex, parser combinators/parity, platform/compiler
package/driver/proof work, Unicode normalization/collation/default case,
generic `List`/map/set libraries, filesystem/process IO, ABI, and linker
policy.
Round 304 adds only the first parser-owned source-text parser parity tracer for
one basic Bool module projection. Full parser parity, parser combinators,
general source streams, platform/compiler package/driver/proof work, and
self-boot evidence remain open.
Round 305 adds only the next parser-owned source-text parser parity tracer for
one import-exposing Bool module projection and an import-semicolon diagnostic.
Full parser parity, parser combinators, general source streams,
checker/backend/platform/compiler package/driver/proof work, and self-boot
evidence remain open.
Round 306 adds only the next parser-owned source-text parser parity tracer for
one value-definition list: `import Prelude exposing (Int);`,
`def two : Int = 2;`, and `def main : Int = two;`. It proves the committed
parser-program projection for two value-definition spans, an integer literal,
and a lower-case value reference, plus a malformed definition-semicolon
diagnostic through `run-program`. Full parser parity, parser combinators,
general source streams, checker/backend/platform/compiler package/driver/proof
work, and self-boot evidence remain open.
Round 307 adds only the next parser-owned source-text parser parity tracer for
one let/lambda/application expression: `import Prelude exposing (Int);` and
`def main : Int = let id = λx x in id 1;`. It proves the committed
parser-program projection for the carried module/import/value spans and the
stable `let`/bare-lambda/left-associated application expression shape, plus a
malformed missing-`in` diagnostic through `run-program`. Full parser parity,
parser combinators, general source streams,
checker/backend/platform/compiler package/driver/proof work, and self-boot
evidence remain open.
Round 308 adds only the next parser-owned source-text parser parity tracer for
typed source annotations: `import Prelude exposing (Int);` and
`def main : Int = let id : ∀a. a -> a = λ(x : Int) x in (id 1 : Int);`.
It proves the committed parser-program projection for typed let annotations,
annotated lambda parameters, expression annotations, arrow source types, and
forall source types, plus malformed annotation evidence through `run-program`.
Full parser parity, parser combinators, general source streams,
checker/backend/platform/compiler package/driver/proof work, and self-boot
evidence remain open.
Round 309 adds only the next parser-owned source-text parser parity tracer for
data declaration syntax:
`module Main export (Nat(..), main) { data Nat = Zero : Nat | Succ : Nat -> Nat; def main : Nat = Succ Zero; }`.
It proves ordered export items, `ExportTypeWithConstructors`, data declaration
and constructor declaration spans, `Nat -> Nat` source-type rendering,
constructor application rendering, plus malformed constructor-colon evidence
through `run-program`. Full parser parity, parser combinators, general source
streams, checker/backend/platform/compiler package/driver/proof work, and
self-boot evidence remain open.

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
- The parser parity tracer at
  `test/programs/compiler-parser-parity/basic-module-def-bool/` prints the
  same parser-program projection and source-span evidence as the Haskell
  canonical parser for the committed basic Bool definition fixture.
- The parser parity tracer at
  `test/programs/compiler-parser-parity/import-exposing-def-bool/` prints the
  same parser-program projection and source-span evidence as the Haskell
  canonical parser for one `import Prelude exposing (Bool);` declaration
  before the carried Bool definition, and records malformed import-semicolon
  evidence through `run-program`.
- The parser parity tracer at
  `test/programs/compiler-parser-parity/value-def-list-int-ref/` prints the
  same parser-program projection and source-span evidence as the Haskell
  canonical parser for `import Prelude exposing (Int);` plus two `Int`
  value definitions, and records malformed value-definition semicolon evidence
  through `run-program`.
- The parser parity tracer at
  `test/programs/compiler-parser-parity/let-lambda-application/` prints the
  same parser-program projection and source-span evidence as the Haskell
  canonical parser for `import Prelude exposing (Int);` plus
  `def main : Int = let id = λx x in id 1;`, and records malformed missing-`in`
  evidence through `run-program`.
- The parser parity tracer at
  `test/programs/compiler-parser-parity/typed-annotation-types/` prints the
  same parser-program projection and source-span evidence as the Haskell
  canonical parser for `import Prelude exposing (Int);` plus
  `def main : Int = let id : ∀a. a -> a = λ(x : Int) x in (id 1 : Int);`,
  and records malformed annotation evidence through `run-program`.
- The parser parity tracer at
  `test/programs/compiler-parser-parity/data-declaration-constructor-spans/`
  prints the same parser-program projection and source-span evidence as the
  Haskell canonical parser for `module Main export (Nat(..), main)` with
  `data Nat = Zero : Nat | Succ : Nat -> Nat;` plus
  `def main : Nat = Succ Zero;`, and records malformed constructor-colon
  evidence through `run-program`.
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
- No broad source-text lexer/parser, checker, optimizer, backend, package
  manager, linker, or driver implementation is written in `.mlfp`.
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
- After that corpus exists, continue from the bounded parser parity tracer
  toward full canonical `.mlfp` parser parity on top of the native-capable
  broad string/`Char` substrate. The current `Char` and `String`
  literal tracers plus the first `stringLength` operation tracer,
  `stringIsEmpty` classification tracer, `stringContainsChar` scalar search
  tracer, `stringContains` substring search tracer, `stringEquals` exact
  equality tracer, `stringStartsWith` prefix search tracer, `stringEndsWith`
  suffix search tracer, `stringAppend`
  Unicode-scalar-preserving append tracer, `stringReplaceChar`
  Unicode-scalar character replacement tracer, `stringReplace`
  Unicode-scalar substring replacement tracer, `stringIndexOfChar`
  Unicode-scalar first-match index search tracer, `stringIndexOf`
  Unicode-scalar substring index search tracer, `stringSplit`
  Unicode-scalar substring splitting tracer, `stringFromChar`
  Unicode-scalar singleton construction tracer, `stringFromInt` decimal integer string
  conversion tracer, `stringFromBool` lowercase Boolean string conversion
  tracer, `stringFromNat` decimal Nat string conversion tracer, pure Prelude
  `stringFromUnit` Unit string conversion tracer, `stringFromList`
  list-to-string conversion tracer, `stringToList`
  string-to-list conversion tracer, `stringDrop` drop slicing tracer, `stringTake`
  take slicing tracer, `stringSlice` range slicing tracer, `stringCharAt`
  in-range cursor/index tracer, `stringCharAtOption` safe optional cursor
  lookup tracer, and
  `charIsDigit` decimal `Char` classification tracer,
  `charIsAsciiLower` explicit ASCII lowercase `Char` classification tracer,
  `charIsAsciiUpper` explicit ASCII uppercase `Char` classification tracer,
  `charIsAsciiAlpha` explicit ASCII alphabetic `Char` classification tracer,
  `charIsAsciiAlphaNum` explicit ASCII alphanumeric `Char` classification
  tracer, `charIsAsciiIdentifierStart` explicit ASCII identifier-start
  `Char` classification tracer, `charIsAsciiIdentifierContinue` explicit
  ASCII identifier-continuation `Char` classification tracer,
  `charIsAsciiWhitespace` explicit ASCII whitespace `Char` classification
  tracer, `charIsAsciiPunctuation` explicit ASCII punctuation `Char`
  classification tracer, and `charIsAsciiPrintable` explicit ASCII printable
  `Char` classification tracer
  are not the complete broad string library; continue
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
