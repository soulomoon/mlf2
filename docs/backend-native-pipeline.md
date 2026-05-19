# Backend Native Pipeline

This document describes the native executable path used by the LLVM backend
tests. xMLF remains the thesis-faithful typed elaboration IR, and
`MLF.Backend.IR` is the single executable eager backend IR. Both
`emit-backend` and `emit-native` consume the same `MLF.Backend.IR` program
produced by `MLF.Backend.Convert`. This document is a test and inspection
contract for checked pure `.mlfp` programs; it is not a second executable IR,
not a second checked-program authority, and not a separate source-language
runtime or an IO implementation.

Any ANF-like normalization, layout-only structure, or lowerability-only
representation stays private to backend-owned lowering helpers rather than
becoming a public `LowerableBackend.IR`.

A later lower IR may be introduced only when all of the following hold:

- distinct backend-owned executable invariants that cannot live in
  `MLF.Backend.IR` or a private lowering helper;
- a dedicated validation/evidence owner for that new boundary; and
- a later accepted roadmap revision before any new durable or public surface
  is added.

`emit-backend` is the raw inspection/lowering output from that same
`MLF.Backend.IR` program. `emit-native` is that same eager IR plus private
native-entrypoint/runtime support only. There are no thunks, no update frames,
no CAF update semantics, no graph reduction, and no implicit laziness rescue
in the native path.

Row-4 ADT/case ownership is part of the raw/native inspection contract:
semantic constructor/case nodes stay in `MLF.Backend.IR`, while runtime tags, field slots, closure-record storage for function-like fields, and nullary tag-only representation stay private to LLVM/native lowering. Raw LLVM output
may be inspected for the current lowerer-owned policy: declaration-order
zero-based constructor tags drive emitted `switch` targets, object offset `0`
stores the tag word, field slots follow the tag word, function-like constructor fields store explicit closure records, and nullary constructors
emit tag-only heap objects. This is evidence for private lowering policy, not
a second IR or public lowering interface.

Row-5 primitive/eager ownership is part of the same raw/native inspection
contract. The current primitive surface is the inventory-owned reserved
runtime-binding set in `MLF.Primitive.Inventory`: `__mlfp_and`,
`__string_length`, `__string_is_empty`, `__string_contains_char`,
`__string_contains`, `__string_starts_with`, `__string_ends_with`,
`__string_append`, `__string_from_char`, `__string_from_int`,
`__string_from_bool`, `__string_from_nat`, `__string_to_list`,
`__string_drop`, `__string_take`, `__string_slice`, `__string_char_at`,
`__char_is_digit`,
`__char_is_ascii_lower`, `__char_is_ascii_upper`, `__char_is_ascii_alpha`,
`__char_is_ascii_alpha_num`,
`__char_is_ascii_identifier_start`,
`__char_is_ascii_identifier_continue`, `__char_is_ascii_whitespace`,
`__char_is_ascii_punctuation`, `__char_is_ascii_printable`, plus the IO
primitive names classified there for native support. Native lowering consumes that inventory directly for
lowerable primitive names, while wrapper bodies, C
runtime symbol names, closure layout, and eager sequencing implementation
details remain lowerer-local.
`emit-backend` and `emit-native` keep those primitives on the same
`BackendVar`, `BackendApp`, and `BackendTyApp` surface, with no new `BackendPrim`,
no second executable IR, and no broad FFI lane.

The eager sequencing contract stays explicit on that same path:

- let RHS before body;
- case scrutinee before branch selection;
- direct/primitive call arguments in written order; and
- effect sequencing remains explicit through `__io_bind`.

Native execution relies on that explicit eager contract instead of fallback
runtime execution or implicit laziness rescue.

Row-6 polymorphism/lowerability is part of the same raw/native inspection
contract. checked `Backend.IR` may still carry `BackendTyAbs` and `BackendTyApp`.
LLVM/native lowering owns only the specialization-based lowerable subset.
Complete type applications may specialize privately inside the lowerer.
Residual runtime polymorphism remains unsupported and must fail with explicit diagnostics without widening the backend boundary. Unsupported polymorphic `main` bindings, unspecialized polymorphic values, escaping type abstractions or polymorphic bindings, partial type applications, and non-native-renderable polymorphic results must fail before emission rather than silently surviving as raw or native rows.

## Emission Modes

`emit-backend` prints the raw LLVM module lowered from that same
`MLF.Backend.IR` program. The checked `.mlfp` `main` binding remains a
module-qualified LLVM function such as `Main__main`, and the output is used
for backend inspection, `llvm-as` validation, and selected
`llc -filetype=obj` smoke checks.

`emit-native` starts from the same `MLF.Backend.IR` program, then adds a C ABI
`i32 @main()` process entrypoint. For pure mains, the entrypoint calls the
checked zero-argument `.mlfp` `main`, renders the pure result to stdout, prints
exactly one trailing newline, writes no stderr on success, and returns exit
status `0`. For `IO` mains, the entrypoint calls the main closure to execute
the IO action and returns exit status `0` without rendering. The added support
is limited to the private native-entrypoint/runtime symbols described below; it
does not create a second executable IR or a lazy runtime.

## Generated Artifacts

The Hspec helper writes temporary files only under the system temporary
directory:

- `program.ll` is the emitted LLVM module.
- `program.o` is produced by `llc -filetype=obj`.
- `program` is the linked native executable.

The temporary build directory is removed after the test action finishes.

## Toolchain

Assembly validation looks for `llvm-as`. Object-code and native execution look
for `llc`. Tool discovery checks tool-specific environment variables first,
then `PATH`, then standard local LLVM installation paths used by Homebrew LLVM.
LLVM 14-era tools are retried with `-opaque-pointers`; LLVM 15 and newer accept
the emitted opaque-pointer IR directly.

Native linking looks for a C compiler/linker through `CC`, then `cc`, `clang`,
or `gcc` on `PATH`. `CC` may include launcher arguments, for example
`ccache clang` or `xcrun clang`.

When a required LLVM or native linker tool is absent, the relevant Hspec
assertion fails with the missing tool diagnostic. The row remains part of the
mechanical coverage matrix; missing tools are validation blockers, not a reason
to silently remove native pipeline coverage.

## Runtime Support

Native emission owns the small process/runtime surface it needs:

- `main`: the C ABI wrapper added only by `emit-native`.
- `printf`: declared for deterministic result printing.
- `sprintf`: declared for narrow decimal `Int` and `Nat` to `String` helpers.
- `malloc`: declared when heap-allocated constructors or closure records need
  allocation.
- `putchar`: declared for character-by-character String rendering.
- `__mlfp_and`: inventory-classified as the backend-owned boolean primitive
  and emitted when no program binding owns that name.
- `__string_length`: inventory-classified as the native-capable broad string
  operation; the native helper counts UTF-8 scalar starts and returns an
  `Int`.
- `__string_is_empty`: inventory-classified as the first native-capable broad
  String classification operation; the native helper checks the valid-text
  empty boundary and returns a `Bool`.
- `__string_contains_char`: inventory-classified as the first native-capable
  single-character `String`/`Char` search operation; the native helper compares
  Unicode scalar values and returns a `Bool`.
- `__string_contains`: inventory-classified as the first native-capable
  substring `String` search operation; the native helper compares valid UTF-8
  scalar-sequence slices from scalar boundaries and returns a `Bool`.
- `__string_starts_with`: inventory-classified as the first native-capable
  prefix `String` search operation; the native helper compares valid UTF-8
  scalar-sequence bytes from the start of the haystack and returns a `Bool`.
- `__string_ends_with`: inventory-classified as the first native-capable
  suffix `String` search operation; the native helper compares valid UTF-8
  scalar-sequence bytes at the end of the haystack and returns a `Bool`.
- `__string_append`: inventory-classified as the first native-capable append
  `String` operation; the native helper copies valid UTF-8 scalar sequences
  from both inputs and returns a new `String` pointer.
- `__string_from_char`: inventory-classified as the first native-capable
  `Char` to singleton `String` construction operation; the native helper
  encodes the Unicode scalar as valid UTF-8 and returns a new `String`
  pointer.
- `__string_from_int`: inventory-classified as the first native-capable
  decimal `Int` to `String` conversion operation; the native helper writes a
  decimal integer string into a new `String` pointer.
- `__string_from_bool`: inventory-classified as the first native-capable
  `Bool` to `String` conversion operation; the native helper returns ordinary
  string values for lowercase `true` and `false`.
- `__string_from_nat`: inventory-classified as the first native-capable `Nat`
  to `String` conversion operation; the native helper walks canonical Prelude
  `Zero`/`Succ` constructor chains and writes a decimal count into a new
  `String` pointer.
- `__string_drop`: inventory-classified as the first native-capable drop
  slicing `String` operation; the native helper advances by UTF-8 scalar
  starts for a non-negative count and returns the remaining `String` pointer.
- `__string_take`: inventory-classified as the first native-capable take
  slicing `String` operation; the native helper copies complete UTF-8 scalar
  sequences up to a non-negative count and returns a new `String` pointer.
- `__string_slice`: inventory-classified as the first native-capable range
  slicing `String` operation; the native helper advances by UTF-8 scalar starts
  for a non-negative start offset, copies complete UTF-8 scalar sequences up to
  a non-negative count, and returns a new `String` pointer.
- `__string_char_at`: inventory-classified as the first native-capable
  in-range cursor/index `String` operation; the native helper advances by
  UTF-8 scalar starts and returns the Unicode scalar value as a `Char`.
- `__char_is_digit`: inventory-classified as the first native-capable `Char`
  classification operation; the native helper compares the Unicode scalar
  value against the ASCII decimal digit range.
- `__char_is_ascii_lower`: inventory-classified as an explicit ASCII
  lowercase `Char` classification operation; the native helper compares the
  Unicode scalar value against the ASCII `a` through `z` range.
- `__char_is_ascii_upper`: inventory-classified as an explicit ASCII
  uppercase `Char` classification operation; the native helper compares the
  Unicode scalar value against the ASCII `A` through `Z` range.
- `__char_is_ascii_alpha`: inventory-classified as an explicit ASCII
  alphabetic `Char` classification operation; the native helper compares the
  Unicode scalar value against the ASCII `a` through `z` and `A` through `Z`
  ranges.
- `__char_is_ascii_alpha_num`: inventory-classified as an explicit ASCII
  alphanumeric `Char` classification operation; the native helper compares the
  Unicode scalar value against the ASCII `a` through `z`, `A` through `Z`, and
  `0` through `9` ranges.
- `__char_is_ascii_identifier_start`: inventory-classified as an explicit
  ASCII identifier-start `Char` classification operation matching the current
  parser start set; the native helper compares the Unicode scalar value against
  the ASCII `a` through `z`, `A` through `Z`, and `_` ranges.
- `__char_is_ascii_identifier_continue`: inventory-classified as an explicit
  ASCII identifier-continuation `Char` classification operation matching the
  current parser continuation set; the native helper compares the Unicode
  scalar value against the ASCII `a` through `z`, `A` through `Z`, `0` through
  `9`, `_`, and apostrophe cases.
- `__char_is_ascii_whitespace`: inventory-classified as an explicit ASCII
  whitespace `Char` classification operation; the native helper compares the
  Unicode scalar value against exactly space, tab, newline, carriage return,
  form feed, and vertical tab.
- `__char_is_ascii_punctuation`: inventory-classified as an explicit ASCII
  punctuation `Char` classification operation; the native helper compares the
  Unicode scalar value against exactly the ASCII punctuation ranges
  `0x21..0x2f`, `0x3a..0x40`, `0x5b..0x60`, and `0x7b..0x7e`.
- `__char_is_ascii_printable`: inventory-classified as an explicit ASCII
  printable `Char` classification operation; the native helper compares the
  Unicode scalar value against exactly ASCII scalar values `0x20..0x7e`.
- Inventory-classified IO primitives such as `__io_pure`, `__io_bind`,
  `__io_putStrLn`, and `__io_getArgs`: emitted as closure-allocating wrapper
  functions with entry-point implementations.
- `__mlfp_native_render$...`: private renderer functions generated for each
  native-renderable result type reachable from the program `main` result.
- `__mlfp_native_*` string globals: format strings, booleans, punctuation, and
  constructor names used by generated renderers.

Source bindings that collide with native-owned runtime symbols are rejected
before native LLVM is emitted.

## Result Comparison

Native run-result checks compare the executable process output with the same
runtime expectations used by the shared `ProgramSpec` interpreter matrix.
Supported result shapes are:

- `Int`
- `Bool`
- `Char` (currently covered by the Unicode scalar tracer that renders `'λ'` as `'\955'` and the `stringCharAt` tracer that renders `'b'`)
- `String` (rendered with quoted escaping via `putchar`; the current
  non-ASCII coverage is the two-byte Unicode scalar tracer that renders `"λ"`
  as `"\\955"`)
- `stringLength : String -> Int` is the first native-capable broad string
  operation tracer and counts Unicode scalar values rather than bytes.
- `stringIsEmpty : String -> Bool` is the first native-capable broad String
  classification tracer and distinguishes `""` from a non-empty Unicode string.
- `stringContainsChar : String -> Char -> Bool` is the first native-capable
  single-character String search tracer and compares Unicode scalar values.
- `stringContains : String -> String -> Bool` is the first native-capable
  substring String search tracer and compares non-empty Unicode scalar
  substring examples through native execution.
- `stringStartsWith : String -> String -> Bool` is the first native-capable
  prefix String search tracer and compares non-empty Unicode scalar prefix
  examples through native execution.
- `stringEndsWith : String -> String -> Bool` is the first native-capable
  suffix String search tracer and compares non-empty Unicode scalar suffix
  examples through native execution.
- `stringDrop : String -> Int -> String` is the first native-capable drop
  slicing String tracer and drops non-negative Unicode scalar prefix counts
  through native execution.
- `stringTake : String -> Int -> String` is the first native-capable take
  slicing String tracer and keeps non-negative Unicode scalar prefix counts
  through native execution.
- `stringSlice : String -> Int -> Int -> String` is the first native-capable
  range slicing String tracer and keeps non-negative Unicode scalar start/count
  examples through native execution.
- `stringCharAt : String -> Int -> Char` is the first native-capable in-range
  cursor/index String tracer and returns Unicode scalar values through native
  execution.
- `stringAppend : String -> String -> String` is the first native-capable
  append String tracer and concatenates valid Unicode-scalar strings through
  native execution while keeping formatting, parser parity, platform contracts,
  and proof records out of scope.
- `stringReplaceChar : String -> Char -> Char -> String` is the first
  native-capable character replacement String tracer and replaces every
  matching Unicode scalar `Char` while preserving no-match inputs through
  native execution. Substring replacement, splitting, regex, Unicode
  normalization, locale behavior, parser parity, platform contracts, and proof
  records remain out of scope.
- `stringIndexOfChar : String -> Char -> Option Int` is the first
  native-capable first-match String/Char index search tracer and reports
  zero-based Unicode scalar indexes through `Some` while returning `None` for
  absent matches. Broader substring index APIs, splitting, regex, Unicode
  normalization, locale behavior, complete cursor APIs, parser parity,
  platform contracts, and proof records remain out of scope.
- `stringIndexOf : String -> String -> Option Int` is the first native-capable
  substring index search tracer and reports zero-based Unicode scalar indexes
  through `Some`, returns `None` for absent substrings, and returns `Some 0`
  for an empty needle. Splitting, substring replacement, regex, Unicode
  normalization, locale behavior, complete cursor APIs, parser parity,
  platform contracts, and proof records remain out of scope.
- `stringFromChar : Char -> String` is the first native-capable Char to
  singleton String construction tracer and encodes Unicode scalar values
  through native execution while keeping formatting, parser parity, platform
  contracts, and proof records out of scope.
- `stringFromInt : Int -> String` is the first native-capable decimal Int to
  String conversion tracer and formats decimal integer strings through native
  execution while keeping general Show support, interpolation, printf-style
  formatting, locale behavior, parser parity, platform contracts, and proof
  records out of scope.
- `stringFromBool : Bool -> String` is the first native-capable Bool to
  String conversion tracer and formats lowercase `true` and `false` strings
  through native execution while keeping general Show support, interpolation,
  printf-style formatting, locale behavior, parser parity, platform contracts,
  and proof records out of scope.
- `stringFromNat : Nat -> String` is the first native-capable Nat to String
  conversion tracer and formats canonical Prelude `Zero`/`Succ` values as
  decimal strings through native execution while keeping general Show support,
  generic ADT rendering, interpolation, printf-style formatting, locale
  behavior, parser parity, platform contracts, and proof records out of scope.
- `stringFromUnit : Unit -> String` is the first native-capable Unit to String
  conversion tracer. The Prelude definition stays pure and returns the `"Unit"`
  string literal for canonical `Unit`; no reserved primitive, runtime dispatch,
  or backend primitive lowerer is added, and general Show support, generic ADT
  rendering, interpolation, printf-style formatting, locale behavior, parser
  parity, platform contracts, and proof records remain out of scope.
- `stringFromList : List Char -> String` is the first native-capable `List Char`
  to String conversion tracer. The Prelude definition stays high-level over
  `List`, `stringFromChar`, and `stringAppend`; linked native execution proves
  Unicode scalar list construction while formatting, parser parity, platform
  contracts, and proof records remain out of scope.
- `stringToList : String -> List Char` is the first native-capable String to
  `List Char` conversion tracer. The reserved `__string_to_list` primitive
  decodes valid UTF-8 into Unicode scalar `Char` list cells while formatting,
  broader collection APIs, parser parity, platform contracts, and proof records
  remain out of scope.
- `charIsDigit : Char -> Bool` is the first native-capable Char
  classification tracer and classifies ASCII decimal digit code points through
  native execution while keeping broader classification families out of scope.
- `charIsAsciiLower : Char -> Bool` is an explicit ASCII lowercase Char
  classification tracer and classifies only ASCII `a` through `z` code points
  through native execution while keeping Unicode category and parser-family
  completion out of scope.
- `charIsAsciiUpper : Char -> Bool` is an explicit ASCII uppercase Char
  classification tracer and classifies only ASCII `A` through `Z` code points
  through native execution while keeping combined alphabetic, Unicode category,
  and parser-family completion out of scope.
- `charIsAsciiAlpha : Char -> Bool` is an explicit ASCII alphabetic Char
  classification tracer and classifies only ASCII `a` through `z` and `A`
  through `Z` code points through native execution while keeping
  identifier-continuation, Unicode category, and parser-family completion out
  of scope.
- `charIsAsciiAlphaNum : Char -> Bool` is an explicit ASCII alphanumeric Char
  classification tracer and classifies only ASCII letters or decimal digit
  code points through native execution while keeping underscore/apostrophe
  identifier-continuation semantics, Unicode category, and parser-family
  completion out of scope.
- `charIsAsciiIdentifierStart : Char -> Bool` is an explicit ASCII
  identifier-start Char classification tracer and matches the current parser
  start set through native execution: lowercase ASCII, uppercase ASCII, and
  underscore are true while ASCII digit, apostrophe, and non-ASCII scalars are
  false. Identifier-continuation behavior is exposed separately and full
  parser-family completion remains out of scope.
- `charIsAsciiIdentifierContinue : Char -> Bool` is an explicit ASCII
  identifier-continuation Char classification tracer and matches the current
  parser continuation set through native execution: lowercase ASCII, uppercase
  ASCII, ASCII digit, underscore, and apostrophe are true while non-ASCII
  scalars are false. Unicode categories, broader punctuation families, and
  parser-family completion remain out of scope.
- `charIsAsciiWhitespace : Char -> Bool` is an explicit ASCII whitespace Char
  classification tracer and classifies exactly ASCII space, tab, newline,
  carriage return, form feed, and vertical tab through native execution while
  keeping Unicode whitespace categories, locale, regex, and parser-family
  completion out of scope.
- `charIsAsciiPunctuation : Char -> Bool` is an explicit ASCII punctuation
  Char classification tracer and classifies exactly the ASCII punctuation
  ranges `0x21..0x2f`, `0x3a..0x40`, `0x5b..0x60`, and `0x7b..0x7e` through
  native execution while keeping Unicode punctuation categories, locale, regex,
  formatting, and parser-family completion out of scope.
- `charIsAsciiPrintable : Char -> Bool` is an explicit ASCII printable Char
  classification tracer and classifies exactly ASCII scalar values
  `0x20..0x7e` through native execution while keeping Unicode printability
  categories, locale, regex, formatting, and parser-family completion out of
  scope.
- `IO Unit` (executes the action closure, does not render the result)
- first-order ADT values whose fields are recursively native-renderable

The native renderer uses the same value text expected by `run-program`, adds one
newline, and requires empty stderr plus `ExitSuccess`.

For `IO` mains, the native entrypoint calls the main closure to execute the IO
action and exits with status 0 without rendering any result value.

Unsupported result shapes fail before native execution instead of becoming
assembly-only rows. Current unsupported shapes include function-valued,
polymorphic, variable-headed, and structurally recursive main results that have
no named data runtime. These rows stay named in `BackendLLVMSpec` with the
expected diagnostic fragment.

## Coverage Contract

`Parity.ProgramMatrix.NativePolicy` owns the test-support policy that maps the
shared `programSpecToLLVMParityCases` interpreter-success rows into backend
parity layers. `BackendLLVMSpec` consumes those policy rows rather than keeping
its own object-code or native-run string lists. Each policy entry names the
source-checking result, interpreter/runtime expectation, backend LLVM assembly
expectation, selected raw object-code smoke coverage, native executable run
expectation, and required LLVM/native tools.

Every current row is native-run checked with raw LLVM assembly validation,
native compile/link/run, and result comparison. Selected rows also receive raw
object-code smoke validation through the same policy owner.

The current `ProgramSpec` parity matrix has no native-unsupported exceptions.

Advanced rows added by the typeclass/evidence, first-class polymorphism, and
higher-order backend slices are required native-run rows when their result is
native-renderable. This prevents supported LLVM parity rows from silently
remaining unlinked or unexecuted.
