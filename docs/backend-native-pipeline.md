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
contract. The current primitive surface is the closed reserved runtime-binding set
`__mlfp_and`, `__io_pure`, `__io_bind`, and `__io_putStrLn`.
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
assertion is marked pending. The row remains part of the mechanical coverage
matrix; it is not silently removed from the native pipeline.

## Runtime Support

Native emission owns the small process/runtime surface it needs:

- `main`: the C ABI wrapper added only by `emit-native`.
- `printf`: declared for deterministic result printing.
- `malloc`: declared when heap-allocated constructors or closure records need
  allocation.
- `putchar`: declared for character-by-character String rendering.
- `__mlfp_and`: emitted as a backend-owned boolean primitive when no program
  binding owns that name.
- `__io_pure`, `__io_bind`, `__io_putStrLn`: IO runtime primitives emitted as
  closure-allocating wrapper functions with entry-point implementations.
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
- `String` (rendered with quoted escaping via `putchar`)
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

`BackendLLVMSpec` drives the shared `programSpecToLLVMParityCases` matrix
through one explicit coverage entry per interpreter-success row. Every row is
native-run checked with raw LLVM assembly validation, native compile/link/run,
and result comparison. Selected rows also receive object-code smoke validation.

The current `ProgramSpec` parity matrix has no native-unsupported exceptions.

Advanced rows added by the typeclass/evidence, first-class polymorphism, and
higher-order backend slices are required native-run rows when their result is
native-renderable. This prevents supported LLVM parity rows from silently
remaining unlinked or unexecuted.
