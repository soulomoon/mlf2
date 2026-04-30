# mlf4 (mlf2)

An implementation of MLF inference and elaboration, aligned to:

- `papers/these-finale-english.txt` (primary source of truth)
- `papers/xmlf.txt` (supplementary where thesis is silent)

The pipeline takes eMLF terms, builds and solves graphic constraints, and elaborates to explicit xMLF terms.

## Repository layout

- `src/` — internal implementation (`MLF.Frontend.*`, `MLF.Constraint.*`, `MLF.Elab.*`, etc.)
- `src-public/` — public API modules (`MLF.API`, `MLF.Program`, `MLF.Pipeline`, `MLF.XMLF`)
- `app/` — executable entrypoint (`mlf2`)
- `test/` — Hspec test suite
- `tasks/` — active/archive task execution logs
- `papers/` — thesis/paper references
- `docs/` — architecture/phase/syntax documentation

## Build and test

```bash
cabal build
cabal test
```

Useful variants:

```bash
cabal test --test-show-details=direct
./scripts/thesis-conformance-gate.sh
cabal run mlf2
cabal repl mlf2
cabal repl mlf2-test
```

## Continuous integration

GitHub Actions runs two matrix-parameterized jobs on every push and pull
request, defined in `.github/workflows/thesis-conformance.yml`:

| Job                  | What it runs                                      |
|----------------------|---------------------------------------------------|
| `build-and-test`     | `cabal build all` then `cabal test`               |
| `thesis-conformance` | `./scripts/thesis-conformance-gate.sh` (after build) |

**Supported matrix lane:** `ubuntu-latest` / GHC 9.14.1.

**Excluded lane:** Windows — the thesis-conformance gate and supporting
scripts under `scripts/` are Unix shell; no Windows lane is promised until
those scripts are made portable.

### Authoritative verification commands

These two commands are the authoritative verification gates both locally and
in CI:

```bash
cabal build all && cabal test
./scripts/thesis-conformance-gate.sh
```

CI reuses the same repo commands; there is no CI-only verification logic.

## Public entry points

- `MLF.API` — surface syntax plus eMLF / `.mlfp` parsing, pretty-printing, and normalization helpers
- `MLF.Pipeline` — canonical public constraint-generation / elaboration / runtime API, including `.mlfp` elaboration/checking on the shared eMLF/xMLF path
- `MLF.Program` — thin compatibility re-export for the same unified `.mlfp` surface
- `MLF.XMLF` — xMLF syntax, parser, and pretty-printer

Active multi-step work is tracked under `tasks/todo/`; root-level `task_plan.md`,
`findings.md`, and `progress.md` are historical leftovers rather than the active
workflow.

## Unified `.mlfp` program surface

`.mlfp` programs now share the main frontend/pipeline ownership boundary:

- `MLF.API` parses and pretty-prints `.mlfp` programs
- `MLF.Pipeline` elaborates `.mlfp` programs onto the existing eMLF/xMLF path and checks them with the existing MLF typechecker
- `MLF.Program` remains available only as a thin compatibility re-export

The unified `.mlfp` surface includes:

- `module` / `import ... exposing (...)` plus qualified `import ... as ...`
- `data` declarations with explicit constructor signatures
- `case` / constructor pattern matching
- recursive GADT-style constructor result types
- existential constructors via `forall`
- single-parameter typeclasses, class constraints, and schema instances
- `deriving Eq` for nullary, recursive, and parameterized ADTs whose fields have Eq evidence
- a built-in Prelude with `Nat`, `Option`, `List`, `Eq`, `Unit`, opaque
  `IO`, minimal `Monad IO`, `pure`, `bind`, `putStrLn`, `and`, and `id`

`.mlfp` modules are same-compilation-unit modules today: imports resolve among
modules in the parsed program, with the CLI runner adding the built-in Prelude
as an explicit module. Separate module compilation and interface files are not
supported yet; see `docs/mlfp-language-reference.md` for the user-facing module
contract.

Internally, `.mlfp` now reuses the old MLF ownership boundary:

- `MLF.Frontend.Program.Check` assembles module/import/class/data environments
- `MLF.Frontend.Program.Elaborate` lowers executable `.mlfp` bindings to `SurfaceExpr`
- `MLF.Frontend.Program.Finalize` normalizes those surface eMLF terms and calls the internal detailed eMLF pipeline entrypoint
- `MLF.Elab.TypeCheck` remains the typing-judgment owner for checked `.mlfp` / xMLF terms
- `MLF.Frontend.Program.Run` evaluates pure checked bindings through the existing xMLF reducer and executes checked `main : IO Unit` actions through the reserved IO primitive boundary; static module/import/data/class validation may still fail before the eMLF pipeline
- `MLF.Backend.IR` defines the private typed backend IR boundary and local invariant validator for checked `.mlfp` programs before LLVM lowering
- `MLF.Backend.LLVM` lowers supported first-order backend IR programs to real LLVM IR text

Frozen examples live under `test/programs/recursive-adt/`, and the Phase-0
syntax/corpus freeze is documented in
`docs/plans/2026-04-13-recursive-adt-syntax-freeze.md`.

The current user-facing `.mlfp` language contract is documented in
`docs/mlfp-language-reference.md`.

The first-class-polymorphism program example lives at
`test/programs/unified/first-class-polymorphism.mlfp`.

Run a program file directly from the executable:

```bash
cabal run mlf2 -- run-program test/programs/recursive-adt/plain-recursive-nat.mlfp
```

Emit LLVM IR for a checked program:

```bash
cabal run mlf2 -- emit-backend test/programs/unified/authoritative-let-polymorphism.mlfp
```

Emit LLVM IR with a native process entrypoint:

```bash
cabal run mlf2 -- emit-native test/programs/unified/authoritative-let-polymorphism.mlfp
```

`emit-backend` keeps the raw backend contract: the checked `.mlfp` `main`
binding remains a module-qualified LLVM function such as `Main__main`. It is a
pure LLVM subset contract today: checked `main : IO Unit`, direct `__io_*`
primitive calls, and pure entrypoints with reachable IO-typed dependencies are
rejected with an unsupported backend diagnostic before LLVM is emitted.
`emit-native` adds the process contract used by native execution tests. It emits
a C ABI `i32 @main()` wrapper that calls the checked zero-argument `.mlfp`
`main`, renders supported pure results to stdout with the same value text used
by `run-program`, prints one trailing newline, writes no stderr on success, and
returns exit status `0`. Native rendering currently supports `Int`, `Bool`, and
first-order ADT results whose fields are recursively renderable. Function,
polymorphic, `String`, unknown, and IO-like results are rejected before native
run assertions use them. Native mode declares libc `malloc` and vararg `printf`
and defines the backend-owned `__mlfp_and` primitive when no program binding owns
that runtime name; broader IO runtime linking remains outside this pure
contract.

Backend LLVM validation tests use LLVM command-line tools with opaque pointer
support when available. The test suite looks for `llvm-as` and `llc` on `PATH`,
with standard Homebrew LLVM locations also accepted on macOS. Native executable
runner tests additionally look for a C compiler/linker via `CC`, then `cc`,
`clang`, or `gcc` on `PATH`. LLVM-dependent assertions are marked pending when
required tools are absent, so `cabal test` can run in environments without LLVM
or a native linker while still exercising the checks wherever the tools are
installed. LLVM 14 tools are run with `-opaque-pointers` when needed, while LLVM
15+ tools accept the emitted opaque-pointer IR by default.

## Syntax and paper alignment

- Canonical syntax spec: `docs/syntax.md`
- `.mlfp` language reference: `docs/mlfp-language-reference.md`
- Implementation notes and thesis alignment: `implementation_notes.md`
- Roadmap: `roadmap.md`
- Known issues / faithfulness gaps: `Bugs.md`

## Validation command

```bash
cabal build all && cabal test
```
