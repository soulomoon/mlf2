# mlf4 (mlf2)

An implementation of MLF inference and elaboration, aligned to:

- `papers/these-finale-english.txt` (primary source of truth)
- `papers/xmlf.txt` (supplementary where thesis is silent)

The pipeline takes eMLF terms, builds and solves graphic constraints, and elaborates to explicit xMLF terms.

## Repository layout

- `src/` — internal implementation (`MLF.Frontend.*`, `MLF.Constraint.*`, `MLF.Elab.*`, etc.)
- `src-public/` — public API modules (`MLF.API`, `MLF.Pipeline`, `MLF.XMLF`)
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

| Job                  | What it runs                                                           |
|----------------------|------------------------------------------------------------------------|
| `build-and-test`     | installs LLVM tools, then `cabal build all` and `cabal test`           |
| `thesis-conformance` | installs LLVM tools, builds all targets, then runs the thesis gate     |

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
- `MLF.Pipeline` — canonical public constraint-generation / elaboration / runtime API, including local `.mlfp` package discovery/checking/runtime on the shared eMLF/xMLF path
- `MLF.XMLF` — xMLF syntax, parser, and pretty-printer

Raw frontend source types also accept Unicode type lambdas (`Λa. τ`) and
explicit type-lambda applications when normalization can beta-reduce them away
before core erasure. Residual type lambdas and non-normalized general type
applications remain fail-closed at the core/backend boundary.

The `.mlfp` parser, pretty-printer, and checker also accept closed `type
family` declarations with ordered equations and kind-variable family kinds.
Reducible family applications normalize and erase before the resolver/core
boundary; stuck and cyclic reductions fail with explicit diagnostics.

Active multi-step work is tracked under `tasks/todo/`; root-level `task_plan.md`,
`findings.md`, and `progress.md` are historical leftovers rather than the active
workflow.

## Unified `.mlfp` program surface

`.mlfp` programs now share the main frontend/pipeline ownership boundary:

- `MLF.API` parses and pretty-prints `.mlfp` programs
- `MLF.Pipeline` elaborates `.mlfp` programs and local `.mlfp` packages onto the existing eMLF/xMLF path and checks them with the existing MLF typechecker

The unified `.mlfp` surface includes:

- `module` / `import ... exposing (...)` plus qualified `import ... as ...`
- `data` declarations with explicit constructor signatures
- `case` / constructor pattern matching
- recursive GADT-style constructor result types
- existential constructors via `∀`
- single-parameter typeclasses, class constraints, schema instances,
  zero-method multi-parameter class evidence, and method-bearing
  multi-parameter dispatch when each class argument is fixed by supplied
  arguments, expected result type, local evidence, or functional-dependency
  closure; superclass constraints are flattened into method evidence and
  instance prerequisites, and ambiguous/fundep-conflicting instance shapes fail
  closed with structured diagnostics
- closed `type family` declarations that normalize and erase before the core
  boundary
- `deriving Eq` for nullary, recursive, and parameterized ADTs whose fields have Eq evidence
- a built-in Prelude with `Nat`, `Option`, `List`, `Eq`, `Show`, `Unit`,
  opaque `IO`, coherent `Functor IO` / `Applicative IO` / `Monad IO`,
  `map`, `pure`, `ap`, `bind`, `putStrLn`, `and`, and `id`

`.mlfp` modules are same-local-package modules today. The durable execution
model is local package mode: imports resolve among modules discovered under the
selected local root plus ordered `--search-path` roots. Passing one `.mlfp`
file to the CLI is still supported, but it is represented as one trivial
package source unit rather than a separate one-file semantic mode. The CLI adds
the built-in Prelude as an explicit module. Separate module compilation and
persisted interface files are not supported yet; see
`docs/mlfp-language-reference.md` for the user-facing module contract.

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
Self-boot readiness and remaining compiler-in-`.mlfp` gaps are tracked in
`docs/mlfp-self-boot-readiness.md`. The accepted Full Self-Boot ordering is
`docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`: after the current
readiness-ledger baseline, implementation starts with the shared file-based
conformance corpus, not direct compiler self-boot.

### Compiler frontend seed fixture

The current compiler-in-`.mlfp` proof is the ordinary local package root at
`test/programs/compiler-seed/frontend-contract/`. It contains `.mlfp` source
modules for a frontend seed contract, symbolic source spans/input, tokens,
lexer diagnostics/results, a tiny AST, parser diagnostics/results, and a
`Main` module that prints asserted lexer and parser evidence.

Run the seed through the same package entrypoints as any local package:

```bash
cabal run mlf2 -- check-program test/programs/compiler-seed/frontend-contract
cabal run mlf2 -- run-program test/programs/compiler-seed/frontend-contract
```

The expected interpreter evidence is:

```text
lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol
parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true
```

`ProgramCompilerSeedSpec` also checks package discovery, module graph order,
source paths, backend/native LLVM emission, object-code generation, and linked
native execution for this bounded seed. That evidence is not a self-hosting
claim: it does not provide a source-text lexer/parser, checker-in-`.mlfp`,
optimizer, backend-in-`.mlfp`, package manager, stable ABI/linker, separate
compilation mode, shared conformance corpus, compiler driver, first-proof
workflow, or arbitrary compiler workload native support.

The first-class-polymorphism program example lives at
`test/programs/unified/first-class-polymorphism.mlfp`.

Check a local package root from the executable:

```bash
cabal run mlf2 -- check-program test/programs/packages/cross-module-let
```

Set `MLF_PROGRAM_TIMING=1` to emit package-loading and frontend-check stage
timings to stderr while preserving normal stdout:

```bash
MLF_PROGRAM_TIMING=1 cabal run mlf2 -- check-program test/programs/packages/cross-module-let
```

Use `MLF_PROGRAM_TIMING_DETAIL=1` when the `program.check.modules` stage needs
per-module and per-module-phase timings:

```bash
MLF_PROGRAM_TIMING_DETAIL=1 cabal run mlf2 -- check-program test/programs/packages/cross-module-let
```

Use `MLF_PROGRAM_TIMING_OPERATIONS=1` only for narrow profiling runs. It
includes the detail timings plus per-constructor, per-instance-method, and
per-definition timings:

```bash
MLF_PROGRAM_TIMING_OPERATIONS=1 cabal run mlf2 -- check-program test/programs/packages/cross-module-let
```

Run a local package root directly from the executable:

```bash
cabal run mlf2 -- run-program test/programs/packages/cross-module-let
```

Additional local package roots are searched in order:

```bash
cabal run mlf2 -- run-program test/programs/packages/search-path-main --search-path test/programs/packages/search-path-lib
```

A single `.mlfp` file remains a trivial package input:

```bash
cabal run mlf2 -- run-program test/programs/recursive-adt/plain-recursive-nat.mlfp
```

Emit LLVM IR for a checked local package:

```bash
cabal run mlf2 -- emit-backend test/programs/packages/cross-module-let
```

Emit LLVM IR with a native process entrypoint:

```bash
cabal run mlf2 -- emit-native test/programs/packages/cross-module-let
```

The `.mlfp` CLI accepts a file as a trivial local package source unit or a
directory as a local package root for `check-program`, `run-program`,
`emit-backend`, and `emit-native`. This is local filesystem package projection
only: there is no package manager, registry, remote dependency solver, stable
`.mlfp` ABI, or linker guarantee.

`emit-backend` keeps the raw backend contract: the checked `.mlfp` `main`
binding remains a module-qualified LLVM function such as `Main__main`.
`emit-native` adds the process contract used by native execution tests. It emits
a C ABI `i32 @main()` wrapper that calls the checked zero-argument `.mlfp`
`main`; for `main : IO Unit`, it executes the IO action closure and exits
without rendering a result. For pure mains it renders supported results to
stdout with the same value text used by `run-program`, prints one trailing
newline, writes no stderr on success, and returns exit status `0`. Native
rendering currently supports `Int`, `Bool`, `Char`, `String`, and first-order
ADT results whose fields are recursively renderable. Function-valued, polymorphic,
variable-headed, and structurally recursive main results without a named data
runtime are rejected before native run assertions use them. Native mode declares
libc `malloc`, vararg `printf`, `putchar`, and defines the backend-owned
`__mlfp_and` plus primitive IO runtime wrappers when no program binding owns
those runtime names. The full test pipeline is documented in
`docs/backend-native-pipeline.md`.

Backend LLVM validation tests use LLVM command-line tools with opaque pointer
support. The test suite requires `llvm-as` and `llc` on `PATH`, with standard
Homebrew LLVM locations also accepted on macOS. Native executable runner tests
additionally require a C compiler/linker via `CC`, then `cc`, `clang`, or `gcc`
on `PATH`. CI installs Ubuntu `llvm` and `clang` packages before running these
checks. LLVM 14 tools are run with `-opaque-pointers` when needed, while LLVM 15+
tools accept the emitted opaque-pointer IR by default.

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
