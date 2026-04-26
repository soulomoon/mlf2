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
- `MLF.Frontend.Program.Run` evaluates checked bindings through the existing xMLF reducer; static module/import/data/class validation may still fail before the eMLF pipeline
- `MLF.Backend.IR` defines the private typed backend IR boundary and local invariant validator for checked `.mlfp` programs before textual or LLVM-like lowering

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
