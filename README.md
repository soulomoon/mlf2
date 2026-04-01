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

- `MLF.API` — surface syntax, parsing, pretty-printing, and normalization helpers
- `MLF.Pipeline` — canonical public constraint-generation / elaboration / runtime API
- `MLF.XMLF` — xMLF syntax, parser, and pretty-printer

Active multi-step work is tracked under `tasks/todo/`; root-level `task_plan.md`,
`findings.md`, and `progress.md` are historical leftovers rather than the active
workflow.

## Syntax and paper alignment

- Canonical syntax spec: `docs/syntax.md`
- Implementation notes and thesis alignment: `implementation_notes.md`
- Roadmap: `roadmap.md`
- Known issues / faithfulness gaps: `Bugs.md`

## Validation command

```bash
cabal build all && cabal test
```
