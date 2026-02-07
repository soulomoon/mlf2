# mlf4 (mlf2)

An implementation of MLF inference and elaboration, aligned to:

- `papers/these-finale-english.txt` (primary source of truth)
- `papers/xmlf.txt` (supplementary where thesis is silent)

The pipeline takes eMLF terms, builds and solves graphic constraints, and elaborates to explicit xMLF terms.

## Repository layout

- `src/` — internal implementation (`MLF.Frontend.*`, `MLF.Constraint.*`, `MLF.Elab.*`, etc.)
- `src-public/` — public API modules (`MLF.API`, `MLF.Pipeline`, `MLF.XMLF`, `MyLib`)
- `app/` — executable entrypoint (`mlf2`)
- `test/` — Hspec test suite
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
cabal run mlf2
cabal repl mlf2
cabal repl mlf2-test
```

## Public entry points

- `MLF.API` — surface syntax + pipeline helpers + eMLF parse/pretty
- `MLF.Pipeline` — pipeline-focused API
- `MLF.XMLF` — xMLF syntax, parser, and pretty-printer

## Syntax and paper alignment

- Canonical syntax spec: `docs/syntax.md`
- Implementation notes and thesis alignment: `implementation_notes.md`
- Roadmap: `roadmap.md`
- Known issues / faithfulness gaps: `Bugs.md`

## Validation command

```bash
cabal build all && cabal test
```
