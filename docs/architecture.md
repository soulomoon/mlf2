# Architecture / Repo Layout

This repository implements the MLF → xMLF pipeline described in `papers/these-finale-english.txt` (see also `papers/xmlf.txt`).
Goal: keep the implementation paper-faithful to the thesis and document any deviations; use `papers/xmlf.txt` only as supplementary detail when the thesis is silent.

## Public API (downstream users)

Downstream code should import:

- `MLF.API` — umbrella module (surface syntax + pipeline entry points + xMLF result types + xMLF checking/reduction helpers)
- `MLF.Pipeline` — pipeline entry points + xMLF checking/reduction helpers (e.g. `inferConstraintGraph`, `runPipelineElab`, `typeCheck`, `step`, `normalize`)

`MyLib` remains as a legacy compatibility wrapper that re-exports `MLF.API`.

Public modules live under `src-public/` and the public Cabal library only exposes:

- `src-public/MLF/API.hs`
- `src-public/MLF/Pipeline.hs`
- `src-public/MyLib.hs`

## Internal implementation (package-private)

All implementation modules live under `src/` and are built as a private sublibrary:

- Cabal sublibrary: `library mlf2-internal` with `visibility: private`

The code is organized by domain (not by phase) under `src/MLF/`:

- `MLF.Frontend.*` — surface syntax, desugaring, constraint generation
- `MLF.Constraint.*` — constraint graph types + normalize + acyclicity + presolution + solve
- `MLF.Binding.*` — binding tree queries + executable χe ops + harmonization
- `MLF.Witness.*` — ω execution helpers (base χe operations)
- `MLF.Elab.*` — elaboration to xMLF (Φ/Σ translation, reify/generalize, plus xMLF typechecking/reduction)
- `MLF.Util.*` — shared utilities (order keys, union-find, etc.)

## Witness Representation (Φ/Σ)

- `EdgeWitness.ewWitness` stores the Ω-only instance operations (Graft/Merge/Raise/Weaken/RaiseMerge).
- `EdgeWitness.ewSteps` stores the interleaved step stream used for Φ translation, including `StepIntro` (O) in expansion order and `StepOmega` for Ω operations.
- `EdgeTrace` (per instantiation edge) tracks the expansion root, binder→argument pairs, interior `I(r)`, and copy-map provenance used by `phiFromEdgeWitnessWithTrace`.

## Executable

- `app/Main.hs` builds the `mlf2` binary (demo runner).

## Tests

The test suite depends on both:

- `mlf2` (public library) and
- `mlf2:mlf2-internal` (private internal library)

This keeps the downstream surface small while still allowing specs to import internal modules.
