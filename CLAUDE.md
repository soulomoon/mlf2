# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository implements the MLF → xMLF type inference pipeline described in `papers/these-finale-english.txt`. The goal is to keep the implementation paper-faithful to the thesis and document any deviations.

## Build and Test Commands

```bash
cabal build                              # Build library and executable
cabal test                               # Run the Hspec test suite
cabal test --test-show-details=direct   # Run tests with per-example output
cabal run mlf2                           # Run the demo executable
cabal repl mlf2                          # Open GHCi with library loaded
cabal repl mlf2-test                     # Open GHCi with test suite loaded
```

## Architecture

### Pipeline Phases

The type inference pipeline consists of six phases:

1. **ConstraintGen** (`MLF.Frontend.ConstraintGen`): Builds the constraint graph from surface syntax
2. **Normalize** (`MLF.Constraint.Normalize`): Local rewrites (grafting + merge/unify)
3. **Acyclicity** (`MLF.Constraint.Acyclicity`): Derives dependency order for instantiation edges
4. **Presolution** (`MLF.Constraint.Presolution`): Chooses minimal expansions, records witnesses
5. **Solve** (`MLF.Constraint.Solve`): Discharges remaining unifications via union-find
6. **Elab** (`MLF.Elab.*`): Reifies solved graph and witnesses into xMLF terms/types

### Module Organization

**Public API** (`src-public/`):
- `MLF.API` — umbrella module with surface syntax + pipeline entry points + xMLF types
- `MLF.Pipeline` — pipeline entry points (`inferConstraintGraph`, `runPipelineElab`, `typeCheck`, `step`, `normalize`)

**Internal implementation** (`src/MLF/`):
- `MLF.Frontend.*` — surface syntax, desugaring, constraint generation
- `MLF.Constraint.*` — constraint graph types, normalize, acyclicity, presolution, solve
- `MLF.Binding.*` — binding tree queries, executable χe ops, harmonization
- `MLF.Witness.*` — ω execution helpers (base χe operations)
- `MLF.Elab.*` — elaboration to xMLF (Φ/Σ translation, reify/generalize, typechecking/reduction)
- `MLF.Util.*` — shared utilities (order keys, union-find, etc.)

### Key Data Types

- `Expr` (`MLF.Frontend.Syntax`): Surface language (eMLF terms)
- `Constraint` (`MLF.Constraint.Types`): The constraint graph with nodes, edges, and binding tree
- `TyNode`: Type nodes in the term-DAG (TyVar, TyArrow, TyForall, TyBase, TyExp, TyBottom)
- `InstEdge`: Instantiation edges (≤) between nodes
- `BindParents`: Binding tree as child → (parent, BindFlag) map
- `Expansion`: Presolution recipes (identity, ∀-intro, instantiation, composition)
- `EdgeWitness`: Per-edge witness metadata for xMLF instantiation reconstruction

## Coding Conventions

- 4-space indentation, explicit module export lists
- GHC-style `{- Note [...] -}` blocks for design rationale
- Builds must be warning-free (`-Wall` is enabled)
- Module naming: `src/MLF/Foo/Bar.hs` → `module MLF.Foo.Bar`
- Test naming: `test/FooSpec.hs` defines `spec :: Spec`

## Testing

- Framework: Hspec with QuickCheck
- When adding a new spec module:
  1. Add to `mlf2.cabal` → `test-suite mlf2-test` → `other-modules`
  2. Import and call `spec` in `test/Main.hs`

## Task Management (Ralph)

The `tasks/` folder contains PRDs and progress tracking for autonomous agent execution:

```
tasks/
├── prd.json                    # Current Ralph PRD (JSON format for autonomous execution)
├── prd-*.md                    # Human-readable PRD documentation
├── progress.txt                # Progress log updated by Ralph during execution
└── archive/                    # Completed PRDs organized by date and feature
    └── YYYY-MM-DD-feature-name/
        ├── prd.json
        ├── prd-*.md
        └── progress.txt
```

**For autonomous agents (Ralph):**
- Read `tasks/prd.json` to get the current user stories and acceptance criteria
- Update `tasks/progress.txt` with iteration results
- Stories are ordered by priority/dependency — execute in order
- Each story should be completable in one iteration (one context window)
- Mark `passes: true` in prd.json when a story's acceptance criteria are met

**Validation command:** `cabal build all && cabal test`

## Paper References

- Primary source: `papers/these-finale-english.txt` (thesis)
- Supplementary: `papers/xmlf.txt` (used only when thesis is silent)
- Document any conflicts or deviations from the papers
