# Project Contract

This file records repo-wide invariants shared by every roadmap family and
round. Keep roadmap revisions focused on current coordination; point here for
stable contracts instead of restating them in every role or roadmap file.

## Stable Interfaces

- Source of truth: `papers/these-finale-english.txt`.
- User-facing build and usage guidance: `README.md`.
- Durable architecture and module ownership guidance: `docs/architecture.md`.
- Public Haskell API surfaces: `src-public/MLF/API.hs` and
  `src-public/MLF/Pipeline.hs`.
- Cabal package and module registration: `mlf2.cabal`.
- Test-suite registration: `test/Main.hs`.
- Architectural decisions: `docs/adr/`.
- Event schemas: none discovered yet
- Golden logs and fixtures: repository fixtures under `test/` and
  roadmap-specific examples when selected by a round.
- Dry-run or command-rendering output: none discovered yet
- Package and module boundaries: production internals live under `src/`,
  public facades live under `src-public/`, research-only code lives under
  `src-research/`.
- Public compatibility facades: not a default promise; keep any compatibility
  surface only when the active roadmap or ADR explicitly requires it.

## Alignment Invariants

- Prefer thesis faithfulness over implementation convenience.
- Fix root causes instead of preserving compatibility aliases, parser fallbacks,
  or migration shims unless a paper-backed design requires them.
- Backwards compatibility is not a default project goal.
- Keep the production surface narrow; expose low-level helpers to tests through
  test-support seams rather than widening public facades.
- Type-level conventions in `AGENTS.md` are repo-wide guidance: phase-indexed
  `Constraint`, typed `NodeRefTag` at boundaries, `ForallSpec` binder count
  derived from `fsBounds`, and witness construction through smart
  constructors.
- Explicit non-goals that should not be reopened without a new roadmap family:
  preserving retired legacy syntax as aliases, adding broad raw-view
  compatibility bridges, introducing duplicate public backend IR layers, or
  importing lazy-runtime/STG machinery by default.
- Compatibility promises: none discovered yet

## Verification Anchors

- Behavior-changing rounds must update tests and relevant docs in the same
  change.
- Focused validation should run while iterating; before claiming
  behavior-changing work complete, run `cabal build all && cabal test`.
- Pure guidance/docs edits do not require the full Cabal gate, but reviewers
  must still run `git diff --check` and verify the docs do not overclaim
  implementation evidence.
- Standard local commands include `cabal build`, `cabal test`,
  `cabal repl mlf2`, `cabal repl mlf2-test`, `cabal run mlf2`, and
  `cabal run frozen-parity-gen -- --generated-on YYYY-MM-DD --source-commit <sha>`.
- Builds must remain warning-free; `-Wall` is enabled in `mlf2.cabal`.
- When adding modules under `src/`, `src-public/`, or `test/`, update the
  corresponding `mlf2.cabal` stanza.
- When adding a new spec module, wire it into both `mlf2.cabal` and
  `test/Main.hs`.
- Baseline commands that protect shared contracts: `git diff --check`,
  focused tests named by the round plan, and `cabal build all && cabal test`
  for behavior-changing work.

## Update Rule

Update this file only when the repo-wide invariant itself changes. When a
roadmap temporarily narrows or extends an invariant, record the override in the
active roadmap bundle and keep the durable rule here.
