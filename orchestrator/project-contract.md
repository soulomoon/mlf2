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
- Accepted ADRs are binding context for future architecture rounds. Legacy
  Surface Retirement and Backend Structural Recursive Data Matching are settled
  directions, not fresh candidate-discovery topics.
- Architecture deepening rounds should consolidate duplicated ownership into
  existing domain owners and `docs/architecture.md` before introducing new
  owner modules; any new owner must be justified by a selected roadmap
  direction and covered by focused tests or review evidence.
- Package-substrate work for `.mlfp` should treat package/module semantics as
  the intended durable program model. Existing one-file examples may remain
  only as trivial package inputs, not as a second semantic mode with its own
  resolver/checker contract.
- Compatibility promises: no durable promise to preserve the old one-file
  `.mlfp` program model as a separate mode.
- Compiler-in-`.mlfp` prerequisite work should begin from ordinary package-mode
  compiler source modules and interpreter-runnable frontend seed evidence.
  Native/backend behavior must be classified by layer and must not be forced
  into the first seed unless a selected roadmap direction authorizes it.
- Self-boot readiness claims must stay layer-separated: source checking,
  interpreter/runtime, backend/native, object code, package build mode, and
  compiler-in-`.mlfp` implementation.
- Full Self-Boot follows the accepted staged order in
  `docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`; the active
  control-plane family keeps the entire effort together, and implementation
  still begins with readiness alignment and Pre-Self-Boot Test Migration before
  broad string/parser work, platform ABI work, or direct compiler
  implementation.
- Pre-Self-Boot Test Migration owns the Shared File-Based Compiler Conformance
  Corpus under `test/conformance/mlfp/`. It is a behavior-preserving shared
  oracle for both compiler implementations, not a second parser, checker,
  resolver, backend, or compatibility mode.
- Conformance fixtures must use metadata for package roots, command modes,
  pass/fail status, normalization profiles, stage applicability, and
  behavioral tags. Stage-inapplicable or unsupported cases belong in metadata,
  not ad hoc skips.
- Committed conformance expected outputs are reviewed oracle artifacts.
  Ordinary test runs must not dynamically regenerate or accept new expected
  outputs.
- Internal implementation invariants enter the conformance corpus only through
  honest user-visible behavioral projections; private invariants without such a
  projection remain Haskell tests.
- Full canonical `.mlfp` parser parity depends on native-capable broad
  Unicode-scalar `Char` and `String` support; parser-private text helpers or
  interpreter-only string support cannot satisfy that stage.
- First self-boot platform work must keep trusted substrate explicit,
  versioned, fingerprinted, and shared by both compiler implementations through
  stable contracts. Trusted substrate provides capabilities, not compiler
  semantics.
- First self-boot proof is conformance-first and compares normalized semantic
  artifacts after both stages pass the declared suite. Native object and
  executable bytes are regenerated and recorded but are not the first-proof
  equality oracle.
- Self-boot stages must use checked locked local package identities, shared
  local packages, declared substrate fingerprints, and stage-owned output
  directories. Cross-stage reuse of generated semantic, backend, object,
  executable, link, native-execution, or conformance-output caches invalidates
  proof evidence.

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
- Thesis-conformance preserving work should keep
  `./scripts/thesis-conformance-gate.sh` green when the selected round touches
  thesis obligations, language semantics, compiler seed behavior, or roadmap
  closeout evidence that depends on the thesis ledger.
- Architecture deepening rounds must name the ownership boundary they changed,
  prove that callers now depend on that owner instead of duplicating policy,
  and update `docs/architecture.md` when module ownership changes.
- Package-substrate rounds must prove package/module/interface behavior with
  focused tests before the full gate, and must classify user-facing support by
  source checking, interpreter/runtime, backend/native, object code, and
  package build mode when those layers differ.
- Compiler frontend seed rounds must prove executable `.mlfp` seed behavior
  through focused assertions, keep unsupported native/backend behavior
  fail-closed, and update readiness docs without claiming full self-hosting.
- Conformance-corpus rounds must prove migrated fixtures through focused
  metadata and expected-output assertions, preserve current public behavior by
  default, and keep normalization narrow and explicit.
- Conformance-corpus closeout must keep `test/conformance/mlfp/` usable as a
  shared oracle for the current Haskell compiler and a future `.mlfp` compiler:
  fixture metadata, expected files, and actual-output policy must not depend on
  hidden Haskell-only defaults.
- Broad text and parser-parity rounds must prove behavior through source
  checking, interpreter/runtime, backend emission, object generation, and native
  execution when the selected stage requires native-capable support.
- Platform-contract rounds must prove generated binding drift checks, ABI
  version/target/substrate identity, managed GC/root policy, FFI
  ownership/error/export/callback rules, lock validation, toolchain identity,
  native link records, native execution records, and ambient-input policy as
  applicable to the selected slice.
- First-proof rounds must record stage command, link, native execution,
  conformance, and comparison evidence with stable proof action IDs and a
  machine-readable failure taxonomy.

## Update Rule

Update this file only when the repo-wide invariant itself changes. When a
roadmap temporarily narrows or extends an invariant, record the override in the
active roadmap bundle and keep the durable rule here.
