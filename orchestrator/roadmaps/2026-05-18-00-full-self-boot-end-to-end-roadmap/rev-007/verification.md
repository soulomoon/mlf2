# Verification

This file tailors verification for the active full-self-boot roadmap revision.
The shared process contract lives in `orchestrator/active-roadmap-bundle.md`,
`orchestrator/artifact-manifest.md`, `orchestrator/role-contract.md`, and the
role prompts under `orchestrator/roles/`.

Planner owns `Complexity` and `Verification profile` in `plan.md`.
`orchestrator/active-roadmap-bundle.md` owns the canonical Round Execution
Profiles rubric. Verification profile describes surrounding evidence needs. A
simple task may still require standard or closeout verification, and the
planner must complete that task directly when it chooses `Complexity: simple`.

## Baseline Checks

- Every round must run `git diff --check` before finalization.
- Behavior-changing Haskell work must run the narrowest focused test slice
  named in `plan.md` before broader gates.
- Before claiming behavior-changing work complete, run
  `cabal build all && cabal test` unless `plan.md` and final evidence justify a
  narrower focused profile for a non-closeout slice.
- Pure guidance or control-plane edits do not require the full Cabal gate, but
  they must still pass `git diff --check` and must not overclaim runtime or
  implementation evidence.
- Any work touching thesis obligations, language semantics, compiler seed
  behavior, readiness ledgers, parser/checker contracts, backend/native
  behavior, package substrate, platform substrate, or proof evidence must run
  or explicitly justify deferring `./scripts/thesis-conformance-gate.sh`.
- New modules under `src/`, `src-public/`, or `test/` require corresponding
  `mlf2.cabal` updates. New spec modules require both `mlf2.cabal` and
  `test/Main.hs` wiring.

## Alignment Checks

- Source of truth remains `papers/these-finale-english.txt`; thesis
  faithfulness wins over convenience.
- Do not add compatibility aliases, broad fallback bridges, or legacy parser
  shims unless a selected direction and thesis-backed design require them.
- Preserve the staged full-self-boot order unless a delegated semantic
  `update-roadmap` revision changes the roadmap.
- Keep claims layer-separated: source checking, interpreter/runtime,
  backend/native, object code, package build mode, and proof comparison.
- Shared conformance fixtures must stay useful to both the current Haskell
  compiler and a future `.mlfp` compiler.
- Trusted substrate work must expose capabilities, versions, fingerprints,
  locks, toolchains, and ambient inputs explicitly; it must not smuggle
  compiler semantics through host behavior.
- First-proof evidence compares normalized semantic artifacts after both stages
  pass the declared suite. Native object and executable bytes are recorded but
  are not the primary first-proof equality oracle.

## Task-Specific Checks

- `focused` profile:
  - Run `git diff --check`.
  - Run the focused commands named by `plan.md`.
  - Explain why the selected slice does not require full closeout gates.
- `standard` profile:
  - Run `git diff --check`.
  - Run focused commands for the touched owner surface.
  - Run `cabal build all && cabal test` for behavior-changing source changes,
    or record the concrete reason a narrower standard gate is sufficient for
    documentation/control-plane-only work.
  - Run `./scripts/thesis-conformance-gate.sh` when the slice touches
    thesis-facing behavior or self-boot readiness claims.
- `closeout` profile:
  - Run `git diff --check`.
  - Run all focused commands that support the milestone closeout claim.
  - Run `cabal build all && cabal test`.
  - Run `./scripts/thesis-conformance-gate.sh`.
  - Record the milestone selector, target status, completion pointer, and any
    compact `roadmap-history.md` entry in the final role artifact.
- Parser parity slices:
  - Use one aggregate parser/conformance run when multiple simple cases share
    parser owner surface, verification command, and failure mode.
  - Add negative coverage when the selected syntax family has meaningful reject
    behavior.
  - Do not bypass the canonical parser or satisfy parity through private helper
    parsing.
- Checker-facing parser work:
  - Prefer one honest aggregate program run for shared-context cases instead of
    separate synthetic runs that hide cross-definition behavior.
  - Keep checker policy changes on the roadmap path rather than folding them
    into parser-only slices.
- Compiler-seed/parser ergonomics substrate slices:
  - Name the reusable substrate surface being improved: parser
    state/result/combinator helpers, string/char/stream/list APIs,
    source-span/diagnostic helpers, or a narrowly justified reduction of
    repeated case/lambda plumbing.
  - Prove the slice through the narrowest honest owner-surface command named
    in `plan.md`, such as compiler-seed frontend-contract `check-program` /
    `run-program`, the aggregate parser-parity Hspec group, or focused helper
    tests for the touched library surface.
  - Preserve non-claims explicitly: bounded seed/parity evidence is not full
    parser parity, compiler-package implementation, platform/proof progress,
    native/backend completion, package-manager/linker work, or self-boot
    completion.
  - Reject fixture-name shortcuts, pre-rendered parser outputs, canonical
    parser bypasses, retired syntax aliases, and parser-private helpers that
    hide missing reusable substrate.
- Package/platform/proof slices:
  - Name generated records, substrate fingerprints, lock identities, toolchain
    identities, and stage output directories when the selected surface touches
    them.
  - Do not reuse proof-stage generated semantic, backend, object, executable,
    link, native-execution, or conformance-output caches across stages.

## Manual Checks

- Review changed docs for overclaims against current implementation evidence.
- Confirm `plan.md`, `implementation-notes.md`, and `review.md` use structured
  Markdown fields as authority; do not create paired JSON records.
- For `Complexity: simple`, confirm planner-authored `implementation-notes.md`
  includes `Direct Verification` and, when applicable, `Direct Closeout`.
- For non-simple work, confirm reviewer-authored `review.md` records approval,
  retry target, verification profile assessment, and closeout classification.
- Confirm status-only closeout edits only change milestone status markers,
  completion pointers, and compact history entries.
- Confirm semantic coordination changes use delegated `update-roadmap` and a
  new roadmap revision.

## Roadmap Overrides

- Parser/conformance additions should be batched when their goals,
  implementation paths, verification boundaries, owner surfaces, and failure
  modes are shared.
- Routine parser-parity wiring, fixture registration, expected projections,
  negative cases, shortcut guards, docs, and aggregate parser batch enrollment
  do not by themselves make a parser-library slice non-simple.
- Parser-library changes remain simple when they follow an established shared
  parser path and the selected evidence directly proves the slice. Treat them
  as non-simple only when the task content itself needs new design judgment,
  changes structural shared parser behavior, introduces a new abstraction or
  failure mode, or lacks a clear local verification boundary.
- In rev-007, milestone-4 planners may select ergonomics/library substrate
  before another syntax-family parser-parity slice when current evidence shows
  source forms are expressible but parser/compiler-frontend authoring remains
  too fixture-shaped or boilerplate-heavy.
- Bounded parser-parity evidence through round 339 is useful regression
  evidence and seed-path evidence, but it must not be used as full parser
  parity closeout or as authorization for compiler-package, platform, driver,
  proof, native/backend, package-manager, linker, or self-boot work.
- Planner may directly implement and close simple tasks, including simple docs,
  fixture, parser-library, or control-plane maintenance, when it also runs the
  selected verification profile and records direct evidence.
- Runtime must skip implementer and reviewer dispatch for planner-completed
  simple tasks with passing direct verification evidence.
- Runtime must delegate implementer and reviewer only for non-simple work.
- Retry policy beyond the shared runtime mechanics: none.
