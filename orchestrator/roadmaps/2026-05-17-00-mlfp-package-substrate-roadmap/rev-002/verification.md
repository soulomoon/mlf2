# Verification Contract

Roadmap family: `2026-05-17-00-mlfp-package-substrate-roadmap`
Revision: `rev-002`
Contract: `orchestrator-v2`

## Baseline Checks

Every round must satisfy the checks matching its touched scope.

1. **Build and test gate**
   - For behavior-changing work, run `cabal build all && cabal test` before
     approval.
   - While iterating, run the narrowest focused test slice that covers package,
     module, interface, CLI, or fixture behavior before expanding to the full
     gate.
   - Package/module rounds should consider focused coverage under
     `test/ProgramSpec.hs`, `test/PublicSurfaceSpec.hs`,
     `test/BackendEmissionPrepareSpec.hs`, `test/BackendConvertSpec.hs`,
     `test/BackendLLVMSpec.hs`, and package-specific specs added by the round.
   - Parser/API rounds should consider `test/FrontendParseSpec.hs`,
     `test/FrontendPrettySpec.hs`, `test/XMLFParseSpec.hs`, and public API
     adapter tests when the selected change touches those entrypoints.
   - Fixture migration rounds should run the affected fixtures under
     `test/programs/` plus the parity tests that consume them.
   - Full type-level handoff merge rounds must treat prior handoff validation
     as stale evidence until it is rerun on the reconciled current-base result.

2. **Diff hygiene**
   - Run `git diff --check`.
   - Verify no orphaned imports, unregistered modules, duplicate module
     resolver policy, or stale one-file compatibility helpers remain.
   - Verify new modules under `src/`, `src-public/`, or `test/` are registered
     in `mlf2.cabal`; new spec modules must also be registered in
     `test/Main.hs`.
   - Verify the dirty detached handoff checkout is preserved on an intentional
     branch before staging, and verify the handoff-untracked files are present
     in the reconciled diff.

3. **Scope discipline**
   - Confirm the selected round stays inside the milestone and direction named
     by `selection-record.json`.
   - Confirm production public surfaces under `src-public/` are unchanged
     unless the selected direction explicitly requires package-facing public
     API work.
   - Confirm `docs/architecture.md` is updated when package/module/interface
     ownership or CLI/API responsibilities change.
   - Confirm one-file `.mlfp` behavior is handled as trivial package input,
     not as a second durable semantic path.

4. **Lineage and closeout**
   - Confirm `selection-record.json`, `round-plan-record.json`, and any
     approving `review-record.json` name the active `roadmap_id`,
     `roadmap_revision`, and `roadmap_dir`.
   - Confirm `review-record.json` validates against
     `orchestrator/round-finalization-schema.md`.
   - If closeout is status-only, confirm every selector resolves through
     `roadmap-view.json` anchors and does not change future coordination.
   - If future coordination, compatibility posture, package-mode scope, or
     verification meaning changes, require semantic-update mode.

## Alignment Checks

- The round must move `.mlfp` toward one package/module source of truth or
  produce a concrete no-change justification tied to the current code.
- The round must not preserve the old one-file model as a separate durable
  semantic mode. Existing examples may stay only through trivial package
  handling.
- The round must preserve thesis-facing eMLF/xMLF ownership and keep `.mlfp`
  package semantics in the program/package layer.
- The round must not introduce broad compatibility shims, duplicate resolvers,
  public package facades, package-manager scope, public backend IR layers,
  lazy-runtime/STG machinery, or broad FFI lanes unless a semantic roadmap
  update explicitly authorizes the scope.
- The round must keep support-layer claims separate: source checking,
  interpreter/runtime, backend/native, object code, and package build mode.
- Full type-level handoff work must preserve the distinction between source
  type-level support, erased core/backend behavior, native fail-closed cases,
  and published public contracts.
- Tests must remain real assertions. Do not replace package/module/interface,
  CLI, backend, or parity coverage with smoke-only checks.
- Worker fan-out, if used, must be justified by non-overlapping ownership in
  `round-plan-record.json` and reviewed only after integrated verification.

## Task-Specific Checks

- **milestone-1 (Package Module Owner)**
  - Verify package identity, module identity, source-unit shape, and
    trivial-package handling have one owner or explicit owner map.
  - Verify current one-file entrypoints go through that owner rather than
    retaining independent module semantics.
  - Verify focused tests prove existing single-file examples still check/run as
    trivial packages.

- **milestone-2 (Filesystem Discovery And Dependency Graph)**
  - Verify package roots discover `.mlfp` modules across files with stable
    source paths and diagnostics.
  - Verify dependency ordering is explicit and module cycles fail closed.
  - Verify cross-file imports respect exports, aliases, abstract types, and
    hidden constructors.

- **milestone-3 (Interface Artifact Contract)**
  - Verify interface artifacts or typed interface representations cover checked
    values, abstract types, constructors, class methods, instances, and
    dependency metadata needed by package imports.
  - Verify stale or malformed interface artifacts fail closed.
  - Verify interfaces are not treated as a second typechecker authority.

- **milestone-4 (Package Roots, Search Paths, Cache, And Build Graph)**
  - Verify package roots and search paths are explicit and deterministic.
  - Verify cache invalidation keys depend on source/interface metadata rather
    than ad hoc file checks.
  - Verify stale cache and changed dependency behavior has focused coverage.
  - Verify no package-manager or remote-dependency scope is introduced.

- **milestone-5 (CLI And Public Entry Point Migration)**
  - Verify `run-program`, `emit-backend`, `emit-native`, and public `.mlfp`
    check/runtime adapters use package mode.
  - Verify `MLF.Program.CLI` stays command/file orchestration while semantic
    package preparation stays package-owned.
  - Verify package CLI modes can check/run/build across files.
  - Verify no public lowering API is added solely for tests.

- **milestone-6 (Fixture Migration And Self-Boot Readiness Evidence)**
  - Verify `.mlfp` fixture corpus runs through package mode.
  - Verify README, `docs/mlfp-language-reference.md`, and `docs/syntax.md` no
    longer present one-file programs as the durable model.
  - Verify a self-boot readiness ledger records remaining primitives, stdlib,
    native driver, backend lowerability, and compiler-in-`.mlfp` gaps by
    layer.
  - Verify the next-family recommendation is explicit and does not claim full
    self-boot completion.

- **milestone-7 (Full Type-Level MLFP Handoff Merge And Publication)**
  - Verify `/Users/ares/.codex/worktrees/b648/mlf4` is preserved on an
    intentional branch or equivalent before any staging, reset, or merge
    operation.
  - Verify the three handoff-untracked files are included:
    `src/MLF/Frontend/Program/TypeFamilies.hs`,
    `src/MLF/Frontend/TypeLevel.hs`, and `test/FrontendTypeLevelSpec.hs`.
  - Verify kinded type-level frontend support, Unicode type lambdas, closed
    type families, multi-parameter typeclasses, superclasses, functional
    dependencies, and kind-erased variable-headed type application are covered
    by real tests.
  - Verify module/import/export scoping for type families, RHS variable scope,
    simultaneous substitution, delayed lambda-body normalization, and
    cycle/fuel/stuck diagnostics are covered by focused regressions.
  - Verify core/backend variable-headed type application behavior is either
    accepted through supported paths or fails closed with explicit diagnostics.
  - Verify native IO method support matches the handoff contract:
    `Functor IO.map` supported where renderable, `Applicative IO.ap`
    fail-closed if function-valued `IO` results remain non-renderable.
  - Verify docs and public contracts reflect the reconciled implementation
    without overclaiming separate compilation, stable ABI, package manager,
    remote dependencies, or full compiler-in-`.mlfp` self-hosting.
  - Verify generated depfile churn, especially
    `runtime/mlfp_io/target/release/libmlfp_io.d`, is absent from the final
    diff unless intentionally changed for source reasons.
  - Required gates after reconciliation: `git diff --check`, conflict-marker
    scan over `AGENTS.md CHANGELOG.md README.md docs src src-public test
    mlf2.cabal runtime scripts`, `cabal build all`, `cabal test`, and
    `./scripts/thesis-conformance-gate.sh`.

## Manual Checks

- For package/interface rounds, manually inspect diagnostics to ensure they
  identify the package, module, source file, or interface artifact that failed.
- For docs-only rounds, manually check that wording does not claim self-boot
  readiness beyond current source, tests, and reviewer evidence.
- For CLI/API rounds, manually compare old single-file fixture behavior with
  trivial package behavior so migration does not hide a semantic split.
- For readiness closeout, manually classify remaining gaps by layer: source
  checking, interpreter/runtime, backend/native, package build mode, and
  compiler-in-`.mlfp` implementation.
- For full type-level handoff merge, manually inspect the preserved branch
  state, broad staged diff, public-surface changes, and support-layer wording
  before approving publication or merge.
- For roadmap closeout, manually compare `roadmap.md` and `roadmap-view.json`
  to confirm milestone statuses, dependencies, direction ids, and anchors
  agree.

## Roadmap Overrides

- Retry behavior for this family should stay same-round by default while the
  selected plan remains valid and the retry stays within the selected
  milestone, direction, branch, and worktree.
- After three rejected attempts for the same round, require recovery review
  before another same-mechanism retry.
- If a retry would preserve one-file semantics as a separate path, change the
  package-mode replacement posture, add package-manager scope, widen into
  primitives/stdlib/native-driver work, or start compiler-in-`.mlfp`
  implementation, classify it as `semantic-update-required` instead of
  continuing as status-only closeout.
- For milestone-7, same-round retries are valid while preserving and
  reconciling the named handoff. If the dirty checkout cannot be preserved, the
  branch cannot be reconciled with current `master`, or validation cannot be
  rerun, record a precise blocker rather than staging a partial merge.
