# Verification Contract

Roadmap family: `2026-05-17-00-mlfp-package-substrate-roadmap`
Revision: `rev-001`
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

2. **Diff hygiene**
   - Run `git diff --check`.
   - Verify no orphaned imports, unregistered modules, duplicate module
     resolver policy, or stale one-file compatibility helpers remain.
   - Verify new modules under `src/`, `src-public/`, or `test/` are registered
     in `mlf2.cabal`; new spec modules must also be registered in
     `test/Main.hs`.

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
