# MLFP Package Substrate Roadmap

Roadmap family: `2026-05-17-00-mlfp-package-substrate-roadmap`
Revision: `rev-002`
Base branch: `master`
Created: 2026-05-17
Contract: `orchestrator-v2`

## Goal

Scaffold the first self-boot substrate family for `.mlfp`: a full
module/package layer that replaces the current one-file program model as the
durable `.mlfp` compilation model.

The family does not implement the compiler in `.mlfp` yet. It creates the
package, module, interface, search path, cache/build graph, CLI/API, and
fixture migration foundation needed before later roadmaps can write compiler
components in `.mlfp` without building on duplicate or temporary module
semantics.

## Alignment Summary

- Approved boundary: full self-hosted compiler campaign overall, but this first
  family is the bootstrapping substrate phase.
- Approved first-family scope: full module/package layer.
- Approved sequencing posture: modules first.
- Approved compatibility posture: replace the one-file `.mlfp` program model
  with package mode. Existing one-file examples should become trivial packages
  during migration rather than remain a separate durable semantic mode.
- Chosen strategy: package-first replacement ladder. Establish one package and
  module owner, then migrate interface artifacts, package roots/search paths,
  cache and build graph policy, CLI/API entrypoints, and fixtures through that
  owner.
- Success means the repo has one durable package/module source of truth, not
  competing one-file and package semantics. The CLI/API should expose checked
  multi-file package behavior, interface artifacts and dependency order should
  be validated, and fixtures should prove current one-file programs still work
  through trivial package handling.
- Preserve thesis faithfulness: raw eMLF and xMLF remain thesis-facing; `.mlfp`
  owns modules, packages, diagnostics, runtime value rendering, and package
  build policy.
- Preserve current backend architecture: xMLF remains the elaboration IR and
  `MLF.Backend.IR` remains the executable backend IR seam. Do not introduce a
  second public backend IR, lazy runtime, broad FFI lane, or compatibility
  resolver.
- New rev-002 extension: before starting fresh compiler-in-`.mlfp` work,
  merge and publish the completed full type-level `.mlfp` handoff from
  `/var/folders/36/_743psv11gv2wrj9dclrpd500000gn/T/handoff-XXXXXX.md.obimvbtKjv`.
  Treat its prior validation as useful evidence, but revalidate after
  transplanting it onto current `master`.
- Deferred alternatives after rev-002: primitives-first substrate,
  driver-first substrate, end-to-end vertical seed compiler, and new
  compiler-in-`.mlfp` implementation.

## Outcome Boundaries

- In scope:
  - package identity and package-root model;
  - filesystem module discovery;
  - acyclic package/module dependency ordering;
  - interface artifact shape and validation;
  - import/export resolution across files and package roots;
  - search paths, cache invalidation, and build graph policy;
  - CLI/API entrypoint migration from one-file program handling to package
    handling;
  - current fixture migration so one-file examples run as trivial packages; and
  - readiness evidence for a later compiler-in-`.mlfp` roadmap; and
  - merge/publish triage for the completed full type-level `.mlfp`
    implementation described by the handoff file.
- Out of scope:
  - implementing a compiler in `.mlfp`;
  - broad primitive, string, byte, collection, parser-combinator, or stdlib
    expansion unless it is a direct package-layer blocker;
  - native driver expansion beyond what package build-mode tests need;
  - preserving the old one-file program model as a second supported semantic
    path;
  - public compatibility shims for old module semantics;
  - new public backend IR layers or lazy-runtime/STG machinery.
- Public APIs under `src-public/` may change only when a selected milestone
  proves the package substrate needs that public entrypoint. Keep production
  facades narrow and prefer owner-local adapters plus test-support seams.
- Behavior-changing work must use focused package/module/interface/CLI tests
  while iterating and `cabal build all && cabal test` before approval.
- Docs-only rounds must run `git diff --check` and verify wording does not
  overclaim self-boot readiness beyond produced evidence.

## Global Sequencing Rules

- Default sequencing is serial. This family intentionally replaces the core
  `.mlfp` program model, so owner-boundary work should settle before broad
  migration.
- Milestone 1 must establish the package/module owner before interface or CLI
  work depends on it.
- Milestone 2 must establish discovery and dependency ordering before
  persistent interface artifacts or cache policy can be trusted.
- Milestone 3 must settle interface artifacts before cache invalidation and
  incremental package checks depend on interface meaning.
- Milestone 4 defines package roots, search paths, and cache/build graph policy
  after interfaces exist.
- Milestone 5 migrates CLI/API surfaces after the semantic package owner and
  build graph policy are usable.
- Milestone 6 migrates fixtures/docs and records readiness evidence for the
  next self-boot family.
- Milestone 7 consumes the completed full type-level `.mlfp` handoff only
  after milestone 6 is done. It must attach the dirty detached checkout to an
  intentional branch, transplant or reconcile it with current `master`, and
  re-run the required gates before merge or publication.
- If a round discovers that primitive, stdlib, or native-driver work is a hard
  blocker to the package layer, it should produce a semantic roadmap update
  rather than silently widening the selected milestone.
- Status-only closeout may update milestone status and compact completion
  pointers only. Any change to package-mode posture, sequencing, scope,
  verification, or compatibility policy requires semantic roadmap update.

## Parallel Lanes

- `lane-package-core`: package identity, module owner, discovery, dependency
  graph, and resolver integration.
- `lane-interface-build`: interface artifacts, cache invalidation, build graph,
  and package root/search path policy.
- `lane-cli-api-fixtures`: CLI/API entrypoints, fixtures, docs, and migration
  evidence.
- `lane-type-level-handoff`: merge/publish work for completed type-level
  `.mlfp` feature branches that must be reconciled with the active package
  substrate before publication.
- `lane-docs`: documentation-only alignment that does not change source,
  tests, roadmap coordination, or active round state.

The current controller keeps `max_parallel_rounds` at `1`. These lanes are
ownership hints for future planner reasoning, not permission to run concurrent
rounds unless controller state and a selected plan explicitly allow it.

## Milestones

### [done] 1. Package Module Owner

- Milestone id: `milestone-1`
- Depends on: none
- Intent: define one internal owner for `.mlfp` package/module semantics and
  route current program checking through it without preserving one-file
  semantics as a separate durable contract.
- Completion signal: package identity, module identity, source-unit shape, and
  trivial-package handling have one owner or explicit owner map; current
  in-memory one-file programs enter through that owner; focused tests prove
  existing single-file examples still check through trivial package handling;
  `docs/architecture.md` records ownership; required validation passes.
- Parallel lane: `lane-package-core`
- Coordination notes: start by inspecting `MLF.Frontend.Syntax.Program`,
  `MLF.Frontend.Program.Check`, `MLF.Frontend.Program.Resolve`,
  `MLF.Frontend.Program.Prelude`, `MLF.Program.CLI`, `MLF.Pipeline`, and
  `.mlfp` parser entrypoints.

#### Completion Pointers: milestone-1

- Completed by round-245; evidence: private package/module owner routes
  trivial package checking, CLI, and backend preparation, with focused
  package-owner tests and `cabal build all && cabal test` passing.

#### Candidate Direction: Package Module Owner

- Direction id: `direction-1a-package-module-owner`
- Summary: create or designate one internal package/module owner and migrate
  current `.mlfp` checking/runtime preparation to depend on it.
- Why it matters now: self-boot work cannot safely add filesystem packages or
  interface artifacts while module semantics are still owned by the one-file
  `Program` path.
- Preconditions: inspect current parsed/resolved program types, module
  resolver, checker, Prelude injection, CLI preparation, public pipeline
  adapters, and current single-file fixtures at HEAD.
- Parallel hints: serial. This is the owner-boundary foundation for the whole
  family.
- Boundary notes: do not add a compatibility resolver or duplicate public
  package facade. Keep old one-file behavior only as trivial package input.
- Extraction notes: a lawful first round may be an owner extraction plus
  adapter tests without implementing filesystem discovery yet.

### [done] 2. Filesystem Discovery And Dependency Graph

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: add filesystem-backed package source discovery and acyclic
  dependency ordering through the package/module owner.
- Completion signal: package roots can discover `.mlfp` modules across files;
  dependency order is explicit and rejects cycles with stable diagnostics;
  imports can resolve across files without bypassing export visibility;
  focused module graph and CLI/API tests pass; required validation passes.
- Parallel lane: `lane-package-core`
- Coordination notes: preserve current same-compilation-unit forward import
  behavior only where the package graph defines it. Recursive module cycles
  must continue to fail closed unless a later accepted roadmap changes that
  contract.

#### Completion Pointers: milestone-2

- Completed by round-246; evidence: one-root filesystem discovery, package
  module graph ordering, missing/cycle diagnostics, cross-file export
  visibility tests, and `cabal build all && cabal test` passing.

#### Candidate Direction: Filesystem Discovery And Graph

- Direction id: `direction-2a-filesystem-discovery-graph`
- Summary: teach the package owner to discover source files, build the module
  graph, topologically order checks, and report missing/cyclic modules.
- Why it matters now: separate compilation and self-hosted compiler components
  need modules across files before interface artifacts or packages can be
  meaningful.
- Preconditions: inspect module import cycle handling, parser source-span
  handling, CLI file IO, test program fixtures, and current diagnostics.
- Parallel hints: serial with milestone 3; docs for package layout may run in
  `lane-docs` after implementation shape is selected.
- Boundary notes: do not create package search path or cache policy here
  unless needed to prove discovery. Keep package graph semantics internal.
- Extraction notes: first extraction can target one package root with explicit
  module-to-file mapping before generalizing search paths.

### [done] 3. Interface Artifact Contract

- Milestone id: `milestone-3`
- Depends on: `milestone-1`, `milestone-2`
- Intent: define the `.mlfp` interface artifact shape for checked exports,
  abstract types, constructors, class methods, instances, and dependency
  metadata.
- Completion signal: interface artifacts have a schema or typed internal
  representation; export/import visibility validates through the interface
  path; stale or malformed interfaces fail closed; focused tests cover values,
  abstract types, constructors, classes, instances, and package dependencies;
  required validation passes.
- Parallel lane: `lane-interface-build`
- Coordination notes: keep interface artifacts aligned with source-level
  visibility and resolved symbol identities. Do not treat interface files as a
  second typechecker authority.

#### Completion Pointers: milestone-3

- Completed by round-247; evidence: private typed `.mlfp` interface artifacts
  now cover checked exports, local data/class summaries, visible instances,
  source/dependency metadata, fail-closed validation, and interface-backed
  package import visibility.

#### Candidate Direction: Interface Artifact Contract

- Direction id: `direction-3a-interface-artifact-contract`
- Summary: introduce the interface representation and validation boundary used
  by package imports and later separate compilation.
- Why it matters now: package builds need a durable contract for what a module
  exports without reusing raw source internals as an implicit interface.
- Preconditions: inspect `docs/mlfp-resolved-symbol-identities.md`,
  import/export resolution, data/class/instance metadata, and existing public
  surface tests.
- Parallel hints: serial after milestone 2. Schema docs and fixture drafting
  may split only if the selected plan keeps ownership disjoint.
- Boundary notes: interfaces are package-layer artifacts, not a public
  compatibility ABI until a later roadmap explicitly publishes that contract.
- Extraction notes: a lawful round may first cover values/types/constructors,
  then leave class/instance interface details to a follow-up round.

### [done] 4. Package Roots, Search Paths, Cache, And Build Graph

- Milestone id: `milestone-4`
- Depends on: `milestone-2`, `milestone-3`
- Intent: settle package roots, import search paths, interface cache
  invalidation, and build graph policy for multi-file packages.
- Completion signal: package roots and search paths are explicit; build graph
  invalidation depends on source/interface metadata instead of ad hoc file
  checks; stale cache behavior is tested; package build diagnostics identify
  the failing module or interface; required validation passes.
- Parallel lane: `lane-interface-build`
- Coordination notes: keep cache policy deterministic and reviewable. Avoid
  hidden global state or compatibility fallbacks that silently reparse source
  when an interface should fail.

#### Completion Pointers: milestone-4

- Completed by round-248; evidence: explicit local package roots/search
  paths, deterministic source/interface build graph metadata, stale source and
  dependency-interface cache rejection, duplicate-root diagnostics, and private
  build graph/cache ownership passed focused checks and the full Cabal gate.

#### Candidate Direction: Package Build Graph Policy

- Direction id: `direction-4a-package-build-graph-policy`
- Summary: add package root/search path semantics and deterministic build graph
  invalidation around source and interface artifacts.
- Why it matters now: self-hosting needs repeatable builds and package-level
  dependency behavior, not only one-off multi-file checking.
- Preconditions: inspect CLI commands, test helpers, temporary artifact
  handling, current module dependency docs, and native/backend preparation.
- Parallel hints: serial with CLI migration. Cache-specific tests can be
  extracted separately after package root semantics are fixed.
- Boundary notes: do not introduce a package manager or remote dependency
  system. This milestone is local package roots and deterministic build graph
  policy only.
- Extraction notes: begin with local package root and cache-key design, then
  add stale-interface and dependency invalidation tests.

### [done] 5. CLI And Public Entry Point Migration

- Milestone id: `milestone-5`
- Depends on: `milestone-1`, `milestone-2`, `milestone-3`, `milestone-4`
- Intent: migrate executable and public API entrypoints so run/check/backend
  emission operate through package mode, with one-file examples treated as
  trivial packages.
- Completion signal: `run-program`, `emit-backend`, `emit-native`, and public
  `.mlfp` checking/runtime adapters use the package owner; package CLI modes
  can check/run/build across files; one-file examples remain covered as
  trivial packages; no separate durable one-file semantic path remains;
  required validation passes.
- Parallel lane: `lane-cli-api-fixtures`
- Coordination notes: keep `MLF.Program.CLI` as command/file orchestration and
  route semantic package preparation through package-owned adapters.

#### Completion Pointers: milestone-5

- Completed by round-249; evidence: CLI and public `.mlfp` package check/run/backend/native adapters use local package mode across file, root, and ordered search-path inputs, preserve one-file inputs as trivial packages, keep private interface/build-graph/backend lowering internals out of public APIs, and pass focused plus full Cabal validation.

#### Candidate Direction: CLI API Package Migration

- Direction id: `direction-5a-cli-api-package-migration`
- Summary: route CLI and public package-facing adapters through package mode
  and preserve current examples as trivial packages.
- Why it matters now: the package substrate is not self-boot useful until the
  user-facing commands and APIs exercise it.
- Preconditions: inspect `app/Main.hs`, `MLF.Program.CLI`,
  `MLF.Backend.Emission.Prepare`, `src-public/MLF/API.hs`,
  `src-public/MLF/Pipeline.hs`, and CLI/backend tests.
- Parallel hints: CLI adapter and docs/fixture migration may split only after
  the package owner APIs are stable.
- Boundary notes: do not add a broad public lowering API. Public API changes
  must stay narrow and package-oriented.
- Extraction notes: a lawful round may first migrate `run-program`, then
  backend emission and public adapters in follow-up rounds.

### [done] 6. Fixture Migration And Self-Boot Readiness Evidence

- Milestone id: `milestone-6`
- Depends on: `milestone-5`
- Intent: migrate docs and fixtures to the package model and publish a
  readiness ledger for the next compiler-in-`.mlfp` roadmap.
- Completion signal: `.mlfp` fixture corpus runs through package mode;
  docs no longer describe one-file programs as the durable model; known
  remaining substrate gaps for self-hosting are captured in a concrete ledger;
  the next roadmap recommendation is explicit; required validation passes.
- Parallel lane: `lane-cli-api-fixtures`
- Coordination notes: this milestone is evidence and migration closeout, not
  compiler implementation. Do not start lexer/parser/compiler work here.

#### Completion Pointers: milestone-6

- Completed by round-250; evidence: existing recursive/unified fixtures run through package-mode file helpers, static package-root and search-path fixtures prove durable local package mode, docs describe one-file inputs only as trivial packages, and the self-boot readiness ledger records remaining compiler-in-.mlfp gaps without overclaiming self-hosting.

#### Candidate Direction: Fixture Migration And Readiness Ledger

- Direction id: `direction-6a-fixture-migration-readiness-ledger`
- Summary: finish fixture/docs migration and record exactly what remains before
  compiler-in-`.mlfp` implementation can begin.
- Why it matters now: later self-hosting work needs repo-truth about what the
  package substrate provides and what remains deferred.
- Preconditions: inspect README, `docs/mlfp-language-reference.md`,
  `docs/syntax.md`, fixture directories under `test/programs/`, parity policy,
  and any package CLI docs added by prior milestones.
- Parallel hints: docs and fixture updates can split after package CLI/API
  behavior is green.
- Boundary notes: do not claim self-boot readiness unless the evidence shows
  the package substrate is complete and remaining gaps are explicitly outside
  this family.
- Extraction notes: a lawful final round may be docs/fixture closeout plus a
  self-boot readiness matrix and next-family recommendation.

### [done] 7. Full Type-Level MLFP Handoff Merge And Publication

- Milestone id: `milestone-7`
- Depends on: `milestone-6`
- Intent: consume the completed handoff implementation for full type-level
  `.mlfp` support, attach the dirty detached checkout to an intentional branch,
  reconcile it with current `master`, and prepare the result for merge or
  publication with current validation.
- Completion signal: the handoff work from
  `/Users/ares/.codex/worktrees/b648/mlf4` is preserved on an intentional
  branch or merged equivalent; the three handoff-untracked files are included;
  kinded type-level frontend, Unicode type lambdas, closed type families,
  multi-parameter typeclasses, superclasses, functional dependencies,
  variable-headed type application, native IO method support, docs, tests, and
  public contracts are reconciled with current `master`; stale generated
  depfile churn is absent; `git diff --check`, conflict-marker scan,
  `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass on the reconciled result; the
  publication state is explicit.
- Parallel lane: `lane-type-level-handoff`
- Coordination notes: this milestone is merge/publish triage for an already
  implemented handoff. Do not discard the dirty detached checkout, do not
  silently drop untracked files, and do not broaden into new compiler-in-`.mlfp`
  implementation beyond reconciling the handoff.

#### Completion Pointers: milestone-7

- Completed by round-251; evidence: full type-level handoff reconciled, docs retry fixed backend/native IO support wording, focused tests, cabal build all, cabal test, and thesis gate passed.

#### Candidate Direction: Full Type-Level Handoff Merge

- Direction id: `direction-7a-full-type-level-handoff-merge`
- Summary: attach, preserve, reconcile, validate, and publish the completed
  full type-level `.mlfp` implementation described by the handoff.
- Why it matters now: the package substrate is terminal, and the handoff
  contains completed type-level language work that needs repo-truth
  integration on current `master` before it can be trusted or published.
- Preconditions: inspect the handoff file, `/Users/ares/.codex/worktrees/b648/mlf4`,
  current `master`, the branch/worktree state, untracked files
  `src/MLF/Frontend/Program/TypeFamilies.hs`,
  `src/MLF/Frontend/TypeLevel.hs`, and `test/FrontendTypeLevelSpec.hs`,
  plus the broad diff before staging.
- Parallel hints: keep serial by default. The merge/reconciliation surface is
  broad across parser, checker, elaboration, backend, public docs, and tests,
  so splitting is unsafe until the preserved branch and current-base conflict
  set are known.
- Boundary notes: the handoff claims previous review and validation, but that
  evidence is stale relative to current `master`; reviewer approval must rest
  on validation after reconciliation. Native `Applicative IO.ap` may remain
  fail-closed if function-valued `IO` results are still not renderable.
- Extraction notes: a lawful first round may preserve the detached dirty
  checkout on `codex/full-type-level-mlfp`, inspect/rebase or merge current
  `master`, include all handoff files, rerun validation, and then either merge
  locally or leave an explicit publish-ready branch/PR state.
