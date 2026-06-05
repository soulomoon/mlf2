# Project Contract

This file records repo-wide invariants shared by every roadmap family and
round. Keep roadmap revisions focused on current coordination; point here for
stable contracts instead of restating them in every role or roadmap file.

These invariants combine the shared scaffolded orchestrator control-plane
contract with mlf4's repo-specific domain invariants. Keep roadmap-specific
overrides in the active roadmap bundle.

## Stable Interfaces

- Repo-local control plane: `orchestrator/` is the durable orchestration
  Module. Its shared Interface is the tracked file set named by
  `orchestrator/artifact-manifest.md`, including `state.json`,
  `state-schema.md`, `artifact-manifest.md`, `project-contract.md`,
  `active-roadmap-bundle.md`, `role-contract.md`, role files, round Markdown
  artifacts, roadmap-update artifacts, roadmap families, and worktree staging.
- Controller state: `state.json` is machine-oriented and minimal. It records
  `contract_version: "orchestrator-v2"`, active roadmap metadata,
  `controller_stage`, `max_parallel_rounds`, `active_rounds`,
  `roadmap_update`, `resume_errors`, and `retry`. Do not persist derived
  mirror fields such as preferred round, merge readiness, summary state,
  worker mode, artifact paths, roadmap style, or top-level resume error.
- Active roadmap bundle: `state.json.roadmap_dir` names the only live roadmap
  revision directory. That directory contains `roadmap.md` and
  `verification.md`; the family directory contains `roadmap-history.md`.
  Top-level roadmap, verification, and retry-policy pointer stubs are not
  supported.
- Strategy-backlog roadmap adapter: strategy-backlog is the only supported
  roadmap shape. Structured fields in `roadmap.md` are the source for milestone
  ids, direction ids, dependencies, terminal status, and closeout selectors.
- Artifact and path resolution: `artifact-manifest.md` owns shared file layout,
  round artifact keys, worker artifact paths, roadmap-update artifact paths,
  and live-vs-archived path resolution.
- Role Interface: `role-contract.md` owns shared role inputs, ownership rules,
  output rules, boundaries, and self-checks. Files under `roles/` should carry
  only role-specific behavior.
- Round artifacts: do not create paired JSON records for Markdown artifacts.
  `plan.md` owns selected round lineage, scheduler fields, `Complexity`,
  `Verification profile`, and optional worker fan-out.
  `implementation-notes.md` owns implementation evidence and simple-round
  direct verification. `review.md` owns non-simple approval/rejection, retry
  target, required changes, and closeout classification.
- Semantic roadmap updates: `roadmap-update-schema.md` owns
  `state.json.roadmap_update`, merged-round and planner-request triggers,
  update branch/worktree conventions, update and review artifacts, rejection
  handling, and activation.
- Retry policy: shared retry mechanics live in runtime references.
  Roadmap-specific retry overrides live under active-bundle `verification.md`
  `## Roadmap Overrides`; do not create a separate required retry-policy file.
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

- The orchestrator is a controller, not an implementer. It owns controller
  state, branch/worktree coordination, round finalization, and activation of
  approved semantic roadmap updates.
- The basic serial workflow is the default runtime front door: simple rounds go
  `plan` -> `finalize-round`; non-simple rounds go `plan` -> `implement` ->
  `review` -> `finalize-round`, followed by terminal roadmap recheck. Advanced
  recovery, worker fan-out, parallel execution, and semantic roadmap-update
  machinery load only when their triggers are present.
- The planner owns roadmap stewardship: normal task selection, round planning,
  and semantic `update-roadmap` authoring. It writes `plan.md` for selected
  implementable rounds, or `roadmap-update-request.md` when current evidence
  shows the active roadmap must first be split or resequenced. For
  `Complexity: simple` rounds, the planner completes the task directly and
  writes direct evidence. During `update-roadmap`, the same role writes
  `roadmap-update.md` and the proposed roadmap revision.
- Reviewer approval gates non-simple round merge. Simple rounds merge from
  planner-authored direct evidence after recorded verification passes.
  Rejected reviews record `Retry target` as `implement`, `plan`, or `blocked`
  plus specific required changes.
- `finalize-round` is controller-owned. It applies reviewer-approved
  status-only closeout for non-simple rounds when needed, checks
  planner-authored direct evidence and any planner-authored status-only
  closeout for simple rounds, derives merge admissibility from machine records
  and base freshness, performs squash merge bookkeeping, removes the live round
  from `active_rounds`, and dispatches semantic `update-roadmap` only when
  required.
- Status-only closeout may change only approved milestone status markers,
  compact completion pointers, and compact roadmap-history entries through
  structured `roadmap.md` milestone ids. It must not change future
  coordination, milestone or direction meaning, sequencing, parallel lanes,
  extraction scope, verification meaning, or retry policy.
- Semantic roadmap updates are delegated, reviewable, and serialized through
  `state.json.roadmap_update`. They may come from a merged round or from a
  planner-requested pre-implementation split, but only one semantic roadmap
  update may be active. Approved updates publish a new roadmap revision before
  activation.
- Used roadmap revisions are durable history. Do not rewrite a used revision
  except for reviewer-approved status-only closeout for non-simple rounds or
  planner-authored direct status-only closeout for `Complexity: simple` rounds
  in the canonical round worktree before merge.
- Legacy compatibility layers are retired. Do not reintroduce legacy mirror
  fields, legacy roadmap style, `roadmap_style`, compact completion summary
  state, persisted merge readiness, prose selection handoffs, top-level pointer
  stubs, paired JSON records for Markdown artifacts, or a dedicated
  merge-preparation role.
- Worktrees live under `orchestrator/worktrees/` and are ignored by tracked
  ignore rules. Durable machine state, role files, roadmap bundles, and round
  artifacts stay in the main repo-visible control plane.
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

- Verify the scaffolded `orchestrator/` file set and path-resolution rules
  against `artifact-manifest.md`.
- Verify `state.json` against `state-schema.md`: minimal top-level fields,
  `contract_version: "orchestrator-v2"`, no retired compatibility fields, live
  rounds only in `active_rounds[]`, controller blockage only in
  `resume_errors.controller`, and no persisted merge-readiness or worker-mode
  mirrors.
- Verify active roadmap bundle resolution through `state.json.roadmap_dir`.
  Treat missing bundle files, invalid structured `roadmap.md` fields,
  duplicate ids, unknown statuses, or missing closeout selectors as controller
  errors, not terminal roadmaps.
- Verify terminal completion only when every `roadmap.md` milestone is `done`,
  `active_rounds` is empty, `roadmap_update` is `null`, and no unresolved
  resume errors remain.
- Verify every round Markdown artifact has its required structured fields
  before relying on it. Do not create duplicate JSON copies of `plan.md`,
  `review.md`, or `implementation-notes.md`.
- Verify selected lineage stays consistent across `plan.md`, `review.md`, and
  closeout edits. Treat duplicate lineage outside those artifacts only as
  integrity checks, not authority.
- Verify status-only closeout selectors resolve through structured
  `roadmap.md` milestone ids and that every applied edit appears in the
  approved `review.md` for non-simple rounds or direct
  `implementation-notes.md` for simple rounds. After base refresh, revalidate
  the closeout evidence before merge.
- Verify semantic roadmap updates use `roadmap-update-schema.md`, record the
  correct trigger, have reviewer approval before activation, and clear
  `state.json.roadmap_update` only after the approved revision is active.
- Verify new roadmap families preserve prior families, prior revisions, prior
  rounds, worktrees, and role files unless an approved migration explicitly
  says otherwise.
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
