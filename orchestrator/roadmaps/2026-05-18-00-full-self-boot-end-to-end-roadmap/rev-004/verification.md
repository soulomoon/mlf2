# Verification Contract

Roadmap family: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
Revision: `rev-004`
Contract: `orchestrator-v2`

## Baseline Checks

Every round must satisfy the checks matching its touched scope.

1. **Build, test, and thesis gate**
   - For behavior-changing implementation work, use the `tdd` skill at
     `/Users/ares/.agents/skills/tdd/SKILL.md`. Each selected behavior slice
     must follow a vertical RED -> GREEN -> refactor cycle: write one
     public-interface behavior test, confirm it fails, implement only enough
     code to pass, and refactor only while the focused slice is green.
   - Do not use horizontal slices that batch all tests before implementation,
     implement broad layers before observable behavior, or postpone behavior
     evidence until the end of the round.
   - For behavior-changing self-boot work, run the focused test slice named by
     the round plan before broader validation.
   - Before approving behavior-changing closeout, run `cabal build all` and
     `cabal test`.
   - Run `./scripts/thesis-conformance-gate.sh` when a round touches thesis
     obligations, language semantics, parser/checker behavior, native/backend
     behavior, package/interface semantics, platform contracts, proof records,
     or self-boot readiness claims.
   - If a round is docs-only or control-plane-only, run `git diff --check` and
     manually verify the docs do not claim implementation evidence the repo
     does not have.

2. **Diff hygiene**
   - Run `git diff --check`.
   - Verify no orphaned imports, unregistered modules, stale generated files,
     duplicate package loaders, compatibility parser fallbacks, dynamic golden
     acceptance, or host-only proof shortcuts remain.
   - Verify new modules under `src/`, `src-public/`, or `test/` are registered
     in `mlf2.cabal`; new spec modules must also be registered in
     `test/Main.hs`.
   - Verify generated bindings, package locks, expected outputs, proof records,
     and fixtures are either tracked intentionally or written only to declared
     stage-owned actual-output roots.

3. **Scope discipline**
   - Confirm the selected round stays inside the milestone and direction named
     by `selection-record.json`.
   - Confirm implementation does not skip ADR-ordered prerequisites without a
     semantic roadmap update.
   - Confirm the selected extraction is small enough to verify as one round.
     If the planner or reviewer needs new milestone boundaries, direction
     meaning, dependencies, ownership lanes, or verification rules to keep work
     bounded, require semantic-update mode before execution continues.
   - For milestone 3 after rev-004, reject any next normal implementation plan
     that selects only one remaining string function. The lawful next
     extraction is the integrated
     `item-302-broad-string-library-completion` whole-library item unless a
     later approved semantic update changes the contract.
   - Confirm behavior-changing implementation plans name the `tdd` skill path
     `/Users/ares/.agents/skills/tdd/SKILL.md` and are vertical TDD slices with
     a first public behavior and focused failing test named before coding. If
     the only available plan is horizontal, require replanning or semantic
     roadmap splitting before implementation continues.
   - Confirm self-boot claims are separated by layer: source checking,
     interpreter/runtime, backend/native, object code, package build mode,
     conformance corpus, compiler-in-`.mlfp`, driver, and proof.
   - Confirm trusted substrate provides capabilities only and does not perform
     compiler semantics.

4. **Lineage and closeout**
   - Confirm `selection-record.json`, `round-plan-record.json`, and any
     approving `review-record.json` name the active `roadmap_id`,
     `roadmap_revision`, and `roadmap_dir`.
   - Confirm `review-record.json` validates against
     `orchestrator/round-finalization-schema.md`.
   - If closeout is status-only, confirm every selector resolves through
     `roadmap-view.json` anchors and does not change future coordination.
   - If stage ordering, milestone meaning, proof oracle, compiler-semantics
     boundary, platform contract, or verification meaning changes, require
     semantic-update mode.

## Alignment Checks

- The round must move toward Full Self-Boot as defined by the accepted
  end-to-end ADR or produce a concrete no-change justification tied to current
  code.
- The round must not claim whole-platform self-hosting.
- The round must not claim first self-boot until stage-shared conformance
  passes and normalized semantic artifacts compare equal.
- The round must not use native object or executable byte equality as the
  first-proof oracle.
- The round must not let proof runner, trusted substrate, generated bindings,
  or Haskell helpers decide `.mlfp` compiler semantics.
- The round must preserve conformance outputs as committed oracle artifacts,
  with explicit normalization and metadata for unsupported or
  stage-inapplicable cases.
- Behavior-changing implementation evidence must be public-interface
  behavior-first. Tests should specify observable `.mlfp` compiler, package,
  runtime, backend/native, platform, driver, or proof behavior rather than
  private implementation structure.
- The round must keep stage-owned outputs isolated. Cross-stage reuse of
  semantic interfaces, backend IR, object, executable, link, native execution,
  or conformance-output caches is invalid proof behavior.
- Worker fan-out, if used, must be justified by non-overlapping ownership in
  `round-plan-record.json` and reviewed only after integrated verification.

## Task-Specific Checks

- **milestone-1 (Readiness Ledger Baseline)**
  - Verify readiness docs and glossary separate current evidence from future
    obligations.
  - Verify the docs point to the accepted end-to-end ADR and do not overclaim
    self-hosting.

- **milestone-2 (Shared File-Based Conformance Corpus)**
  - Verify fixtures live under `test/conformance/mlfp/` with metadata,
    expected files, normalization profiles, and focused harness assertions.
  - Verify dynamic expected-output acceptance and ad hoc skips are rejected.
  - Verify production behavior changes are bug fixes exposed by migration.

- **milestone-3 (Native-Capable Broad Text Substrate)**
  - Verify `Char` and string behavior across source checking,
    interpreter/runtime, backend emission, object generation, and native
    execution.
  - Verify Unicode scalar semantics, `String`/`List Char` conversion,
    slicing/classification, search, formatting, and parser-needed cursor
    behavior are tested.
  - For `item-302-broad-string-library-completion`, verify the implementation
    covers the rev-004 whole-library matrix in `roadmap.md`: carried-forward
    rounds 265-301 behavior, `stringJoin`, `stringSplitChar`,
    `stringCompare`, slicing/cursor boundary semantics, `String`/`List Char`
    round-trips, ASCII hex/line-break/control classification, ASCII
    char/string case helpers, carried search/split/replace edge behavior, and
    exact native metadata for every native string-producing helper.
  - Verify the selected plan uses grouped public-interface RED -> GREEN ->
    refactor cycles rather than one future round per function. Required groups
    are public API/import, slicing/cursor boundaries,
    search/split/replace/join, classification/case/compare, exact metadata,
    carried-neighbor regressions, and full closeout.
  - Verify `stringCompare` is deterministic Unicode scalar value ordering
    returning `-1`, `0`, or `1`; it must not claim locale collation or Unicode
    Collation Algorithm behavior.
  - Verify ASCII case helpers change only ASCII letters and leave all other
    Unicode scalar values unchanged. Do not treat them as Unicode default case
    mapping.
  - Verify Unicode normalization is explicitly absent from milestone 3:
    equality, search, split, replace, and comparison operate on exact Unicode
    scalar sequences, and canonically equivalent but differently encoded
    strings remain distinct.
  - Verify exact native metadata with embedded U+0000 strings produced by
    source literals and native helpers, then consumed by equality, length,
    search, split/replace, rendering, and linked native execution without
    C-string truncation.
  - Verify no generic `List` library, parser-owned cursor/combinator module,
    maps/sets, filesystem/process IO, package lock, ABI/linker, platform,
    compiler package, driver, or proof scope is introduced by the string
    library completion round.
  - Verify locale and regex stay out of scope unless the roadmap changes.

- **milestone-4 (Full Canonical `.mlfp` Parser Parity)**
  - Verify parser-owned `.mlfp` modules produce the same parsed program syntax
    artifact and source spans as the current canonical parser for selected
    fixtures.
  - Verify parser diagnostics are committed conformance behavior where
    relevant.
  - Verify no checker/backend/driver scope is claimed by parser parity.

- **milestone-5 (Self-Boot Platform Contract Implementation)**
  - Before selecting broad platform work, verify the planner reassessed the
    current ADRs, `CONTEXT.md`, readiness ledger, architecture docs, tests, and
    codebase boundaries. If those sources show separable ABI, runtime/GC/FFI,
    substrate, package-lock, toolchain, or proof-record fronts, require a
    semantic roadmap revision that splits this milestone first.
  - Verify ABI version, target triple, substrate fingerprint, GC/root model,
    FFI value/ownership/error/export/callback rules, manifests, locks,
    generated binding drift checks, toolchain identity, native link records,
    native execution records, and ambient-input policy.
  - Verify Rust trusted substrate is shared and versioned through the stable
    substrate ABI path.
  - Verify canonical declaration and lock formats are not host-owned proof
    shortcuts.

- **milestone-6 (Compiler Source Package In `.mlfp`)**
  - Before selecting broad compiler-package work, verify the planner reassessed
    current parser/platform evidence, conformance fixtures, ADRs, `CONTEXT.md`,
    architecture docs, and code ownership. If parser integration, resolution,
    checking, interfaces, backend decisions, diagnostics, package validation,
    or driver-facing semantics need distinct proof surfaces, require a
    semantic roadmap revision that splits this milestone first.
  - Verify compiler source modules own parser, resolver, checker, semantic
    interface, backend artifact-decision, diagnostic, package validation, and
    driver-facing semantics.
  - Verify trusted substrate calls provide capabilities only.
  - Verify conformance and semantic artifacts expose behavior changes.

- **milestone-7 (Small Real Self-Boot Driver)**
  - Before selecting broad driver work, verify current manifest, lock,
    command-record, native-emission, and conformance-runner contracts are clear
    enough for one bounded round. If not, require a semantic roadmap revision
    that splits this milestone by command mode or proof-record boundary.
  - Verify `check`, `emit-backend`, `emit-native`, and `run-conformance` over
    explicit manifests and checked locks.
  - Verify the driver validates and recomputes checked locks deterministically.
  - Verify proof orchestration remains outside compiler semantics.

- **milestone-8 (First Self-Boot Proof)**
  - Before selecting final proof work, verify current proof-runner,
    conformance-suite, proof-manifest, normalized semantic artifact, platform,
    and failure-taxonomy contracts are clear enough for one bounded round. If
    not, require a semantic roadmap revision that splits proof-runner
    hardening, conformance-first execution, manifest validation, and final
    stage-equivalence closeout.
  - Verify stage 0 and stage 1 consume the same locked inputs and substrate
    fingerprint.
  - Verify both stages pass the shared conformance suite before semantic
    comparison.
  - Verify normalized semantic artifacts compare equal.
  - Verify the proof manifest records action IDs, command records, link
    records, native execution records, conformance results, stage-owned output
    roots, hashes, and failure taxonomy.

## Manual Checks

- Manually audit support claims after every docs/readiness change.
- For behavior-changing implementation rounds, manually inspect
  `implementation-notes.md` for per-cycle RED -> GREEN -> refactor evidence
  from the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md` and reject
  horizontal all-tests-then-all-code batches.
- Manually compare generated bindings, locks, and expected outputs against
  canonical declarations or explicit regeneration commands.
- Manually inspect proof records for stage-owned output isolation, normalized
  environment, loader-affecting environment policy, and root-bounded paths.
- Manually verify native records are audit evidence, not byte-equality proof.
- For roadmap closeout, manually compare `roadmap.md` and
  `roadmap-view.json` to confirm milestone statuses, dependencies, direction
  ids, and anchors agree.
- For milestone-3 closeout after rev-004, manually confirm the reviewer record
  names the rev-004 whole-library matrix and states whether milestone 4 may
  start without inventing additional Prelude-level string requirements.

## Roadmap Overrides

- Retry behavior for this family should stay same-round by default while the
  retry stays within the selected milestone, direction, branch, and worktree.
- After three rejected attempts for the same round, require recovery review
  before another same-mechanism retry.
- If a retry would reorder self-boot stages, change the proof oracle, make
  trusted substrate own compiler semantics, introduce remote package solving,
  compare native bytes as first-proof evidence, or broaden beyond the selected
  stage, classify it as `semantic-update-required` instead of continuing as
  status-only closeout.
- If a retry or next selection reveals that a coarse milestone needs smaller
  milestones before a bounded round can be planned or reviewed, classify it as
  `semantic-update-required` and publish a refined revision before continuing
  implementation.
- The `tdd` skill requirement uses
  `/Users/ares/.agents/skills/tdd/SKILL.md` and applies to behavior-changing
  implementation rounds only. Pure docs-only, control-plane-only, review-only,
  semantic roadmap-update, and status-only closeout rounds are exempt, but
  their normal artifact, diff hygiene, and claim-audit checks still apply.
- A planner may not select a behavior-changing implementation round under the
  `tdd` skill path `/Users/ares/.agents/skills/tdd/SKILL.md` unless it can
  name the first public-interface behavior and focused RED test slice. If the
  work can only be expressed as a horizontal layer, split or resequence through
  semantic-update mode before implementation.
- For milestone 3 after rev-004, the next implementation extraction must be
  `item-302-broad-string-library-completion`: one integrated Broad String
  Library completion round with grouped public behavior tests. The controller
  and reviewer should classify any one-function string-library follow-up plan
  as requiring replanning or another semantic update.
