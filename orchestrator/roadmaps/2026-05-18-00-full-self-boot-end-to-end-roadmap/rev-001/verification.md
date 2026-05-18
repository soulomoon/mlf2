# Verification Contract

Roadmap family: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
Revision: `rev-001`
Contract: `orchestrator-v2`

## Baseline Checks

Every round must satisfy the checks matching its touched scope.

1. **Build, test, and thesis gate**
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
  - Verify locale and regex stay out of scope unless the roadmap changes.

- **milestone-4 (Full Canonical `.mlfp` Parser Parity)**
  - Verify parser-owned `.mlfp` modules produce the same parsed program syntax
    artifact and source spans as the current canonical parser for selected
    fixtures.
  - Verify parser diagnostics are committed conformance behavior where
    relevant.
  - Verify no checker/backend/driver scope is claimed by parser parity.

- **milestone-5 (Self-Boot Platform Contract Implementation)**
  - Verify ABI version, target triple, substrate fingerprint, GC/root model,
    FFI value/ownership/error/export/callback rules, manifests, locks,
    generated binding drift checks, toolchain identity, native link records,
    native execution records, and ambient-input policy.
  - Verify Rust trusted substrate is shared and versioned through the stable
    substrate ABI path.
  - Verify canonical declaration and lock formats are not host-owned proof
    shortcuts.

- **milestone-6 (Compiler Source Package In `.mlfp`)**
  - Verify compiler source modules own parser, resolver, checker, semantic
    interface, backend artifact-decision, diagnostic, package validation, and
    driver-facing semantics.
  - Verify trusted substrate calls provide capabilities only.
  - Verify conformance and semantic artifacts expose behavior changes.

- **milestone-7 (Small Real Self-Boot Driver)**
  - Verify `check`, `emit-backend`, `emit-native`, and `run-conformance` over
    explicit manifests and checked locks.
  - Verify the driver validates and recomputes checked locks deterministically.
  - Verify proof orchestration remains outside compiler semantics.

- **milestone-8 (First Self-Boot Proof)**
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
- Manually compare generated bindings, locks, and expected outputs against
  canonical declarations or explicit regeneration commands.
- Manually inspect proof records for stage-owned output isolation, normalized
  environment, loader-affecting environment policy, and root-bounded paths.
- Manually verify native records are audit evidence, not byte-equality proof.
- For roadmap closeout, manually compare `roadmap.md` and
  `roadmap-view.json` to confirm milestone statuses, dependencies, direction
  ids, and anchors agree.

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
