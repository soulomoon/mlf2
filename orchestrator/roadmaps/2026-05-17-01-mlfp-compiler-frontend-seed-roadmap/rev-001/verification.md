# Verification Contract

Roadmap family: `2026-05-17-01-mlfp-compiler-frontend-seed-roadmap`
Revision: `rev-001`
Contract: `orchestrator-v2`

## Baseline Checks

Every round must satisfy the checks matching its touched scope.

1. **Build, test, and thesis gate**
   - For behavior-changing compiler-seed work, run `cabal build all`,
     `cabal test`, and `./scripts/thesis-conformance-gate.sh` before approval.
   - While iterating, run the narrowest focused test slice that covers the
     selected seed behavior before expanding to the full gate.
   - Candidate focused tests should start from `test/ProgramSpec.hs`,
     `test/FrontendParseSpec.hs`, `test/FrontendPrettySpec.hs`,
     package-mode tests, public API adapter tests, and any seed-specific specs
     added by the round.
   - If a round is docs-only, run `git diff --check` and manually verify the
     docs do not claim implementation evidence the repo does not have.

2. **Diff hygiene**
   - Run `git diff --check`.
   - Verify no orphaned imports, unregistered modules, stale generated files,
     duplicate package loaders, or compatibility parser fallbacks remain.
   - Verify new modules under `src/`, `src-public/`, or `test/` are registered
     in `mlf2.cabal`; new spec modules must also be registered in
     `test/Main.hs`.
   - Verify compiler seed fixtures live in an intentional package-mode
     location and do not recreate one-file semantics.

3. **Scope discipline**
   - Confirm the selected round stays inside the milestone and direction named
     by `selection-record.json`.
   - Confirm public surfaces under `src-public/` are unchanged unless the
     selected direction explicitly requires package-facing public API work.
   - Confirm primitive, stdlib, runtime, or backend changes are justified by
     runnable frontend seed evidence, not by a general convenience wishlist.
   - Confirm unsupported native/backend behavior remains fail-closed and is
     classified by layer.

4. **Lineage and closeout**
   - Confirm `selection-record.json`, `round-plan-record.json`, and any
     approving `review-record.json` name the active `roadmap_id`,
     `roadmap_revision`, and `roadmap_dir`.
   - Confirm `review-record.json` validates against
     `orchestrator/round-finalization-schema.md`.
   - If closeout is status-only, confirm every selector resolves through
     `roadmap-view.json` anchors and does not change future coordination.
   - If seed scope, native policy, primitive budget, compatibility posture, or
     verification meaning changes, require semantic-update mode.

## Alignment Checks

- The round must move toward an interpreter-runnable compiler frontend seed in
  `.mlfp` or produce a concrete no-change justification tied to current code.
- The round must keep compiler source modules in ordinary package-mode `.mlfp`
  source units.
- The round must not claim self-hosting, checker-in-`.mlfp`, backend-in-`.mlfp`,
  package-manager support, stable ABI, linker support, persisted interface
  format, or separate object build unless a later roadmap explicitly delivers
  that evidence.
- The round must keep support claims separate by layer: source checking,
  interpreter/runtime, backend/native, object code, package build mode, and
  compiler-in-`.mlfp` implementation.
- The round must not force the frontend seed into native lowerability; native
  behavior is classified and may be extended only for selected seed shapes.
- Tests must be real assertions over seed outputs, diagnostics, or fail-closed
  behavior. Do not replace seed coverage with smoke-only checks.
- Worker fan-out, if used, must be justified by non-overlapping ownership in
  `round-plan-record.json` and reviewed only after integrated verification.

## Task-Specific Checks

- **milestone-1 (Compiler Source Package And Seed Contract)**
  - Verify compiler seed source modules live under an intentional package-mode
    package or fixture root.
  - Verify the seed package is checked and run through the interpreter path.
  - Verify docs identify the seed owner, input fixture policy, and non-goals.

- **milestone-2 (Token, Span, Diagnostic, And Lexer Seed)**
  - Verify token, source-position/span, diagnostic, and lexer result data are
    represented in `.mlfp`.
  - Verify positive and negative lexer/tokenization fixtures produce asserted
    interpreter results.
  - Verify missing text, character, byte, collection, or error APIs are
    recorded as concrete gaps instead of hidden in test helpers.

- **milestone-3 (Parser Seed And AST Contract)**
  - Verify the parser consumes the seed token stream and returns an asserted
    AST or diagnostic result.
  - Verify at least one accepted form and one rejected form have focused
    evidence.
  - Verify parser tests check semantic result structure rather than merely
    proving execution did not crash.

- **milestone-4 (Primitive And Standard Library Gap Budget)**
  - Verify each primitive or stdlib gap is tied to seed code, fixture evidence,
    or a documented fail-closed path.
  - Verify text/characters or bytes, collection operations, maps/sets, parser
    helpers, error accumulation, IO helpers, and diagnostics are each classified
    as needed now, deferred, or unnecessary.
  - Verify broad convenience layers are rejected unless a semantic roadmap
    update authorizes the scope.

- **milestone-5 (Runtime And Backend Layer Classification)**
  - Verify each seed module is classified as source-checked,
    interpreter-runnable, backend-lowerable, native-runnable, or explicitly not
    lowerable.
  - Verify backend/native unsupported cases fail closed with explicit
    diagnostics or are documented as out of scope for this family.
  - Verify no second public backend IR, lazy runtime, or broad FFI lane is
    introduced.

- **milestone-6 (Seed Fixtures, Docs, And Next-Stage Handoff)**
  - Verify seed fixtures are stable and discoverable.
  - Verify README, `docs/mlfp-language-reference.md`,
    `docs/mlfp-self-boot-readiness.md`, and `docs/architecture.md` describe
    the seed without claiming self-hosting.
  - Verify the next-family recommendation names the next compiler component and
    the remaining primitive/stdlib/runtime/backend blockers.
  - Required closeout gates: `git diff --check`, `cabal build all`,
    `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

## Manual Checks

- Manually inspect seed diagnostics to ensure they identify the failing token,
  parser position, source span, or bounded input fixture.
- Manually compare docs against source and tests before approving any
  self-boot readiness wording.
- Manually confirm the selected input representation is honest about current
  text/character/byte support and does not hide Haskell-only behavior behind
  test helpers.
- Manually inspect layer classification for source checking,
  interpreter/runtime, backend/native, object code, and package build mode.
- For roadmap closeout, manually compare `roadmap.md` and
  `roadmap-view.json` to confirm milestone statuses, dependencies, direction
  ids, and anchors agree.

## Roadmap Overrides

- Retry behavior for this family should stay same-round by default while the
  retry stays within the selected milestone, direction, branch, and worktree.
- After three rejected attempts for the same round, require recovery review
  before another same-mechanism retry.
- If a retry would widen into checker/backend implementation, package-manager
  scope, stable ABI/linker/separate compilation, native-first policy, broad
  primitive expansion, or old one-file compatibility semantics, classify it as
  `semantic-update-required` instead of continuing as status-only closeout.
