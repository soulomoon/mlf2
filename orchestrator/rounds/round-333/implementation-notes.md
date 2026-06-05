### Changes Made
- test/conformance/mlfp/parser-parity/module-integrated-recursive-existential/src/Main.mlfp: added the conformance fixture source copied from the existing recursive ADT module-integrated program.
- test/conformance/mlfp/parser-parity/module-integrated-recursive-existential/expected/parser-program.txt: added the canonical parser projection expected by the parser-parity fixture.
- test/programs/compiler-parser-parity/module-integrated-recursive-existential/ParserParityFixture.mlfp: added the thin fixture root exposing `sourceFile` and `sourceText`.
- test/programs/compiler-parser-parity/module-integrated-recursive-existential/Main.mlfp: added the thin package entrypoint that calls `renderParserParityProjectionFromSourceText`.
- test/ProgramParserParitySpec.hs: registered the new positive fixture in the aggregate batch, added direct shared-parser coverage, added malformed imported-expression negative coverage, and extended shortcut/static guards for this slice.
- test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp: extended shared parser-library grammar paths for the selected Core/User module shape, exported recursive/existential surfaces, applied source types, annotated parenthesized expressions, nested constructor/function applications, dynamic diagnostics, and final module-span rendering.
- test/programs/compiler-parser-parity/parser-library/ParserParityLexer.mlfp: extended the bounded line-number helper far enough for this fixture's canonical final span.
- implementation_notes.md: recorded the bounded parser-parity evidence and non-claims for round 333.
- CHANGELOG.md: added a bounded project-progress entry for the new parser-parity fixture.
- docs/mlfp-self-boot-readiness.md: updated only repo-facing parser-parity/readiness evidence with explicit non-claims.

### Tests
- test/ProgramParserParitySpec.hs: verifies the aggregate parser parity batch, direct shared-parser projection for module-integrated recursive existential source, malformed imported-expression diagnostics, and static guards against fixture shortcuts or pre-rendered outputs.
- PASS `git diff --check`.
- PASS `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'` in 3925.2565 seconds; 42 examples, 0 failures.
- RETRY PASS `git diff --name-only -- orchestrator/active-roadmap-bundle.md orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006/verification.md orchestrator/roles/planner.md`; returned no paths after restoring the reviewer-rejected out-of-plan guidance diffs.
- RETRY PASS `git diff --name-only -- '**/state.json' orchestrator/state.json`; returned no paths.

### Notes
Scope stayed within the approved parser-parity slice. I did not edit any `state.json`, did not rewrite `plan.md`, did not merge, and did not add fixture-specific whole-source recognition, static negative shortcuts, pre-rendered rows, compatibility aliases, or canonical-parser bypasses.

The full `cabal build all && cabal test` gate and `./scripts/thesis-conformance-gate.sh` were not run because the approved plan selected focused verification for this non-closeout parser-parity slice and the implementation did not edit thesis obligation ledgers or make a broader thesis/readiness claim.

Retry cleanup: removed the reviewer-blocking out-of-plan tracked guidance diffs from `orchestrator/active-roadmap-bundle.md`, `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006/verification.md`, and `orchestrator/roles/planner.md`. Parser-parity implementation files, fixtures, focused test evidence, and bounded repo-facing notes were left intact.

Controller recommendation: advance round 333 to review.
