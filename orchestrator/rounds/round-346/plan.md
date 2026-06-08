### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-346-annotated-lambda-rhs-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build one reusable shared parser-library substrate for bounded annotated lambda
RHS depth handling, then use it for the existing nested annotated lambda RHS
parser paths in `ParserParityParser.mlfp`.

The round should reduce the copied `parseAnnotatedLambdaRhs...` depth plumbing
while preserving the existing parser-parity behavior proved through the
aggregate parser Hspec group. This is milestone-4 parser/compiler-frontend
ergonomics substrate only. It must not claim full parser parity,
compiler-package implementation, platform/proof progress, native/backend
completion, package-manager/linker work, or self-boot completion.

### Approach
Use the rev-007 roadmap adjustment: milestone 4 remains in progress, and
lawful work may target reusable parser ergonomics substrate instead of another
fixture-shaped syntax slice. Keep `orchestrator/state.json` untouched; the
assigned worktree's checked-in state snapshot still names rev-006, so this
plan uses the controller/user-assigned rev-007 planning context recorded above.

Rounds 341-345 already centralized diagnostic expectations, bounded projection
row lists, bounded case-branch rows, bounded general/simple application
arguments, and nested parenthesized application depth handling. Build on those
substrates without reopening them.

Target the remaining annotated lambda RHS depth family in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:

- `parseAnnotatedLambdaRhsExpression5` through
  `parseAnnotatedLambdaRhsExpression1`;
- their repeated open, parameter, colon, type, close, and body continuations;
  and
- the top-level `parseAnnotatedLambdaRhsExpression` entry point only where it
  can route through the same helper without changing non-nested behavior.

Introduce the smallest owner-local helper family that expresses the repeated
shape: parse `lambda`, parse an annotated parameter, parse the body with a
bounded recursive annotated-lambda RHS depth, and finish through
`finishAnnotatedLambdaExpression`. Keep it in `ParserParityParser.mlfp`; do
not move source-language lambda policy into `ParserParityParserCombinator.mlfp`
or expose a production parser API.

Prefer first-order helper entry points when they keep depth boundaries
explicit. Preserve the existing depth budget, annotated-parameter parsing,
source-type parsing, fallback body choices, case-expression fallback,
application fallback, and text rendering through `finishAnnotatedLambdaExpression`.
Do not redesign source-type arrow parsing, plain lambda parsing, let parsing,
case branch parsing, source-span rendering, or diagnostic payloads in this
round.

Do not add compatibility aliases, fixture-name shortcuts, pre-rendered
projections, static negative evidence, canonical-parser bypasses, retired
syntax shims, parser-private shortcuts, or compiler-seed/package/platform/proof
hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content changes a shared parser-library grammar
  path across nested annotated lambda RHS depth variants and needs
  implementation judgment to preserve depth budgets, fallback body choices,
  source-type parsing boundaries, and text rendering. Focused verification is
  sufficient because the work is confined to the shared parser-parity
  library/spec/docs owner surface, preserves existing behavior, does not touch
  production parser, checker, resolver, backend, package, platform, proof, or
  native code, and makes no milestone closeout or self-boot claim.

### Steps
1. Inspect the existing annotated lambda RHS family in
   `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
   and confirm the behavior that must be preserved: nested RHS depth,
   annotated parameter shape, source-type parser boundary, case/application
   fallback choices, and `finishAnnotatedLambdaExpression` rendering.
2. Add one owner-local helper family in `ParserParityParser.mlfp` for bounded
   annotated lambda RHS depth handling. Keep it bounded to the parser-library
   source-expression path and do not make it a broad parser framework.
3. Migrate the depth-5 through depth-1 annotated lambda RHS paths onto the
   helper or focused helper entry points while preserving each depth's recursive
   body parser boundary.
4. Route the top-level `parseAnnotatedLambdaRhsExpression` through the helper
   only if that is a direct replacement that preserves the current top-level
   body fallback behavior. Otherwise leave the top-level entry point as the
   stable caller into the bounded helper.
5. Remove migrated duplicated depth-specific annotated lambda RHS definitions
   instead of leaving compatibility aliases.
6. Add focused static coverage in `test/ProgramParserParitySpec.hs` proving the
   bounded annotated lambda RHS substrate exists, the nested RHS call sites use
   it, and the migrated helper names were not reintroduced as compatibility
   aliases.
7. Run the focused parser-parity gate and static shortcut/overclaim guards.
   Update repo-facing notes only if they record bounded substrate evidence, and
   keep all non-claims explicit.

### Verification
- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A focused static guard over
  `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`,
  `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`,
  and `test/ProgramParserParitySpec.hs` showing the bounded annotated lambda
  RHS helper is present, nested RHS call sites use it, and migrated helper
  aliases were not reintroduced.
- A shortcut/overclaim guard over changed parser-library, spec, and docs lines
  showing no fixture-name shortcuts, pre-rendered projections, static negative
  evidence, canonical-parser bypasses, compiler-seed/package/platform/proof
  hooks, native/backend claims, package-manager/linker claims, self-boot
  claims, or full parser parity claims were added.

The aggregate parser-parity Hspec group is the focused owner-surface gate for
this substrate slice because it exercises annotated lambda parsing through
existing let/lambda/application fixtures, type-family kind lambda coverage,
compiler-seed parser paths, public generated `run-program` batches, negative
diagnostics, and shortcut guards. Full closeout gates,
`cabal build all && cabal test`, and `./scripts/thesis-conformance-gate.sh`
are not required unless the implementation widens beyond parser-library/spec/docs
scope or makes production parser, thesis-facing semantic, milestone closeout,
package, platform, proof, native/backend, or self-boot claims.

### Scheduler
- Depends on round ids: round-340, round-341, round-342, round-343, round-344, round-345
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
