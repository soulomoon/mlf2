### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-343-bounded-case-branch-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build one reusable shared parser-library substrate for bounded case-branch row
accumulation, then use it for the existing source and nested case-expression
branch parsers.

The round should reduce the copied semicolon/close-brace continuation ladders in
`ParserParityParser.mlfp` while preserving the existing parser-parity behavior
proved through the aggregate parser Hspec group. This is milestone-4
parser/compiler-frontend ergonomics substrate only. It must not claim full
parser parity, compiler-package implementation, platform/proof progress,
native/backend completion, package-manager/linker work, or self-boot completion.

### Approach
Use the rev-007 roadmap adjustment approved after round 340: milestone 4 remains
in progress, and lawful work may target reusable parser ergonomics substrate
instead of another fixture-shaped syntax slice. The assigned worktree's
checked-in `orchestrator/state.json` may still lag the live controller revision,
so keep it untouched and treat this plan's selected extraction as the
controller/user-assigned rev-007 planning context.

Round 341 centralized parser diagnostic expectations. Round 342 added the first
bounded projection-row list substrate and showed that first-order selector data
fits this `.mlfp` parser-library path better than higher-order helper shapes.
Build on that evidence without reopening either slice.

Target the repeated bounded case-branch pattern now visible in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:

- `parseSourceCaseMoreOrClose8` through `parseSourceCaseMoreOrClose1`,
  `parseSourceCaseNextBranch*`, and `appendSourceCaseBranchAndContinue*`;
- `parseNestedCaseBranchMoreOrClose8` through
  `parseNestedCaseBranchMoreOrClose1`, their `parseNestedCaseBranchNextBranch*`
  and `appendNestedCaseBranchAndContinue*` continuations; and
- the depth-indexed nested variants such as
  `parseNestedCaseBranchMoreOrClose8Depth4` through depth 1 and their matching
  next-branch/append continuations.

Introduce a parser-library helper family for bounded semicolon-separated
case-branch accumulation. Keep it owner-local to
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
unless the implementation can keep a genuinely generic helper in
`ParserParityParserCombinator.mlfp` without leaking source-case semantics into
the generic combinator module.

Prefer a first-order selector shape, analogous to the projection-row substrate,
that can choose the branch parser for ordinary source cases and each existing
nested-depth branch parser. Preserve the current maximum branch budget, branch
row ordering via `appendSourceCaseBranchText`, close-brace behavior through
`finishSourceCaseExpression`, and the final-budget close-only path through
`parseSourceCaseClose`. Do not attempt to redesign case-expression parsing,
lambda parsing, application-chain parsing, source-span rendering, or
diagnostic payloads in this round; those are separate substrate slices.

Do not add compatibility aliases, fixture-name shortcuts, pre-rendered
projections, canonical-parser bypasses, retired syntax shims, parser-private
shortcuts, or compiler-seed/package/platform/proof hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content introduces a reusable bounded case-branch
  parser substrate and migrates multiple existing parser-library continuation
  families across it, so it needs implementer/reviewer design judgment and is
  not simple. Focused verification is sufficient because the work is confined to
  the shared parser-parity library/spec/docs owner surface, preserves existing
  behavior, does not touch production parser, checker, resolver, backend,
  package, platform, proof, or native code, and makes no milestone closeout or
  self-boot claim.

### Steps
1. Inspect the existing source and nested case branch ladders in
   `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
   and confirm the behavior that must be preserved: semicolon consumption,
   branch row append order, branch budget, close-brace handling, nested-depth
   branch parser selection, and final-budget close-only behavior.
2. Add one owner-local bounded case-branch helper family in
   `ParserParityParser.mlfp`. Prefer first-order selector data such as a
   `CaseBranchParser` value over higher-order function passing unless the
   implementation proves the latter typechecks cleanly in this `.mlfp` path.
3. Migrate the ordinary source-case branch list from
   `parseSourceCaseMoreOrClose*` / `parseSourceCaseNextBranch*` /
   `appendSourceCaseBranchAndContinue*` onto the helper. Remove migrated
   duplicated definitions instead of leaving compatibility aliases.
4. Migrate the non-depth nested case branch list from
   `parseNestedCaseBranchMoreOrClose*` / `parseNestedCaseBranchNextBranch*` /
   `appendNestedCaseBranchAndContinue*` onto the same helper while preserving
   nested body parsing through `parseNestedCaseBranchInnerBranch`.
5. Migrate the depth-indexed nested case branch lists for depths 4 through 1
   onto the same helper or onto the smallest selector-extension needed for the
   same helper. Preserve each depth's body parser boundary and do not collapse
   expression-depth semantics in this round.
6. Add focused static coverage in `test/ProgramParserParitySpec.hs` proving the
   bounded case-branch substrate exists, ordinary and nested case parsers use
   it, and the migrated numbered case-branch ladder names were not reintroduced
   as compatibility aliases.
7. Run the focused parser-parity gate and static shortcut/overclaim guards.
   Update repo-facing notes only if they record bounded substrate evidence, and
   keep all non-claims explicit.

### Verification
- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A focused static guard over
  `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`,
  `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`,
  and `test/ProgramParserParitySpec.hs` showing the bounded case-branch helper
  is present, ordinary and nested case branch parsers call it, and the migrated
  numbered case-branch helper aliases were not reintroduced.
- A shortcut/overclaim guard over changed parser-library, spec, and docs lines
  showing no fixture-name shortcuts, pre-rendered projections, static negative
  evidence, canonical-parser bypasses, compiler-seed/package/platform/proof
  hooks, native/backend claims, package-manager/linker claims, self-boot claims,
  or full parser parity claims were added.

The aggregate parser-parity Hspec group is the focused owner-surface gate for
this substrate slice because it exercises shared case parsing through existing
case-expression, recursive ADT, recursive tree, typeclass-integration,
abstract-recursive-ADT, module-integrated-recursive-existential,
complex-recursive-program, named-recursive-ADT, compiler-seed data-model, and
compiler-seed lexer positive/negative parser evidence. Full closeout gates,
`cabal build all && cabal test`, and `./scripts/thesis-conformance-gate.sh` are
not required unless the implementation widens beyond parser-library/spec/docs
scope or makes production parser, thesis-facing semantic, milestone closeout,
package, platform, proof, native/backend, or self-boot claims.

### Scheduler
- Depends on round ids: round-340, round-341, round-342
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
