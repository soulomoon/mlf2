### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4a-canonical-parser-parity
- Extracted item id: item-334-complex-recursive-program-parser-parity
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-006
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006

### Goal
Extend the shared parser-owned `.mlfp` parser parity library with one bounded
single-module complex recursive program slice based on
`test/programs/recursive-adt/complex-recursive-program.mlfp`.

This round should prove canonical parser parity for a source module where
`ComplexRecursiveProgram` exports `Eq`, `Nat(..)`, `Tree(..)`, `eq`,
`mirror`, `leftDepth`, `rightDepth`, and `main`; derives `Eq` for recursive
`Nat`; defines a recursive binary `Tree`; defines three recursive Tree
traversals; and computes equality over nested `leftDepth`/`rightDepth`,
`mirror`, and `Branch` applications.

### Approach
Use the existing shared parser-library path. Do not add a fixture-specific
whole-source recognizer, pre-rendered projection rows, compatibility alias,
static negative evidence, or canonical-parser bypass.

Add a parser-parity conformance fixture under
`test/conformance/mlfp/parser-parity/complex-recursive-program/`, add the
matching thin `.mlfp` package root under
`test/programs/compiler-parser-parity/complex-recursive-program/`, and register
the case in the aggregate public CLI parser-parity driver.

Extend `test/programs/compiler-parser-parity/parser-library/` only for the
selected syntax family:

- parse the named `ComplexRecursiveProgram` module without requiring a `Main`
  module name;
- parse the combined export surface with a class export, constructor-exported
  `Nat(..)` and `Tree(..)`, derived method `eq`, three recursive helper values,
  and `main`;
- reuse the established `Eq` class and recursive `Nat deriving Eq` parser
  paths from earlier typeclass/recursive-ADT slices;
- reuse the established `Tree`, `Leaf`, two-field `Branch`, wildcard pattern,
  and recursive `mirror` paths from the recursive-tree slices;
- parse `leftDepth` and `rightDepth` as shared annotated-lambda Tree
  traversals whose branch arms recursively call the selected helper over a
  chosen field;
- render nested multi-line constructor/function application expressions in
  `main` from parsed structure rather than hard-coded text; and
- add one malformed Tree traversal branch or nested `main` expression negative
  case through `renderParserNegativeEvidenceFromSourceText`.

Update repo-facing notes only with bounded parser-parity language. Respect
`orchestrator/project-contract.md` for shared invariants and do not claim full
parser parity, checker/resolver/backend progress, compiler-package progress,
platform work, proof work, or self-boot progress.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The task is bounded to one existing recursive-ADT source fixture,
  but its content is not only mechanical fixture registration. It composes
  recursive-tree parsing, derived `Eq Nat`, multiple recursive Tree traversal
  definitions, multi-export rows, and deeply nested constructor/function
  application rendering in one shared parser-library path. That requires
  structural parser behavior across previously separate syntax surfaces.
  Focused verification is sufficient because this is a non-closeout
  parser-parity slice with no production parser replacement, no
  checker/backend behavior claim, and no milestone completion claim.

### Steps
1. Add
   `test/conformance/mlfp/parser-parity/complex-recursive-program/src/Main.mlfp`
   from `test/programs/recursive-adt/complex-recursive-program.mlfp` and
   commit the canonical parser projection at
   `test/conformance/mlfp/parser-parity/complex-recursive-program/expected/parser-program.txt`.
2. Add the thin parser-owned package root under
   `test/programs/compiler-parser-parity/complex-recursive-program/` that
   exposes `sourceFile` and `sourceText`, then calls
   `renderParserParityProjectionFromSourceText`.
3. Extend `ProgramParserParitySpec` with source/expected/root constants,
   positive batch registration, direct shared-parser assertion for the new
   fixture, and one negative batch case for malformed Tree traversal branch or
   nested `main` expression syntax.
4. Extend `ParserParityParser.mlfp` and related parser-library modules so the
   new fixture is parsed through shared token, parser-state, projection-row,
   diagnostic, and dynamic negative-evidence paths.
5. Extend parser shortcut/static guards in `ProgramParserParitySpec` so
   fixture-name, whole-source, pre-rendered `ComplexRecursiveProgram` rows,
   exact `mirror`/`leftDepth`/`rightDepth`/`main` expression shortcuts, and
   static-negative shortcuts for this slice are rejected.
6. Update `implementation_notes.md`, `CHANGELOG.md`, and
   `docs/mlfp-self-boot-readiness.md` with bounded parser-parity evidence and
   explicit non-claims.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`

The focused Hspec group is the aggregate parser/conformance run for this owner
surface. It compiles the test suite, runs canonical projection checks, runs the
generated public CLI parser batch, covers the selected negative evidence, and
exercises the shared shortcut/static guards.

Do not run full closeout gates for this round unless implementation widens
beyond the selected parser slice. This plan does not authorize milestone
closeout, production parser replacement, checker policy changes, platform
work, compiler-package work, or proof claims. Run
`./scripts/thesis-conformance-gate.sh` only if implementation edits thesis
obligation ledgers or makes a thesis/readiness claim beyond the bounded
parser-parity notes above; otherwise record the explicit deferral reason in
implementation evidence.

### Scheduler
- Depends on round ids: none
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
