### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-309-parser-parity-data-declaration-constructor-spans`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Implement the next bounded source-text parser parity tracer. Parser-owned
`.mlfp` modules must parse one canonical module that carries the already-proven
module/export/definition/expression baseline and expands only data declaration
syntax:

```mlfp
module Main export (Nat(..), main) {
  data Nat =
      Zero : Nat
    | Succ : Nat -> Nat;

  def main : Nat = Succ Zero;
}
```

The `.mlfp` parser package must emit the same committed parsed-program syntax
projection and source-span evidence as the current Haskell canonical parser for
that fixture. This round proves multiple export items including
type-with-constructors export syntax, a single data declaration, constructor
declaration spans, carried `Nat -> Nat` source type rendering, and a carried
constructor application expression. It does not claim checker, backend,
package-manager, driver, platform, proof, parser-combinator library,
class/instance/type-family syntax, case-expression syntax, or full parser
parity scope.

### Approach
Use the TDD skill at `/Users/ares/.agents/skills/tdd/SKILL.md` for the
behavior-changing implementation. Keep the cycle vertical: add one public
parser-parity behavior test first, confirm RED, implement only enough
parser-owned `.mlfp` package behavior and test projection support to pass,
then add the focused malformed-data-declaration negative evidence.

First public behavior and focused failing test before coding:

```text
MLF.Program parser parity / parser-owned .mlfp parser matches canonical parser for data declarations and constructor spans
```

Focused RED command:

```sh
cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for data declarations and constructor spans/"'
```

The test should exercise the public package path by running the parser-owned
`.mlfp` tracer through `runProgramArgs`, compare its output with the Haskell
canonical parser projection from `parseLocatedProgramWithFile`, and compare
both against the committed expected parser-program artifact under
`test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/`.

### Steps
1. Add the focused RED matcher above to `ProgramParserParitySpec`, preserving
   the round-304 through round-308 parser-parity tests and malformed-input
   evidence.
2. Add the selected parser-parity conformance fixture and committed expected
   parser projection under
   `test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/`.
3. Add a parser-parity `.mlfp` package under
   `test/programs/compiler-parser-parity/data-declaration-constructor-spans/`.
   Any shared helpers must stay parser-owned and test-package-owned; do not
   widen Prelude, public facades, or production parser APIs.
4. Extend only the test-owned Haskell projection renderer as needed for this
   selected syntax: ordered export lists, `ExportTypeWithConstructors`,
   `DeclData`, constructor declarations, constructor declaration source spans,
   and carried constructor-value application rendering. Keep the renderer
   public-behavior oriented and fixture-scoped.
5. Implement parser-owned source/token/AST/parser modules for exactly this
   grammar family: carried module header, export list `(Nat(..), main)`, one
   `data Nat` declaration with `Zero : Nat` and `Succ : Nat -> Nat`, one
   `def main : Nat`, constructor application `Succ Zero`, and carried module
   closing syntax.
6. Add malformed data-declaration negative evidence through the public
   `run-program` path, for example a missing constructor colon or missing data
   semicolon that renders a stable parser category/span projection without
   matching exact Megaparsec prose.
7. Refactor only after the focused data-declaration behavior is green. If this
   slice exposes repeated declaration rendering or expectation code, keep it
   inside test support or the parser-parity package unless a later roadmap
   item authorizes a broader parser helper.
8. Update relevant readiness/progress docs for only this bounded parser-only
   tracer. Carry forward the existing round-304 through round-308 parser
   fixture list when touching `test/conformance/mlfp/README.md`; do not claim
   full parser parity, parser-combinator completion,
   class/instance/type-family/case syntax, checker/backend/driver/platform/proof
   work, or self-boot readiness.

### Verification
- RED evidence: the focused `ProgramParserParitySpec` matcher above fails
  before adding the data-declaration parser-parity fixture/package.
- Focused positive:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for data declarations and constructor spans/"'`
- Focused negative:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed data declarations through public run-program/"'`
- Neighbor parser-parity group:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Direct package smoke for the new fixture:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/data-declaration-constructor-spans`
- Regression package smokes:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/let-lambda-application`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typed-annotation-types`
- Diff hygiene:
  `git diff --check`
- Full behavior-changing closeout:
  `cabal build all && cabal test`
- Thesis/parser semantics gate:
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also wrote `selection-record.json` and `round-plan-record.json` beside this
plan. The selected lineage is
`item-309-parser-parity-data-declaration-constructor-spans`.
