### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-306-parser-parity-value-definition-list-spans`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Implement the next bounded source-text parser parity tracer. Parser-owned
`.mlfp` modules must parse one canonical module that carries the round-305
import-exposing shape and expands only the value-definition grammar family:

```text
module Main export (main) {
  import Prelude exposing (Int);
  def two : Int = 2;
  def main : Int = two;
}
```

The `.mlfp` parser package must emit the same committed parsed-program syntax
projection and source-span evidence as the current canonical Haskell parser for
that fixture. This round proves a declaration list with two value definitions,
an `Int` type name, an integer literal expression, and a lower-case value
reference expression. It does not claim name resolution, checking, backend,
driver, platform, compiler-package, proof, or full parser parity.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Work in
vertical RED -> GREEN -> refactor cycles, starting from one public-interface
behavior test before coding.

First public behavior and focused failing test:

```text
MLF.Program parser parity / parser-owned .mlfp parser matches canonical parser for multiple value definitions and value-reference spans
```

Focused command:

```bash
cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for multiple value definitions and value-reference spans/"'
```

The test should exercise the public package path through `runProgramArgs` and
compare against the Haskell canonical parser projection from
`parseLocatedProgramWithFile`. Keep projection expansion deliberately small and
stable: module name/export, one import module span, the import exposing item
span for `Int`, two value-definition spans, the `2` integer literal expression,
and the `two` value-reference expression.

Add value-definition-list negative evidence through the parser-owned `.mlfp`
package, for example a missing semicolon after the first definition or a
malformed second `def` keyword that renders the failing source span. The
diagnostic evidence should be committed or asserted by the Haskell spec as a
parser category/span projection, not as exact Megaparsec prose.

### Steps
1. Add the focused RED test named above to `ProgramParserParitySpec`, preserving
   the round-304 basic Bool tests and round-305 import-exposing tests.
2. Add the selected parser-parity conformance fixture and committed expected
   parser projection under
   `test/conformance/mlfp/parser-parity/value-def-list-int-ref/`.
3. Add a parser-parity `.mlfp` package under
   `test/programs/compiler-parser-parity/value-def-list-int-ref/`. Any shared
   helpers must stay parser-owned and test-package-owned; do not widen Prelude
   or production facades with parser helpers.
4. Extend the Haskell-side projection helper only as much as needed to render a
   list of value definitions and the two selected expression atoms: integer
   literal and lower-case value reference. Keep helper code in the spec or test
   support.
5. Implement parser-owned source/token/AST/parser modules for exactly this
   grammar family: carried module header/export, carried single import exposing
   item, two `def` declarations, `Int` type tokens, integer literal `2`, lower
   value reference `two`, and semicolon-delimited declaration sequencing.
6. Add value-definition-list negative evidence through the public `run-program`
   path. The evidence must prove the parser-owned `.mlfp` tokenizer/parser no
   longer accepts malformed declaration sequencing as the positive fixture.
7. Refactor only after the focused value-definition-list test is green. If this
   grammar expansion forces repeated cursor or expectation code, extract only a
   narrow parser-owned helper inside the parser-parity package; do not create a
   Prelude parser library or broad parser-combinator surface.
8. Update relevant readiness/progress docs to record only this bounded parser
   parity tracer and explicitly avoid claiming full parser parity,
   parser-combinator completion, checker/backend/driver/platform/proof work,
   compiler-in-`.mlfp`, or self-boot.

### Verification
- RED evidence: the focused `ProgramParserParitySpec` matcher above fails
  before value-definition-list parser-parity implementation.
- Focused GREEN:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for multiple value definitions and value-reference spans/"'`
- Negative parser evidence:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed value-definition sequencing through public run-program/"'`
- Neighbor parser-parity slice:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Direct package smoke for the new fixture:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref`
- Regression smokes:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool`
- Diff hygiene: `git diff --check`
- Full behavior-changing closeout: `cabal build all && cabal test`
- Thesis/language-semantics gate:
  `./scripts/thesis-conformance-gate.sh`
- Manual closeout checks: confirm the implementation changed no checker,
  backend, driver, platform, package-manager, ABI, proof, or self-hosting
  scope; confirm any new Haskell spec/support module is registered in both
  `mlf2.cabal` and `test/Main.hs`; confirm docs do not overclaim beyond
  `item-306-parser-parity-value-definition-list-spans`.

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-306/selection-record.json`
- `orchestrator/rounds/round-306/round-plan-record.json`
