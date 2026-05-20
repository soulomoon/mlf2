### Roadmap Update Required
- Round id: `round-302`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`
- Reason: rev-003 lawfully selects milestone-3 broad text work, but it does not define a bounded whole-library extraction for the remaining Broad String Library. Planning round 302 as one more single-function tracer would violate the operator's hard constraint, while planning "the rest of the string library" directly from the current roadmap would silently choose API surface, Unicode-default semantics, cursor boundaries, verification meaning, and milestone-3 closeout criteria that the active revision has not yet made explicit.

### Current Evidence
- Docs/ADRs/context/code inspected:
  - `orchestrator/state.json`
  - `orchestrator/role-contract.md`
  - `orchestrator/roles/planner.md`
  - `orchestrator/selection-record-schema.md`
  - `orchestrator/round-plan-record-schema.md`
  - `orchestrator/roadmap-update-schema.md`
  - `orchestrator/active-roadmap-bundle.md`
  - `orchestrator/project-contract.md`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/roadmap.md`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/roadmap-view.json`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/verification.md`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/roadmap-history.md`
  - `docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`
  - `docs/adr/2026-05-18-native-broad-string-library-before-parser-parity.md`
  - `CONTEXT.md`
  - `docs/mlfp-self-boot-readiness.md`
  - `docs/mlfp-language-reference.md`
  - `docs/backend-native-pipeline.md`
  - `CHANGELOG.md`
  - `src/MLF/Frontend/Program/Prelude.hs`
  - `src/MLF/Primitive/Inventory.hs`
  - `src/MLF/Frontend/Program/Run.hs`
  - `src/MLF/Backend/LLVM/Lower.hs`
  - `test/BackendLLVMSpec.hs`
  - `test/PrimitiveInventorySpec.hs`
  - `orchestrator/rounds/round-301/selection-record.json`
  - `orchestrator/rounds/round-301/plan.md`
  - `orchestrator/rounds/round-301/round-plan-record.json`
  - `orchestrator/rounds/round-301/review-record.json`
- Codebase or test boundaries inspected:
  - Current public Prelude text surface already exposes and tests `stringLength`, `stringIsEmpty`, `stringContainsChar`, `stringContains`, `stringEquals`, `stringStartsWith`, `stringEndsWith`, `stringAppend`, `stringReplaceChar`, `stringReplace`, `stringIndexOfChar`, `stringIndexOf`, `stringSplit`, `stringFromChar`, `stringFromInt`, `stringFromBool`, `stringFromNat`, `stringFromUnit`, `stringFromList`, `stringToList`, `stringDrop`, `stringTake`, `stringSlice`, `stringCharAt`, `stringCharAtOption`, `charIsDigit`, `charIsAsciiLower`, `charIsAsciiUpper`, `charIsAsciiAlpha`, `charIsAsciiAlphaNum`, `charIsAsciiIdentifierStart`, `charIsAsciiIdentifierContinue`, `charIsAsciiWhitespace`, `charIsAsciiPunctuation`, and `charIsAsciiPrintable` through source checking, `run-program`, backend/object, emit-native/native-object, and linked native execution.
  - `src/MLF/Primitive/Inventory.hs`, `src/MLF/Frontend/Program/Run.hs`, and `src/MLF/Backend/LLVM/Lower.hs` have matching primitive/runtime/native entries for that current surface, and `test/PrimitiveInventorySpec.hs` checks the native-lowerable primitive inventory.
  - `test/BackendLLVMSpec.hs` contains one focused native tracer per current operation, reflecting the prior one-function-per-round pattern through round 301.
  - `docs/mlfp-self-boot-readiness.md`, `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`, and `CONTEXT.md` still identify remaining Broad String Library fronts: broader formatting, full slicing coverage, broader `String`/`List Char` collection APIs, broader classification families, complete cursor APIs, broader substring index APIs, replacement-family completion beyond `stringReplace`, split-family APIs beyond `stringSplit`, Unicode-default case conversion/comparison/normalization decisions, and broad exact metadata for every string-producing helper.
- Why current milestone/direction is too coarse:
  - Milestone 3 says the completion signal includes `String`/`List Char` conversion, slicing, classification, search, formatting, and parser-needed cursor operations, while the direction only says to add native-capable Unicode scalar `Char` and broad string operations and to prove source/interpreter/backend/object/native behavior together. It does not name the remaining whole-library API, signatures, semantics, or closeout inventory.
  - The accepted native broad string ADR requires a Prelude-level Broad String Library with Unicode-default text operations, explicit formatting, valid-text boundaries, plain search, and full native slicing/classification, but it intentionally does not enumerate the exact implementation surface.
  - `CONTEXT.md` defines Broad String Library and Unicode Default Text Operations broadly enough to include case conversion, normalization, comparison, formatting, and search without locale. Implementing or deferring those items is a semantic coordination decision, not a local extraction detail.
  - The readiness and language-reference docs repeatedly say the current tracers are not the complete broad string library and list multiple still-open fronts. A round plan that picks a subset would continue the narrow tracer pattern; a round plan that covers every listed front would invent missing semantics and verification criteria.
  - The rev-003 verification contract requires behavior-changing implementation rounds to name the first public-interface behavior and focused RED test before coding. A whole-library completion round needs an approved API/behavior matrix and grouped verification strategy first; otherwise the first RED test would arbitrarily define the library boundary.
  - Marking milestone 3 done after a whole-library implementation would require reviewers to know which remaining text gaps are now intentionally complete, deferred, or out of scope. The current roadmap does not provide that milestone closeout criterion, so status-only closeout would be ambiguous.

### Requested Split
Author a semantic roadmap update that reshapes milestone 3 from a broad open-ended tracer lane into an exact Broad String Library completion contract for the next implementation round.

The update should not select another one-function item. It should define a single coherent whole-library implementation round, or an explicitly integrated same-round implementation plan, with:

- a current completed inventory carried forward from rounds 265 through 301;
- a required remaining public API table with names, signatures, owners, and native-support expectations;
- exact semantics for each remaining whole-library category:
  - formatting beyond the existing `stringFromInt`, `stringFromBool`, `stringFromNat`, and `stringFromUnit` tracers;
  - full slicing and cursor behavior, including negative, boundary, end-of-input, and out-of-range policy;
  - split, index, replace, and search family completion beyond the current first operations;
  - `String`/`List Char` collection helpers that are truly required before parser parity;
  - classification helpers required by parser/source text work, with ASCII helpers explicitly named as ASCII and Unicode-default helpers explicitly named as Unicode-default;
  - case conversion, comparison/ordering, normalization, and "broad exact metadata" decisions: either implement them in milestone 3 with fixed Unicode-default semantics and concrete validation, or explicitly defer/exclude them with a recorded reason that does not overclaim the Broad String Library;
- a clear boundary between Prelude-level string library operations and parser-owned cursor/combinator modules;
- grouped TDD and verification expectations suitable for a whole-library round, not one focused matcher per future round;
- milestone-3 closeout criteria that let a reviewer decide whether the Broad String Library is complete enough to unlock milestone 4 without silently changing parser parity scope.

The semantic update should preserve rev-003 ordering: milestone 3 still precedes milestone 4 parser parity and milestone 5 platform work. It may change milestone-3 extraction meaning and closeout criteria because that is exactly what the current plan stage cannot safely infer.

### Non-Goals
- Do not implement code during this planning stage.
- Do not edit active roadmap files in this planning stage.
- Do not create `selection-record.json`, `plan.md`, or `round-plan-record.json` for round 302 until the roadmap update is approved.
- Do not select another one-function string tracer.
- Do not broaden into locale-sensitive APIs, regex, parser combinators, full parser parity, platform contracts, compiler package work, driver work, proof work, maps/sets, filesystem/process IO, package locks, ABI/linker contracts, or self-boot proof records.
- Do not use this request as an approved roadmap diff; it is evidence for the delegated `update-roadmap` stage.
