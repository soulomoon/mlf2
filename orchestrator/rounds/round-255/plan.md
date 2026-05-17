### Selected Extraction
- Milestone: Primitive And Standard Library Gap Budget
- Milestone id: `milestone-4`
- Direction id: `direction-4a-seed-driven-primitive-budget`
- Extracted item id: `item-255-primitive-stdlib-gap-budget`
- Roadmap id: `2026-05-17-01-mlfp-compiler-frontend-seed-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-17-01-mlfp-compiler-frontend-seed-roadmap/rev-001`

### Goal
Produce a seed-driven primitive and standard-library gap budget from the merged lexer/parser seed. The round should classify concrete gaps by layer and priority, update the readiness ledger or adjacent docs as needed, and avoid adding primitives, standard-library APIs, parser combinators, or native/backend support.

### Approach
Keep the extraction serial and inside `milestone-4`. The roadmap dependencies are satisfied because milestones 1, 2, and 3 are merged; milestone 5 remains blocked on this budget. Use the merged compiler seed as evidence, especially the interpreter/CLI lines:

- `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`
- `parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true`

The implementer should inspect current seed modules, tests, docs, and support surfaces, then produce a compact budget that classifies each required category from `verification.md`: text/characters or bytes, collection operations, maps/sets, parser helpers, error accumulation, IO helpers, and diagnostics. Each entry must be tied to seed code, fixture evidence, current primitive/Prelude/runtime/backend support, or an explicit fail-closed path.

This is a budgeting and classification round, not a feature expansion round. If inspection finds that a broad API, package manager, persisted ABI, separate compilation model, FFI surface, or native runtime redesign would be needed, record that as deferred or roadmap-update-required instead of implementing it.

### Steps
1. Inspect the current seed and evidence before editing: `test/programs/compiler-seed/frontend-contract/SeedSource.mlfp`, `SeedToken.mlfp`, `SeedDiagnostic.mlfp`, `SeedLexer.mlfp`, `SeedAst.mlfp`, `SeedParser.mlfp`, `Main.mlfp`, `test/ProgramCompilerSeedSpec.hs`, and the round-253/round-254 notes.
2. Inspect current support surfaces: `src/MLF/Frontend/Program/Prelude.hs`, `src/MLF/Primitive/Inventory.hs`, interpreter program-running code, backend/native primitive support, `docs/mlfp-self-boot-readiness.md`, `docs/mlfp-language-reference.md`, and `docs/architecture.md`.
3. Create or update the narrowest repo-owned readiness ledger for the gap budget, likely in `docs/mlfp-self-boot-readiness.md` unless an existing more specific artifact is clearly the better owner. Avoid roadmap edits.
4. For each required category, record current evidence, layer ownership, classification, and next action:
   - `needed-before-larger-compiler`: directly blocks a larger compiler component in `.mlfp`.
   - `deferred`: real gap, but not needed for the current bounded lexer/parser seed.
   - `unnecessary-for-this-seed`: no current seed evidence or fail-closed path makes it unnecessary.
   - `roadmap-update-required`: would exceed this roadmap family or milestone boundary.
5. Preserve layer separation in the wording. Distinguish source checking, interpreter/runtime, backend/native/object behavior, package/build mode, and compiler-in-`.mlfp` readiness. Do not turn interpreter seed evidence into backend/native or self-hosting claims.
6. Add a focused structural guard only if the repo already has a low-brittleness pattern for budget coverage. Do not add rename-brittle prose substring tests. If no suitable guard exists, keep validation manual plus `git diff --check`.
7. Review scope before closeout: no new primitives, no new broad Prelude helpers, no parser-combinator library, no package manager, no ABI/linker work, no separate-compilation work, no native runtime redesign, and no roadmap-file changes.

### Verification
For a docs/ledger-only implementation, run:

```sh
git diff --check
```

If any focused guard or test changes are added, also run the narrow relevant target, for example:

```sh
cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'
```

Before approval, run the roadmap gate unless the reviewer explicitly accepts the round as docs-only:

```sh
cabal build all
cabal test
./scripts/thesis-conformance-gate.sh
```

Manual review must confirm that every gap entry is evidence-tied, all required categories are classified, broad convenience layers are rejected without a semantic roadmap update, docs do not overclaim implementation status, and unsupported native/backend behavior remains fail-closed and out of scope.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this plan. They are the machine authority for lineage and worker scheduling.
