### Selected Extraction
- Milestone: Readiness Ledger Baseline
- Milestone id: `milestone-1`
- Direction id: `direction-1a-evidence-ledger-alignment`
- Extracted item id: `item-258-readiness-ledger-baseline-audit`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-001`

### Goal
Complete the readiness-ledger baseline by proving, and correcting only if needed, that the current readiness ledger, glossary, and user-facing docs separate current compiler-seed evidence from future Full Self-Boot obligations and point to the accepted 2026-05-18 ADR ordering. This is a docs/evidence-alignment round, not an implementation round.

### Approach
Keep the round serial with `worker_mode: none`. Start from the accepted ADRs and current seed/package evidence, then audit the docs that users and future agents will read first: `docs/mlfp-self-boot-readiness.md`, `CONTEXT.md`, `README.md`, and `docs/mlfp-language-reference.md`.

If those docs already match the ADRs, do not manufacture code or docs churn. Record the no-change evidence in `implementation-notes.md`. If an overclaim or stale pointer is found, make the smallest docs-only correction on the round branch. Do not edit production code, tests, roadmap files, `orchestrator/state.json`, or `test/conformance/mlfp/` in this round.

### Steps
1. Reconfirm the selected lineage from `selection-record.json` and the active bundle files in `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-001/`.
2. Compare `docs/mlfp-self-boot-readiness.md` against these accepted ADRs:
   - `docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`
   - `docs/adr/2026-05-18-file-based-conformance-before-self-boot.md`
   - `docs/adr/2026-05-18-native-broad-string-library-before-parser-parity.md`
   - `docs/adr/2026-05-18-self-boot-platform-contract.md`
3. Audit `CONTEXT.md`, `README.md`, and `docs/mlfp-language-reference.md` for the same claims:
   - current compiler frontend seed evidence is bounded to `test/programs/compiler-seed/frontend-contract/`;
   - the repo is not self-hosting;
   - next implementation starts with the shared file-based conformance corpus, not direct compiler self-boot;
   - broad native text, parser parity, platform contracts, compiler package, driver, and proof work remain future obligations.
4. Cross-check current seed/package evidence against `test/ProgramCompilerSeedSpec.hs` and the fixture files under `test/programs/compiler-seed/frontend-contract/`. Treat that spec and fixture tree as the objective source for current seed evidence.
5. Confirm `test/conformance/mlfp/` is absent or still only future work. Do not create it in this milestone-1 round.
6. If docs need edits, keep them limited to the audited docs above and preserve the accepted ADR ordering. If no docs need edits, leave the repo content unchanged except for the round artifacts and implementation notes.
7. In `implementation-notes.md`, record inspected files, exact commands used, whether docs changed, and the closeout recommendation for `milestone-1`.

### Verification
Required checks for this round:

```bash
git diff --check
```

```bash
jq -e '.schema_version == "roadmap-view-v1" and .roadmap_id == "2026-05-18-00-full-self-boot-end-to-end-roadmap" and .roadmap_revision == "rev-001" and (.milestones[] | select(.milestone_id=="milestone-1") | .status == "pending" and (.depends_on|length==0)) and (.directions[] | select(.direction_id=="direction-1a-evidence-ledger-alignment") | .milestone_id=="milestone-1")' orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-001/roadmap-view.json
```

```bash
rg -n 'docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md|not a self-hosting claim|not self-hosting|not claim the compiler is implemented in `.mlfp`|not whole-platform self-hosting|not a compiler-only rewrite' docs/mlfp-self-boot-readiness.md CONTEXT.md README.md docs/mlfp-language-reference.md docs/adr/2026-05-18-*.md
```

```bash
rg -n 'test/programs/compiler-seed/frontend-contract|ProgramCompilerSeedSpec|lexer-positive:def-main-equals-true|parser-positive:ast-def-main-bool-true' docs/mlfp-self-boot-readiness.md README.md docs/mlfp-language-reference.md test/ProgramCompilerSeedSpec.hs test/programs/compiler-seed/frontend-contract
```

```bash
find test/programs/compiler-seed/frontend-contract -maxdepth 1 -type f | sort
```

```bash
if [ -d test/conformance/mlfp ]; then find test/conformance/mlfp -maxdepth 3 -type f | sort; else printf 'test/conformance/mlfp absent; milestone-2 not started\n'; fi
```

For a no-code/no-doc-change audit, do not run `cabal build all && cabal test`; the active verification contract says pure guidance/docs edits do not require the full Cabal gate. If this round changes executable seed claims or behavior, stop and request replanning because that is outside the selected extraction.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementer round with no worker fan-out.
