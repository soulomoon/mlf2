### Selected Extraction
- Milestone: Seed Fixtures, Docs, And Next-Stage Handoff
- Milestone id: `milestone-6`
- Direction id: `direction-6a-frontend-seed-handoff`
- Extracted item id: `item-257-frontend-seed-handoff`
- Roadmap id: `2026-05-17-01-mlfp-compiler-frontend-seed-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-17-01-mlfp-compiler-frontend-seed-roadmap/rev-001`

### Goal
Close the compiler frontend seed family by making the seed fixture, evidence, layer classification, and next-family boundary easy to find from the repository docs. The round should preserve the runnable seed evidence, update the named user-facing and architecture docs without overclaiming, and leave an explicit next-family recommendation for the next compiler-in-`.mlfp` component.

### Approach
Keep the extraction serial and inside `milestone-6`. Milestones 4 and 5 are done, so the primitive/stdlib budget and runtime/backend layer classification are already available. This round should be documentation and fixture-handoff focused unless inspection finds a small missing fixture discoverability guard that is better covered by an existing focused test.

Use current evidence from:

- `test/programs/compiler-seed/frontend-contract/`
- `test/ProgramCompilerSeedSpec.hs`
- `docs/mlfp-self-boot-readiness.md`
- `orchestrator/rounds/round-255/implementation-notes.md`
- `orchestrator/rounds/round-256/implementation-notes.md`
- `docs/backend-native-pipeline.md`

The handoff should recommend the next compiler-in-`.mlfp` family as a larger frontend component, with the likely first dependency being source-text lexer/parser expansion backed by narrow text or byte input, practical stream/list operations, parser-state/result helpers, diagnostic accumulation, and real source-range diagnostics. Record maps/sets and IO driver helpers as deferred unless the selected next slice directly needs them.

Do not edit roadmap files, do not update `orchestrator/state.json`, and do not treat this closeout as proof of self-hosting. Status-only roadmap closeout is controller/reviewer-owned after this round is reviewed.

### Steps
1. Inspect current evidence before editing: `test/programs/compiler-seed/frontend-contract/*.mlfp`, `test/ProgramCompilerSeedSpec.hs`, `docs/mlfp-self-boot-readiness.md`, `docs/backend-native-pipeline.md`, and the round-255/round-256 implementation notes.
2. Confirm fixture stability and discoverability. If `ProgramCompilerSeedSpec` already asserts package discovery, module order, source paths, check/run, CLI output, backend/native emission, object generation, and native-run evidence, record that as existing coverage rather than adding duplicate tests. Add or adjust a focused test only if a milestone-6 fixture-discoverability requirement is genuinely uncovered.
3. Update `README.md` so the compiler frontend seed is findable from the user-facing entrypoint. Mention the fixture path, the exact evidence boundary, and the fact that it is not a self-hosting, package-manager, ABI/linker, separate-compilation, or arbitrary native workload claim.
4. Update `docs/mlfp-language-reference.md` only where it helps users understand that compiler-source seed fixtures are ordinary local package modules and that the current seed uses bounded symbolic input and a tiny parser/diagnostic surface. Keep source-text lexer/parser and broader stdlib support clearly out of scope.
5. Update `docs/architecture.md` only for stable ownership and fixture location facts: package-mode compiler seed source lives under the existing `MLF.Frontend.Program.*`, `MLF.Program.CLI`, and backend/native owners, not under a second loader, public backend IR, lazy runtime, or compatibility path.
6. Update `docs/mlfp-self-boot-readiness.md` as the primary handoff ledger. It should summarize current executable proof, primitive/stdlib budget, layer classification, remaining blockers, and the recommended next roadmap/component in one place without contradicting the detailed budget/classification tables already present.
7. If docs changes add commands or claims, verify them against the current code or tests. Prefer exact existing command names and fixture paths over broad prose. Avoid brittle prose-substring tests unless an existing structural guard can cover the claim.
8. Review scope before closeout: no seed grammar changes, no parser or lexer contract changes, no primitive/stdlib expansion, no public API widening, no checker/backend implementation in `.mlfp`, no package manager, no ABI/linker/separate-compilation claim, no roadmap changes, and no controller-state changes.

### Verification
Run the focused seed gate first:

```sh
cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'
```

If any docs command examples are changed, run the referenced commands, including:

```sh
cabal run mlf2 -- check-program test/programs/compiler-seed/frontend-contract
cabal run mlf2 -- run-program test/programs/compiler-seed/frontend-contract
cabal run mlf2 -- emit-backend test/programs/compiler-seed/frontend-contract
cabal run mlf2 -- emit-native test/programs/compiler-seed/frontend-contract
```

Then run the required milestone-6 closeout gates:

```sh
git diff --check
cabal build all
cabal test
./scripts/thesis-conformance-gate.sh
```

Manual review must confirm README, `docs/mlfp-language-reference.md`, `docs/mlfp-self-boot-readiness.md`, and `docs/architecture.md` describe the seed consistently, the fixture remains stable and discoverable, the next-family recommendation names the next compiler component and remaining blockers, and no doc claims self-hosting or unsupported package/backend/native guarantees.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this plan. They are the machine authority for lineage and worker scheduling.
