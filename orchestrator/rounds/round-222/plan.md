# Round 222 Plan

- Round: `round-222`
- Roadmap:
  `2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`
- Milestone: `milestone-1`
- Direction: `direction-1a-freeze-one-backend-ir-contract`
- Extracted item: absent
- Retry: `null`
- Actionable slice: `settlement-contract freeze`
- Execution shape: serial, one milestone-1 contract-freeze slice only,
  docs/module-note/mechanism-table/guard synchronization, no worker fan-out,
  no production semantics changes, and no controller-state edits

## Objective

Freeze the one-backend-IR role-separation and non-duplication contract on top
of current `HEAD = 2677b649`:

- xMLF remains the thesis-faithful typed elaboration IR.
- `MLF.Backend.IR` remains the first and only backend-owned executable eager
  IR after checked-program acceptance.
- `MLF.Backend.Convert` remains the only checked-program to backend-IR
  conversion boundary.
- Any ANF-like normalization, layout-only structure, or
  lowerability-only representation stays private to backend-owned lowering
  helpers unless a later accepted roadmap revision proves that a distinct
  durable boundary with its own invariants is required.
- Native LLVM emission remains downstream of the same `MLF.Backend.IR`
  program; it is not a second executable IR, not a second checked-program
  authority, and not a new source-language runtime.

This round must stay strictly inside `milestone-1`. Do not widen into the
milestone-2 eager-runtime contract, callable-shape refinement, layout
ownership, primitive/evaluation-order policy, polymorphism lowerability, or
family-wide closeout.

## Authorized Write Scope

Modify only these repo-facing files:

- `docs/architecture.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/backend-native-pipeline.md`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `test/RepoGuardSpec.hs`

Do not create or modify:

- `orchestrator/state.json`
- `orchestrator/roadmap.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `orchestrator/roadmaps/**`
- `AGENTS.md`
- `README.md`
- `Bugs.md`
- `docs/mlfp-language-reference.md`
- `src-public/**`
- `src/MLF/Backend/LLVM.hs`
- `test/BackendIRSpec.hs`
- `test/BackendConvertSpec.hs`
- `test/BackendLLVMSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`
- any new `src-public/MLF/Backend*` or `*LowerableBackend*` surface

## Locked Context

- Active selection:
  `orchestrator/rounds/round-222/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- Roadmap row anchor:
  row 1, `IR role separation and non-duplication`, is the first lawful `NO`
  in
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- Retry state: `null`; there is no same-round retry delta to honor

Current worktree state is already non-pristine. Respect controller-owned edits
and do not revert them:

- `M orchestrator/state.json`
- `M orchestrator/roadmap.md`
- `M orchestrator/verification.md`
- `M orchestrator/retry-subloop.md`
- `?? orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/`
- `?? orchestrator/rounds/round-222/`

## Sequential Plan

### Task 1: Freeze the authoritative one-backend-IR contract across the backend-owned guidance surfaces

**Files**

- `docs/architecture.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/backend-native-pipeline.md`

**Required edits**

- Tighten the `docs/architecture.md` backend-boundary section so it states,
  without ambiguity, that xMLF remains the typed elaboration/theory IR and
  `MLF.Backend.IR` is the single executable eager backend IR in the current
  repo architecture.
- Extend the top-level backend IR note in `src/MLF/Backend/IR.hs` so the
  module contract names the non-duplication rule explicitly:
  no second executable backend IR, no public `LowerableBackend.IR`, and no
  second checked-program authority inside this family.
- Extend the module header in `src/MLF/Backend/Convert.hs` so it records that
  checked-program conversion stops at `MLF.Backend.IR`; unsupported checked
  shapes must fail there instead of being rerouted through a second IR layer.
- Add or extend a boundary note in `src/MLF/Backend/LLVM/Lower.hs` so any
  ANF-like, layout-only, or lowerability-only normalization is described as a
  private lowering helper rather than as a new durable backend IR boundary.
- Tighten the introduction or emission-mode language in
  `docs/backend-native-pipeline.md` so `emit-backend` and `emit-native` are
  both described as consumers of the same `MLF.Backend.IR` program, not as
  separate executable-IR authorities.
- Use one shared future-lower-IR criteria list across these surfaces. The
  criteria must require all of the following before any new lower IR can be
  introduced:
  distinct backend-owned executable invariants that cannot live in
  `MLF.Backend.IR` or a private lowering helper;
  a dedicated validation/evidence owner for that new boundary; and
  a later accepted roadmap revision before any new durable or public surface is
  added.

### Task 2: Refresh the mechanism table for row 1 only

**File**

- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`

**Required edits**

- Refresh the `Last updated (UTC)` line if the artifact changes.
- Update only the row
  `IR role separation and non-duplication` to reflect the landed contract
  freeze:
  change its gap summary from missing-contract language to accepted-contract
  language, expand its evidence column to cite the synchronized doc/module-note
  surfaces plus the new focused guard, and flip its gate from `NO` to `YES`.
- Change that row's `Next action` so the next live blocker points at
  milestone-2 / row-2 eager-runtime work instead of repeating milestone-1.
- Keep mechanism order fixed and keep rows 2 through 7 at `NO`.
  This round does not authorize any broader table closure.

### Task 3: Add one focused repository guard for the contract and no-public-duplicate rule

**File**

- `test/RepoGuardSpec.hs`

**Required edits**

- Add one new `it` block under `Repository guardrails` with a name that makes
  the contract obvious, for example
  `one-backend-IR contract stays explicit and no public lower IR leaks`.
- In that example, read and check these exact files:
  `docs/architecture.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/backend-native-pipeline.md`, and
  `mlf2.cabal`.
- Reuse the existing `extractPublicLibraryStanza` helper so the guard fails if
  the public library stanza ever exposes any `MLF.Backend.` module or any
  `LowerableBackend.` module.
- Make the guard fail if the synchronized wording markers for the one-IR
  contract disappear from the architecture/module-note/native-pipeline
  surfaces.
- Keep this test textual and repo-topology-focused only. Do not broaden it
  into eager-runtime, lowering-behavior, or LLVM execution coverage; those are
  later milestones.

## Binary Acceptance Criteria

- PASS if the only repo-facing implementation edits are the seven files named
  in `Authorized Write Scope`.
- PASS if `docs/architecture.md`, `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`, `src/MLF/Backend/LLVM/Lower.hs`, and
  `docs/backend-native-pipeline.md` all agree on the same contract:
  xMLF is the typed elaboration IR;
  `MLF.Backend.IR` is the single executable eager backend IR;
  conversion stops at `MLF.Backend.IR`;
  lowerability/layout helpers stay private; and
  any future lower IR requires later accepted roadmap revision plus distinct
  new invariants.
- PASS if the mechanism table flips row 1 to `YES` and leaves rows 2 through 7
  at `NO`.
- PASS if `test/RepoGuardSpec.hs` enforces both halves of the contract:
  the synchronized wording markers and the absence of any public
  `MLF.Backend.*` / `LowerableBackend.*` exposure from the public library.
- FAIL if the round introduces a second executable backend IR, a public backend
  IR surface, a new checked-program authority path, milestone-2+ contract work,
  or any controller-state / roadmap-history change.

## Verification Commands

Run these in order:

1. Diff hygiene:

```sh
git diff --check
```

2. Focused guard slice for the new repo-contract test:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/one-backend-IR contract stays explicit and no public lower IR leaks/"'
```

3. Mechanism-table gate check:

```sh
table=docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md
test "$(awk -F'|' '$2 ~ /^ IR role separation and non-duplication / {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
for mechanism in \
  'Eager runtime lowering contract' \
  'Direct calls, closure values, and callable shapes' \
  'ADT/case semantics versus layout' \
  'Primitive operations and eager evaluation order' \
  'Polymorphism erasure and lowerability' \
  'Validation, evidence, and guidance synchronization'
do
  test "$(awk -F'|' -v m="$mechanism" '$2 == " " m " " {gsub(/ /, "", $7); print $7}' "$table")" = "NO"
done
```

4. Full required gate, because `test/` is in scope:

```sh
cabal build all && cabal test
```

## Worker Fan-Out

`worker-plan.json` is not needed for this round. The write scope is one
serial contract-freeze slice across tightly coupled docs/module-note/table/test
surfaces, and splitting it would add wording-drift risk without creating
independent production ownership.
