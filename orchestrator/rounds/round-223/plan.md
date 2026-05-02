# Round 223 Plan

- Round: `round-223`
- Roadmap:
  `2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`
- Milestone: `milestone-2`
- Direction: `direction-2a-pin-eager-runtime-contract`
- Extracted item: absent
- Retry: `null`
- Actionable slice: `settlement-contract freeze`
- Execution shape: serial, one milestone-2 eager-runtime contract slice only,
  docs/module-note/mechanism-table/guard synchronization, no worker fan-out,
  no runtime/codegen implementation changes, and no controller-state edits

## Objective

Freeze the row-2 eager-runtime lowering contract on top of current
`HEAD = 5365d975`:

- `MLF.Backend.IR` owns the first backend-owned typed eager executable
  representation after checked-program acceptance, including the explicit
  executable term shapes the rest of the backend consumes.
- `MLF.Backend.Convert` remains the only checked-program to backend-IR cut and
  must publish eager executable structure into `MLF.Backend.IR` rather than
  inventing lowerer-only or lazy-runtime repair paths.
- `MLF.Backend.LLVM.Lower` and native emission own only the private lowering
  and process-runtime concerns downstream of the same `MLF.Backend.IR`
  program: closure ABI details, layout-only lowering helpers,
  native wrapper/runtime symbol emission, and executable rendering support.
- Lazy STG-like machinery stays explicitly out of scope:
  no thunks, no update frames, no CAF update semantics, no graph reduction,
  and no implicit laziness rescue.

This round must stay strictly inside `milestone-2`. Do not widen into
milestone-3 callable-shape refinement, milestone-4 ADT/layout ownership,
milestone-5 primitive/evaluation-order policy, milestone-6 polymorphism
lowerability, or milestone-7 closeout.

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
- `src-public/**`
- `src/MLF/Backend/LLVM.hs`
- `src/MLF/Backend/LLVM/Syntax.hs`
- `src/MLF/Backend/LLVM/Ppr.hs`
- `test/BackendIRSpec.hs`
- `test/BackendConvertSpec.hs`
- `test/BackendLLVMSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`
- any new `src-public/MLF/Backend*` or `*LowerableBackend*` surface

## Locked Context

- Active selection:
  `orchestrator/rounds/round-223/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- Roadmap row anchor:
  row 2, `Eager runtime lowering contract`, is the first lawful `NO` in
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- Retry state: `null`; there is no same-round retry delta to honor

Current worktree state is already non-pristine. Respect controller-owned edits
and do not revert them:

- `M orchestrator/state.json`
- `M orchestrator/roadmap.md`
- `M orchestrator/verification.md`
- `M orchestrator/retry-subloop.md`
- `?? orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/`
- `?? orchestrator/rounds/round-223/`

## Sequential Plan

### Task 1: Publish the eager-runtime ownership split in the backend contract surfaces

**Files**

- `docs/architecture.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`

**Required edits**

- Tighten the `docs/architecture.md` backend-boundary section so it states,
  without ambiguity, what belongs inside `MLF.Backend.IR`:
  the typed eager executable nodes and their validation-visible invariants,
  including direct application, explicit closures/closure calls, ADT
  construct/case, lets, lambdas, type abstraction/application, and
  roll/unroll.
- Add language in that same architecture section that `MLF.Backend.IR` is not
  the place where closure-record layout, native process entrypoints, renderer
  helpers, or other lowering-only runtime details are owned.
- Extend `{- Note [Typed backend IR boundary] -}` in
  `src/MLF/Backend/IR.hs` so the module note names the eager executable
  contract directly and explicitly excludes lazy STG-style machinery:
  no thunks, no update frames, no CAF update semantics, no graph reduction,
  and no implicit laziness rescue.
- Extend the `src/MLF/Backend/Convert.hs` module header so it records that
  checked-program conversion publishes eager executable structure into
  `MLF.Backend.IR`, and unsupported checked shapes fail there instead of being
  normalized into lazy runtime artifacts, lowerer-private layout forms, or
  native-wrapper-specific machinery.

### Task 2: Publish the LLVM/native-only ownership boundary and keep lazy machinery excluded

**Files**

- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/backend-native-pipeline.md`

**Required edits**

- Extend `{- Note [One backend IR lowering boundary] -}` and/or
  `{- Note [Closure ABI] -}` in `src/MLF/Backend/LLVM/Lower.hs` so the lowerer
  note states exactly what belongs downstream of `MLF.Backend.IR`:
  private closure ABI details, environment-record layout, layout-only
  lowering helpers, native wrapper/runtime symbol emission, and executable
  rendering support.
- Make that lowerer note explicit that raw LLVM emission and native emission
  both start from the same `MLF.Backend.IR` program, and that these private
  lowering/runtime details do not constitute a second executable IR or a lazy
  runtime.
- Tighten `docs/backend-native-pipeline.md` so `emit-backend` is documented as
  raw inspection/lowering output and `emit-native` is documented as that same
  eager IR plus private native-entrypoint/runtime support only.
- Add explicit exclusion language to `docs/backend-native-pipeline.md` that the
  native path does not own or introduce thunks, update frames, CAF update
  semantics, graph reduction, or implicit laziness rescue.

### Task 3: Refresh the mechanism table for row 2 only

**File**

- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`

**Required edits**

- Refresh the `Last updated (UTC)` line if the artifact changes.
- Update only the row `Eager runtime lowering contract` to reflect the landed
  row-2 contract freeze:
  change its gap summary from implicit-contract language to accepted-contract
  language, expand its evidence column to cite the synchronized doc/module-note
  surfaces plus the focused guard, and flip its gate from `NO` to `YES`.
- Change that row's `Next action` so the next live blocker points at
  milestone-3 / row-3 callable-shape work instead of repeating milestone-2.
- Keep mechanism order fixed, keep row 1 at `YES`, and keep rows 3 through 7
  at `NO`. This round does not authorize any broader table closure.

### Task 4: Add one focused repository guard for the eager-runtime split and lazy-STG exclusions

**File**

- `test/RepoGuardSpec.hs`

**Required edits**

- Add one new `it` block under `Repository guardrails` named exactly:
  `eager-runtime lowering contract stays explicit and lazy STG machinery stays out of scope`.
- In that example, read and check these exact files:
  `docs/architecture.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`, and
  `docs/backend-native-pipeline.md`.
- Make the guard fail if the synchronized wording markers for the row-2 split
  disappear:
  `MLF.Backend.IR` owns the eager executable representation;
  LLVM/native lowering owns closure ABI plus native wrapper/runtime details;
  raw and native emission still consume the same backend IR; and
  the explicit no-thunks / no-update-frames / no-CAF-update-semantics /
  no-graph-reduction / no-implicit-laziness-rescue exclusions remain present.
- Keep this guard textual and contract-focused only. Do not broaden it into
  callable-shape behavior changes, layout-policy closure, primitive/effect
  policy, or native toolchain execution coverage.

## Binary Acceptance Criteria

- PASS if the only repo-facing implementation edits are the seven files named
  in `Authorized Write Scope`.
- PASS if `docs/architecture.md`, `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`, `src/MLF/Backend/LLVM/Lower.hs`, and
  `docs/backend-native-pipeline.md` all agree on the same row-2 contract:
  `MLF.Backend.IR` owns the eager executable boundary;
  `MLF.Backend.Convert` publishes that eager structure from checked programs;
  LLVM/native lowering owns only private closure ABI, layout/runtime support,
  and process-entrypoint concerns downstream of the same IR; and
  lazy STG-style machinery is explicitly excluded.
- PASS if the mechanism table flips row 2 to `YES`, preserves row 1 at `YES`,
  and leaves rows 3 through 7 at `NO`.
- PASS if `test/RepoGuardSpec.hs` enforces the row-2 wording markers across the
  synchronized contract surfaces.
- FAIL if the round introduces callable-shape redesign, ADT/layout policy
  closure, primitive/evaluation-order semantics, polymorphism-lowerability
  policy, runtime/codegen behavior changes, a second executable/backend IR, a
  public lowering surface, lazy-runtime semantics, or any controller-state /
  roadmap-history change.

## Verification Commands

Run these in order:

1. Diff hygiene:

```sh
git diff --check
```

2. Focused guard slice for the new row-2 repository contract test:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/eager-runtime lowering contract stays explicit and lazy STG machinery stays out of scope/"'
```

3. Focused backend slices that keep the published row-2 contract honest:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/validates explicit closure construction and indirect closure calls/"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/native process entrypoint/"'
```

4. Mechanism-table gate check:

```sh
table=docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md
test "$(awk -F'|' '$2 == " IR role separation and non-duplication " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
test "$(awk -F'|' '$2 == " Eager runtime lowering contract " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
for mechanism in \
  'Direct calls, closure values, and callable shapes' \
  'ADT/case semantics versus layout' \
  'Primitive operations and eager evaluation order' \
  'Polymorphism erasure and lowerability' \
  'Validation, evidence, and guidance synchronization'
do
  test "$(awk -F'|' -v m="$mechanism" '$2 == " " m " " {gsub(/ /, "", $7); print $7}' "$table")" = "NO"
done
```

5. Full required gate, because `src/` and `test/` are in scope:

```sh
cabal build all && cabal test
```

## Worker Fan-Out

`worker-plan.json` is not needed for this round. The write scope is one serial
milestone-2 contract-freeze slice across tightly coupled docs, backend module
notes, the mechanism table, and one focused repository guard, so splitting it
would add wording-drift risk without creating independent ownership.
