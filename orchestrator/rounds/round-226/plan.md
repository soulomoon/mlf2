# Round 226 Plan

- Round: `round-226`
- Roadmap:
  `2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`
- Milestone: `milestone-5`
- Direction: `direction-5a-lock-primitive-and-evaluation-order-contract`
- Extracted item: absent
- Retry: `null`
- Actionable slice: `bounded production proof slice`
- Execution shape: serial, one milestone-5 primitive/evaluation-order
  settlement slice only, shared `dist-newstyle/` discipline, no worker
  fan-out, no controller-state edits

## Objective

Close mechanism-table row 5 on top of current `HEAD = 5adb3702` by making the
current primitive-operation and eager-sequencing contract explicit enough that:

- the supported primitive surface remains the closed reserved runtime-binding
  set already present in the backend (`__mlfp_and`, `__io_pure`,
  `__io_bind`, and `__io_putStrLn`) rather than a new `BackendPrim` family,
  a public lowering surface, or broad FFI expansion;
- checked-program conversion continues to publish those primitives through the
  existing `BackendVar` / `BackendApp` / `BackendTyApp` backend expression
  forms instead of inventing a second executable representation;
- eager sequencing is reviewable at the current backend boundary:
  `BackendLet` RHS before body,
  `BackendCase` scrutinee before branch selection, and
  direct/primitive call arguments in written order, with any ANF-like cleanup
  remaining private to the LLVM backend; and
- effect sequencing remains explicit through the current IO primitive contract,
  especially `__io_bind`, while unsupported broader primitive or
  ordering-sensitive shapes keep failing with backend diagnostics instead of
  falling through to a fallback runtime path.

This round must stay strictly inside `milestone-5`. Do not widen into
milestone-6 polymorphism-lowerability work, milestone-7 closeout
synchronization, a second public backend IR, a public lowering surface, lazy
STG machinery, fallback runtime paths, or broad FFI expansion.

## Decision

This round should be behavior-preserving but code/test-bearing rather than
docs-only.

Why:

- `src/MLF/Backend/IR.hs`, `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs` already implement a real primitive baseline
  through `backendRuntimePrimitiveTypes`, `backendBuiltinTermTypes`,
  `ioPrimitiveNames`, `lowerCall`, and `lowerGlobalCall`, so row 5 is a
  contract/evidence gap rather than a missing backend feature.
- the current backend docs and module notes already freeze row-1 through row-4
  ownership, but they do not yet state the closed primitive surface or the
  eager sequencing rules clearly enough for review to treat them as an earned
  backend contract.
- `test/BackendLLVMSpec.hs` already proves individual primitive rows such as
  `__mlfp_and`, `putStrLn`, and direct `__io_bind`, but it does not yet lock
  the sequencing behavior that row 5 needs in order to move from design intent
  to evidence-backed `YES`.
- existing conversion coverage already proves primitives reach
  `MLF.Backend.IR` through the current executable term surface, so the hidden
  assumption gap sits in the row-5 contract publication and LLVM/native
  sequencing evidence rather than in a missing public backend-IR shape.
- a docs-only round would leave row 5 resting on prose while the native
  sequencing behavior remained mechanically under-specified.

The bounded fix is therefore: publish the closed primitive surface and eager
sequencing contract across the durable backend surfaces, keep any sequencing
normalization private to `MLF.Backend.LLVM.Lower`, and add focused LLVM/native
plus repository-guard evidence. Do not add `BackendPrim`, a new executable IR,
public lowering APIs, lazy-runtime recovery, or broader FFI support.

## Authorized Write Scope

Modify only these repo-facing files:

- `docs/architecture.md`
- `docs/backend-native-pipeline.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `test/BackendLLVMSpec.hs`
- `test/RepoGuardSpec.hs`

Do not create or modify:

- `orchestrator/state.json`
- `orchestrator/roadmap.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-226/worker-plan.json`
- `AGENTS.md`
- `README.md`
- `Bugs.md`
- `CHANGELOG.md`
- `src-public/**`
- `src/MLF/Backend/LLVM.hs`
- `src/MLF/Backend/LLVM/Syntax.hs`
- `src/MLF/Backend/LLVM/Ppr.hs`
- `test/BackendIRSpec.hs`
- `test/BackendConvertSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`
- any new `BackendPrim` node family, public `LowerableBackend.*` surface,
  fallback runtime path, lazy-runtime surface, or broad FFI expansion

## Locked Context

- Active selection:
  `orchestrator/rounds/round-226/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- Roadmap row anchor:
  row 5, `Primitive operations and eager evaluation order`, is the first
  lawful `NO` in
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- Retry state: `null`; there is no same-round retry delta to honor

Current worktree state is already non-pristine. Respect controller-owned edits
and do not revert them:

- `M orchestrator/retry-subloop.md`
- `M orchestrator/roadmap.md`
- `M orchestrator/state.json`
- `M orchestrator/verification.md`
- `?? orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/`
- `?? orchestrator/rounds/round-226/`

## Worker Fan-out

Worker fan-out is not required. Keep `worker_mode = none` and do not create
`orchestrator/rounds/round-226/worker-plan.json`.

## Sequential Plan

### Task 1: Publish the closed primitive surface and eager sequencing contract

**Files**

- `docs/architecture.md`
- `docs/backend-native-pipeline.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`

**Required edits**

- In `docs/architecture.md`, extend the backend-boundary section so it states
  explicitly that the current primitive surface is the closed reserved
  runtime-binding set already implemented by the backend, and that those
  primitives are represented through the existing backend expression forms
  rather than a new primitive IR or public lowering interface.
- In `src/MLF/Backend/IR.hs`, tighten the typed-backend-boundary note so row 5
  is explicit at the IR boundary:
  primitive support is carried through reserved runtime bindings plus ordinary
  backend applications/type-applications,
  the backend boundary remains eager, and
  no new `BackendPrim` or broad FFI surface is authorized in `rev-001`.
- In `src/MLF/Backend/Convert.hs`, state that checked-program conversion
  preserves the current primitive representation and the source-visible
  eager application/let/case structure instead of inventing a lowerer-private
  primitive executable form, fallback runtime lane, or reordered effect path.
- In `src/MLF/Backend/LLVM/Lower.hs`, publish one lowerer-local note and/or
  small private helper naming the current eager sequencing rules the module
  relies on:
  let RHS before body,
  case scrutinee before branch,
  direct/primitive call arguments lowered in written order, and
  effect sequencing expressed through `__io_bind`.
  Keep any normalization private and behavior-preserving.
- In `docs/backend-native-pipeline.md`, add the test-facing primitive/eager
  contract so the native/backend document matches the implementation:
  the primitive set is closed,
  `emit-backend` / `emit-native` still consume the same backend IR, and
  native execution relies on explicit eager sequencing rather than fallback or
  implicit laziness rescue.

### Task 2: Lock row-5 behavior with focused LLVM/native and guard evidence

**Files**

- `test/BackendLLVMSpec.hs`
- `test/RepoGuardSpec.hs`

**Required edits**

- Strengthen `test/BackendLLVMSpec.hs` so row-5 evidence covers both primitive
  recognition and sequencing. At minimum, keep the existing `__mlfp_and` and
  direct `__io_bind` proofs, and add one native-run row that demonstrates
  nested `__io_bind` / `__io_putStrLn` actions execute in written order.
- Keep the row-5 evidence localized to the existing primitive sections instead
  of opening a broad new suite. Prefer tightening the current rows
  `accepts primitive IO operations and emits native LLVM`,
  `executes __io_bind main through the native IO runtime`, and
  `preserves referenced Prelude bindings and lowers runtime primitive calls`
  so the sequencing proof stays attached to the primitive contract rather than
  scattering into unrelated backend areas.
- Add one repository guard named exactly:
  `primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary`.
  The guard must read
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs`,
  and fail if the synchronized row-5 markers disappear.
- If the strengthened row-5 tests expose a silent or vague
  unsupported-shape path, refine the existing lowerer diagnostic at that
  failure site only. Do not add a fallback primitive executor, broaden FFI,
  or introduce a new backend IR node family.

### Task 3: Refresh mechanism-table row 5 only

**File**

- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`

**Required edits**

- Refresh the `Last updated (UTC)` line if the artifact changes.
- Update only the row `Primitive operations and eager evaluation order` to
  reflect the landed contract freeze:
  change the gap summary from hidden-assumption language to accepted-contract
  language,
  expand the evidence column to cite the synchronized doc/module-note surfaces
  plus the focused `BackendLLVMSpec` and `RepoGuardSpec` coverage, and
  flip the gate from `NO` to `YES`.
- Keep row 5 honest about the preserved boundary:
  primitive support remains the current closed runtime-binding set represented
  through the existing backend IR shapes,
  eager sequencing may still use private lowerer-only normalization so long as
  the published order contract is preserved, and
  row 5 does not authorize a second executable IR, a public lowering surface,
  lazy runtime machinery, or broad FFI expansion.
- Change that row's `Next action` so the next live blocker points at
  milestone-6 / row-6 polymorphism-lowerability work.
- Keep mechanism order fixed, keep rows 1 through 4 at `YES`, and keep rows 6
  and 7 at `NO`.

## Binary Acceptance Criteria

- PASS if the only repo-facing implementation edits are the eight files named
  in `Authorized Write Scope`.
- PASS if `docs/architecture.md`, `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`, `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs` all agree that the current primitive surface
  is closed, remains inside the existing backend expression forms, and has an
  explicit eager sequencing contract without a new public boundary.
- PASS if `test/BackendLLVMSpec.hs` proves the native IO path executes nested
  `__io_bind` effects in written order while keeping primitive lowering
  anchored in the existing runtime primitive rows.
- PASS if `test/RepoGuardSpec.hs` enforces the synchronized row-5 markers
  across the five backend-owned contract surfaces above.
- PASS if the mechanism table flips row 5 to `YES`, points the next action at
  milestone-6 / row 6, and leaves rows 6 and 7 at `NO`.
- FAIL if the round introduces `BackendPrim`, a second executable/backend IR,
  a public lowering surface, lazy STG machinery, fallback runtime execution,
  broad FFI expansion, row-6 polymorphism-lowerability claims, row-7 closeout
  edits, worker fan-out, or any controller-state / roadmap-history change.

## Verification Commands

Run these in order:

1. Diff hygiene:

```sh
git diff --check
```

2. Focused LLVM/native primitive contract slice:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/IO backend contract/"'
```

3. Focused runtime-primitive lowering row:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/preserves referenced Prelude bindings and lowers runtime primitive calls/"'
```

4. Focused repository guard slice for the new row-5 contract guard:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary/"'
```

5. Full repo gate required because `src/` and `test/` surfaces are in scope:

```sh
cabal build all && cabal test
```
