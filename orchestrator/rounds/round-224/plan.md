# Round 224 Plan

- Round: `round-224`
- Roadmap:
  `2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`
- Milestone: `milestone-3`
- Direction: `direction-3a-clarify-direct-vs-closure-callable-shapes`
- Extracted item: absent
- Retry: `null`
- Actionable slice: `bounded production proof slice`
- Execution shape: serial, one milestone-3 callable-shape refinement slice
  only, shared `dist-newstyle/` discipline, no worker fan-out, no
  controller-state edits

## Objective

Close mechanism-table row 3 on top of current `HEAD = 006eb569` by making the
callable-shape contract explicit enough that:

- `BackendApp` means a direct first-order call only.
- `BackendClosureCall` means an indirect call through a closure value only.
- direct local first-order aliases stay on the direct-call path.
- closure-valued aliases, captured closures, and case/let-selected closure
  values stay on the explicit closure-call path.
- malformed backend IR reports the violated callable invariant directly instead
  of relying on lowerer-only recovery.

This round must stay strictly inside `milestone-3`. Do not widen into
milestone-4 ADT/layout ownership, milestone-5 primitive/evaluation-order
policy, milestone-6 polymorphism-lowerability policy, milestone-7 guidance
closeout, a second backend IR, a public `LowerableBackend.IR`, or lazy STG
machinery.

## Decision

This round should refine callable representation/diagnostics rather than keep
the current shapes and only add stronger tests/guards.

Why:

- `src/MLF/Backend/IR.hs` already distinguishes `BackendApp` from
  `BackendClosureCall`, but validation still reconstructs closure-headed call
  misuse through helpers such as `backendAppClosureHead`,
  `backendDefiniteClosureValueName`, and alias/case traversal.
- `src/MLF/Backend/LLVM/Lower.hs` still recovers callable intent with
  lowerer-local helpers such as `backendExprIsExplicitClosure`,
  `pushCallIntoExpression`, and `applyEvidenceWrapperArgs`.
- `test/BackendLLVMSpec.hs` currently contains positive lowerer acceptance for
  ambiguous shapes:
  `lowers BackendApp case heads that select direct closures` and
  `lowers BackendApp let heads that select direct closures`.
- `test/BackendIRSpec.hs` already rejects comparable malformed backend IR, so a
  tests-only round would preserve disagreement between the validator contract
  and lowerer convention instead of removing the convention dependency named by
  the roadmap.

The bounded fix is therefore: publish one backend-owned callable classifier and
its diagnostics in `MLF.Backend.IR`, make conversion and lowering consume that
single contract, and replace the lowerer acceptance of ambiguous `BackendApp`
closure heads with explicit rejection coverage.

## Authorized Write Scope

Modify only these repo-facing files:

- `docs/architecture.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `test/BackendIRSpec.hs`
- `test/BackendConvertSpec.hs`
- `test/BackendLLVMSpec.hs`
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
- `docs/backend-native-pipeline.md`
- `src-public/**`
- `src/MLF/Backend/LLVM.hs`
- `src/MLF/Backend/LLVM/Syntax.hs`
- `src/MLF/Backend/LLVM/Ppr.hs`
- `test/Main.hs`
- `mlf2.cabal`
- any new `src-public/MLF/Backend*`, `*LowerableBackend*`, fallback path, or
  second executable-IR surface

## Locked Context

- Active selection:
  `orchestrator/rounds/round-224/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- Roadmap row anchor:
  row 3, `Direct calls, closure values, and callable shapes`, is the first
  lawful `NO` in
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- Retry state: `null`; there is no same-round retry delta to honor

Current worktree state is already non-pristine. Respect controller-owned edits
and do not revert them:

- `M orchestrator/state.json`
- `M orchestrator/roadmap.md`
- `M orchestrator/verification.md`
- `M orchestrator/retry-subloop.md`
- `?? orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/`
- `?? orchestrator/rounds/round-224/`

## Sequential Plan

### Task 1: Publish one explicit callable-shape contract at the backend IR boundary

**Files**

- `docs/architecture.md`
- `src/MLF/Backend/IR.hs`

**Required edits**

- Tighten the backend-boundary section in `docs/architecture.md` so it states
  one explicit callable rule set:
  `BackendApp` is for direct first-order callables;
  `BackendClosureCall` is for closure values;
  direct local aliases that remain first-order stay direct;
  closure-valued aliases, case binders, constructor-field projections, and
  captured closure values require `BackendClosureCall`.
- Extend the backend IR note in `src/MLF/Backend/IR.hs` so the callable
  contract is stated alongside the existing eager-runtime contract, including
  the direct-vs-closure boundary and the expected diagnostics for confused
  shapes.
- Add one backend-owned callable classifier/decision surface inside
  `MLF.Backend.IR` that validator and lowerer can both consume. Keep this
  inside the current IR boundary: no second IR, no fallback call path, and no
  lazy-runtime recovery.
- Tighten `BackendValidationError` coverage so direct-call misuse and
  closure-call misuse identify the violated backend invariant rather than
  reporting only a generic unsupported head.

### Task 2: Make conversion and lowering obey that shared callable contract

**Files**

- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`

**Required edits**

- Update `src/MLF/Backend/Convert.hs` so callable emission is driven by the
  published callable contract instead of depending on lowerer cleanup:
  direct local first-order calls stay on `BackendApp`,
  while closure-valued aliases, captured closures, and case/let-selected
  closure heads are emitted as `BackendClosureCall`.
- Update `src/MLF/Backend/LLVM/Lower.hs` so lowering consumes the shared
  callable classifier rather than accepting malformed `BackendApp` heads that
  only become closures after `let`/`case` peeling.
- Remove or rewrite the lowerer-only acceptance path represented today by the
  ambiguous `BackendApp` let/case-head tests. Lowering may still normalize
  valid direct-call alias wrappers, but it must not silently legalize backend
  IR that violates the callable contract.
- Keep LLVM-side diagnostics explicit when malformed backend IR reaches the
  lowerer: validation failure should identify direct-vs-closure misuse instead
  of degrading into an opaque lowerer-only call error.

### Task 3: Refresh focused callable-shape evidence and replace convention-based LLVM acceptance

**Files**

- `test/BackendIRSpec.hs`
- `test/BackendConvertSpec.hs`
- `test/BackendLLVMSpec.hs`
- `test/RepoGuardSpec.hs`

**Required edits**

- In `test/BackendIRSpec.hs`, add or retarget focused callable-shape coverage
  so both success and rejection paths are explicit:
  direct local first-order calls validate;
  explicit closure calls through aliases/case binders validate; and
  malformed direct-call versus closure-call heads fail with named backend
  diagnostics.
- In `test/BackendConvertSpec.hs`, keep the existing direct local call proof
  and extend the callable-shape assertions so conversion proves the intended
  split:
  direct local first-order flows emit `BackendApp`,
  closure-valued aliases/captures/case projections emit `BackendClosureCall`,
  and ambiguous closure-headed `BackendApp` forms are no longer the accepted
  backend output.
- In `test/BackendLLVMSpec.hs`, replace the current positive tests
  `lowers BackendApp case heads that select direct closures` and
  `lowers BackendApp let heads that select direct closures`
  with rejection coverage for malformed backend IR. Keep the positive LLVM
  proofs on the already explicit call path, especially the existing
  `case-selected closure callees through the explicit closure ABI`,
  `let-selected closure callees through the explicit closure ABI`, and direct
  first-order alias tests.
- In `test/RepoGuardSpec.hs`, add one new `it` block named exactly:
  `callable-shape contract stays explicit and direct-vs-closure call heads stay unambiguous`.
  That guard must read
  `docs/architecture.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs`,
  and fail if the synchronized callable-shape markers disappear.

### Task 4: Refresh mechanism-table row 3 only

**File**

- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`

**Required edits**

- Refresh the `Last updated (UTC)` line if the artifact changes.
- Update only the row `Direct calls, closure values, and callable shapes` to
  reflect the landed callable-contract refinement:
  change the gap summary from convention-dependence language to
  accepted-contract language, expand the evidence column to cite the
  synchronized contract surfaces plus the focused IR/convert/LLVM/guard tests,
  and flip the gate from `NO` to `YES`.
- Change that row's `Next action` so the next live blocker points at
  milestone-4 / row-4 ADT/layout ownership work.
- Keep mechanism order fixed, keep rows 1 and 2 at `YES`, and keep rows 4
  through 7 at `NO`.

## Binary Acceptance Criteria

- PASS if the only repo-facing implementation edits are the nine files named
  in `Authorized Write Scope`.
- PASS if `docs/architecture.md` and `src/MLF/Backend/IR.hs` publish one
  explicit callable-shape contract that distinguishes:
  direct first-order calls,
  local first-order aliases,
  closure values, and
  indirect closure calls.
- PASS if `src/MLF/Backend/Convert.hs` and `src/MLF/Backend/LLVM/Lower.hs`
  consume that same callable contract, and the lowerer no longer accepts
  malformed `BackendApp` let/case heads that are closure values in disguise.
- PASS if focused tests cover both success and rejection paths:
  valid direct local calls,
  valid explicit closure calls, and
  direct-vs-closure invariant violations with named diagnostics.
- PASS if `test/BackendLLVMSpec.hs` no longer encodes
  `BackendApp`-headed closure selection as a valid lowering contract, while
  keeping positive LLVM evidence for explicit closure-call lowering and direct
  first-order calls.
- PASS if `test/RepoGuardSpec.hs` enforces the callable-shape wording markers
  across the synchronized contract surfaces.
- PASS if the mechanism table flips row 3 to `YES`, preserves rows 1 and 2 at
  `YES`, and leaves rows 4 through 7 at `NO`.
- FAIL if the round lands only additional tests/guards without removing the
  validator/lowerer convention dependency, or if it widens into milestone-4+,
  a second backend IR, a public lowering surface, fallback paths, lazy STG
  machinery, worker fan-out, or controller-state/roadmap-history edits.

## Verification Commands

Run these in order:

1. Diff hygiene:

```sh
git diff --check
```

2. Focused repository guard for the callable contract:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/callable-shape contract stays explicit and direct-vs-closure call heads stay unambiguous/"'
```

3. Focused backend IR callable-shape rejection/success slices:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/validates explicit closure construction and indirect closure calls/"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/rejects malformed closure IR/"'
```

4. Focused conversion and lowering slices that prove the direct-vs-closure split:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.Convert/keeps direct first-order local calls on the direct application path/"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.Convert/classifies function-valued case pattern fields as closure locals/"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers case-selected closure callees through the explicit closure ABI/"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers let-selected closure callees through the explicit closure ABI/"'
```

5. Focused malformed-backend rejection proof for the retired lowerer convention:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/rejects BackendApp heads that select closure values through let or case/"'
```

6. Mechanism-table gate check:

```sh
table=docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md
test "$(awk -F'|' '$2 == " IR role separation and non-duplication " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
test "$(awk -F'|' '$2 == " Eager runtime lowering contract " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
test "$(awk -F'|' '$2 == " Direct calls, closure values, and callable shapes " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
for mechanism in \
  'ADT/case semantics versus layout' \
  'Primitive operations and eager evaluation order' \
  'Polymorphism erasure and lowerability' \
  'Validation, evidence, and guidance synchronization'
do
  test "$(awk -F'|' -v m="$mechanism" '$2 == " " m " " {gsub(/ /, "", $7); print $7}' "$table")" = "NO"
done
```

7. Full required gate, because `src/` and `test/` are in scope:

```sh
cabal build all && cabal test
```

## Worker Fan-Out

`worker-plan.json` is not needed for this round. The write scope is one serial
callable-shape refinement slice across tightly coupled IR, conversion,
lowering, contract docs, focused tests, and one mechanism-table row. The
current evidence also showed `cabal` contention against shared
`dist-newstyle/`, so splitting this work would add coordination risk without
creating an independent lane.
