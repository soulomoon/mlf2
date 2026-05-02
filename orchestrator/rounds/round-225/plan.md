# Round 225 Plan

- Round: `round-225`
- Roadmap:
  `2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`
- Milestone: `milestone-4`
- Direction: `direction-4a-freeze-adt-layout-ownership`
- Extracted item: absent
- Retry: `null`
- Actionable slice: `bounded production proof slice`
- Execution shape: serial, one milestone-4 ADT/layout ownership freeze only,
  shared `dist-newstyle/` discipline, no worker fan-out, no controller-state
  edits

## Objective

Close mechanism-table row 4 on top of current `HEAD = 2c1661b3` by freezing
the current semantic/layout split so that:

- `BackendData`, `BackendConstructor`, `BackendConstruct`, and `BackendCase`
  continue to own semantic ADT/case structure only.
- checked-program conversion continues to publish those semantic nodes without
  inventing runtime tags, field offsets, nullary strategy, or public
  layout-only forms.
- LLVM/native lowering remains the sole owner of runtime layout policy for
  that same IR, including constructor-order tag assignment, tag-plus-field
  object layout, closure-vs-runtime field storage, and nullary-constructor
  representation.
- the evidence for that split becomes reviewable in synchronized docs/module
  notes plus focused LLVM/guard coverage, so row 4 can move from `NO` to
  evidence-backed `YES` without widening the backend boundary.

This round must stay strictly inside `milestone-4`. Do not widen into
milestone-5 primitive/evaluation-order work, milestone-6
polymorphism-lowerability work, milestone-7 closeout synchronization, a second
backend IR, a public lowering surface, lazy STG machinery, fallback runtime
paths, or any backend-boundary revision not explicitly authorized by
`rev-001`.

## Decision

This round should be behavior-preserving but code/test-bearing rather than
docs-only.

Why:

- `docs/architecture.md`, `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`, and `src/MLF/Backend/LLVM/Lower.hs` already
  freeze the one-backend-IR, eager-runtime, and callable-shape contracts, but
  they do not yet identify row-4 ownership explicitly enough to say where tag
  numbering, field slot placement, closure-field boxing, and nullary strategy
  live.
- `src/MLF/Backend/LLVM/Lower.hs` already fixes those behaviors through
  helpers such as `constructorRuntimes`, `lowerConstruct`, `lowerHeapCase`,
  and `constructorFieldStoredValueKind`, so the current mechanism-table gap is
  correctly about contract/evidence, not a missing backend feature.
- `test/BackendLLVMSpec.hs` already proves selected ADT lowering behavior, but
  the current assertions are still too loose to serve as a durable row-4
  layout-policy freeze by themselves.
- a docs-only round would leave row 4 resting on prose while the concrete
  lowerer-owned layout policy remained mechanically under-specified.

The bounded fix is therefore: publish the semantic-vs-layout ownership split
across the durable backend surfaces, allow only a private lowerer-local
consolidation of existing layout helpers if needed for clarity, and strengthen
focused LLVM/guard evidence around the current tag/layout/nullary/boxing
behavior. No new public/backend IR surface and no runtime-policy redesign are
authorized.

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
- any new `src-public/MLF/Backend*`, `*LowerableBackend*`, fallback runtime
  path, lazy-runtime surface, or second executable-IR surface

## Locked Context

- Active selection:
  `orchestrator/rounds/round-225/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- Roadmap row anchor:
  row 4, `ADT/case semantics versus layout`, is the first lawful `NO` in
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- Retry state: `null`; there is no same-round retry delta to honor

Current worktree state is already non-pristine. Respect controller-owned edits
and do not revert them:

- `M orchestrator/retry-subloop.md`
- `M orchestrator/roadmap.md`
- `M orchestrator/state.json`
- `M orchestrator/verification.md`
- `?? orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/`
- `?? orchestrator/rounds/round-225/`

## Sequential Plan

### Task 1: Publish the semantic-versus-layout ownership boundary

**Files**

- `docs/architecture.md`
- `docs/backend-native-pipeline.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`

**Required edits**

- In `docs/architecture.md`, extend the backend-boundary section so it states
  explicitly that `BackendData`, `BackendConstructor`, `BackendConstruct`, and
  `BackendCase` preserve semantic ADT/case structure only, while runtime tag
  values, field layout, function-field storage/boxing policy, and
  nullary-constructor representation stay private to LLVM/native lowering.
- In `src/MLF/Backend/IR.hs`, tighten the boundary note so the semantic side of
  row 4 is explicit at the IR boundary:
  constructor metadata and case structure live here;
  tags, field offsets, nullary representation, and layout-only witnesses do
  not.
- In `src/MLF/Backend/Convert.hs`, state that checked-program conversion
  publishes semantic constructor/case nodes only and must not assign runtime
  layout policy.
- In `src/MLF/Backend/LLVM/Lower.hs`, make the current private layout policy
  explicit next to the existing lowering-boundary note. If needed for
  readability, factor the existing behavior into small private helpers or
  named local constants so the review can point to one obvious owner for:
  declaration-order zero-based constructor tags,
  tag storage at object offset `0`,
  successive field slots after the tag word,
  closure-record storage for function-like constructor fields, and
  tag-only heap objects for nullary constructors.
- In `docs/backend-native-pipeline.md`, add the emitted-layout inspection
  contract for raw/native LLVM output so the test-facing doc matches the
  lowerer-owned policy above without turning that policy into a second IR or
  public lowering interface.

### Task 2: Lock row-4 behavior with focused LLVM and guard evidence

**Files**

- `test/BackendLLVMSpec.hs`
- `test/RepoGuardSpec.hs`

**Required edits**

- Strengthen `test/BackendLLVMSpec.hs` so row-4 evidence freezes the current
  lowerer-owned layout behavior rather than only broad ADT lowering success.
  The focused row-4 evidence must cover:
  declaration-order constructor tags driving the emitted `switch`,
  field loads/stores occurring after the tag slot,
  function-valued constructor fields staying on the explicit closure ABI, and
  nullary constructors using the same semantic `BackendConstruct` /
  `BackendCase` path with tag-only runtime representation.
- Prefer tightening the existing tests
  `lowers Nat construction and case analysis to heap tags and switch`,
  `loads only constructor fields used by a case branch`,
  `lowers source closure-valued constructor fields through the explicit closure ABI`,
  and `lowers nullary and recursive-list constructors through case`
  so the evidence stays localized to row 4 instead of scattering into a new
  mini-suite.
- Add one repository guard named exactly:
  `ADT and case semantic boundary stays explicit while lowerer-owned layout policy stays private and frozen`.
  The guard must read
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs`,
  and fail if the synchronized row-4 ownership markers disappear.
- Do not add new primitive/evaluation-order claims, polymorphism-lowerability
  claims, alternative runtime layouts, or fallback decoding paths while
  strengthening this evidence.

### Task 3: Refresh mechanism-table row 4 only

**File**

- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`

**Required edits**

- Refresh the `Last updated (UTC)` line if the artifact changes.
- Update only the row `ADT/case semantics versus layout` to reflect the landed
  contract freeze:
  change the gap summary from implicit-lowerer-behavior language to accepted
  contract language,
  expand the evidence column to cite the synchronized doc/module surfaces plus
  the focused `BackendLLVMSpec` and `RepoGuardSpec` coverage, and
  flip the gate from `NO` to `YES`.
- Keep row 4 honest about the preserved ownership split:
  semantic ADT/case nodes remain in `MLF.Backend.IR`,
  layout policy remains lowerer-owned and private,
  and row 4 does not authorize a second executable IR or a public lowering
  boundary.
- Change that row's `Next action` so the next live blocker points at
  milestone-5 / row-5 primitive-operation and eager-evaluation-order work.
- Keep mechanism order fixed, keep rows 1 through 3 at `YES`, and keep rows 5
  through 7 at `NO`.

## Binary Acceptance Criteria

- PASS if the only repo-facing implementation edits are the eight files named
  in `Authorized Write Scope`.
- PASS if `docs/architecture.md`, `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`, `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs` all agree that:
  semantic constructor/case structure belongs to `MLF.Backend.IR`, while
  tag assignment, field layout, function-field storage, and nullary strategy
  are lowerer-owned private policy.
- PASS if focused LLVM evidence now freezes the current declaration-order tag
  policy, tag-skipping field offsets, closure-field storage path, and nullary
  tag-only representation.
- PASS if `test/RepoGuardSpec.hs` enforces the synchronized row-4 ownership
  markers across the named contract surfaces.
- PASS if the mechanism table flips row 4 to `YES`, preserves rows 1 through 3
  at `YES`, and leaves rows 5 through 7 at `NO`.
- FAIL if the round widens into milestone-5, milestone-6, or milestone-7
  claims; changes backend behavior beyond freezing the current layout policy;
  introduces a second backend IR, a public lowering surface, lazy STG
  machinery, fallback runtime paths, worker fan-out, or controller-state /
  roadmap-history edits.

## Verification Commands

Run these in order:

1. Diff hygiene:

```sh
git diff --check
```

2. Focused repository guard for the row-4 ownership contract:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/ADT and case semantic boundary stays explicit while lowerer-owned layout policy stays private and frozen/"'
```

3. Focused semantic-node baseline proofs that row 4 must preserve:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/accepts ADT construction and case analysis through constructor metadata/"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.Convert/recovers explicit backend constructors and cases from checked ADT paths/"'
```

4. Focused LLVM row-4 layout-policy evidence:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers Nat construction and case analysis to heap tags and switch/"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/loads only constructor fields used by a case branch/"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers source closure-valued constructor fields through the explicit closure ABI/"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers nullary and recursive-list constructors through case/"'
```

5. Mechanism-table gate check:

```sh
table=docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md
test "$(awk -F'|' '$2 == " IR role separation and non-duplication " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
test "$(awk -F'|' '$2 == " Eager runtime lowering contract " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
test "$(awk -F'|' '$2 == " Direct calls, closure values, and callable shapes " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
test "$(awk -F'|' '$2 == " ADT/case semantics versus layout " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
for mechanism in \
  'Primitive operations and eager evaluation order' \
  'Polymorphism erasure and lowerability' \
  'Validation, evidence, and guidance synchronization'
do
  test "$(awk -F'|' -v m="$mechanism" '$2 == " " m " " {gsub(/ /, "", $7); print $7}' "$table")" = "NO"
done
```

6. Full required gate, because `src/` and `test/` are in scope:

```sh
cabal build all && cabal test
```

## Worker Fan-Out

`worker-plan.json` is not needed for this round. The write scope is one serial
ADT/layout ownership freeze across tightly coupled contract docs, backend
module notes, lowerer-private layout helpers, focused LLVM assertions, and one
mechanism-table row. The roadmap family is explicitly serial, and shared
`dist-newstyle/` state still makes parallel worker execution higher-risk than
useful for this slice.
