# Round 110 Plan (`item-2` Bounded Same-Pocket Authoritative-Handoff Architecture Amendment)

## Objective

Execute only roadmap item `2` and land one bounded implementation round for
the already-selected same-lane `C2` / `C5` / `C7` pocket.

This round must change only the writable slice frozen by accepted
`round-109`:

- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/TermClosure.hs`
- `test/PipelineSpec.hs`

The implementation target is exact and narrow:

- keep the same exact packet, tuple, family, anchor, owner-local frame,
  route, and clear-boundary-only status;
- keep the same exact
  `runPipelineElabWith` / `checkedAuthoritative` /
  `typeCheck termClosed` authoritative-handoff path;
- carry one bounded single-component cyclic-structure result through that
  same authoritative-handoff path for the one selected same-pocket packet;
- preserve `iso-recursive = keep`,
  `non-equi-recursive = keep`, and
  `no-fallback = keep`;
- keep one public interface and one public-entrypoint chain;
- keep rev-001 items `6` through `8` blocked; and
- keep multi-SCC search, second interfaces, fallback widening, helper-route
  redesign, rollout, hardening, and broad capability claims blocked.

This round is the first code-changing rev-003 round. TDD is mandatory:
write the failing exact-pocket public-output test first, verify the failure,
then implement the minimal bounded handoff amendment to make that exact test
pass without widening beyond the frozen slice.

## Locked Round Context

- Round id: `round-110`
- Roadmap item: `item-2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Active branch: `codex/round-110`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-110`
- Fixed live subject: one bounded same-pocket authoritative-handoff
  architecture amendment for the already-selected same-lane `C2` / `C5` /
  `C7` pocket only
- Fixed inherited boundary:
  `explicit-only / iso-recursive / non-equi-recursive / no-fallback / one-interface / no-multi-SCC`

Current worktree state is already non-pristine at the control-plane level.
Respect existing edits and do not revert unrelated work:

- controller-owned `orchestrator/state.json` is modified and must remain
  untouched by implementation work; and
- existing `orchestrator/rounds/round-110/selection.md` is a round input and
  must remain untouched.

Reviewer outcome constraints for this stage remain:

- `accepted + finalize`
- `accepted + retry`
- `rejected + retry`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-110/selection.md`
  fixes this round to roadmap item `2` only, limits the writable boundary to
  `Pipeline.hs`, `TermClosure.hs`, and `PipelineSpec.hs`, and keeps all
  inherited blocked work blocked.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/roadmap.md`
  makes item `2` the lowest-numbered unfinished item whose dependencies are
  now satisfied by accepted `round-109`.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/retry-subloop.md`
  allows retry for item `2` and requires `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`
  on every review.
- `orchestrator/rounds/round-109/review-record.json`
  is the authoritative rev-003 item-1 acceptance record. It froze the same
  selected `C2` / `C5` / `C7` pocket, the same exact packet and tuple, the
  exact `runPipelineElabWith` / `checkedAuthoritative` /
  `typeCheck termClosed` handoff slice, the exact future writable boundary,
  and the read-only audit anchors.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-architecture-amendment-contract-and-writable-slice-freeze.md`
  is the authoritative rev-003 item-1 freeze and therefore the direct scope
  contract for this implementation round.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
  is accepted blocker-proof context. It records the current root-handoff
  replay facts: `rootScheme = Forall [("a",Nothing)] TVar "a"`,
  `typeCheck term = Right (TForall "a" Nothing (TVar "a"))`,
  `typeCheck termSubst = Right (TForall "a" Nothing (TVar "a"))`,
  `typeCheck termClosed = Right (TForall "a" Nothing (TVar "a"))`, and the
  root elaborated term already entering the handoff as
  `ELet "u" Forall [("a",Nothing)] TVar "a" ...`.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
  remains the authoritative public-output path audit and fixes
  `checkedAuthoritative` plus `termClosed` / `typeCheck termClosed` as the
  exact same-pocket continuity-loss site.
- `test/PipelineSpec.hs`
  already contains the exact-pocket current-state evidence:
  helper-visible/internal recursion remains visible on the same pocket,
  while the exact authoritative public-output example still expects
  `TForall "a" Nothing (TVar "a")`.

## Working Hypothesis

The exact-pocket public collapse remains localized to the root
`ELet` / scheme / closure handoff in `runPipelineElabWith`, not to the
selected read-only fallback route itself.

Current evidence says:

- helper-visible/internal recursion still survives on the exact same pocket;
- the public collapse is already present in the root elaborated term as
  `ELet "u" Forall [("a",Nothing)] TVar "a" ...`; and
- the current authoritative handoff always returns the checked type of that
  root-closed term.

The bounded item-2 attempt should therefore test one narrow amendment:

- preserve the exact same-pocket retained-child result inside the root
  closure / checked-authoritative handoff by rewriting only the selected
  root closure logic in `Pipeline.hs` / `TermClosure.hs`, while keeping the
  same public entrypoint count and leaving all read-only fallback anchors
  untouched.

This hypothesis must fail closed if the only way to pass the new exact-pocket
test would require:

- editing read-only fallback/result-type/public-interface modules;
- changing helper-route ownership;
- widening to additional packets, routes, or interfaces;
- enabling fallback widening or multi-SCC behavior; or
- introducing broad behavior changes outside the one selected packet.

## File Map

### Writable Files

- `test/PipelineSpec.hs`
  - add the failing exact-pocket public-output expectation first;
  - keep assertions exact-pocket-only;
  - retain neighboring same-pocket helper-visible tests so the amendment
    continues to preserve internal continuity.
- `src/MLF/Elab/Run/Pipeline.hs`
  - make the minimal root authoritative-handoff change;
  - keep `runPipelineElabWith` on the same exact handoff path;
  - do not widen to additional result-type routes or interfaces.
- `src/MLF/Elab/TermClosure.hs`
  - add only the minimal helper needed to preserve the same-pocket retained-
    child recursive result through the selected root closure path;
  - keep helpers total and narrowly responsibility-scoped.

### Read-Only Audit Anchors

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/ResultType.hs`
- `src/MLF/Elab/Run.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`

### Preserve Unchanged

- `orchestrator/state.json`
- `orchestrator/rounds/round-110/selection.md`
- roadmap files under
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/`
- `Bugs.md`
- every `docs/plans/` artifact
- `src/` outside the exact writable slice
- `src-public/`
- `app/`
- `mlf2.cabal`

## TDD Task Sequence

### Task 1 - Write the exact-pocket failing public-output test

Write the smallest failing test in `test/PipelineSpec.hs` that proves item
`2` has not landed yet.

Required target:

- the exact same packet currently covered by
  `same-lane retained-child exact packet authoritative public output stays forall identity`.

Replace or split that expectation so the new test instead requires:

- the exact authoritative public output for the exact same packet is no
  longer the current `forall identity` collapse; and
- the returned checked/public type carries one bounded recursive component
  (`containsMu True`) for that same exact packet only.

Keep the test exact-pocket-only:

- no second packet;
- no broader family matrix;
- no second interface;
- no change to unrelated current-state guards.

### Task 2 - Run the new test and verify RED

Run only the new exact-pocket public-output test first.

Expected RED result:

- the test fails because the current authoritative public output is still
  `TForall "a" Nothing (TVar "a")` on the exact same packet.

If the failure is not the expected behavioral failure, fix the test before
touching production code.

### Task 3 - Add the minimal bounded handoff amendment

Implement the narrowest possible fix in `Pipeline.hs` / `TermClosure.hs`.

Implementation constraints:

- keep the change centered on the selected root closure /
  checked-authoritative handoff;
- keep the same one-interface chain;
- do not edit read-only fallback/result-type/public-interface modules;
- do not widen beyond the exact same packet;
- prefer a term-closure / root-let coherence adjustment over a new route,
  new interface, or generalized fallback authority switch.

The production change should aim to:

- preserve the same-pocket retained-child recursive structure already visible
  internally;
- surface that structure on the authoritative public path for this one exact
  packet; and
- keep unrelated let/annotation paths unchanged.

### Task 4 - Verify GREEN with bounded targeted reruns

After the code change, rerun the new exact-pocket public-output test and the
neighboring exact-pocket continuity tests:

- `same-lane retained-child exact packet clears Phase 6 elaboration`
- `keeps retained-child fallback recursive through a same-lane local TypeRef root`
- `keeps local-binding recursive retention processable through a direct wrapper`

Goal:

- the new public-output test passes;
- the exact same-pocket helper-visible/internal continuity tests still pass;
- no unrelated exact-pocket guard regresses.

### Task 5 - Run the full repo gate

Because this round touches `src/` and `test/`, run the full required gate:

- `cabal build all && cabal test`

This full gate is mandatory before review can accept the round.

## Acceptance Criteria

1. `test/PipelineSpec.hs` contains one exact-pocket item-2 test that fails
   before implementation because the current public output still collapses to
   `forall identity`.
2. `src/MLF/Elab/Run/Pipeline.hs` and, if needed,
   `src/MLF/Elab/TermClosure.hs` change only enough to carry one bounded
   recursive component through the exact same authoritative-handoff path for
   the one selected packet.
3. No file outside the frozen writable slice changes.
4. The exact-pocket helper-visible/internal continuity tests remain green.
5. The exact-pocket public-output test no longer reports
   `TForall "a" Nothing (TVar "a")` for that packet and instead shows a
   bounded recursive component (`containsMu True`).
6. `cabal build all && cabal test` passes.
7. The diff does not imply multi-SCC search, second interfaces, fallback
   widening, helper-route redesign, or broad capability claims.

## Errors Encountered

- A read-only Haskell probe attempted with `cabal exec runghc` in the fresh
  `round-110` worktree failed because that worktree did not yet have a local
  `dist-newstyle` package database.
- Rerunning the same probe from the base worktree also failed due a local
  Cabal package-environment mismatch (`cannot satisfy -package-id
  base-4.21.0.0-958c`).
- These probe failures do not block item-2 implementation because the
  accepted docs and existing exact-pocket tests already fix the relevant
  root-handoff facts.
