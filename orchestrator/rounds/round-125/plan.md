# Round 125 Plan (`item-2` Bounded `P1` Implementation Slice)

## Objective

Execute roadmap item `2` only:
implement and validate one bounded current-architecture `P1`
authoritative-surface continuation slice for the exact frozen packet
`ELam "x" (EVar "x")`.

This round is `attempt-1` with `retry: null`.

## Frozen Inputs

- the March 14 baseline contract;
- the March 25 capability contract;
- the accepted March 28 `C1` / `P2` successor gate;
- the accepted March 28 item-1 freeze
  `docs/plans/2026-03-28-p1-local-recursive-shape-successor-authority-success-bar-and-writable-slice-freeze.md`;
- `selection.md` for `round-125`; and
- the existing local-shape coverage in `test/PipelineSpec.hs`.

## Exact Lawfulness Risk

The frozen live packet is the plain identity lambda
`ELam "x" (EVar "x")`.
Under the inherited architecture, making that exact packet recursively visible
may be semantically suspect rather than a mere continuity bug.

This round must not silently assume success.
It must first answer a narrower question:

- does the exact frozen packet already acquire lawful recursive structure on a
  current-architecture internal path that the authoritative surfaces later
  drop; or
- is the exact frozen packet non-recursive by the inherited semantics, in
  which case forcing `containsMu True` would be an unauthorized widening?

If the second read wins, the round must fail closed under review instead of
manufacturing a recursive public result.

## Writable Slice

The round may write only inside the item-1 frozen slice:

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`
- `test/PipelineSpec.hs`
- `test/Research/P1LocalRecursiveShapeSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`
- `orchestrator/rounds/round-125/*`

The round may narrow below this list. It may not widen beyond it.

## Parallel-Lane Decision

No parallel sidecars are authorized.
This item has one critical path and one authoritative round-owned output set.

## Sequential Task List

1. Establish the exact current read for the frozen packet and the inherited
   control.
   - Reconfirm the current authoritative-surface behavior for:
     - the positive same-lane local `TypeRef` control in `test/PipelineSpec.hs`;
     - the exact unannotated packet `ELam "x" (EVar "x")`.
   - Audit only the writable-slice pipeline/result-type files to determine
     whether the unannotated packet ever carries recursive structure on a
     lawful current-architecture internal path before public output.
   - Record the branch point explicitly in `implementation-notes.md`:
     `continuity-loss candidate found`
     or
     `no lawful recursive carrier found`.

2. Take only the bounded branch justified by task 1.
   - If task 1 finds a lawful continuity-loss candidate, implement the
     smallest fix that preserves recursive structure for the exact frozen
     packet on the existing authoritative surfaces only.
   - Any such fix must stay inside the inherited architecture:
     no equi-recursive reasoning,
     no cyclic or multi-SCC search,
     no second interface,
     no fallback widening,
     and no promotion of helper-only or diagnostic-only surfaces into
     authority.
   - If task 1 does not find a lawful recursive carrier, do not force a
     recursive output. Preserve production semantics, and limit the round to a
     bounded fail-closed evidence slice in tests plus round notes.

3. Refresh focused coverage for the exact packet only.
   - Keep the annotated same-lane local `TypeRef` control as inherited
     contrast, not as a second live lane.
   - Update `test/PipelineSpec.hs` and only add
     `test/Research/P1LocalRecursiveShapeSpec.hs` if the route audit needs a
     tighter exact-packet harness.
   - The focused coverage must make one of the following review-visible:
     - lawful authoritative-surface recursive visibility for the exact
       unannotated packet; or
     - explicit fail-closed evidence that no lawful current-architecture
       recursive carrier exists for that packet.

4. Verify and record the bounded outcome.
   - Run the focused `P1` checks under serialized or isolated build outputs.
   - Because any code or test change inside the frozen slice triggers the live
     verification contract, run `cabal build all && cabal test` after the
     focused reruns if `src/`, `src-public/`, `test/`, or `mlf2.cabal` changed.
   - Record the chosen branch, commands, and outcome in
     `implementation-notes.md` so review can either accept the bounded result
     or fail it closed without widening the family.

## Verification Commands

- `cabal test mlf2-test --builddir=dist-newstyle-round125-control --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
- `cabal test mlf2-test --builddir=dist-newstyle-round125-p1 --test-show-details=direct --test-options='--match "does not infer recursive shape for the corresponding unannotated variant"'`
- `if git diff --name-only -- src src-public app test mlf2.cabal | grep -q .; then cabal build all && cabal test; else printf '\''skip full cabal gate for docs-only round\n'\''; fi`
- `git diff --check`

## Exit Criteria

This round is complete only when all of the following are true:

- the work stays inside the frozen `P1` writable slice and exact packet;
- the round explicitly resolves the lawfulness probe in one direction:
  either `lawful continuity fix validated` or `fail-closed no-lawful-path`;
- focused `P1` evidence is refreshed for the exact unannotated packet plus the
  inherited control;
- the full Cabal gate passes if any code or test path was touched; and
- `implementation-notes.md` records the branch taken, the evidence, and any
  remaining exact-packet blocker debt without promoting the result into
  general `P1` success or repo-level readiness.
