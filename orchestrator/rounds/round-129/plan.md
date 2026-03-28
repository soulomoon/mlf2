# Round 129 Plan (`item-2` Bounded `P5` Implementation Slice)

## Stage Scope

Implement roadmap item `2` only for `round-129`, `attempt-1`, with
`retry: null`.

The live subject stays frozen to one exact `P5` packet only:

- family row: `C3`
- bounded control: `sameLaneClearBoundaryExpr`
- exact quantified-crossing packet: `nestedForallContrastExpr`

This round may improve or clarify only the current authoritative-surface read
for that exact packet inside the inherited current architecture.
It must not reopen `C1`, the settled same-lane `C2` / `C5` / `C7` pocket, or
the exact settled `P1` packet.

## Binding Authority

Carry forward only the following authority for this plan:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
- `docs/plans/2026-03-28-post-p1-local-recursive-shape-successor-gate-and-immediate-handoff-decision.md`
- `docs/plans/2026-03-28-p5-polymorphism-nested-forall-successor-authority-success-bar-and-writable-slice-freeze.md`
- `orchestrator/rounds/round-129/selection.md`
- `orchestrator/rounds/round-118/lanes/p5-provenance-summary.md`
- `orchestrator/rounds/round-128/review.md`

`Bugs.md` is context only here. The open `BUG-2026-03-16-001` does not define
the `P5` read and does not pre-authorize treating this packet as a production
bug.

## Lawfulness Discriminator

The central planning risk is semantic, not just mechanical:
the quantified-crossing packet may remain fail-closed by construction under
the inherited quantified-boundary guard.

This round must therefore distinguish exactly one of these reads before any
fix is treated as lawful:

1. `continuity-loss candidate found`
   The exact packet acquires a lawful recursive carrier on an existing
   current-architecture route, and that carrier is later dropped or blocked
   before the current authoritative surfaces finalize the result.

2. `quantified-boundary fail-closed remains lawful`
   Once the wrapper crosses the nested `forall` boundary, the exact packet
   does not lawfully carry recursive structure on the inherited route, or the
   current authoritative entrypoints fail closed for the same inherited reason
   without evidence that a lawful recursive carrier was already present.

If the second read wins, item `2` must not force recursive visibility for the
packet. That would be unauthorized widening.

If the first read wins but the only plausible fix point lies outside the
item-1 frozen writable slice, item `2` still must not widen the slice. Record
that outcome in `implementation-notes.md` and stop at bounded evidence rather
than editing out-of-scope files.

## Frozen Writable Slice

The round may write only inside:

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`
- `test/Research/P5ClearBoundarySpec.hs`
- `test/PipelineSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`
- `orchestrator/rounds/round-129/*`

The round may narrow below this list.
It may not widen beyond it.

## Parallelism

No parallel sidecars are authorized.
This plan is intentionally serial because one lawfulness decision gates every
later action, and the verification contract forbids treating shared-build
collisions as domain evidence.

## Serial Task List

1. Reconfirm the inherited bounded contrast under isolated builds.
   - Rerun the existing `P5` research harness for the exact control and exact
     quantified-crossing packet.
   - Reconfirm the inherited starting read:
     clear-boundary control stays recursive on the fallback surface;
     quantified-crossing contrast fails closed on the fallback surface.
   - Reconfirm the current authoritative entrypoint behavior for the same two
     expressions through the existing research harness before proposing any
     code change.

2. Audit the exact continuity path inside the frozen production slice.
   - Inspect only the current authoritative path reachable from:
     `src/MLF/Elab/Run/ResultType/Fallback.hs`,
     `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Pipeline.hs`,
     and `src-public/MLF/Pipeline.hs`.
   - Determine whether the quantified-crossing packet differs from the control
     because recursive structure is lost after a lawful carrier exists, or
     because the inherited quantified-boundary rule blocks a lawful carrier
     before authority is reached.
   - Record the discriminator explicitly in
     `orchestrator/rounds/round-129/implementation-notes.md` as either:
     `continuity-loss candidate found`
     or
     `quantified-boundary fail-closed remains lawful`.

3. Take only the branch justified by task 2.
   - If the audit shows a real authoritative-surface continuity bug and the
     smallest fix lies inside the frozen writable slice, implement only that
     minimal fix.
   - If the audit shows lawful fail-closed behavior, or if the only fix point
     sits outside the frozen writable slice, do not force recursion and do not
     widen the slice. Limit item `2` to bounded evidence and test updates that
     make the fail-closed read explicit on the current authoritative surfaces.

4. Refresh focused coverage for the exact packet only.
   - Keep `sameLaneClearBoundaryExpr` as bounded control only.
   - Update `test/Research/P5ClearBoundarySpec.hs` first, because it already
     owns the exact packet names and current authoritative-entrypoint probes.
   - Touch `test/PipelineSpec.hs` only if one additional production-path
     anchor is needed to make the authoritative-surface consequence
     review-visible.
   - The refreshed coverage must make exactly one item-2 outcome explicit for
     the exact packet:
     lawful recursive visibility on the current authoritative surfaces,
     or lawful fail-closed behavior on those surfaces.

5. Verify serially and record the bounded outcome.
   - Run focused reruns with isolated build directories and no concurrent
     Cabal builds.
   - If any file under `src/`, `src-public/`, `test/`, or `mlf2.cabal`
     changed, run the full gate `cabal build all && cabal test`.
   - Run the baseline controller checks required by the verification contract.
   - Record commands, build dirs, branch choice, and remaining exact-packet
     blocker debt in `implementation-notes.md`.

## Verification Commands

- `cabal test mlf2-test --builddir=dist-newstyle-round129-p5 --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
- `if git diff --name-only -- test/PipelineSpec.hs | grep -q .; then cabal test mlf2-test --builddir=dist-newstyle-round129-pipeline --test-show-details=direct --test-options='--match "nested forall boundary"'; else printf 'skip extra PipelineSpec anchor rerun\n'; fi`
- `if git diff --name-only -- src src-public app test mlf2.cabal | grep -q .; then cabal build all && cabal test; else printf 'skip full cabal gate for docs-only round\n'; fi`
- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`

## Exit Criteria

This round is complete only when all of the following are true:

- the work remains inside the exact `P5` packet and the frozen writable slice;
- the round explicitly resolves the lawfulness discriminator in one direction:
  `continuity-loss candidate found`
  or
  `quantified-boundary fail-closed remains lawful`;
- the chosen branch is made review-visible on current authoritative surfaces
  without silently widening semantics, interfaces, or search;
- focused reruns are serialized or isolated and recorded as such;
- the full Cabal gate passes if code or test paths changed; and
- `implementation-notes.md` records the bounded result without promoting it
  into general `P5` settlement or repo-level readiness.
