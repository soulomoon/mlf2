# Round 129 Implementation Notes

Date: 2026-03-28
Round: `round-129`
Roadmap item: `item-2`
Attempt: `attempt-1`
Outcome branch: `quantified-boundary fail-closed remains lawful`

## Lawfulness Probe

- Audited the frozen writable production slice around:
  `src/MLF/Elab/Run/ResultType/Fallback.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`.
- Discarded an inherited out-of-scope worktree hunk in
  `src/MLF/Elab/Elaborate/Annotation.hs` before taking evidence, because the
  item-1 freeze does not authorize writes there and the hunk changed the
  authoritative failure shape.
- The exact quantified-crossing packet
  `nestedForallContrastExpr` still fails closed on the fallback surface with
  `containsMu False`, while the clear-boundary control
  `sameLaneClearBoundaryExpr` remains recursive with `containsMu True`.
- A focused authoritative-entrypoint probe shows the quantified-crossing
  packet does not carry a lawful recursive result to current authoritative
  surfaces. Instead, both `runPipelineElab` and `runPipelineElabChecked`
  fail at the same Phase 6 elaboration point with:
  `PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation ..."]`.
- No lawful recursive carrier for the exact quantified-crossing packet was
  found on a current-architecture path that later public output merely drops.

## Chosen Branch

- `quantified-boundary fail-closed remains lawful`
- No production code change was made.
- The round stays bounded and fail-closed: preserve inherited semantics for
  the exact packet, sharpen the exact authoritative-surface read, and do not
  manufacture recursive visibility for `P5`.

## Verification Outcome

- Focused `P5` harness rerun passed in isolated build dir
  `dist-newstyle-round129-p5-fresh`.
- A first full-gate attempt in isolated build dir `dist-newstyle-round129-full`
  failed during test-suite object-file renames before the suite ran. This read
  is treated as build-system noise, not semantic evidence about the packet.
- The full repo gate then passed serially in isolated build dir
  `dist-newstyle-round129-full-serial`:
  `cabal build all -j1 --builddir=dist-newstyle-round129-full-serial && cabal test -j1 --builddir=dist-newstyle-round129-full-serial`.
- `git diff --check` passed in the round worktree.
- `python3 -m json.tool orchestrator/state.json >/dev/null` passed at the
  repo root.

## Files Changed

- `test/Research/P5ClearBoundarySpec.hs`
- `orchestrator/rounds/round-129/implementation-notes.md`

## Commands

- `cabal test mlf2-test --builddir=dist-newstyle-round129-p5-fresh --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
- `cabal build all --builddir=dist-newstyle-round129-full && cabal test --builddir=dist-newstyle-round129-full`
- `cabal build all -j1 --builddir=dist-newstyle-round129-full-serial && cabal test -j1 --builddir=dist-newstyle-round129-full-serial`
- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
