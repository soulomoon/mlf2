# Round 118 `P5` Provenance Lane Summary

Date: 2026-03-27
Round: `round-118`
Roadmap item: `item-2`
Lane: `P5 provenance`
Authoritative status: non-authoritative lane-local scratch only

## Harness Presence

- `test -f test/Research/P5ClearBoundarySpec.hs` -> `pass`

## Focused Command

- Command:
  `cabal test mlf2-test --builddir=dist-newstyle-round118-p5 --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
- Build dir: `dist-newstyle-round118-p5`
- Result: `pass`
- Harness summary: `2 examples, 0 failures`

## Fresh Clear-Boundary Versus Nested-`forall` Read

- Clear-boundary same-lane retained-child contrast:
  the fallback surface remains recursive with `containsMu True`.
- Nested-`forall` contrast:
  once the same wrapper crosses a nested `forall` boundary, the fallback
  surface fails closed with `containsMu False`.

These reads come from the passing expectations in
`test/Research/P5ClearBoundarySpec.hs` and were republished only after the
isolated rerun above passed.

## Scope Note

This lane recorded provenance-only contrast context. It did not reopen the
settled same-lane `C2` / `C5` / `C7` pocket as live debt and did not edit the
canonical refreshed matrix artifact or `implementation-notes.md`.
