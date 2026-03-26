# Round 118 `C1` Provenance Lane Summary

Date: 2026-03-27
Round: `round-118`
Roadmap item: `item-2`
Lane: `C1 provenance`
Authoritative status: non-authoritative lane-local scratch only

## Harness Presence

- `test -f test/Research/C1AuthoritativeSurfaceSpec.hs` -> `pass`

## Focused Command

- Command:
  `cabal test mlf2-test --builddir=dist-newstyle-round118-c1 --test-show-details=direct --test-options='--match "C1 authoritative-surface harness"'`
- Build dir: `dist-newstyle-round118-c1`
- Result: `pass`
- Harness summary: `2 examples, 0 failures`

## Fresh Authoritative-Surface Read

- Fallback surface:
  `TBase (BaseTy "Int")` with `containsMu False`.
- Public pipeline entrypoint `runPipelineElab`:
  `TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))` with
  `containsMu False`.
- Public pipeline entrypoint `runPipelineElabChecked`:
  `TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))` with
  `containsMu False`.

These reads come from the passing expectations in
`test/Research/C1AuthoritativeSurfaceSpec.hs` and were republished only after
the isolated rerun above passed.

## Scope Note

This lane recorded provenance only. It did not edit the canonical refreshed
matrix artifact or `implementation-notes.md`.
