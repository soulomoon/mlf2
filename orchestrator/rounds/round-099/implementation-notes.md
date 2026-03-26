# Round 099 Implementation Notes

- This round implements only roadmap item `1` for the refreshed global
  `non-cyclic-graph` settlement family and lands the canonical docs artifact
  at
  `docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`.
- The new artifact freezes the exact later item-5 acceptance bar for
  `non-cyclic-graph = keep` versus
  `reopen the non-cyclic-graph revision question`,
  freezes the unresolved family ledger across `P1` through `P6` and `N1`
  through `N6`, and preserves the accepted predecessor evidence chain
  without selecting either outcome now.
- The inherited boundary remains unchanged:
  explicit-only baseline,
  iso-recursive only,
  non-equi-recursive,
  non-cyclic-graph,
  no second interface,
  and no fallback widening.
- Accepted rounds `round-094` through `round-098` remain bounded
  exact-pocket predecessor evidence only, not repo-level settlement by
  themselves.
- This round stays docs-only. It does not touch `src/`, `src-public/`,
  `app/`, `test/`, `mlf2.cabal`, `Bugs.md`, roadmap contracts,
  `orchestrator/state.json`, or the preserved round-099 recovery / selection
  / plan artifacts.
