# Round 179 — Task Selection

**Selected item**: item-3
**Item title**: Define fail-closed candidate generation, ambiguity rejection, and bounded termination discipline
**Roadmap identity**:
- roadmap_id: 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001
- roadmap_item_id: item-3

## Why now

`item-3` is the lowest-numbered unfinished roadmap item, its dependencies
`item-1` and `item-2` are now done, and no live retry state forces a
same-round retry.

Accepted `round-177` froze this family's predecessor authority chain,
unresolved semantic matrix, family success bar, and first concrete
deliverable. Accepted `round-178` then published the current-architecture
mechanism map and made the current baseline honest: the repo can presently
explain one bounded non-local arm (`rootNonLocalSchemeAliasBaseLike`), one
bounded same-lane retained-child arm (`sameLaneLocalRetainedChildTarget`),
and the retained-child guards centered on `boundHasForallFrom`,
`keepTargetFinal`, and `targetC`, but those remain named packet-facing seams
rather than a general account of candidate generation, ambiguity rejection,
or bounded search.

The next honest move is therefore the roadmap's item-3 docs-first search
contract, not item-4 reconstruction-visible readiness work, not item-5
implementation, and not any widened capability claim. Item-3 is the first
place this family may define, for the currently named route arms only, when
recursive candidates may be introduced or preserved, how competing anchors /
owners / binder-side placements are compared or rejected, and why the live
`N1`, `N2`, and `N6` pressure remains bounded without fallback, heuristic
guessing, cyclic search, multi-SCC handling, or equi-recursive equality.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `642b395`
  (`Advance full-inference roadmap after round-178`)
- Active roadmap revision: `rev-001`, with items `1` and `2` done and items
  `3` through `7` pending
- Live retry state: none recorded for this family
- Accepted item-1 freeze artifact:
  `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze.md`
- Accepted item-2 mechanism-map artifact:
  `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
- Mechanism-map baseline carried forward: current read-only seams explain one
  bounded non-local alias-bound / base-like arm
  `rootNonLocalSchemeAliasBaseLike`, one bounded same-lane retained-child arm
  `sameLaneLocalRetainedChildTarget`, and retained-child admissibility guards
  centered on `boundHasForallFrom`, `keepTargetFinal`, and `targetC`, but
  they are still route-specific fragments rather than a family-wide search
  law
- Named bounded predecessor truth carried forward:
  `sameLaneAliasFrameClearBoundaryExpr` and
  `sameLaneDoubleAliasFrameClearBoundaryExpr` remain accepted `narrow
  success` packets only on `runPipelineElab` and `runPipelineElabChecked`
- Still-live obligations carried forward from items `1` and `2`: positive
  `P2` through `P6` plus fail-closed pressure `N1`, `N2`, and `N6`
- Out-of-scope negative families remain unchanged: `N3`
  equi-recursive-required, `N4` cyclic-or-multi-SCC-required, and `N5`
  second-interface-or-fallback-required
- The inherited boundary remains explicit-only / iso-recursive /
  non-equi-recursive / `non-cyclic-graph = unknown` / no-fallback, with no
  second interface and no cyclic or multi-SCC widening authorized
- Controller-prepared round branch:
  `orchestrator/round-179-define-fail-closed-candidate-generation`
- Controller-prepared round worktree: `orchestrator/worktrees/round-179`
- The round worktree already contains a pre-existing `orchestrator/state.json`
  modification that remains out of scope for this round
- No accepted artifact in this family yet defines fail-closed candidate
  generation, ambiguity rejection, or bounded termination discipline for the
  current named route arms only

## Scope

- Author one docs-only item-3 search-contract artifact at
  `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`
- Start from the accepted item-2 mechanism map rather than re-auditing the
  family baseline
- Define, for the currently named route arms only, when recursive candidates
  may be introduced or preserved, how competing anchors / owners /
  binder-side placements are compared or rejected, and why representative
  `N1`, `N2`, and `N6` pressure remains bounded
- Keep the definition fail-closed and non-widening: no fallback, no
  heuristic guessing, no cyclic search, no multi-SCC handling, and no
  equi-recursive equality
- Permit bounded pseudo-algorithm or harness-planning detail only if it stays
  docs-first and leaves runtime semantics unchanged
- Do not drift into item-4 reconstruction-visible readiness criteria, item-5
  implementation, roadmap/controller-state edits, production/test/Cabal
  edits, or any repo-level readiness or boundary-revision claim
