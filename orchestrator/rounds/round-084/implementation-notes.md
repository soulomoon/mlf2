# Round 084 Implementation Notes

## Change Summary

- Added the canonical docs-only item-3 artifact at
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`.
- The new artifact fixes the round to `attempt-1` with `retry: null`,
  reasserts the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-second-interface /
  no-fallback boundary, and treats the accepted `N4`-`N7` and `N11`-`N14`
  chains as bounded predecessor evidence only rather than as a general
  capability claim.
- Reconstructed one concise predecessor inventory for the accepted non-local
  `baseTarget -> baseC` packet and the accepted same-lane retained-child
  `boundVarTarget -> targetC` packet, including recursive-shape anchors,
  owner / binder placement, target / consumer alignment, locality,
  polymorphism conditions, review-visible output facts, and fail-closed
  contrasts.
- Added one bounded mechanism schema covering recursive-shape discovery,
  binder / owner placement, target / consumer alignment, local versus
  non-local propagation, interaction with polymorphism and instantiation,
  reconstruction obligations, and fail-closed ambiguity / unsafe-case
  handling, with explicit later-item ownership for missing justification.
- Recorded the item-3 pressure read: `P2`, `P3`, and `P4` look only partially
  explained inside the inherited acyclic model, `P5` remains
  negative-only / unresolved, and the accepted record still depends on named
  packet guards plus later item `4`, `5`, and `6` work.

## Verification

- `test ! -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`
  - Result: passed before edits; confirmed the canonical item-3 artifact did
    not exist yet.
- `rg -n "Mechanism Map|recursive-shape discovery|boundVarTarget -> targetC|baseTarget -> baseC" docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md orchestrator/rounds/round-084/implementation-notes.md`
  - Result: failed before edits because both files were absent. This was the
    docs-only red step.
- `git diff --check`
  - Result: passed in
    `.worktrees/round-084`.
- `python3 -m json.tool orchestrator/rounds/round-084/state-snapshot.json >/dev/null`
  - Result: passed.
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-084/state-snapshot.json`
  - Result: matched `contract_version: 2` and `retry: null`.
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/roadmap.md`
  - Result: passed; ordered roadmap item list remains parseable with items
    `1` and `2` done and item `3` pending.
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
  - Result: passed.
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - Result: passed.
- `test -f docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  - Result: passed.
- `test -f orchestrator/rounds/round-081/review-record.json`
  - Result: passed.
- `test -f orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/retry-subloop.md`
  - Result: passed.
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`
  - Result: passed after edits.
- `git diff --name-only -- src test src-public app mlf2.cabal`
  - Result: passed with no output; the diff stayed out of the code / test /
    public / executable / Cabal surface.
- `rg -n 'Stage Contract Freeze|Accepted Packet Inventory|Mechanism Schema|Direct Comparison|Item-2 Pressure Read Against The Acyclic Model|Docs-Only Verification Note|baseTarget -> baseC|boundVarTarget -> targetC|P5 polymorphism-nested-forall|bounded predecessor evidence only|item `4`|item `5`|item `6`' docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`
  - Result: passed; matched the required scope freeze, packet inventory,
    bounded mechanism schema, comparison section, pressure read, docs-only
    verification note, both accepted packet chains, the bounded `P5` read,
    the predecessor-evidence warning, and the later-item dependency owners.
- `cabal build all && cabal test`
  - Result: intentionally skipped. This round is docs-only and the diff stays
    out of `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`.
