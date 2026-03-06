# Findings — 2026-03-07 TMT Per-Row Fresh Review

## Scope
- Primary target: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Comparison basis: newest working tree + `papers/these-finale-english.txt`
- Review method: one fresh agent per table row

## Initial findings
- Current table has 14 live mechanism rows.
- The row titles are captured in `rows.txt` in this task folder.
- The current note already claims every row is `Yes`; this pass is a re-audit, not an assumption that changes are required.

## Technical Decisions
| Decision | Rationale |
|----------|-----------|
| Ask each row reviewer for `keep` vs `change` plus replacement row text if needed | Makes integration deterministic |
| Require thesis/code/test evidence in every agent response | Keeps row updates evidence-based |

## Issues Encountered
| Issue | Resolution |
|-------|------------|
| None yet | N/A |

## Row review snapshots
- Row 4 (`Per-edge propagation transform`): `KEEP`.
  - Reviewer confirms the row still holds as an interpreter-path claim.
  - Possible minor strengthening only: tighten thesis/code references to include direct `mergeExpansions` / `decideMinimalExpansion` support and note that any remaining synthesized-wrapper-specific logic is planner-side, not interpreter-side.
- Row 5 (`Graph operation execution (Graft/Merge/Weaken/Raise)`): `KEEP`.
  - Reviewer confirms the consolidated edge-local omega execution entrypoint still matches the thesis-facing row claim.
  - Possible minor strengthening only: sharpen thesis anchors to the exact Fig. 15.3.4 / `T(e)` lines and optionally add newer normalization/Φ-Ω supporting references.
- Row 1 (`Elaboration input`): `CHANGE`.
  - `Thesis-exact` stays `Yes`, but the row should refresh stale test references and explicitly cite the current `ElabEnv` / `PhiEnv` / `resolveContext` surfaces.
- Row 3 (`Ordering of transformations`): `CHANGE`.
  - `Thesis-exact` stays `Yes`, but the closeout note and evidence drifted: `Task 46` is no longer the right closeout label for row 3, and the code references should now include `EdgeProcessing/Unify.hs`.
- Row 2 (`Result-type context wiring`): `CHANGE`.
  - Reviewer reports the current row overclaims `Yes`: the explicit solved fields are gone, but the live path still seeds `PresolutionView` from `Solved` and `buildResultTypeView` still validates through `ChiQuery.chiSolved`.
  - This is the first row review that suggests a real `Yes -> No` reclassification rather than reference drift.
- Row 6 (`Replay-map producer normalization (upfront strict contract)`): `CHANGE`.
  - `Thesis-exact` stays `Yes`, but the thesis column should stop implying that the thesis itself defines a replay-map runtime contract.
  - The refresh is documentary: sharpen the thesis claim and update producer-boundary evidence references.
- Row 7 (`Replay-map consumer bridge in Phi`): `KEEP`.
  - Reviewer confirms the strict pass-through/validation boundary still matches both thesis intent and the current Φ/Ω runtime path.
- Row 9 (`Canonicalization source used by Phi`): `KEEP`.
  - Reviewer confirms canonicalization remains downstream lookup machinery, not a runtime target-repair source.
- Row 10 (`Identity reconciliation mechanism`): `KEEP`.
  - Reviewer confirms the live Φ/Ω path carries identity directly enough for the row’s current `Yes` classification.
- Row 8 (`Translatability normalization`): `CHANGE`.
  - Reviewer reports a real `Yes -> No` reclassification: the live path enforces Definition 15.2.10 / Theorem 15.2.11 constructive translatability, but not §15.2.8’s stronger all-inert `W` normalization.
- Row 11 (`Non-root weaken/raise binder resolution`): `KEEP`.
  - Reviewer confirms the direct-target fail-fast behavior still matches the thesis-facing row claim.
- Row 12 (`Graph mutation during solve/presolution`): `CHANGE`.
  - `Thesis-exact` stays `Yes`, but the row should acknowledge the remaining snapshot finalization/rebuild step in the runtime pipeline more precisely.
- Row 14 (`Campaign classification status`): `KEEP`.
  - Reviewer confirms the meta/documentary campaign state still matches live repo metadata.

## Final outcome
- Reviewed all 14 rows with separate reviewer agents.
- Reclassified rows 2 and 8 from `Yes` to `No`.
- Refreshed wording/evidence for rows 1, 3, 6, 12, and 13.
- Kept rows 4, 5, 7, 9, 10, 11, and 14 materially unchanged.
