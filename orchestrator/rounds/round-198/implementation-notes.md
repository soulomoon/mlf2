## Round 198 implementation notes

- Authored `docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md`
  as the one canonical milestone-3 reread artifact. It freezes the accepted
  `round-193`, `round-191`, `round-181`, and `round-197` ledger only, keeps
  refreshed `P5` evidence separate from preserved `P2` packet folklore, and
  records the remaining-frontier result as `P5 remains the stronger blocker / pressure source`.

- Verification:
  - `python3 -m json.tool orchestrator/state.json >/dev/null` -> exit `0`
  - `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/verification.md" && test -f "$roadmap_dir/retry-subloop.md"` -> exit `0`
  - `rg -n 'continue-bounded|P2 non-local-propagation|P5 polymorphism-nested-forall|sharper blocker' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md` -> exit `0`; matched the accepted `round-193` bounded-routing / sharper-blocker baseline
  - `rg -n 'packet-specific folklore|P2 non-local-propagation|P5 polymorphism-nested-forall|current-architecture blockers' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md` -> exit `0`; matched the accepted `round-191` `P2` / `P5` aggregate classification
  - `rg -n 'sameLaneAliasFrameClearBoundaryExpr|nestedForallContrastExpr|PhiTranslatabilityError|test-only|runPipelineElab|runPipelineElabChecked' docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md` -> exit `0`; matched the settled milestone-2 `P5` lane evidence
  - `rg -n 'C1|baseTarget -> baseC|runPipelineElab|runPipelineElabChecked|packet-specific folklore' orchestrator/rounds/round-181/review-record.json orchestrator/rounds/round-181/implementation-notes.md docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md` -> exit `0`; matched the exact `C1` packet provenance and packet boundary
  - `test -f docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md` -> exit `0`
  - `rg -n 'round-198|direction-3a-refresh-the-p5-vs-p2-gap-ledger|sameLaneAliasFrameClearBoundaryExpr|nestedForallContrastExpr|PhiTranslatabilityError|C1|packet-specific folklore|runPipelineElab|runPipelineElabChecked' docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md` -> exit `0`
  - `python3 - <<'PY' ... PY` -> exit `0`; printed `ROUND198_REMAINING_FRONTIER_LEDGER_OK`
  - `git diff --check` -> exit `0`
  - `python3 - <<'PY' ... PY` docs-only scope check -> exit `1`; reported `OUT_OF_SCOPE_PATHS: orchestrator/rounds/round-198/implementation-notes.md` because the plan helper allowlist omits the required round-owned notes path
  - `git ls-files --others --exclude-standard` -> exit `0`; listed only `docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md`, `orchestrator/rounds/round-198/implementation-notes.md`, and the round-owned `plan.md` / `selection.md`
  - `git status --short` -> exit `0`; showed the pre-existing controller-owned `M orchestrator/state.json` plus the new artifact and round-owned `round-198` docs as untracked
