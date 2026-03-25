# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage, including docs-only public-output / architecture-pressure rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON
  after every round.
- Command: `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
  Why: the live controller must preserve the v2 retry schema and the
  revisioned-roadmap locator contract.
- Command: `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  Why: the live control plane must resolve one authoritative roadmap bundle
  through `orchestrator/state.json`.
- Command: `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
  Why: the active roadmap bundle must keep a parseable ordered item list with
  explicit status markers.
- Command: `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  Why: the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary remains the live baseline.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  Why: accepted item `2` remains the authoritative source for
  `non-cyclic-graph = unknown`.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  Why: this contract still defines the persistence tuple, phase-and-surface
  ledger, and lawful positive / blocker-debt / fail-closed vocabulary.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  Why: accepted item `6` still records bounded-subset-only feasibility and
  keeps `non-cyclic-graph` pressure live without broad success.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  Why: accepted item `7` still records `continue within the current
  architecture` as the strongest lawful strategic read before this loop.
- Command: `test -f docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-end-to-end-revalidation-and-classification.md`
  Why: accepted round `092` fixes the exact public-output collapse and
  blocker-debt classification for the frozen pocket.
- Command: `test -f docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-successor-decision-gate.md`
  Why: accepted round `093` fixed the current bounded successor posture and
  explicitly refused to reopen `non-cyclic-graph` on weaker evidence.
- Command: `test -f orchestrator/rounds/round-092/review-record.json && test -f orchestrator/rounds/round-093/review-record.json`
  Why: the immediate predecessor classification and successor-decision records
  must remain present as continuity evidence.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  Why: any round that asserts runtime continuity for this pocket must still
  keep the accepted Phase 6 elaboration clearance green.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
  Why: any round that audits or changes the public-output path must recheck
  the current authoritative public-output anchor for the exact frozen pocket,
  or explicitly replace it with a new exact-pocket assertion in the same
  round.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
  Why: rounds that touch the accepted elaboration / reconstruction handoff
  path must preserve the exact item-3 anchor while changing later surfaces.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  Why: any round that touches the selected code/test path or asserts new
  runtime persistence behavior must rerun the bounded prototype
  characterization.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`,
  `src-public/`, `app/`, `test/`, or `mlf2.cabal`.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- public-output-case-freeze checks proving item `1` freezes exactly one
  same-lane retained-child pocket, the exact helper-visible/public split, and
  the accepted predecessor chain without widening the subject;
- public-path-audit checks proving item `2` localizes the exact
  authoritative public-output continuity-loss site for that same pocket only,
  without relitigating earlier accepted stages or broadening into neighboring
  routes;
- implementation-slice checks proving item `3` stays limited to the exact
  same-lane retained-child route, the authoritative public-output path, and
  corresponding focused tests/docs, without silently touching the alias-bound
  family, nested-`forall`, cyclic search, second interfaces, or fallback
  widening;
- end-to-end revalidation checks proving item `4` records solver,
  elaboration, reconstruction, internal output, public output, and
  reviewer-visible evidence for the same frozen pocket and uses exactly one
  item-5/item-6 outcome token for that pocket;
- architecture-pressure decision checks proving item `5` consumes only the
  accepted exact-pocket item-4 result and records exactly one bounded
  architecture outcome for this pressure point, rather than silently widening
  into a general architecture or capability claim;
- inherited-boundary checks proving rounds do not silently enable broad
  automatic recursive inference, equi-recursive reasoning, cyclic structural
  graphs, multi-SCC search, second interfaces, or convenience fallbacks
  unless an accepted roadmap item explicitly makes that lawful;
- predecessor-continuity checks proving accepted rounds `089` through `093`,
  accepted strategic items `2`, `5`, `6`, and `7`, and completed rounds
  `001` through `088` are treated as bounded predecessor evidence only;
- retry-subloop checks proving retry-capable reviews record
  `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`,
  and that `accepted + retry` is never used for the final bounded-decision
  item;
- artifact-history checks proving earlier retry attempts remain immutable in
  `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl`;
- roadmap-update checks proving accepted `update-roadmap` edits preserve
  completed-item truth, keep the next unfinished item concrete, and do not
  silently widen into the alias-bound family, nested-`forall`, or broad
  automatic-recursive-inference claims;
- docs-diff review when a round intentionally changes only `orchestrator/`,
  `docs/`, or bounded campaign artifacts; and
- skip-note review when a round does not trigger focused or full Cabal gates,
  including the exact reason the reviewer judged those code-path gates out of
  scope.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly
- justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- The round preserves the current live-subject boundary, inherited
  explicit-only / non-equi-recursive / non-cyclic-graph constraints,
  predecessor continuity, and bounded-gate framing unless an accepted
  roadmap update explicitly changes the live plan.
- No unresolved blocking issue remains.

## Reviewer Record Format

### Round `<round-id>` Attempt `<n>`

- Baseline checks:
- Task-specific checks:
- Implemented stage result:
- Attempt verdict:
- Stage action:
- Retry reason:
- Fix hypothesis:
- Decision summary:
- Evidence summary:
