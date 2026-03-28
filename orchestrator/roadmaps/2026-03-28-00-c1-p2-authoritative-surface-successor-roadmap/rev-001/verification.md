# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON.
- Command: `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"controller_stage":|"max_parallel_rounds":|"active_rounds":|"pending_merge_rounds":|"retry": null|"retry": \{' orchestrator/state.json`
  Why: the live controller must preserve the v2 state schema and the active
  roadmap locator contract.
- Command: `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  Why: the live control plane must resolve one authoritative roadmap bundle
  through `orchestrator/state.json`.
- Command: `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
  Why: the active roadmap bundle must keep a parseable ordered item list with
  explicit status markers.
- Command: `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  Why: the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic / no-fallback baseline remains the live boundary.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  Why: the repo-level capability contract and representative family matrix
  remain binding predecessor authority.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  Why: authoritative visible-output obligations remain binding for this family.
- Command: `test -f docs/plans/2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision.md`
  Why: the accepted March 27 gate is the direct authority that opens the
  bounded `C1` / `P2` successor lane only.
- Conditional command: `if git diff --name-only -- src src-public app test mlf2.cabal | grep -q .; then cabal build all && cabal test; else printf 'skip full cabal gate for docs-only round\n'; fi`
  Why: code-changing rounds must satisfy the repo full Cabal gate, while
  docs-only rounds must record an explicit skip.

## Task-Specific Checks

- successor-boundary checks proving item `1` freezes the direct predecessor
  chain, the exact `C1` packet and route, the exact success bar, and the
  writable slice while keeping the same-lane `C2` / `C5` / `C7` pocket settled
  and `P5` out of scope;
- focused writable-slice checks proving item `2` stays inside the accepted
  bounded `C1` / `P2` lane and does not silently widen into unrelated family
  work or architecture changes;
- focused evidence checks for item `2` proving cited `C1` harness or
  `PipelineSpec` evidence was rerun under serialized or isolated build-output
  conditions; shared `dist-newstyle` parallel Cabal runs do not count;
- regression checks for item `2` proving any touched `test/Research/C1AuthoritativeSurfaceSpec.hs`,
  `test/PipelineSpec.hs`, `test/Main.hs`, or `mlf2.cabal` updates remain wired
  and reviewable;
- settlement-surface checks proving item `3` records the exact post-item-2
  `C1` / `P2` authoritative-surface read, validates provenance for focused or
  full-gate evidence, and does not silently promote one exact packet into
  broader family success;
- successor-gate checks proving item `4` records exactly one outcome and
  exactly one immediate handoff without reopening the settled same-lane pocket
  or silently promoting `P5`;
- parallel-agent discipline checks proving that any round using multiple
  subagents identified bounded independent sidecars, assigned disjoint write
  scopes, kept one authoritative owner per writable file, and recorded the
  consolidation clearly in `plan.md`, `implementation-notes.md`, `review.md`,
  and a `Parallel execution summary`;
- roadmap-update checks proving completed-item truth remains immutable, the
  next unfinished item stays concrete, and no revision rewrite occurs after a
  round has used that revision; and
- docs-diff review when a round intentionally changes only `orchestrator/`
  and `docs/`, plus an explicit skip note when the full Cabal gate is not
  required.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly
  justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- `selection.md` and `review-record.json` preserve the active `roadmap_id`,
  `roadmap_revision`, `roadmap_dir`, and `roadmap_item_id`.
- No unresolved blocking issue remains within the selected stage contract; any
  remaining exact-packet or repo-scope blocker debt is explicitly recorded.

## Reviewer Record Format

- Commands run
- Pass or fail result
- Evidence summary
- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`
- Approve or reject decision
