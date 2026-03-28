# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON.
- Command: `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"controller_stage":|"max_parallel_rounds":|"active_rounds":|"pending_merge_rounds":|"retry": null|"retry": \{' orchestrator/state.json`
  Why: the live controller must preserve the v2 state schema and active
  roadmap locator contract.
- Command: `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  Why: the live control plane must resolve one authoritative roadmap bundle.
- Command: `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
  Why: the active roadmap bundle must keep a parseable ordered item list.
- Command: `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  Why: the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic / no-fallback baseline remains binding.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  Why: the repo-level capability contract remains binding predecessor
  authority.
- Command: `test -f docs/plans/2026-03-28-post-p5-repo-scope-readiness-successor-gate-and-immediate-handoff-decision.md`
  Why: the accepted post-`P5` repo-scope gate is the direct authority that
  opens this successor family only.
- Conditional command: `if git diff --name-only -- src src-public app test mlf2.cabal | grep -q .; then cabal build all && cabal test; else printf 'skip full cabal gate for docs-only round\n'; fi`
  Why: code-changing rounds must satisfy the repo full Cabal gate, while
  docs-only rounds must record an explicit skip.

## Task-Specific Checks

- successor-boundary checks proving item `1` freezes the exact second-packet
  subject, the success bar, and the writable slice while keeping settled
  predecessor packets closed;
- bounded-slice checks proving item `2` stays inside the exact retained-child
  representative-gap subject and does not widen into unrelated family work or
  architecture changes;
- settlement-surface checks proving item `3` republishes the exact post-item-2
  read and keeps the repo-impact claim non-widening; and
- successor-gate checks proving item `4` records exactly one outcome and
  exactly one immediate handoff without silent readiness or boundary claims.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly
  justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- `selection.md` and `review-record.json` preserve the active `roadmap_id`,
  `roadmap_revision`, and `roadmap_dir`.
