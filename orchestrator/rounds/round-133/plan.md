# Round 133 Plan (`item-2` Post-`P5` Repo-Scope Refreshed Matrix)

## Objective

Publish one canonical docs-only refreshed post-`P5` representative
family-matrix readiness surface.

This round is `attempt-1` with `retry: null`.

## Inputs

- `orchestrator/rounds/round-133/selection.md`
- `docs/plans/2026-03-28-post-p5-repo-scope-successor-authority-evidence-inputs-and-non-widening-boundary-freeze.md`
- the accepted exact same-lane, `C1`, `P1`, and `P5` settlement chains
- the March 27 refreshed repo-scope matrix as historical aggregate evidence
- the active roadmap bundle and verification contract

## Round Type

This is an aggregate docs-only round.
No production or test behavior may change here.

## Task List

1. Republish the representative matrix on a new round-owned surface.
   - Carry forward the settled exact same-lane `C2` / `C5` / `C7` pocket as
     predecessor truth only.
   - Carry forward the settled exact `C1`, `P1`, and `P5` reads honestly.
   - Keep `C4` and `C6` as historical carry-forward rows only.

2. Validate provenance without widening any exact packet into general family
   success.
   - Distinguish carried-forward predecessor truth from historical aggregate
     evidence.

3. Record the current repo-scope read after the refresh.
   - Stop at matrix publication and provenance validation only.
   - Do not select the readiness posture here.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"controller_stage":|"max_parallel_rounds":|"active_rounds":|"pending_merge_rounds":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md && test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md && test -f docs/plans/2026-03-28-post-p5-repo-scope-successor-authority-evidence-inputs-and-non-widening-boundary-freeze.md`
- `git diff --check`

## Exit Criteria

This round is complete only when:

- one canonical docs artifact republishes the refreshed post-`P5` matrix;
- settled exact packets remain explicit predecessor truth only;
- the refreshed repo-scope read is stated without selecting the readiness
  posture yet; and
- the round is ready for an aggregate review decision on item `2`.
