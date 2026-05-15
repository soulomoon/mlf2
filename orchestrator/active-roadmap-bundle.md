# Active Roadmap Bundle Contract

This file is the repo-local Interface for the active roadmap bundle. The active
roadmap bundle is the revision directory named by `orchestrator/state.json`
`roadmap_dir`.

The controller and roles must load this file before interpreting the active
bundle. If this file is missing from an existing control plane, runtime must
record a migration-needed controller error in
`orchestrator/state.json.resume_errors.controller` and stop instead of falling
back to scattered roadmap rules.

This file is not a shortcut to the active roadmap files. It defines how callers
read the active bundle, which files are required, how the machine-readable
roadmap view drives terminal detection and anchors, when status-only round
closeout is controller-owned, and when a semantic roadmap update must create a
new revision.

## Required State Metadata

`orchestrator/state.json` must name the active bundle with all of:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`

Treat `roadmap_id` as an opaque scaffolded identifier. Preserve it verbatim and
do not recompute it from roadmap titles or directory names.

`roadmap_dir` must point at the active revision directory:

```text
orchestrator/roadmaps/<roadmap_id>/<roadmap_revision>/
```

## Required Files

The active revision directory must contain:

- `roadmap.md`
- `roadmap-view.json`
- `verification.md`

The roadmap family directory must contain:

- `roadmap-history.md`

Do not create or use top-level pointer stubs such as
`orchestrator/roadmap.md`, `orchestrator/verification.md`, or top-level retry
policy files.

The complete scaffold file list and path-resolution rules live in
`orchestrator/artifact-manifest.md`.

## `roadmap.md`

`roadmap.md` is the human-readable coordination source for live and future work
in the family. It must be strategic: milestones are larger than rounds, and
candidate directions are extraction hints rather than implementation plans.

Runtime must not parse `roadmap.md` as the machine source for terminal
detection, lineage validation, or closeout anchors. Use `roadmap-view.json` for
those machine decisions, and use `selection-record.json` for selected round
lineage.

Required top-level sections:

- `## Goal`
- `## Alignment Summary`
- `## Outcome Boundaries`
- `## Global Sequencing Rules`
- `## Parallel Lanes`
- `## Milestones`

Each milestone heading under `## Milestones` must use one of these exact status
markers:

- `### [pending] ...`
- `### [in-progress] ...`
- `### [done] ...`

Each milestone must include:

- `Milestone id:`
- `Depends on:`
- `Intent:`
- `Completion signal:`
- `Parallel lane:`
- `Coordination notes:`

Each candidate direction must include:

- `Direction id:`
- `Summary:`
- `Why it matters now:`
- `Preconditions:`
- `Parallel hints:`
- `Boundary notes:`
- `Extraction notes:`

## Terminal Detection

To decide whether the active roadmap bundle has unfinished work, inspect
`roadmap-view.json`.

- Any milestone with `"status": "pending"` is unfinished.
- Any milestone with `"status": "in-progress"` is unfinished.
- A roadmap is terminal only when every milestone in
  `roadmap-view.json.milestones[]` has `"status": "done"`.

The following are validation errors, not terminal roadmaps:

- missing `roadmap-view.json`
- missing or unknown `schema_version`
- duplicate milestone ids or direction ids
- unknown milestone status values
- direction entries that point at missing milestone ids
- milestone `status_anchor` or `completion_anchor` values that are absent from
  `roadmap-view.json.anchors`
- anchors referenced by status-only closeout selectors that are absent from
  `roadmap-view.json.anchors`

On validation error, runtime must record the exact controller error in
`orchestrator/state.json.resume_errors.controller` instead of treating the
roadmap as terminal.

Terminal roadmap status alone is not controller completion. Runtime may claim
terminal completion only when the active bundle is terminal and
`state.json.active_rounds` is empty, no active `roadmap_update` remains, and no
unresolved resume errors remain.

## `roadmap-view.json`

`roadmap-view.json` is the machine-readable view of the active roadmap
revision. It keeps terminal detection, dependency lookup, direction lookup, and
status-only closeout anchors behind one small Interface.

Required top-level fields:

- `schema_version`: `roadmap-view-v1`
- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`
- `roadmap_file`
- `milestones`
- `directions`
- `anchors`

Milestone entries must include:

- `milestone_id`
- `title`
- `status`: `pending`, `in-progress`, or `done`
- `depends_on`
- `parallel_lane`
- `status_anchor`
- `completion_anchor`
- `direction_ids`

Direction entries must include:

- `direction_id`
- `milestone_id`
- `summary`
- `preconditions`
- `parallel_hints`

Anchor entries map an `anchor_id` to a repo-relative `target_file` and a stable
selector inside that file. Status-only round closeout resolves selectors through
these anchors instead of relying on reviewers to copy exact Markdown headings.

`roadmap.md` and `roadmap-view.json` should describe the same roadmap. If they
conflict, runtime must record a controller error instead of guessing which
surface is current.

## `verification.md`

`verification.md` is the repo- and roadmap-specific checklist for the active
revision.

It must include:

- `## Baseline Checks`
- `## Alignment Checks`
- `## Task-Specific Checks`
- `## Manual Checks`
- `## Roadmap Overrides`

Keep universal reviewer duties, lineage requirements, evidence requirements, and
approve/reject output formats in `orchestrator/roles/reviewer.md`. Keep
repo-wide invariants in `orchestrator/project-contract.md`.

Roadmap-specific retry policy belongs in `## Roadmap Overrides` only when the
active revision needs behavior beyond the shared runtime retry mechanics. If no
roadmap-specific retry policy exists, record `none`.

## Status-Only Round Closeout

After a round is approved and before it is squash-merged, the controller may
apply status-only round closeout directly to the active revision copy in the
canonical round worktree only when `review-record.json` explicitly approves it
under `orchestrator/round-finalization-schema.md`.

Status-only round closeout may do only these edits:

- change the selected milestone status in `roadmap-view.json` between
  `pending`, `in-progress`, and `done`, and update the corresponding
  human-readable `roadmap.md` projection through the milestone anchor recorded
  in `review-record.json`;
- add or update compact completion pointers that name the round id and reviewer
  evidence through `roadmap-view.json` anchors; and
- add compact history entries under `roadmap-history.md` that only summarize
  completed work already approved by the round reviewer and use
  `roadmap-view.json` anchors.

Status-only round closeout must not change:

- future coordination;
- milestone or candidate-direction meaning;
- sequencing;
- parallel lanes;
- extraction scope;
- verification meaning; or
- retry policy.

If the approved reviewer record is missing, ambiguous, or asks for any semantic
change, runtime must return to review or recovery before merge instead of
guessing controller-owned edits. After merge, semantic changes use the
delegated `update-roadmap` path.

The controller must record applied status-only closeout in
`orchestrator/rounds/<round-id>/closeout-record.json` following
`orchestrator/round-finalization-schema.md`.

## Revision And History Rules

Used roadmap revisions are durable history. The current active revision may be
modified in place on the round branch only through status-only round closeout,
such as marking completed work in `roadmap-view.json` and `roadmap.md` or
adding compact completion pointers, when the reviewer approves that no future
coordination meaning changed.

Publish a new `rev-00N+1` directory under the same `roadmap_id` when a roadmap
update changes any of:

- future coordination
- milestone or direction meaning
- sequencing
- parallel lanes
- extraction scope
- verification meaning
- retry policy

Move completed detail to
`orchestrator/roadmaps/<roadmap_id>/roadmap-history.md`, or keep only compact
completion pointers in the active revision when those pointers do not change
remaining work.
