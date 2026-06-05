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
read the active bundle, which files are required, how structured `roadmap.md`
fields drive terminal detection and closeout selectors, when status-only round
closeout is controller-owned or planner-authored for simple rounds, and when
semantic roadmap updates must create a new revision.

If a plan-stage planner discovers from current docs, ADRs, context, code, or
tests that the active milestone is too broad to select a bounded
dependency-ready round, the planner may write `roadmap-update-request.md`.
Runtime must treat that as a request to enter delegated `update-roadmap`, not
as permission to edit the active revision directly.

## Round Execution Profiles

The planner owns process classification for each selected round. The controller
must not decide whether a task is simple by inspecting the diff, task title, or
roadmap wording. Planner-authored `plan.md` records these fields in its
`Execution Profile` section:

- `Complexity`: `simple`, `standard`, or `closeout`
- `Verification profile`: `focused`, `standard`, or `closeout`

`Complexity` describes only the selected task's own content. A task is
`simple` when the goal is clear, the implementation path follows an
established local pattern, and the verification boundary is clear enough that
the selected evidence can directly prove the slice. Routine wiring such as
fixture registration, expected outputs, negative cases, guard checks, docs, or
aggregate-test enrollment does not by itself upgrade complexity when it is
mechanical and shares the same owner surface and failure mode.

Use `standard` when the task content itself needs new design judgment, changes
structural shared behavior across ownership boundaries, introduces a new
abstraction or failure mode, or cannot be locally proven from a clear
verification boundary. Use `closeout` for milestone closeout or
semantic/public-contract changes. Do not use surrounding roadmap importance,
protected-surface status, validation cost, reviewer need, merge path,
milestone proximity, or downstream risk to upgrade `Complexity`.

`Verification profile` describes the evidence required around the task. Put
everything outside the task's own content here: protected-surface validation,
aggregate regressions, full gates, milestone/proof/readiness claims, and
downstream risk.

For any task with `Complexity: simple`, the planner completes the task directly
during planning, runs the commands required by the selected `Verification
profile`, and writes direct evidence in
`implementation-notes.md`. The controller skips implementer and reviewer
dispatch after that evidence is present and verification commands passed.

Runtime delegates to implementer/reviewer only for planner-classified
non-simple work. Work that requires worker fan-out is non-simple by definition.
The reviewer applies the active `verification.md` checks for the selected
profile and may escalate only with a concrete repo-risk or contract reason.

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
in the family and the single structured roadmap source for controller decisions.
It must be strategic: milestones are larger than rounds, and candidate
directions are extraction hints rather than implementation plans.

Runtime reads the exact structured labels below for terminal detection,
dependency lookup, direction lookup, and status-only closeout selectors. Use
`plan.md` for selected round lineage.

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
- `Completion pointers:`
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
milestone headings under `roadmap.md` `## Milestones`.

- Any milestone heading with `[pending]` is unfinished.
- Any milestone heading with `[in-progress]` is unfinished.
- A roadmap is terminal only when every milestone heading under `## Milestones`
  uses `[done]`.

The following are validation errors, not terminal roadmaps:

- missing `roadmap.md`
- missing required top-level sections or milestone fields
- duplicate milestone ids or direction ids
- unknown milestone status values
- candidate direction blocks outside a valid milestone
- milestone dependencies that point at missing milestone ids
- status-only closeout selectors that name missing milestone ids
- history entries requested without `roadmap-history.md` `## Completed Rounds`

On validation error, runtime must record the exact controller error in
`orchestrator/state.json.resume_errors.controller` instead of treating the
roadmap as terminal.

Terminal roadmap status alone is not controller completion. Runtime may claim
terminal completion only when the active bundle is terminal and
`state.json.active_rounds` is empty, no active `roadmap_update` remains, and no
unresolved resume errors remain.

## Structured Roadmap Fields

`roadmap.md` is the structured roadmap source. Runtime derives:

- milestone status from `### [pending]`, `### [in-progress]`, or `### [done]`
  headings under `## Milestones`;
- milestone identity from each `Milestone id:` field;
- milestone dependencies from `Depends on:` comma-separated milestone ids, with
  an empty value meaning no dependency;
- milestone lane from `Parallel lane:`;
- candidate direction identity from each `Direction id:` field inside a
  milestone's `Candidate directions:` block;
- direction readiness hints from `Preconditions:` and `Parallel hints:`.

Callers resolve a status-only closeout selector by matching exactly one
`Milestone id:` field in the active `roadmap.md`, then editing only that
milestone block and the allowed `roadmap-history.md` section. Ambiguous or
missing structured fields are controller errors.

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

This section owns the decision boundary between `status-only` closeout and
semantic roadmap update. Reviewers classify non-simple rounds in `review.md`
using this section. Planners record direct simple-round closeout in
`implementation-notes.md`; runtime validates those fields instead of applying
controller-inferred edits.

After a non-simple round is approved and before it is squash-merged, the
controller may apply status-only round closeout directly to the active revision
copy in the canonical round worktree only when `review.md` explicitly approves
it.

Status-only round closeout may do only these edits:

- change the selected milestone status marker in `roadmap.md` between
  `[pending]`, `[in-progress]`, and `[done]`;
- add or update compact completion pointers that name the round id and the
  finalization evidence under the selected milestone's `Completion pointers:`
  field; and
- add compact history entries under `roadmap-history.md` that only summarize
  completed work already supported by the finalization evidence.

Status-only round closeout must not change:

- future coordination;
- milestone or candidate-direction meaning;
- sequencing;
- parallel lanes;
- extraction scope;
- verification meaning; or
- retry policy.

If the approved reviewer artifact for a non-simple round is missing, ambiguous,
or asks for any semantic change, runtime must return to review or recovery
before merge instead of guessing controller-owned edits. If a simple round's
`implementation-notes.md` is missing, ambiguous, or records any semantic
change, runtime must return to plan or recovery. After merge, semantic changes
use the delegated `update-roadmap` path.

Planner-completed simple rounds record their direct evidence and any simple
status-only closeout they completed in `implementation-notes.md`. They do not
need reviewer-authored closeout, and the controller must not author missing
simple closeout by inference.

For non-simple rounds, the controller may record concise merge bookkeeping in
`orchestrator/rounds/<round-id>/merge.md`; it must not create a duplicate JSON
closeout record.

## Revision And History Rules

Used roadmap revisions are durable history. The current active revision may be
modified in place on the round branch only through status-only round closeout,
such as marking completed work in `roadmap.md` or adding compact completion
pointers, when `review.md` approves that no future coordination meaning changed
for a non-simple round or `implementation-notes.md` records valid direct
status-only closeout for a simple round.

Publish a new `rev-00N+1` directory under the same `roadmap_id` when a merged
round or planner-requested roadmap update crosses the semantic roadmap update
boundary above.

Move completed detail to
`orchestrator/roadmaps/<roadmap_id>/roadmap-history.md`, or keep only compact
completion pointers in the active revision when those pointers do not change
remaining work.
