# Scaffold Orchestrator Loop Dual-Mode Update Design

Date: 2026-04-02

## Goal

Expand `scaffold-orchestrator-loop` from a bootstrap-only setup skill into a
dual-mode setup skill that can either:

- create a brand-new top-level `orchestrator/` contract when none exists; or
- open a fresh roadmap family inside an already-existing terminal
  `orchestrator/` control plane.

The skill must still stop after the setup checkpoint commit. It must not start
runtime rounds.

## Current State

The current skill contract is too narrow for the common continuation case.

Today it says:

- use this skill only when the repository has no `orchestrator/` directory;
- copy the full scaffold tree into the repo root as `orchestrator/`; and
- stop after the initial checkpoint commit.

That matches first-time bootstrap, but it does not match the common pattern in
which a repository already has a mature `orchestrator/`, the active roadmap
family is complete, and the next honest step is to open a fresh successor
family inside the existing control plane rather than replacing it.

## User-Approved Constraints

- Keep the existing skill name `scaffold-orchestrator-loop`.
- Make the skill dual-mode rather than splitting it into separate skills.
- Allow reuse of an existing `orchestrator/` only when the current
  `orchestrator/state.json` is terminal:
  `stage: "done"`,
  `controller_stage: "done"`,
  and no live rounds.
- Do not treat a non-terminal existing orchestrator as safe to mutate.

## Decision

Revise the skill into an auto-detected dual-mode setup skill.

### Mode A: `bootstrap`

Use this mode when the target repository has no top-level `orchestrator/`
directory.

Behavior:

- survey the repo;
- mint a fresh `roadmap_id`;
- scaffold the full `orchestrator/` contract from assets;
- tailor the initial active roadmap bundle and role files; and
- commit the initial checkpoint.

### Mode B: `next-family`

Use this mode when the target repository already has `orchestrator/` and the
current controller state is terminal.

Behavior:

- survey the repo plus the existing control plane;
- mint a fresh `roadmap_id` for the next family;
- create a new active roadmap bundle under `orchestrator/roadmaps/`;
- repoint controller state and top-level pointer files to that new bundle;
- preserve the rest of the existing control plane; and
- commit the setup checkpoint.

The skill does not run any rounds in either mode.

## Mode Resolution

The skill should determine mode from repo state rather than asking the user for
an explicit flag.

Resolution order:

1. If `orchestrator/` does not exist, enter `bootstrap`.
2. If `orchestrator/` exists, parse `orchestrator/state.json`.
3. Enter `next-family` only if all of the following are true:
   - `stage == "done"`
   - `controller_stage == "done"`
   - `active_round_id == null`
   - `active_rounds == []`
   - `pending_merge_rounds == []`
   - `retry == null`
   - the active roadmap bundle resolved from
     `roadmap_id`, `roadmap_revision`, and `roadmap_dir`
     contains no `[pending]` or `[in-progress]` items
4. If `orchestrator/` exists but any of those conditions fail, stop with a
   precise error telling the user the existing control plane is still live or
   unfinished and must be resumed through `$run-orchestrator-loop` or repaired
   directly before a new family can be scaffolded.

The roadmap-content check is required so stale `done` machine state does not
silently open a new family on top of unfinished old work.

## Next-Family Workflow

`next-family` mode must update the existing control plane rather than replacing
it.

### Repository Survey

In addition to the current repo survey, inspect:

- `orchestrator/state.json`
- the active roadmap bundle named by `state.json`
- top-level pointer files such as
  `orchestrator/roadmap.md`,
  `orchestrator/verification.md`,
  and `orchestrator/retry-subloop.md` when they exist
- the current role files under `orchestrator/roles/`

The survey should establish that the prior family is finished and determine
whether any shared contract files are missing or obviously stale.

### Roadmap Minting

The skill should mint a fresh family id using the existing
`YYYY-MM-DD-NN-<slug>` rule, even when `orchestrator/` already exists.

The new family must start as:

- `orchestrator/roadmaps/<new-roadmap-id>/rev-001/roadmap.md`
- `orchestrator/roadmaps/<new-roadmap-id>/rev-001/verification.md`
- `orchestrator/roadmaps/<new-roadmap-id>/rev-001/retry-subloop.md`

Used older families and revisions remain immutable history.

### Contract Updates

In `next-family` mode, do not recopy the full scaffold tree over the existing
repo contract.

Instead:

- reuse the existing `orchestrator/roles/`, `orchestrator/rounds/`, and
  `orchestrator/worktrees/` directories;
- scaffold only missing shared control-plane files if the repo contract is
  incomplete;
- create the new roadmap family and revision bundle;
- update `orchestrator/state.json` to point at the new active bundle; and
- update top-level pointer files to match the new active bundle.

Role files should be preserved by default. They may be updated only when:

- the files are missing; or
- the user explicitly wants the shared contract refreshed; or
- repo-local wording is clearly incompatible with the new goal and the change
  is part of the setup task itself.

### State Reset Rules

For `next-family`, `state.json` should be reset to an idle-but-runnable state
for the new family:

- preserve `base_branch`
- set the new `roadmap_id`, `roadmap_revision`, and `roadmap_dir`
- set `stage: "select-task"`
- set `controller_stage: "dispatch-rounds"`
- set `active_round_id: null`
- set `active_rounds: []`
- set `pending_merge_rounds: []`
- clear legacy mirror fields such as `current_task`, `branch`,
  `worktree_path`, `active_round_dir`, and `round_artifacts`
- preserve `last_completed_round` as historical data
- clear `resume_error`, `resume_errors`, and `retry`

This leaves the control plane ready for `$run-orchestrator-loop` without
pretending that the new family is already complete.

## Bootstrap Workflow

`bootstrap` mode keeps the current behavior, with one wording change:
the skill should describe itself as creating the initial repo-local control
plane rather than implying that this is the only valid use of the skill.

The bootstrap output still includes:

- the full `orchestrator/` tree
- the initial active roadmap bundle
- tailored verification and retry contracts
- repo-local role files
- tracked ignore coverage for `orchestrator/worktrees/`
- one checkpoint commit

## Guardrails

The revised skill must explicitly forbid the following in `next-family` mode:

- opening a new family while the existing orchestrator is non-terminal;
- rewriting a roadmap revision that has already been used by rounds;
- deleting prior families, prior revisions, or prior round artifacts;
- blindly recopying scaffold assets over an existing customized
  `orchestrator/`; and
- silently widening from “open a fresh bounded family” into “retune the entire
  orchestrator contract” unless the user explicitly asked for that broader
  refresh.

When terminality checks fail, the skill should stop with an exact refusal
instead of guessing.

## Files To Update

The implementation should update these skill files:

- `skills/scaffold-orchestrator-loop/SKILL.md`
- `skills/scaffold-orchestrator-loop/references/repo-contract.md`
- `skills/scaffold-orchestrator-loop/references/roadmap-generation.md`

It may also update:

- `skills/scaffold-orchestrator-loop/references/verification-contract.md`

if pointer-file expectations or `next-family` verification rules need to be
made explicit there.

## Non-Goals

This design does not:

- change `$run-orchestrator-loop`
- start any orchestrator rounds automatically
- authorize opening a new family on top of a live or blocked controller state
- convert the skill into a generic “repair orchestrator state” tool
- require a user-specified mode flag

## Acceptance Criteria

The revised skill is correct when:

1. it still bootstraps a repo with no `orchestrator/`;
2. it can open a fresh roadmap family inside an existing terminal
   `orchestrator/` without replacing historical control-plane files;
3. it refuses to run when `orchestrator/` exists but the controller is not
   terminal or the active family still has unfinished items; and
4. it stops after the setup checkpoint commit in both modes.
