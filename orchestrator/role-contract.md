# Role Contract

This file is the shared Interface for repo-local orchestrator roles. Individual
role prompts under `orchestrator/roles/` contain only role-specific purpose,
duties, artifacts, and extra boundaries.

The controller loads this file together with the active role prompt. A role
prompt's role-specific input, boundary, and self-check sections list only
role-specific additions; the shared rules in this file still apply.

The controller may carry the full orchestrator control-plane picture. Role
subagents should receive a bounded role work context instead: the controller
must provide the role assignment, relevant owned artifacts, relevant artifact
contracts, and enough project evidence for the role to solve the delegated task
without spending its context on unrelated orchestration machinery.

## Shared Inputs

Every role must load the controller-provided task plus any referenced artifacts
from the recorded branch and worktree. The role should load only the
orchestrator artifacts needed for its current stage, artifact ownership, and
structured artifact reads or writes.

When relevant, every role should expect:

- the assigned round, branch, worktree, stage, and role objective
- the role-owned artifacts it must read or write
- `orchestrator/project-contract.md` when repo-wide invariants may constrain
  the task
- the relevant artifact contract for any structured Markdown artifact the role
  reads or writes
- `orchestrator/state-schema.md` or `orchestrator/roadmap-update-schema.md`
  only when the role must read controller state or produce a roadmap update
- active roadmap bundle files needed for the current role decision
- current repository status for the assigned branch/worktree
- project docs, code, tests, and command output needed to solve or verify the
  delegated project problem

## Shared Ownership Rules

- Work only in the branch and worktree assigned by the controller.
- Author only the artifacts owned by the current role and stage.
- Treat structured Markdown fields as the authority for round lineage,
  scheduling, complexity, verification, evidence, review, retry, and closeout.
- Record blockers in the role-owned artifact instead of broadening scope.
- Do not update `orchestrator/state.json`; the controller owns state.
- Do not perform another role's substantive work.

## Shared Output Rules

Every role-owned human artifact must be concise, repository-specific, and
actionable by the next role without chat history.

Every role-owned structured artifact must satisfy the fields required by the
artifact contract. If required fields cannot be filled from observable evidence,
the role must stop with a specific blocker instead of inventing values.

## Shared Boundaries

- Do not merge.
- Do not approve your own work.
- Do not edit roadmap files except for planner-authored status-only closeout in
  a `Complexity: simple` round or a semantic `update-roadmap` assignment.
  Simple-round closeout must not change future coordination.
- Do not infer lineage, scheduler fields, closeout selectors, or worker
  scheduling from prose outside the owning structured artifact.
- Do not create hidden side channels outside `orchestrator/`.

## Shared Self-Check

Before returning, every role must verify:

- Am I using the assigned branch and worktree?
- Did I load the relevant artifact contract before writing a structured
  artifact?
- Did I write only artifacts this role owns?
- Did I keep repo-wide invariants in `project-contract.md` and roadmap-specific
  overrides in the active bundle?
- Did I leave enough evidence for the next role or controller to continue
  without chat history?
