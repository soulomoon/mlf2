# Role Contract

This file is the shared Interface for repo-local orchestrator roles. Individual
role prompts under `orchestrator/roles/` contain only role-specific purpose,
duties, artifacts, and extra boundaries.

## Shared Inputs

Every role must load the controller-provided task plus any referenced artifacts
from the recorded branch and worktree. When relevant, every role should expect:

- `orchestrator/state.json`
- `orchestrator/artifact-manifest.md`
- `orchestrator/project-contract.md`
- `orchestrator/active-roadmap-bundle.md`
- the active roadmap bundle resolved from `state.json.roadmap_dir`
- the relevant schema file for any machine artifact it reads or writes
- current repository status for the assigned branch/worktree

## Shared Ownership Rules

- Work only in the branch and worktree assigned by the controller.
- Author only the artifacts owned by the current role and stage.
- Treat machine artifacts as the authority when a human-facing artifact
  disagrees with a schema-governed record.
- Record blockers in the role-owned artifact instead of broadening scope.
- Do not update `orchestrator/state.json`; the controller owns state.
- Do not perform another role's substantive work.

## Shared Output Rules

Every role-owned human artifact must be concise, repository-specific, and
actionable by the next role without chat history.

Every role-owned machine artifact must conform to its schema exactly. If the
schema cannot be satisfied from observable evidence, the role must stop with a
specific blocker instead of inventing fields.

## Shared Boundaries

- Do not merge.
- Do not approve your own work.
- Do not rewrite roadmap coordination except through a semantic
  `update-roadmap` assignment.
- Do not infer lineage, scheduler fields, closeout selectors, or worker
  scheduling from prose when a schema-governed record exists.
- Do not create hidden side channels outside `orchestrator/`.

## Shared Self-Check

Before returning, every role must verify:

- Am I using the assigned branch and worktree?
- Did I load the relevant schema before writing a machine artifact?
- Did I write only artifacts this role owns?
- Did I keep repo-wide invariants in `project-contract.md` and roadmap-specific
  overrides in the active bundle?
- Did I leave enough evidence for the next role or controller to continue
  without chat history?
