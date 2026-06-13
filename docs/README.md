# Documentation Map

This file is the reader-facing entry point for project documentation. Workflow
rules and ownership policy are still owned by `AGENTS.md`, and more specific
owners such as `tasks/readme` and `orchestrator/artifact-manifest.md` take
precedence for their artifact families.

## Start Here

- `README.md`: user-facing build, test, layout, and usage guidance.
- `AGENTS.md`: repo workflow rules, guidance ownership, and agent conventions.
- `docs/README.md`: this map.

## Canonical References

These documents describe current stable project truth unless a
higher-precedence instruction, a newer accepted ADR, or the active control plane
explicitly supersedes them.

- `docs/architecture.md`: repo layout, public/internal boundaries, module
  ownership, and key shared abstractions.
- `docs/syntax.md`: canonical eMLF and xMLF syntax notes.
- `docs/mlfp-language-reference.md`: checked `.mlfp` language reference.
- `docs/mlfp-self-boot-readiness.md`: current self-boot readiness notes.
- `docs/backend-native-pipeline.md`: backend/native validation pipeline.
- `implementation_notes.md`: current behavior, architecture, and
  thesis-alignment notes that are useful to future implementers.
- `Bugs.md`: canonical implementation defect and thesis-faithfulness gap
  tracker.

## Thesis Ledgers

These files connect the implementation to the thesis source of truth,
`papers/these-finale-english.txt`.

- `docs/paper-map.md`
- `docs/thesis-obligations.yaml`
- `docs/thesis-obligations.md`
- `docs/thesis-claims.yaml`
- `docs/thesis-deviations.yaml`

## Decisions

- `docs/adr/`: accepted architectural decisions and durable trade-offs.

## Roadmaps And Active Work

- `roadmap.md`: long-form algorithm and background roadmap; not the active
  execution state.
- `TODO.md`: repo-facing rolling progress and next goals.
- `orchestrator/state.json`: active control-plane pointer for repo-wide
  round execution.
- `orchestrator/active-roadmap-bundle.md`: contract for interpreting the
  active roadmap bundle selected by `orchestrator/state.json`.
- `tasks/readme`: task-folder workflow quick start.

## Execution Evidence

These files preserve audit trails and implementation history. They may explain
why a decision was made, but they do not override canonical references, accepted
ADRs, or the active control plane unless one of those surfaces cites them as
current authority.

- `docs/plans/`: historical and accepted plan artifacts.
- `docs/notes/`: dated notes, mechanism tables, and trace reports.
- `docs/audit/`: focused audit snapshots.
- `tasks/todo/`: active task packets for current efforts.
- `tasks/archive/`: completed task packets.
- `orchestrator/rounds/`: round artifacts and review/merge records.
- `orchestrator/worktrees/`: execution worktrees and preserved worker state.
