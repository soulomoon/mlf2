# Documentation Guidance Boundaries Findings

- `AGENTS.md` already owned workflow and policy rules through its `Guidance
  Ownership Map`, so the cleanup should extend that map rather than create a
  second rule surface.
- `tasks/readme` already defines the task-packet workflow and should stay
  focused on task folders rather than becoming a full repository documentation
  map.
- `roadmap.md` is useful long-form algorithm and background context, but active
  execution state is split between `TODO.md` and the `orchestrator/state.json`
  active control plane.
- `docs/plans/`, `docs/notes/`, `tasks/archive/`, `orchestrator/rounds/`, and
  `orchestrator/worktrees/` preserve audit history. Moving them in this phase
  would create link-rot risk and weaken traceability.
- A new `docs/README.md` can solve navigation while leaving workflow authority
  with `AGENTS.md`, task artifact authority with `tasks/readme`, and
  orchestrator artifact authority with `orchestrator/artifact-manifest.md`.
