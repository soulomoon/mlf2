# Kiro Specs ("Kiro spec") — a practical guide for Codex

This document teaches a code-generation model (e.g., Codex) what a **Kiro spec** is and how to **generate one correctly**.

## What is Kiro?

**Kiro** is an AI-assisted IDE workflow that emphasizes *spec-driven development*. Instead of jumping straight into code, Kiro encourages creating a structured specification for a feature, then using that spec to drive implementation.

Kiro’s “spec” workflow is **three phases / three documents**:

1. **Requirements** → `requirements.md`
2. **Design** → `design.md`
3. **Implementation plan (Tasks)** → `tasks.md`

These files are meant to be committed to version control and refined iteratively as requirements/design evolve.

## What is a “Kiro spec”?

A **Kiro spec** is the set of Markdown artifacts that formalize a feature in a way that is:

- **Clear** (written like a PM/tech lead would want)
- **Testable** (acceptance criteria can become test cases)
- **Traceable** (tasks reference specific requirements)
- **Executable** (tasks can be executed one-by-one by an implementation agent)

### Expected directory structure

Kiro specs are stored under a directory like:

- `.kiro/specs/<feature_name>/requirements.md`
- `.kiro/specs/<feature_name>/design.md`
- `.kiro/specs/<feature_name>/tasks.md`

Where `<feature_name>` is **kebab-case** (e.g., `user-authentication`, `product-reviews`, `billing-webhooks`).

---

# 1) requirements.md

## Purpose

`requirements.md` turns a rough idea into **user stories** with **acceptance criteria** that are explicit, testable, and unambiguous.

Kiro’s docs describe this file as:
- “user stories with acceptance criteria in EARS notation”
- EARS = Easy Approach to Requirements Syntax

## Required structure

### Document outline

- `# Requirements Document`
- `## Introduction` (brief summary of the feature and scope)
- `## Requirements`
  - `### Requirement 1`
    - `**User Story:** As a [role], I want [feature], so that [benefit].`
    - `#### Acceptance Criteria` with a *numbered list* in EARS form
  - `### Requirement 2`
  - ...

### EARS acceptance criteria format

Acceptance criteria should be written using EARS patterns such as:

- `WHEN <event/condition> THEN THE SYSTEM SHALL <response>`
- `IF <precondition> THEN THE SYSTEM SHALL <response>`
- `WHILE <state> THE SYSTEM SHALL <response>`
- `WHERE <feature/constraint> THE SYSTEM SHALL <response>`

Keep them:
- specific and observable (UI/API behavior, state changes, outputs)
- testable (can translate into unit/integration/e2e tests)
- scoped (avoid mixing unrelated behaviors in one criterion)

### Numbering conventions

- Requirements are numbered: `Requirement 1`, `Requirement 2`, …
- Acceptance criteria are numbered per requirement: `1.`, `2.`, `3.`
- If you want traceability to “granular sub-requirements”, you can refer to criteria as:
  - `1.1` = Requirement 1, acceptance criterion 1
  - `1.2` = Requirement 1, acceptance criterion 2
  - etc.

This becomes important in `tasks.md`, where tasks should reference the most granular relevant items.

---

# 2) design.md

## Purpose

`design.md` explains **how** the system will meet the requirements. It captures architecture, interfaces, data models, error handling, and testing strategy so implementation can follow consistently.

## Required sections (minimum)

Use these headings (exact names not strictly required, but keep the same intent):

- `# Design Document`
- `## Overview`
- `## Architecture`
- `## Components and Interfaces`
- `## Data Models`
- `## Error Handling`
- `## Testing Strategy`

## Guidance

- Design must cover **all** requirements in `requirements.md`.
- Prefer explicit interfaces/contracts:
  - function signatures, API endpoints, schemas, module boundaries
- Include diagrams when helpful (Mermaid is recommended):
  - sequence diagrams for flows
  - architecture diagrams for components
  - state diagrams for lifecycle-heavy features
- Highlight key decisions and tradeoffs:
  - why you chose approach A over B
  - scalability/security/UX implications

### Design detail level

The design should be detailed enough that the implementation plan can be broken into small steps **without re-designing** during implementation—but avoid duplicating line-by-line implementation detail that belongs in code.

---

# 3) tasks.md (Implementation plan)

## Purpose

`tasks.md` is an actionable checklist that can drive implementation *incrementally*.
It should read like a series of prompts a coding agent can execute, one task at a time, in a test-driven way.

## Task granularity (critical)

Kiro executes best when **each checkbox is small, concrete, and independently verifiable**.
If tasks are too “chunky”, agents will:
- implement only part of the intended behavior,
- miss spec details (especially paper-alignment or invariants),
- and still (incorrectly) mark the task as complete.

Use these rules to force smaller steps:

### Granularity rules of thumb

- **One checkbox = one primary outcome.** If the title contains multiple “and/plus/also”, split it.
- **Touch ≤ 2–3 files per checkbox** (excluding test wiring), unless it’s a mechanical rename.
- **Prefer ≤ ~100–200 LOC per checkbox** (ballpark; smaller is better for correctness).
- **Every checkbox must have a “Verification” line** that a reviewer/agent can run or check.
- **Only mark `[x]` when verification passes**. If partially done, leave it unchecked and add a “Gap:” note.

### How to split complex work (repeatable patterns)

For algorithmic / paper-alignment features, split into “thin slices”:

1) **Data model slice**
   - Add types/fields/errors + minimal constructors.
   - Add compile-only or simple unit tests that exercise the new types.

2) **Pure core slice**
   - Implement a pure function that captures the paper transformation/invariant.
   - Add property tests for invariants (and small regression tests for edge cases).

3) **Integration slice**
   - Wire the pure core into exactly one call site / phase.
   - Add an integration/regression test that fails before the wiring and passes after.

4) **Migration slice**
   - Migrate remaining call sites incrementally (one phase/module per checkbox).
   - Keep compatibility shims until the last migration task is done.

5) **Cleanup slice**
   - Remove dead code paths, tighten invariants, and upgrade tests to the final guarantees.

This pattern prevents “big-bang” tasks where the agent must understand and change everything at once.

## Required structure

### Top-level outline

- `# Implementation Plan`
- A numbered checkbox list, maximum **two levels of hierarchy**:
  - `- [ ] 1. <High-level task>`
  - `- [ ] 1.1 <Subtask>` (optional)
  - `- [ ] 1.2 <Subtask>` (optional)
  - `- [ ] 2. ...`

**Strong preference:** make top-level items “phases”, and put most work in `1.1`, `1.2`, … subtasks so each subtask stays small.

### What each task must include

Each checkbox item should include, as indented bullets directly under it:

- concrete steps (what code to create/modify/test)
- files/components involved (when reasonable)
- test expectations (unit/integration/e2e)
- **Verification:** an explicit command or check that proves completion (e.g. `cabal test --test-show-details=direct --match ...`, `pytest -k ...`, `go test ./...`, etc.)
- a requirement mapping line, e.g.:

`- _Requirements: 1.1, 2.3_`

**Important:** tasks must reference *granular requirements* (typically acceptance criteria like `1.2`, `3.1`, etc.), not only broad user stories.

### A “definition of done” template (recommended)

Add this as the last bullet under non-trivial checkboxes:

- **Verification:** <exact command(s) + expected result>

Examples:
- `cabal test --test-show-details=direct --match "/Binding/.*upper/"` passes
- `rg -n "OldFunctionName" src` returns no matches
- `hlint` / formatter runs cleanly (if the repo uses one)

### Constraints / what NOT to include

Only include tasks that involve **writing, modifying, or testing code**.

Do NOT include non-coding tasks like:
- user interviews / manual UAT / stakeholder sign-off
- deployment steps / production rollout checklists
- performance metric gathering (unless automated tests/benchmarks are being written)
- writing user docs/training materials (unless you’re writing code-docstrings or developer docs as code deliverables)

### Quality of a good tasks.md

- incremental, no “big jump” complexity
- validates core functionality early
- wiring/integration happens as you go (no orphaned components)
- test-driven where it makes sense
- covers all design elements that can be implemented in code

### Common anti-patterns (and how to rewrite them)

- **Bad:** “Implement Raise(n) for arbitrary nodes end-to-end.”
  - **Good:** split into:
    - “Add per-node raise trace data type + tests”
    - “Plumb trace through unifier (no behavior change) + tests”
    - “Record OpRaise for exactly the traced nodes + regression test”
    - “Normalize/elide raises under rigid binders + regression test”
    - “Implement Φ translation for non-spine Raise + α-equivalence test”

- **Bad:** “Replace old system with new system.”
  - **Good:** “Add new system behind a flag/shim”, then “migrate call site X”, then “delete shim”.

---

# Output format Codex should produce

When asked to “generate a Kiro spec”, Codex should output **three files** under a single feature directory.

Use a simple, copy/paste-friendly format like:

```text
FILE: .kiro/specs/<feature_name>/requirements.md
<content>

FILE: .kiro/specs/<feature_name>/design.md
<content>

FILE: .kiro/specs/<feature_name>/tasks.md
<content>
```

This makes it easy for downstream tools to split and write files.

---

# A starter template Codex can follow

## requirements.md template

```md
# Requirements Document

## Introduction
<1–2 paragraphs describing the feature, goals, and what is out of scope>

## Requirements

### Requirement 1
**User Story:** As a <role>, I want <capability>, so that <benefit>.

#### Acceptance Criteria
1. WHEN <event> THEN THE SYSTEM SHALL <response>.
2. IF <precondition> THEN THE SYSTEM SHALL <response>.
3. WHEN <event> AND <condition> THEN THE SYSTEM SHALL <response>.

### Requirement 2
**User Story:** As a <role>, I want <capability>, so that <benefit>.

#### Acceptance Criteria
1. WHEN <event> THEN THE SYSTEM SHALL <response>.
2. WHILE <state> THE SYSTEM SHALL <response>.
```

## design.md template

```md
# Design Document

## Overview
<what we are building and how it satisfies the requirements>

## Architecture
<high-level components and interactions; include Mermaid diagram if useful>

## Components and Interfaces
<modules/services, APIs, key function signatures, boundaries>

## Data Models
<schemas, DB tables, types, validation rules>

## Error Handling
<error categories, expected behavior, retries, UX messaging>

## Testing Strategy
<unit/integration/e2e approach; key scenarios mapped to requirements; prefer property tests for invariants>
```

## tasks.md template

```md
# Implementation Plan

- [ ] 1. Establish feature skeleton and public interfaces
  - Create module/package structure for <feature>
  - Add placeholder interfaces/types and minimal wiring
  - Add a “smoke test” to validate the new module loads and basic flow compiles/runs
  - _Requirements: 1.1_

- [ ] 2. Implement core happy-path behavior
  - [ ] 2.1 Implement <core function/service> for happy path
    - Add unit tests for expected outputs
    - Wire into API/UI entrypoint
    - _Requirements: 1.1, 1.2_
  - [ ] 2.2 Implement persistence/integration (if applicable)
    - Add integration tests
    - _Requirements: 2.1_

- [ ] 3. Implement validation and error handling
  - Add input validation and error mapping
  - Add tests for invalid/edge cases
  - _Requirements: 1.3, 2.2_
```

---

# Verification checklist (for Codex “verify the result” step)

When validating a generated Kiro spec, check:

## Structure
- [ ] Files exist at: `.kiro/specs/<feature_name>/{requirements.md,design.md,tasks.md}`
- [ ] `<feature_name>` is kebab-case
- [ ] Each file has clear headings and readable Markdown

## Requirements quality
- [ ] Every requirement has a user story
- [ ] Acceptance criteria are EARS-style and testable
- [ ] Edge cases are covered (validation, missing data, permissions, rate limits, etc. as relevant)
- [ ] No ambiguous terms without definition (“fast”, “secure”, “easy”)

## Design quality
- [ ] Every requirement is addressed by some part of the design
- [ ] Interfaces/contracts are explicit enough to implement
- [ ] Data models are specified (when relevant)
- [ ] Error handling and testing strategy are included

## Tasks quality
- [ ] Tasks are checkboxes and numbered correctly (max 2 levels)
- [ ] Each task is implementable by a coding agent (write/modify/test code)
- [ ] Each task references granular requirements (e.g., `1.2`, `3.1`)
- [ ] Steps are incremental and avoid large complexity jumps
- [ ] Every non-trivial checkbox has an explicit **Verification** line
- [ ] No checkbox is marked complete if it has known gaps (leave `[ ]` + note the gap)
- [ ] No non-coding tasks included

---

# Notes for multi-agent usage (Codex ↔ Kiro)

A practical division of labor that matches your goal:
- **Codex generates the Kiro spec** (these three documents) from a feature description and any repo context you provide.
- **Kiro implements** by executing tasks in `tasks.md` one-by-one (following the design and acceptance criteria).
- **Codex verifies** by checking implementation output against:
  - acceptance criteria (EARS),
  - design decisions (interfaces/data models),
  - task completion and traceability.

---

# Primary references (human-readable)

- Kiro Specs docs (overview + workflow)
- Kiro Specs “Concepts” (requirements/design/tasks definitions, EARS format)
- Kiro Specs “Best practices” (iteration, referencing specs, task execution guidance)
- Community-published “Spec Agent system prompt” that documents expected file paths and formatting constraints
