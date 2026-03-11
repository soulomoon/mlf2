# Task Plan: Recursive types design

## Goal
Design a thesis-aware, codebase-grounded plan for adding recursive types to the language, including syntax, typing/elaboration impact, implementation boundaries, and recommended rollout strategy.

## Current Phase
Phase 4

## Phases

### Phase 1: Requirements & Discovery
- [x] Understand user intent
- [x] Identify constraints and requirements
- [x] Document findings in findings.md
- **Status:** complete

### Phase 2: Thesis & Codebase Analysis
- [x] Read thesis sections that constrain type-language extensions
- [x] Inspect current type/term/runtime architecture
- [x] Identify invariants that recursive types must preserve
- **Status:** complete

### Phase 3: Design Options
- [x] Compare 2–3 recursive-type approaches
- [x] Evaluate thesis-faithfulness and implementation cost
- [x] Choose a recommended direction
- **Status:** complete

### Phase 4: Detailed Design
- [ ] Specify syntax and AST changes
- [ ] Specify typing/runtime/elaboration changes
- [ ] Specify validation/tests and rollout phases
- **Status:** in_progress

### Phase 5: Delivery
- [ ] Review the design for internal consistency
- [ ] Summarize trade-offs and open questions
- [ ] Deliver design to user
- **Status:** pending

## Key Questions
1. Does the thesis already include recursive types, or would this be a principled extension?
2. Should recursive types be iso-recursive or equi-recursive in this implementation?
3. Which phases of the current pipeline can absorb recursive types with the least disruption?
4. How do recursive types interact with bounded quantification, instantiation witnesses, and runtime typechecking?
5. What rollout sequence minimizes risk while staying paper-faithful elsewhere?

## Decisions Made
| Decision | Rationale |
|----------|-----------|
| Treat this as a design-only task, not implementation | User asked for a design after reading thesis + codebase |
| Use thesis as primary source of truth | Repository guidance requires paper-faithfulness |
| Maintain an active task folder under `tasks/todo/` | Required by repository workflow for multi-step work |
| Treat recursive types as an extension, not a thesis-restatement | The thesis/xMLF grammars do not include `μ` or recursive-type rules |
| Prefer a staged iso-recursive design over equi-recursive inference | The current thesis and codebase rely on acyclicity and explicit instance witnesses; equi-recursive equality would cut across both |

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| Unquoted heredoc content executed backticks while updating planning files | 1 | Re-ran updates using quoted heredocs / `apply_patch` only |

## Notes
- Need to distinguish clearly between thesis-backed facts and extension design proposals.
- Prefer a design that preserves existing normalized-pipeline/public-surface boundaries where possible.
- Avoid assuming inference of arbitrary equi-recursive types unless the existing solver architecture genuinely supports it.
