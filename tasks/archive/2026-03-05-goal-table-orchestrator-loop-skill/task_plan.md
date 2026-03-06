# Task Plan: 2026-03-05 Goal Table + Orchestrator Loop Skill

## Goal
Abstract a reusable pattern for: (1) goal-status table generation and (2) multi-agent improving-loop orchestration prompts, then package it as a reusable Codex skill.

## Scope
- Create a goal-agnostic mechanism table template under `docs/notes/`.
- Create a goal-agnostic orchestrator prompt template under `docs/prompts/`.
- Create a round-execution log template under `docs/prompts/`.
- Create and validate a new skill at `.codex/skills/goal-table-orchestrator-loop`.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Gather baseline examples and derive common pattern | complete | Reused existing TMT table + improving-loop prompt artifacts |
| 2. Draft reusable docs templates | complete | Added goal-agnostic table/prompt/run-log templates under `docs/` |
| 3. Build new skill resources | complete | Added SKILL.md, openai metadata, references, and scaffold script |
| 4. Validate skill and finalize logs | complete | Script smoke tests + py_compile passed; quick_validate blocked by missing PyYAML |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| `init_skill.py` rejected interface (`short_description` too long) | 1 | Shorten description and proceed with manual metadata completion |
| `generate_openai_yaml.py` missing `yaml` dependency | 1 | Write `agents/openai.yaml` manually using reference constraints |
| `quick_validate.py` missing `yaml` dependency | 1 | Validate skill by manual frontmatter check + script smoke tests + `py_compile` |
