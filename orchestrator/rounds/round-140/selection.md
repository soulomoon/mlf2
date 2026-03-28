# Round 140 Selection

- `roadmap_id`: `2026-03-29-00-automatic-iso-recursive-type-inference-implementation`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-03-29-00-automatic-iso-recursive-type-inference-implementation/rev-001`

## Selected item

- `item-2` — Wire automatic μ-introduction into the pipeline and add focused test coverage

## Why this runs now

- `item-1` is complete, so the cycle-breaking / μ-introduction pass exists and is ready to be integrated.
- `item-2` depends on `item-1`, and that dependency is now satisfied.
- This round should stay focused on pipeline wiring plus targeted recursive-definition tests.

## Scope

- Wire `breakCyclesAndCheckAcyclicity` into the pipeline path.
- Add focused tests for recursive definitions.
