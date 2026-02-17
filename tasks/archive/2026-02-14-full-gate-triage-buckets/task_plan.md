# Task Plan: Full-Gate Failure Triage into Bug Buckets (2026-02-14)

## Objective
Run a focused triage pass for the current full-suite 42-failure state and split failures into minimal, actionable bug buckets in `Bugs.md`.

## Scope
- Triage only; no behavioral code fixes in this task.
- Use current dirty branch state as the observed baseline.
- Produce minimal repro commands and expected/actual summaries per bucket.

## Phases
1. Baseline evidence capture + failure inventory (complete)
2. Bucketization into minimal root-cause groups (complete)
3. `Bugs.md` updates for open buckets (complete)
4. Verification + summary (complete)

## Decisions
- Run triage in current workspace because uncommitted state is the exact failing baseline under analysis.
- Use subagent-driven-development gates (implementer -> spec review -> quality review) for the triage artifact.
- Keep bucket count at 4: one Phase-1 constraint rewrite bug, one Phase-4 OpRaise/witness bug, and two elaboration/pipeline regressions split by provenance-vs-polymorphism failure mode.
- Preserve all resolved bug history; update only `## Open` in `Bugs.md`.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
