# Task Plan: Solver Pipeline vs Thesis Review

## Goal
Audit the implementation pipeline against `papers/these-finale-english.txt`, focusing on whether graph operations (`GraphOp`) are performed before or after solving, and identify mismatches with concrete code and thesis references.

## Scope
- Read thesis sections describing pipeline order around graph operations and solving.
- Read pipeline/solver code paths in `src/MLF/Constraint/*` and entry points in exposed modules.
- Produce a review with severity-ranked findings and file/line references.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Gather thesis references for order of operations | completed | Captured §12.1.3 SolveConstraint and §15.2/§15.3 translation passages |
| 2. Trace implementation pipeline order | completed | Traced `runPipelineElabWith` and presolution/solve/elab modules |
| 3. Compare and record mismatches | completed | Identified one thesis-alignment risk plus one documentation inconsistency |
| 4. Summarize review findings | completed | Prepared severity-ranked findings with file/line citations |

## Decisions
- Use thesis text (`papers/these-finale-english.txt`) as source of truth.
- Treat this as a code review (findings-first output).

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| None | - | - |
