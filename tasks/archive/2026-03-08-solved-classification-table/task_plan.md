# Task Plan: Solved Classification Table

## Metadata
- Date: 2026-03-08
- Execution mode: documentation-first audit
- Skills in use: using-superpowers, planning-with-files, dispatching-parallel-agents, haskell-pro

## Goal
Finish the evidence-backed 3-column classification table for the full solved ecosystem and land it in docs/architecture.md, using built-in subagents for parallel audit work.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Initialize audit context | completed | Task folder created, rubric seeded, baseline captured |
| 2. Run parallel audits | in_progress | Built-in subagents launched for solved surface and seam/caller coverage; lead continues local audit in parallel |
| 3. Reconcile findings | completed | Final classification matrix now covers every export and named seam exactly once |
| 4. Land documentation | completed | Updated architecture doc, implementation notes, changelog, and TODO |
| 5. Verify evidence | completed | Static audit counts and targeted solved/guard slices are green |

## Classification Rubric
- Must stay somewhere = removing it without an explicit replacement would erase a thesis-relevant semantic boundary, invariant, or phase distinction.
- Can move out now = behavior is still needed, but belongs in an owning module other than Solved; moving it should be semantics-preserving.
- Safe to retire from Solved surface = dead, test-only, diagnostic-only, or redundant production surface.
- Conservative tie-breaker = if audits disagree, do not retire until the lead verifies there is no production dependency and no thesis-relevant obligation.

## Acceptance Criteria
- Every exported Solved symbol is classified exactly once in findings.
- Every adjacent seam in scope is classified exactly once in findings.
- Every architecture-doc row names the semantic boundary preserved, destination owner, or retirement rationale.
- docs/architecture.md and findings.md agree.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Shell heredoc redirection placed inside Python block while generating symbol usage map | 1 | Rerun with shell redirect outside the heredoc |

## Conclusion
- The coarse `Solved` cleanup note is now expanded into an evidence-backed grouped classification of the full solved ecosystem.
- `docs/architecture.md` is the public authoritative summary; the task `findings.md` holds the exact export/seam matrix and supporting evidence.
- Future cleanup should use this classification as the gating architectural reference.
