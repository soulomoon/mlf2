# Task Plan

## Goal
Propose exactly one bounded simplification still needed in the current repository, preserving thesis-exact behavior.

## Phases
- [complete] Initialize research notes
- [complete] Re-check repo state and guidance surfaces
- [complete] Inspect code for bounded simplification candidates
- [complete] Select one thesis-safe proposal
- [complete] Return schema-only result

## Decisions
- Follow `papers/these-finale-english.txt` as the source of truth; consult `papers/xmlf.txt` only if the thesis is silent.
- Avoid already accepted themes from prior rounds and any idea rejected in this round.
- Select the dead `rtvSchemeBodyTarget` wrapper in `MLF.Elab.Run.ResultType.View` as the round-4 simplification candidate because it is currently unused, conflicts with the documented single-owner `schemeBodyTarget` boundary in `MLF.Elab.Run.Scope`, and does not touch thesis-facing logic.

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| `rg` shell command parse error from unescaped regex quotes | 1 | Re-ran the search with a here-doc / safer quoting. |
| Python helper aborted when `rg` returned no hits for a candidate alias | 1 | Re-ran with non-failing subprocess handling and kept only targeted searches. |
| Python regex helper hit a quote-escaping syntax error | 1 | Re-ran with double-quoted regex literals. |
