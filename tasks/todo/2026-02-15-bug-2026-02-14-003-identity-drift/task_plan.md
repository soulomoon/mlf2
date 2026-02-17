# Task Plan: BUG-2026-02-14-003 Identity Drift (Resume)

## Objective
Resolve the remaining BUG-2026-02-14-003 failure by eliminating witness/binder identity drift between presolution emission and Phi/Omega consumption, while preserving strict non-binder rejection semantics and BUG-004 behavior.

## Scope
- Follow systematic-debugging phases strictly (root cause first).
- Make one minimal, thesis-aligned fix after evidence confirms source of drift.
- Re-verify targeted anchors before broader checks.

## Phases
1. Root cause investigation: reproduce failing anchors and capture end-to-end edge data-flow evidence. (completed)
2. Pattern analysis: compare identity handling across presolution/witness canon/Phi translation. (completed)
3. Hypothesis + minimal validation: state one contract and patch one layer only. (completed; source-domain `I(r)` + invariant mismatch path validated with RED/GREEN regression)
4. Implementation + verification: run targeted BUG-003/BUG-004 + copy-map anchors; then witness invariants. (completed for scoped anchors; all requested targeted suites green, full gate still red in separate buckets)
5. Docs/task sync: update findings/progress and bug tracker entry as needed. (completed; notes/logs/tracker updated, bug closure deferred until full gate green)

## Decisions
- Keep strict Phi/Omega rejection for non-binder targets.
- Avoid compatibility fallbacks and permissive grafting.
- Treat raw witness targets (pre-canonicalization) as the source-of-truth signal until proven otherwise.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Direct `ghci -package mlf2` probes failed (`cannot satisfy -package mlf2`) | 1 | Switched to `cabal repl lib:mlf2-internal` with `:script`. |
| Hypothesis 1 (drop source scheme info) no-op on failure class | 1 | Reverted probe and retained evidence only. |
| Hypothesis 2 (preserve witness op IDs only) no-op due Translate remap | 1 | Reverted probe and traced next layer. |
| Hypothesis 3 (preserve Translate step IDs) still fails as non-binder (`target 9`, `canonical 8`) | 1 | Reverted probe and confirmed deeper mismatch. |
| Hypothesis 4 (WitnessNorm non-`TyVar` graft/weaken prune) no-op pre-solve | 1 | Reverted probe (cannot see post-solve collapse). |
| Hypothesis 5 (Omega solve-aware skip) introduced new Phase 6/7 regressions | 1 | Reverted probe; stop after 3+ failed fixes per protocol and escalate architecture discussion. |
| Parallel targeted `cabal test` calls raced on `dist-newstyle/.../package.conf.inplace already exists` | 1 | Re-ran tests sequentially; avoid parallel cabal invocations in this task. |
| Full gate after targeted fixes still fails (`653 examples, 19 failures`) | 1 | Captured failing buckets; deferred bug closure/docs finalization pending broader stabilization. |
| Initial source-domain Omega patch regressed `BUG-004-V2` to `TCArgumentMismatch` | 1 | Traced `OpRaise` semantic-node drift; adopted source->copy mapping for execution while preserving source-domain `I(r)` check. |
| Full gate after surgical Omega closure still fails (`672 examples, 9 failures`) | 1 | Treated as separate/open buckets per scope rule; BUG-2026-02-14-003 remains open pending global stabilization. |

## 2026-02-16 Follow-up (BUG-2026-02-16-003 cluster)
- Additional root-cause/fix cycle completed for the `id id` let-polymorphism regression cluster.
- Root cause confirmed in `AAppF.argInstFromFun` inferred-arg inlining path.
- Minimal fix implemented and verified against BUG-003/004/005/006 repro matrix.
- Bug tracker synchronized: 003/004/005/006 moved to Resolved.
