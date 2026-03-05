# Findings: 2026-03-06 Row6 Replay-Contract Recovery

## Initial context
- Starting from clean worktree branch: codex/row6-replay-contract-recovery-20260306
- Base commit: a82d8a80fe7620c615cbb9bac0eef836ba4ff7d3
- Prior orchestrator run evidence is archived and used as reference only.

## Wave A findings
- Clean baseline is green, but row6 remains open because producer normalization still applies compatibility behavior in `src/MLF/Constraint/Presolution/WitnessNorm.hs`.
- Current producer lane choice is op-shaped, not semantic: when replay binders exist but normalized ops are empty, `normalizeEdgeWitnessesM` drops `etBinderArgs`, clears `etBinderReplayMap`, and emits `ReplayContractNone`.
- Current no-replay compatibility path also silently prunes non-root `OpGraft`/`OpWeaken` when the edge root has no replay binders; this contradicts the planned strict producer boundary, which must reject replay-family non-root ops before Phi.
- `src/MLF/Constraint/Presolution/Driver.hs` validates strict replay-map domain/codomain membership but does not currently reject non-injective replay codomains at the final producer boundary.

## Recovery findings
- The critical discriminator for no-replay projection is source-domain identity, not rewritten/canonical node identity. Using canonical copied nodes to decide wrapper-vs-semantic behavior over-prunes bug-002 and over-retains copied no-replay weakens in `\\y. let id = (\\x. x) in id y`, A6, and annotation-heavy paths.
- Baseline-preserving no-replay behavior is:
  - project `OpGraft` away unconditionally,
  - drop `OpWeaken` when its restored target is a graft target or when there is at most one graft target,
  - allow strict no-replay only when a surviving restored `OpWeaken` remains,
  - reject only the narrowed rogue-graft residual (`single-target`, `source-interior`, non-root, non-source-key).
- No-replay `OpRaise` also splits into two classes:
  - wrapper raises under `GenRef` / missing type-tree binding are producer-only artifacts and must be pruned before Phi,
  - true type-tree-bound raises remain validated so `R-RAISE-INVALID-11` still fails at normalization time.
- Final recovery state is green:
  - targeted row6/checked-authoritative/dual-path gates pass,
  - `cabal build all && cabal test` passes with `954 examples, 0 failures`.
