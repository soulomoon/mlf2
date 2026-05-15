# Round 233 Review

Date: 2026-05-15
Round: `round-233`
Milestone: `milestone-4`
Direction: `direction-4a-forallspec-binder-safety`
Extracted item: `item-4a-phi-spine-index-totalization`
Base branch: `master`
Branch: `orchestrator/round-233-forallspec-binder-safety`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Checks Run

- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Result: pass. The assigned worktree is reviewing `round-233` on `orchestrator/round-233-forallspec-binder-safety`, and the active bundle still resolves to `roadmap_id = 2026-05-05-00-type-level-safety-singletons-roadmap`, `roadmap_revision = rev-001`, `roadmap_dir = orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`.

- Command: `git status --short`
  Result: pass. The implementation payload stays confined to [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-233/mlf2.cabal:153), [src/MLF/Elab/Phi/VSpine.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/VSpine.hs:12), [src/MLF/Elab/Phi/Omega/Interpret/Internal.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/Omega/Interpret/Internal.hs:636), [src/MLF/Elab/Phi/TestSupport.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/TestSupport.hs:1), and [test/ElaborationSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/test/ElaborationSpec.hs:4806), plus reviewer-owned round artifacts. `orchestrator/state.json` remains dirty only as controller-owned stage bookkeeping, and `runtime/mlfp_io/target/release/libmlfp_io.d` is a build-generated validation artifact, not review payload.

- Command: `python3 - <<'PY'`
  ```python
  import json
  from pathlib import Path
  base = Path("orchestrator/rounds/round-233")
  state = json.loads(Path("orchestrator/state.json").read_text())
  selection = json.loads((base / "selection-record.json").read_text())
  plan = json.loads((base / "round-plan-record.json").read_text())
  view = json.loads(Path(state["roadmap_dir"], "roadmap-view.json").read_text())
  assert selection["roadmap_id"] == state["roadmap_id"]
  assert plan["roadmap_revision"] == state["roadmap_revision"]
  for anchor in ["milestone-4", "milestone-4-completion", "roadmap-history-completed-rounds"]:
      assert anchor in view["anchors"]
  print("ok")
  ```
  `PY`
  Result: pass. `selection-record.json`, `round-plan-record.json`, and the active roadmap anchors all line up with the live bundle, so status-only closeout can target `milestone-4`, `milestone-4-completion`, and `roadmap-history-completed-rounds` without semantic roadmap changes.

- Command: `git diff --check`
  Result: pass. No whitespace or patch-format issues.

- Command: `git diff --name-only -- src-public/MLF/API.hs src-public/MLF/Pipeline.hs test/Main.hs src/MLF/Constraint/Types/Witness.hs`
  Result: pass. No public API files, test-suite registration files, or witness-definition files changed. `ForallSpec` therefore still comes from [src/MLF/Constraint/Types/Witness.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Constraint/Types/Witness.hs:45) with `fsBounds` as the only binder-count source and `forallSpecBinderCount = length . fsBounds` at [src/MLF/Constraint/Types/Witness.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Constraint/Types/Witness.hs:54).

- Command: `rg -n "!!" src/MLF/Elab/Phi/VSpine.hs src/MLF/Elab/Phi/Omega/Interpret/Internal.hs src/MLF/Elab/Sigma.hs`
  Result: pass. No `!!` remains in the touched `VSpine` or selected `Phi.Omega.Interpret.Internal` path. The only remaining matches are in [src/MLF/Elab/Sigma.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Sigma.hs:62), [src/MLF/Elab/Sigma.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Sigma.hs:64), [src/MLF/Elab/Sigma.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Sigma.hs:95), and [src/MLF/Elab/Sigma.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Sigma.hs:97), which keeps this round approved as the selected Phi-path slice without overclaiming milestone-wide completion.

- Command: `rg -n "MLF\\.Elab\\.Phi\\.TestSupport" mlf2.cabal && sed -n '1,120p' src/MLF/Elab/Phi/TestSupport.hs`
  Result: pass. [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-233/mlf2.cabal:156) registers `MLF.Elab.Phi.TestSupport` only in the private `mlf2-internal` library, and the module itself is a narrow re-export seam at [src/MLF/Elab/Phi/TestSupport.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/TestSupport.hs:1) that exposes only `VSpine`, `mkVSpine`, `assertSpineSync`, `vSpineBinderAt`, and `vSpineNameAt` for tests.

- Command: `cabal build mlf2-test`
  Result: pass. The focused test target rebuilt successfully after the Phi/VSpine changes.

- Command: `cabal test mlf2-test --test-options='--match "binder-spine safety"'`
  Result: pass. The new regression block in [test/ElaborationSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/test/ElaborationSpec.hs:4806) passed with `3 examples, 0 failures`, covering quantified-type / identity-spine mismatch, out-of-range binder reads, and successful checked binder access.

- Command: `cabal test mlf2-test --test-options='--match "lookupBinderIndex"'`
  Result: pass. The witness-domain lookup slice passed with `8 examples, 0 failures`, preserving binder-index selection semantics adjacent to the touched Phi path.

- Command: `cabal test mlf2-test --test-options='--match "graft-weaken"'`
  Result: pass. The adjacent witness-normalization and Phi elaboration slice passed with `17 examples, 0 failures`, including the graft/weaken normalization cases and bounded bound-match Phi translation behavior.

- Command: `cabal test mlf2-test --test-options='--match "scheme-aware Φ can translate Raise (raise a binder to the front)"'`
  Result: pass. The direct Raise translation preservation case passed with `1 example, 0 failures`.

- Command: `cabal build all && cabal test`
  Result: pass. The full behavior-changing gate completed with `2570 examples, 0 failures` in `353.7282` seconds.

## Plan Compliance

- `Introduce a total binder-spine read boundary in MLF.Elab.Phi.VSpine (or an equally local helper seam) so binder name, bound, and id lookups used by production Phi translation report explicit ElabError/PhiInvariantError instead of relying on partial !!`: met. [src/MLF/Elab/Phi/VSpine.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/VSpine.hs:74) now routes binder reads through `vSpineBinderAt`, `vSpineNameAt`, `vSpineBoundAt`, and `vSpineIdAt`, each returning `Either ElabError ...`.

- `Update MLF.Elab.Phi.Omega.Interpret.Internal to use the checked spine-access path in the selected reorder, binder-name, and raise logic, preserving current semantics when indices are valid and failing explicitly when the witness/domain spine is inconsistent`: met. The reorder path asserts spine sync and builds binder order from checked spine reads at [src/MLF/Elab/Phi/Omega/Interpret/Internal.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/Omega/Interpret/Internal.hs:636), graft/raise reads use checked access at [src/MLF/Elab/Phi/Omega/Interpret/Internal.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/Omega/Interpret/Internal.hs:795), [src/MLF/Elab/Phi/Omega/Interpret/Internal.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/Omega/Interpret/Internal.hs:1075), and [src/MLF/Elab/Phi/Omega/Interpret/Internal.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/Omega/Interpret/Internal.hs:1280).

- `Retire direct partial spine indexing from the touched production call sites so future binder-path edits cannot silently reintroduce the same !!-based risk in the selected translation path`: met. The selected production Phi path is free of `!!`, and the remaining list indexing is confined to [src/MLF/Elab/Sigma.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Sigma.hs:56), outside the touched round payload.

- `Add focused regression coverage in test/ElaborationSpec.hs and/or test/Phi/WitnessDomainSpec.hs for binder-spine mismatch and out-of-range cases, plus a preservation case showing the valid binder-alignment behavior still succeeds after the totalization`: met. [test/ElaborationSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/test/ElaborationSpec.hs:4806) adds all three required cases, and the existing focused `lookupBinderIndex`, `graft-weaken`, and Raise selectors stay green.

- `Update only the touched durable guidance if the accepted local contract changes materially; do not claim milestone-4 closeout from this slice alone, and do not introduce a heavier type-level collection dependency or public-surface change`: met. No public API files changed, no new type-level dependency was added, witness-constructor surfaces are untouched, and this review does not claim milestone-4 completion from the slice alone.

- `Run focused Phi/Witness-domain validation first, then diff hygiene, then the full cabal build all && cabal test gate required for behavior-changing milestone-4 work`: met. `git diff --check`, `cabal build mlf2-test`, the focused binder-spine / witness-domain / adjacent Phi selectors, and the full gate all passed. An initial parallel `cabal` attempt hit `dist-newstyle` contention and was discarded; the authoritative evidence above comes from the serial rerun.

## Decision

**APPROVED**

## Evidence

- The round stays inside the selected milestone-4 direction and does not widen the production surface. `src-public/` is unchanged, [src/MLF/Constraint/Types/Witness.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Constraint/Types/Witness.hs:45) still carries the accepted list-shaped `ForallSpec` contract with binder count derived from `fsBounds`, and milestone-5 witness constructors remain untouched because no witness-definition file changed.

- The selected Phi translation path now fails closed instead of reading binder spines partially. [src/MLF/Elab/Phi/VSpine.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/VSpine.hs:74) introduces explicit checked binder access and [src/MLF/Elab/Phi/VSpine.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/VSpine.hs:134) checks spine/type/id synchronization up front. The touched Omega interpreter uses that boundary in reorder, graft, Raise, and binder-name lookup at [src/MLF/Elab/Phi/Omega/Interpret/Internal.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/Omega/Interpret/Internal.hs:636), [src/MLF/Elab/Phi/Omega/Interpret/Internal.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/Omega/Interpret/Internal.hs:795), [src/MLF/Elab/Phi/Omega/Interpret/Internal.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/Omega/Interpret/Internal.hs:1075), and [src/MLF/Elab/Phi/Omega/Interpret/Internal.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/Omega/Interpret/Internal.hs:1280).

- The test-support seam is acceptable as a narrow internal seam, not a public widening. [src/MLF/Elab/Phi/TestSupport.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Phi/TestSupport.hs:1) re-exports only the specific helpers needed by [test/ElaborationSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/test/ElaborationSpec.hs:4806), and [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-233/mlf2.cabal:156) registers it only under the package-private `mlf2-internal` library.

- Status-only closeout is lawful, but only for `pending -> in-progress`. The plan explicitly says “do not claim milestone-4 closeout from this slice alone” in [orchestrator/rounds/round-233/plan.md](/Volumes/src/mlf4/orchestrator/worktrees/round-233/orchestrator/rounds/round-233/plan.md:31), and the broader source scan still finds remaining list indexing in [src/MLF/Elab/Sigma.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-233/src/MLF/Elab/Sigma.hs:62). This round therefore closes the selected Phi/Omega binder-spine gap without proving the full milestone-4 completion signal end to end.

- The inherited recursive-inference settlement chain remains untouched. The accepted March baseline and strategic items still keep the explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback posture bounded, with `non-cyclic-graph = unknown` and no authority for cyclic search, multi-SCC search, second interfaces, or fallback widening in [docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md](/Volumes/src/mlf4/orchestrator/worktrees/round-233/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md:10), [docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md](/Volumes/src/mlf4/orchestrator/worktrees/round-233/docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md:157), and [docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md](/Volumes/src/mlf4/orchestrator/worktrees/round-233/docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md:59). The accepted `round-094` through `round-098` review records remain bounded predecessor evidence only, and this round touches none of those surfaces.
