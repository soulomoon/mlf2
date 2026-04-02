# Phased-Parallel Full-Inference Orchestrator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Update the live repo-local orchestrator control plane so the full automatic iso-recursive inference family uses a strategy-backlog roadmap with milestone-level strategy, guider-extracted round work, and controlled parallel research lanes.

**Architecture:** Keep the change inside the active `orchestrator/` contract only. Rewrite the active roadmap revision from the old flat item model into the new milestone/direction strategy-backlog format, keep the same active family identity, raise `max_parallel_rounds` from `1` to `3`, and retune the role / verification / retry contract so extracted rounds record `milestone_id`, `direction_id`, and `extracted_item_id` instead of relying on the older flat `roadmap_item_id` shape. Do not change production inference code and do not start runtime rounds.

**Tech Stack:** Markdown control-plane files, JSON state, `git`, `rg`, `sed`

---

## File Structure

### Modify

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
  - Raise the controller parallel cap while preserving the active family identity and idle state.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md`
  - Replace the flat item list with a layered strategy-backlog roadmap that uses milestones, candidate directions, and parallel lanes.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md`
  - Add milestone/direction lineage checks plus integrated worker-fanout review requirements.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md`
  - Add retry reasons for extraction mistakes, illegal co-scheduling, missing worker plans, ownership overlap, and partial-evidence approval.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/guider.md`
  - Replace single-item selection language with milestone/direction extraction and bounded multi-round dispatch.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/planner.md`
  - Teach the planner to plan extracted round work and author `worker-plan.json` when the extracted scope allows it.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/implementer.md`
  - Add extracted-scope and worker-ownership implementation rules.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/reviewer.md`
  - Add extracted-lineage and integrated-result review rules.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/merger.md`
  - Preserve milestone/direction/extracted-item lineage and execution-mode honesty in merge notes.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/recovery-investigator.md`
  - Add parallel-lane / extraction-specific failure diagnosis rules.

### Read-Only Inputs

- `/Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-04-02-phased-parallel-full-inference-orchestrator-design.md`
- `/Users/ares/src/orchestratorpattern/skills/scaffold-orchestrator-loop/SKILL.md`
- `/Users/ares/src/orchestratorpattern/skills/scaffold-orchestrator-loop/references/roadmap-generation.md`
- `/Users/ares/src/orchestratorpattern/skills/scaffold-orchestrator-loop/references/repo-contract.md`
- `/Users/ares/src/orchestratorpattern/skills/scaffold-orchestrator-loop/references/verification-contract.md`

### Do Not Modify

- Any completed predecessor roadmap family under `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/` other than the active `2026-04-02-01-.../rev-001`
- Production inference code under `/Users/ares/.codex/worktrees/d432/mlf4/src/`
- Tests under `/Users/ares/.codex/worktrees/d432/mlf4/test/`
- The top-level pointer stubs unless the active family identity changes, which this plan does not authorize

## Task 1: Rewrite The Active Roadmap Into The New Strategy-Backlog Shape

**Files:**
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md`
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`

- [ ] **Step 1: Confirm the active roadmap is still the legacy flat format**

Run:

```bash
rg -n 'Item id:|Parallel safe:|Parallel group:|Merge after:' \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md
rg -n '"max_parallel_rounds": 1' \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json
```

Expected: matches confirm the roadmap still uses flat `Item id:` metadata and the state still caps parallelism at `1`.

- [ ] **Step 2: Replace the roadmap with a milestone/direction strategy backlog**

Rewrite `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md` so it uses these exact top-level sections:

```markdown
## Goal
## Outcome boundaries
## Global sequencing rules
## Parallel lanes
## Milestones
```

Use this milestone backbone:

```markdown
## Parallel lanes

- `serial-gate`
  - Used for family-freeze and final decision work that must remain singleton.
- `research-wave-a`
  - Used for the three docs-first semantic research fronts that may co-run after the freeze milestone completes.
- `integrated-positive`
  - Used for positive-family evidence work that stays one integrated round even when the planner authors worker fan-out.
- `integrated-negative`
  - Used for negative-family / boundedness evidence work that stays one integrated round even when the planner authors worker fan-out.

## Milestones

1. [pending] Freeze full-inference family contract and lawful extraction boundary
   Milestone id: `milestone-1-family-freeze`
   Depends on: none
   Intent: freeze predecessor authority, unresolved semantic matrix, family success bar, extraction boundaries, and the parallel-lane rules for later milestones.
   Completion signal: one accepted docs-only artifact records the predecessor chain, inherited boundary, active family success bar, and fail-closed extraction rules for later milestones.
   Parallel lane: `serial-gate`
   Coordination notes: serial only; no code campaign; no semantic widening; later milestones may extract work only after this milestone closes.
   Candidate directions:
   - Direction id: `direction-1-authority-and-boundary-freeze`
     Summary: publish the authoritative family freeze and extraction guard.
     Why it matters now: all later extraction depends on one stable authority surface.
     Preconditions: none
     Parallel hints: serial only
     Boundary notes: docs-only; no production changes; no readiness claim
     Extraction notes: extract one round only

2. [pending] Run the parallel semantic research wave
   Milestone id: `milestone-2-semantic-research-wave`
   Depends on: `milestone-1-family-freeze`
   Intent: settle the three docs-first semantic fronts that define the current-architecture research contract.
   Completion signal: accepted artifacts exist for mechanism mapping, search / ambiguity / boundedness, and reconstruction-visible readiness.
   Parallel lane: `research-wave-a`
   Coordination notes: guider may co-schedule directions in this milestone after milestone 1 closes; each extracted round must own one artifact only.
   Candidate directions:
   - Direction id: `direction-1-current-architecture-mechanism-map`
     Summary: explain recursive-shape discovery, propagation, owner/binder-sensitive placement, and polymorphism under the inherited boundary.
     Why it matters now: it defines what is already generalized versus still packet-specific.
     Preconditions: `milestone-1-family-freeze`
     Parallel hints: co-runnable with `direction-2-search-and-boundedness` and `direction-3-reconstruction-readiness`
     Boundary notes: docs-only; no repo-level readiness claim
     Extraction notes: extract one docs-only round for the mechanism artifact
   - Direction id: `direction-2-search-and-boundedness`
     Summary: define recursive candidate generation, ambiguity rejection, soundness guards, and bounded termination.
     Why it matters now: later campaigns need fail-closed search discipline before broader evidence is honest.
     Preconditions: `milestone-1-family-freeze`
     Parallel hints: co-runnable inside `research-wave-a`
     Boundary notes: docs-only; no cyclic search, multi-SCC, fallback, or equi-recursive widening
     Extraction notes: extract one docs-only round for the search / boundedness artifact
   - Direction id: `direction-3-reconstruction-readiness`
     Summary: define what counts as reviewable reconstructed success and the authoritative output surfaces.
     Why it matters now: solver-only success is insufficient for repo-level claims.
     Preconditions: `milestone-1-family-freeze`
     Parallel hints: co-runnable inside `research-wave-a`
     Boundary notes: docs-only; no enablement claim
     Extraction notes: extract one docs-only round for the reconstruction / readiness artifact

3. [pending] Run the integrated positive-family evidence wave
   Milestone id: `milestone-3-positive-family-evidence`
   Depends on: `milestone-2-semantic-research-wave`
   Intent: push representative positive-family evidence under the settled docs-first contracts.
   Completion signal: one integrated aggregate read distinguishes credible general support, packet folklore, and current-architecture blockers across the positive family surface.
   Parallel lane: `integrated-positive`
   Coordination notes: extracted rounds stay milestone-bound; planner may author `worker-plan.json` for disjoint worker ownership inside an extracted round.
   Candidate directions:
   - Direction id: `direction-1-propagation-and-placement`
     Summary: target propagation, retained-child ownership, and binder-sensitive placement evidence.
     Why it matters now: these are the highest-pressure unresolved current-architecture families.
     Preconditions: `milestone-2-semantic-research-wave`
     Parallel hints: keep round-level extraction serial; worker fan-out allowed only inside the extracted round
     Boundary notes: no semantic widening; bounded evidence only
     Extraction notes: extract one integrated round, optionally with worker fan-out
   - Direction id: `direction-2-polymorphism-and-nested-forall`
     Summary: target positive-family evidence around polymorphism and nested `forall`.
     Why it matters now: the mechanism map must cash out in representative higher-structure cases.
     Preconditions: `milestone-2-semantic-research-wave`
     Parallel hints: same as other milestone-3 directions
     Boundary notes: bounded evidence only
     Extraction notes: extract only when milestone-3 sequencing still remains coherent
   - Direction id: `direction-3-reconstruction-visible-positive-support`
     Summary: target positive-family evidence that must survive elaboration and reconstruction visibly.
     Why it matters now: it links implementation evidence back to the readiness contract.
     Preconditions: `milestone-2-semantic-research-wave`
     Parallel hints: same as other milestone-3 directions
     Boundary notes: bounded evidence only
     Extraction notes: extract only when the guider can keep milestone-3 integration honest

4. [pending] Run the integrated negative-family and boundedness wave
   Milestone id: `milestone-4-negative-family-boundedness`
   Depends on: `milestone-2-semantic-research-wave`, `milestone-3-positive-family-evidence`
   Intent: validate ambiguity rejection, soundness guards, and bounded termination under the same architecture.
   Completion signal: one integrated aggregate read shows fail-closed or bounded behavior for representative `N1`, `N2`, and `N6`.
   Parallel lane: `integrated-negative`
   Coordination notes: planner-authored worker fan-out is allowed only inside the extracted round and only with disjoint ownership.
   Candidate directions:
   - Direction id: `direction-1-ambiguity-and-soundness`
     Summary: validate fail-closed rejection for ambiguous and unsound recursive candidates.
     Why it matters now: negative-family evidence must remain stronger than convenience pressure.
     Preconditions: `milestone-2-semantic-research-wave`
     Parallel hints: keep extraction serial; worker fan-out only inside the extracted round
     Boundary notes: no guessed success
     Extraction notes: extract one integrated round when milestone-4 opens
   - Direction id: `direction-2-termination-pressure`
     Summary: validate bounded behavior under representative termination pressure.
     Why it matters now: general inference claims fail if search growth is unexplained.
     Preconditions: `milestone-2-semantic-research-wave`, `milestone-3-positive-family-evidence`
     Parallel hints: same as other milestone-4 directions
     Boundary notes: bounded success or bounded rejection only
     Extraction notes: extract only when the guider can keep milestone-4 integration coherent

5. [pending] Record the final readiness or architecture decision
   Milestone id: `milestone-5-final-decision`
   Depends on: `milestone-3-positive-family-evidence`, `milestone-4-negative-family-boundedness`
   Intent: record one end-state decision for full automatic iso-recursive inference inside or beyond the current architecture.
   Completion signal: one accepted decision artifact records exactly one end-state outcome and one next lawful handoff.
   Parallel lane: `serial-gate`
   Coordination notes: serial only; integrate the whole family evidence surface.
   Candidate directions:
   - Direction id: `direction-1-readiness-or-architecture-decision`
     Summary: publish the final readiness / continue-bounded / boundary-revision decision.
     Why it matters now: the family must close with one honest repo-level read.
     Preconditions: `milestone-3-positive-family-evidence`, `milestone-4-negative-family-boundedness`
     Parallel hints: serial only
     Boundary notes: exactly one explicit end-state outcome
     Extraction notes: extract one docs-only decision round
```

- [ ] **Step 3: Raise the controller parallel cap without changing the active family identity**

Update `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json` so this fragment becomes:

```json
{
  "roadmap_id": "2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap",
  "roadmap_revision": "rev-001",
  "roadmap_dir": "orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001",
  "stage": "select-task",
  "controller_stage": "dispatch-rounds",
  "max_parallel_rounds": 3
}
```

Do not add placeholder round records. The active state should stay idle, with lineage fields appearing later only inside extracted `active_rounds[]` records.

- [ ] **Step 4: Verify the roadmap/state rewrite**

Run:

```bash
python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json
sed -n '1,260p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md
rg -n '## Goal|## Outcome boundaries|## Global sequencing rules|## Parallel lanes|Milestone id:|Direction id:|Extraction notes:|research-wave-a|"max_parallel_rounds": 3' \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md
```

Expected: valid JSON; the roadmap shows the layered strategy-backlog sections; `Milestone id:` and `Direction id:` are present; `state.json` shows `"max_parallel_rounds": 3`.

- [ ] **Step 5: Commit**

```bash
git -C /Users/ares/.codex/worktrees/d432/mlf4 add \
  orchestrator/state.json \
  orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md
git -C /Users/ares/.codex/worktrees/d432/mlf4 commit -m "Rewrite full-inference roadmap as strategy backlog"
```

## Task 2: Align Verification And Retry Contracts With Milestone/Direction Lineage

**Files:**
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md`
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md`

- [ ] **Step 1: Confirm the current contracts still assume the older flat round identity**

Run:

```bash
rg -n 'roadmap_item_id|item-1|item-2|item-3|item-4|item-5|item-6|item-7' \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md
```

Expected: matches show the contracts still refer to the old flat item model.

- [ ] **Step 2: Rewrite the verification contract for extracted-round lineage**

Update `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md` so the baseline checks explicitly require:

```markdown
- `selection.md` records `roadmap_id`, `roadmap_revision`, `roadmap_dir`, `milestone_id`, `direction_id`, and `extracted_item_id`
- `review-record.json` records the same roadmap lineage when the round finalizes
- `roadmap_item_id` appears only when the active roadmap revision is still a legacy flat roadmap or when a compatibility mirror is explicitly required
- any worker-fanout round includes `worker-plan.json`
- worker ownership boundaries were respected
- approval is based on the integrated round result rather than isolated worker slices
```

Replace the old `item-N` task-specific checks with milestone/direction checks shaped like this:

```markdown
- milestone 1 extracted rounds: verify predecessor authority, inherited boundary, success bar, and extraction guard are frozen explicitly
- milestone 2 extracted rounds: verify the selected direction owns exactly one docs-first semantic artifact and does not upgrade packet evidence into repo-level readiness
- milestone 3 extracted rounds: verify the selected positive-family direction stays inside the extracted scope and any worker fan-out is reviewed from the integrated result
- milestone 4 extracted rounds: verify the selected negative-family / boundedness direction remains fail-closed or bounded with evidence
- milestone 5 extracted rounds: verify the final decision records exactly one end-state outcome and one next lawful handoff
```

- [ ] **Step 3: Rewrite the retry contract for extraction mistakes and fanout mistakes**

Update `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md` so it explicitly includes:

```markdown
- stale milestone / direction / extracted-item lineage
- illegal co-scheduling across incompatible parallel lanes
- missing or invalid `worker-plan.json`
- worker ownership overlap
- integration drift from the selected extracted scope
- approval attempts based on partial worker evidence rather than the integrated result
```

Structure the scope section around milestone families rather than flat `item-N` bullets.

- [ ] **Step 4: Verify the contract rewrite**

Run:

```bash
sed -n '1,260p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md
printf '\n@@\n'
sed -n '1,260p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md
rg -n 'milestone_id|direction_id|extracted_item_id|roadmap_item_id appears only|worker-plan.json|integrated round result|illegal co-scheduling|ownership overlap' \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md
```

Expected: the printed contracts show milestone/direction lineage rather than flat item ids, and `rg` finds every required phrase.

- [ ] **Step 5: Commit**

```bash
git -C /Users/ares/.codex/worktrees/d432/mlf4 add \
  orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md \
  orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md
git -C /Users/ares/.codex/worktrees/d432/mlf4 commit -m "Align verification and retry with extracted-round lineage"
```

## Task 3: Retune The Role Contract From Flat Items To Extracted Round Work

**Files:**
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/guider.md`
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/planner.md`
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/implementer.md`
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/reviewer.md`
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/merger.md`
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/recovery-investigator.md`

- [ ] **Step 1: Snapshot the old flat-item wording**

Run:

```bash
rg -n 'Choose exactly one roadmap item|selected roadmap item|roadmap_item_id|Do not run parallel rounds|keep round plans serial|current roadmap item' \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/guider.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/planner.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/implementer.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/reviewer.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/merger.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/recovery-investigator.md
```

Expected: matches confirm the roles still talk in terms of flat items and serial-only guidance.

- [ ] **Step 2: Rewrite the guider and planner around milestone/direction extraction**

Update `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/guider.md` so it explicitly says:

```markdown
- choose one or more ready candidate directions whose milestone dependencies are satisfied and whose parallel-lane guidance permits co-scheduling
- respect `max_parallel_rounds`
- extract concrete round work from `Candidate directions:` instead of copying roadmap text verbatim
- record `milestone_id`, `direction_id`, and `extracted_item_id` in `selection.md`
- use `roadmap_item_id` only when a compatibility mirror is required for legacy flat roadmap consumers
- do not dispatch conflicting extractions across incompatible lanes or overlapping surfaces
```

Update `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/planner.md` so it explicitly says:

```markdown
- write `plan.md` for the selected extracted scope, not for an abstract milestone
- when the extracted scope authorizes integrated worker fan-out, write `worker-plan.json`
- define worker ownership in exact files or exact function/module slices
- keep workers on disjoint write sets
- specify focused verification per worker
- require one integrated verification pass before review approval
```

- [ ] **Step 3: Rewrite the remaining roles around extracted lineage and integrated results**

Update the role files with these exact ideas:

```markdown
implementer.md
- implement the selected extracted round scope only
- when `worker-plan.json` exists, stay strictly inside the assigned ownership slice
- record the worker-owned verification actually run before integration

reviewer.md
- confirm `selection.md` and `review-record.json` preserve `milestone_id`, `direction_id`, and `extracted_item_id`
- confirm `worker-plan.json` exists when required
- approve only from the integrated round result, not isolated worker slices
- reject if two workers silently overlapped or if integration widened the extracted scope

merger.md
- preserve `milestone_id`, `direction_id`, `extracted_item_id`, and any compatibility `roadmap_item_id` in `merge.md`
- record whether the extracted work ran as one serial round, lane-level co-scheduled round work, or one integrated fan-out round

recovery-investigator.md
- distinguish true semantic blockers from bad direction extraction, bad lane co-scheduling, bad worker partitioning, or merge/integration mistakes between parallel slices
```

- [ ] **Step 4: Verify the role rewrite**

Run:

```bash
for f in guider planner implementer reviewer merger recovery-investigator; do
  echo "@@ $f @@"
  sed -n '1,220p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/$f.md
done
rg -n 'milestone_id|direction_id|extracted_item_id|worker-plan.json|integrated round result|parallel lane|compatibility mirror|ownership slice|worker partitioning' \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/guider.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/planner.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/implementer.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/reviewer.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/merger.md \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/recovery-investigator.md
```

Expected: the printed role files consistently use milestone/direction/extracted-scope language and `rg` finds every required phrase.

- [ ] **Step 5: Commit**

```bash
git -C /Users/ares/.codex/worktrees/d432/mlf4 add \
  orchestrator/roles/guider.md \
  orchestrator/roles/planner.md \
  orchestrator/roles/implementer.md \
  orchestrator/roles/reviewer.md \
  orchestrator/roles/merger.md \
  orchestrator/roles/recovery-investigator.md
git -C /Users/ares/.codex/worktrees/d432/mlf4 commit -m "Retune orchestrator roles for extracted round work"
```

## Task 4: Run Final Control-Plane Verification

**Files:**
- Modify: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/**`

- [ ] **Step 1: Run the final verification sweep**

Run:

```bash
python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json
git -C /Users/ares/.codex/worktrees/d432/mlf4 diff --check
test -f /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md
test -f /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md
test -f /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md
rg -n 'Milestone id:|Direction id:|extracted_item_id|research-wave-a|worker-plan.json|"max_parallel_rounds": 3|compatibility mirror' \
  /Users/ares/.codex/worktrees/d432/mlf4/orchestrator
git -C /Users/ares/.codex/worktrees/d432/mlf4 status --short
```

Expected:
- JSON pretty-print succeeds
- `git diff --check` is clean
- the active roadmap bundle files exist
- `rg` finds the new strategy-backlog and extracted-lineage phrases
- `git status --short` shows only the intended orchestrator control-plane changes

- [ ] **Step 2: Create the checkpoint commit**

Run:

```bash
git -C /Users/ares/.codex/worktrees/d432/mlf4 add orchestrator
git -C /Users/ares/.codex/worktrees/d432/mlf4 commit -m "Adopt strategy-backlog orchestrator contract"
```

Expected: one docs/control-plane-only checkpoint commit for the updated redesign.
