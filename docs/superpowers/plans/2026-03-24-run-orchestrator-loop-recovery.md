# Run-Orchestrator-Loop Recovery Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Update the shared `run-orchestrator-loop` skill so it recovers from incidental delegation failures instead of stopping at the first non-observable stage handoff.

**Architecture:** Keep the change inside the shared skill package at `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/`. Add one shared-skill-owned `recovery-investigator` reference, then thread its recovery contract through `SKILL.md`, `references/delegation-boundaries.md`, and `references/resume-rules.md` using grep-based red/green checks to prove the new recovery semantics are actually present.

**Tech Stack:** Markdown documentation, `rg`, `git diff --check`, Git

---

## File Structure

### Create

- `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/recovery-investigator.md`
  - Shared-skill-owned recovery role definition, inputs, outputs, and hard boundaries.

### Modify

- `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/SKILL.md`
  - Add the controller-owned recovery subloop, define incidental delegation failure vs terminal blockage, and require the shared `recovery-investigator`.
- `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/delegation-boundaries.md`
  - Expand controller-allowed recovery actions and define what still must remain delegated.
- `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/resume-rules.md`
  - Define how to resume after a non-observable stage and when direct controller blockage is allowed because recovery investigation itself cannot launch.

### Read-Only Inputs

- `/Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-03-24-run-orchestrator-loop-recovery-design.md`
- `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/state-machine.md`
- `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/worktree-merge-rules.md`

### Do Not Modify

- Repo-local orchestrator contracts inside `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/`
- `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/agents/openai.yaml`

## Task 1: Add The Shared `recovery-investigator` Role Reference

**Files:**
- Create: `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/recovery-investigator.md`
- Test: shell checks against `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/recovery-investigator.md`

- [ ] **Step 1: Write the failing test**

```bash
rg -n 'recovery-investigator' /Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/recovery-investigator.md
```

Expected: FAIL because the file does not exist yet.

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
test -f /Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/recovery-investigator.md
```

Expected: non-zero exit status.

- [ ] **Step 3: Write minimal implementation**

Create `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/recovery-investigator.md` with:

```markdown
# Recovery Investigator

The recovery investigator is a shared-skill-owned real subagent used only when
a delegated stage becomes non-observable or leaves an untrustworthy artifact.

## Inputs

- current `orchestrator/state.json`
- current round directory contents
- branch/worktree status
- role definitions
- prior wait/retry observations
- controller-visible failure evidence
- shared recovery rules

## Outputs

- diagnosis
- recommended recovery action
- recommendation on same-vs-different delegation mechanism
- recommendation on whether the controller can safely continue
- optional controller-owned recovery note

## Boundaries

- may not write `selection.md`, `plan.md`, implementation artifacts,
  `review.md`, `review-record.json`, or `merge.md`
- may not perform guider/planner/implementer/reviewer/merger substantive work
- may not act as the stage reviewer during review-stage failures
- may not make roadmap decisions
- may not merge or finalize rounds
```

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
test -f /Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/recovery-investigator.md && \
rg -n 'shared-skill-owned|diagnosis|recommended recovery action|may not write `selection.md`|may not perform guider/planner/implementer/reviewer/merger substantive work|may not act as the stage reviewer' /Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/recovery-investigator.md
```

Expected: PASS with matching lines.

- [ ] **Step 5: Commit**

```bash
git -C /Users/ares/src/orchestratorpattern add skills/public/run-orchestrator-loop/references/recovery-investigator.md
git -C /Users/ares/src/orchestratorpattern commit -m "Add orchestrator recovery investigator reference"
```

## Task 2: Teach `SKILL.md` The Recovery Subloop

**Files:**
- Modify: `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/SKILL.md`
- Test: shell checks against `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/SKILL.md`

- [ ] **Step 1: Write the failing test**

Run:

```bash
rg -n 'recovery-investigator|incidental delegation failure|available delegation mechanism|directly from controller-visible evidence' /Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/SKILL.md
```

Expected: FAIL or missing the required phrases in the current file.

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
python3 - <<'PY'
from pathlib import Path
text = Path('/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/SKILL.md').read_text()
required = [
    'recovery-investigator',
    'incidental delegation failure',
    'available delegation mechanism',
    'controller-visible evidence',
]
missing = [item for item in required if item not in text]
assert missing, f"Expected missing phrases before edit, got none: {required}"
print('missing-before-edit:', ', '.join(missing))
PY
```

Expected: PASS because at least one required phrase is missing before implementation.

- [ ] **Step 3: Write minimal implementation**

Update `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/SKILL.md` to:

```markdown
- load `references/recovery-investigator.md`
- define incidental delegation failure as missing/untrustworthy stage artifacts
- require a dedicated real-subagent `recovery-investigator`
- allow switching to another available real-subagent mechanism during recovery
- state that terminal blockage is recorded only after lawful recovery paths are exhausted
- require a re-check that the expected stage artifact exists and matches the
  current round, stage, retry attempt, and repo-local contract before the
  controller leaves recovery
- allow broad controller-owned recovery actions without permitting controller-authored role artifacts
- define “available delegation mechanism”
- allow direct blockage recording only when no qualifying recovery investigator can launch
```

Be explicit that repo-local guider/planner/implementer/reviewer/merger role
resolution is unchanged.

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
python3 - <<'PY'
from pathlib import Path
text = Path('/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/SKILL.md').read_text()
required = [
    'references/recovery-investigator.md',
    'incidental delegation failure',
    'recovery-investigator',
    'another available real-subagent mechanism',
    'lawful recovery paths are exhausted',
    'matches the current round, stage, retry attempt, and repo-local contract',
    'available delegation mechanism',
    'controller-visible evidence',
]
missing = [item for item in required if item not in text]
assert not missing, f"Still missing: {missing}"
print('skill-md-ok')
PY
```

Expected: `skill-md-ok`.

- [ ] **Step 5: Commit**

```bash
git -C /Users/ares/src/orchestratorpattern add skills/public/run-orchestrator-loop/SKILL.md
git -C /Users/ares/src/orchestratorpattern commit -m "Add orchestrator recovery subloop rules"
```

## Task 3: Update Delegation And Resume References

**Files:**
- Modify: `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/delegation-boundaries.md`
- Modify: `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/resume-rules.md`
- Test: shell checks against both reference files

- [ ] **Step 1: Write the failing test**

Run:

```bash
rg -n 'recovery-investigator|controller-owned recovery|non-observable|controller-visible evidence' /Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/delegation-boundaries.md /Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/resume-rules.md
```

Expected: FAIL or incomplete matches in the current references.

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
python3 - <<'PY'
from pathlib import Path
files = {
    'delegation': Path('/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/delegation-boundaries.md').read_text(),
    'resume': Path('/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/resume-rules.md').read_text(),
}
checks = {
    'delegation': ['recovery-investigator', 'controller-owned recovery'],
    'resume': ['non-observable', 'controller-visible evidence'],
}
missing = {
    name: [item for item in required if item not in text]
    for name, (text, required) in ((k, (files[k], checks[k])) for k in files)
}
assert any(missing.values()), f'Expected missing phrases before edit, got none: {missing}'
print(missing)
PY
```

Expected: output showing missing phrases.

- [ ] **Step 3: Write minimal implementation**

Update the references so they say all of the following:

```markdown
delegation-boundaries.md:
- controller may run recovery investigation and controller-owned repair actions
- substantive guider/planner/implementer/reviewer/merger outputs remain delegated
- recovery-investigator is shared-skill-owned, not repo-local
- recovery-investigator may not act as the substantive stage reviewer

resume-rules.md:
- a non-observable stage resumes into recovery, not immediate blockage
- controller keeps the same round, same branch, same worktree, and same stage while recovering
- controller also preserves `current_task` and the retry attempt unless the
  missing stage or repo-local retry state is the thing that legitimately
  changes them
- direct blockage is allowed only when no qualifying recovery investigator can launch
```

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
python3 - <<'PY'
from pathlib import Path
checks = {
    '/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/delegation-boundaries.md': [
        'recovery-investigator',
        'controller-owned recovery',
        'shared-skill-owned',
        'substantive stage reviewer',
    ],
    '/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/resume-rules.md': [
        'non-observable',
        'same round',
        'same branch',
        'same worktree',
        'same stage',
        'current_task',
        'retry attempt',
        'controller-visible evidence',
    ],
}
for path, required in checks.items():
    text = Path(path).read_text()
    missing = [item for item in required if item not in text]
    assert not missing, f'{path} missing {missing}'
print('reference-files-ok')
PY
```

Expected: `reference-files-ok`.

- [ ] **Step 5: Commit**

```bash
git -C /Users/ares/src/orchestratorpattern add \
  skills/public/run-orchestrator-loop/references/delegation-boundaries.md \
  skills/public/run-orchestrator-loop/references/resume-rules.md
git -C /Users/ares/src/orchestratorpattern commit -m "Refine orchestrator recovery references"
```

## Task 4: Cross-Document Verification And Final Skill Commit

**Files:**
- Verify: `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/SKILL.md`
- Verify: `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/delegation-boundaries.md`
- Verify: `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/resume-rules.md`
- Verify: `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/references/recovery-investigator.md`

- [ ] **Step 1: Write the failing test**

Run:

```bash
python3 - <<'PY'
from pathlib import Path
base = Path('/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop')
files = {
    'skill': (base / 'SKILL.md').read_text(),
    'delegation': (base / 'references/delegation-boundaries.md').read_text(),
    'resume': (base / 'references/resume-rules.md').read_text(),
    'investigator': (base / 'references/recovery-investigator.md').read_text(),
}
required = {
    'skill': ['recovery-investigator', 'available delegation mechanism', 'matches the current round, stage, retry attempt, and repo-local contract'],
    'delegation': ['controller-owned recovery', 'shared-skill-owned', 'substantive stage reviewer'],
    'resume': ['non-observable', 'same branch', 'same worktree', 'same stage', 'current_task', 'retry attempt', 'controller-visible evidence'],
    'investigator': ['diagnosis', 'recommended recovery action', 'same-vs-different delegation mechanism', 'safely continue', 'guider/planner/implementer/reviewer/merger substantive work', 'stage reviewer'],
}
missing = {
    name: [item for item in required[name] if item not in text]
    for name, text in files.items()
}
assert any(missing.values()), f'Expected at least one unfinished area before final verification, got {missing}'
print(missing)
PY
```

Expected: if earlier tasks were skipped or incomplete, this catches it. If everything already passes, note that the red case was satisfied earlier in the task sequence and proceed.

- [ ] **Step 2: Run test to verify current verification state**

Run:

```bash
git -C /Users/ares/src/orchestratorpattern diff --check
```

Expected: no output.

- [ ] **Step 3: Write minimal implementation**

If any cross-document term is still inconsistent:

```markdown
- align wording so all files agree on:
  - incidental delegation failure
  - available delegation mechanism
  - shared-skill-owned recovery-investigator
  - controller-visible evidence direct-blockage exception
```

If no inconsistencies remain, make no extra edits.

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
python3 - <<'PY'
from pathlib import Path
base = Path('/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop')
paths = [
    base / 'SKILL.md',
    base / 'references/delegation-boundaries.md',
    base / 'references/resume-rules.md',
    base / 'references/recovery-investigator.md',
]
for path in paths:
    assert path.exists(), f'missing {path}'
text = {path.name: path.read_text() for path in paths}
assert 'recovery-investigator' in text['SKILL.md']
assert 'controller-owned recovery' in text['delegation-boundaries.md']
assert 'non-observable' in text['resume-rules.md']
assert 'recommended recovery action' in text['recovery-investigator.md']
assert 'matches the current round, stage, retry attempt, and repo-local contract' in text['SKILL.md']
assert 'same branch' in text['resume-rules.md']
assert 'same worktree' in text['resume-rules.md']
assert 'same stage' in text['resume-rules.md']
assert 'current_task' in text['resume-rules.md']
assert 'retry attempt' in text['resume-rules.md']
assert 'same-vs-different delegation mechanism' in text['recovery-investigator.md']
assert 'safely continue' in text['recovery-investigator.md']
assert 'guider/planner/implementer/reviewer/merger substantive work' in text['recovery-investigator.md']
assert 'stage reviewer' in text['recovery-investigator.md']
assert 'substantive stage reviewer' in text['delegation-boundaries.md']
assert 'no qualifying recovery-investigator can launch' in text['SKILL.md'] or 'no qualifying recovery investigator can launch' in text['SKILL.md']
assert 'repo-local guider/planner/implementer/reviewer/merger sources exactly as it does today' in text['SKILL.md']
print('final-verification-ok')
PY
git -C /Users/ares/src/orchestratorpattern diff --check
```

Expected: `final-verification-ok` and no diff-check output.

- [ ] **Step 5: Commit**

```bash
git -C /Users/ares/src/orchestratorpattern add \
  skills/public/run-orchestrator-loop/SKILL.md \
  skills/public/run-orchestrator-loop/references/delegation-boundaries.md \
  skills/public/run-orchestrator-loop/references/resume-rules.md \
  skills/public/run-orchestrator-loop/references/recovery-investigator.md
git -C /Users/ares/src/orchestratorpattern commit -m "Improve orchestrator delegation recovery flow"
```
