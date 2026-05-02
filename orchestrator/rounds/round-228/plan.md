# Round 228 Plan

- Round: `round-228`
- Roadmap:
  `2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`
- Milestone: `milestone-7`
- Direction: `direction-7a-close-the-mechanism-table-and-guidance-ledger`
- Extracted item: absent
- Retry: `attempt-2` (`state.retry.attempt = 1`, `stage_action = retry`)
- Actionable slice: `hardening`
- Execution shape: serial, retry-only changelog/guard hardening, no worker
  fan-out, no production/backend-surface edits, and no controller-state edits

## Retry Objective

Resolve only the two blockers recorded in
`orchestrator/rounds/round-228/review.md` and preserved in
`orchestrator/rounds/round-228/reviews/attempt-1.md`:

1. `CHANGELOG.md` must restate the preserved merged-`710c92eb` boundary with
   the explicit markers
   `one executable eager backend IR`,
   `no public \`LowerableBackend.IR\``, and
   `no lazy STG`.
2. The row-7 guard in `test/RepoGuardSpec.hs` must require those same
   changelog markers instead of the weaker aggregate-only set.

Treat the accepted attempt-1 payload outside those two files as carried
forward and frozen. Do not reopen mechanism-table row text, `TODO.md`, the
top-level `implementation_notes.md`, or any backend contract surface.

## Retry Contract Anchor

Active `state.json` retry record:

- Retry reason:
  `CHANGELOG.md omits the preserved-boundary markers required by the row-7 closeout ledger, and the row-7 guard does not enforce those changelog markers.`
- Fix hypothesis:
  `Update CHANGELOG.md to restate the one executable eager backend IR, no public LowerableBackend.IR, and no lazy STG boundary on merged 710c92eb, then strengthen the row-7 guard changelog markers to require the same text.`

This retry plan implements that fix hypothesis literally. It does not reopen
the broader attempt-1 row-7 closeout scope.

## Authorized Write Scope

Modify only these files during the retry implementation stage:

- `CHANGELOG.md`
- `test/RepoGuardSpec.hs`
- `orchestrator/rounds/round-228/implementation-notes.md` only if a brief
  attempt-2 evidence delta is needed after verification

Read these files as frozen carried-forward evidence only; do not edit them in
attempt 2:

- `TODO.md`
- `implementation_notes.md`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `orchestrator/rounds/round-228/selection.md`
- `orchestrator/rounds/round-228/review.md`
- `orchestrator/rounds/round-228/reviews/attempt-1.md`

Do not modify:

- `docs/architecture.md`
- `docs/backend-native-pipeline.md`
- `README.md`
- `Bugs.md`
- `mlf2.cabal`
- any file under `src/`, `src-public/`, or `app/`
- any test file other than `test/RepoGuardSpec.hs`
- `TODO.md`
- `implementation_notes.md`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `orchestrator/state.json`
- `orchestrator/roadmap.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- any file under `orchestrator/roadmaps/`

Preserve `orchestrator/rounds/round-228/reviews/attempt-1.md` as an immutable
review snapshot.

## Locked Context

- Active selection:
  `orchestrator/rounds/round-228/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- Merged family baseline:
  accepted `round-227`, merged as `710c92eb`, froze row 6 and left row 7 as
  the final closeout lane
- Attempt-1 carried-forward repo-facing payload already exists in:
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `TODO.md`,
  `implementation_notes.md`,
  `CHANGELOG.md`, and
  `test/RepoGuardSpec.hs`
- Review conclusion:
  row 7 may stay closed only if the changelog wording and the row-7 guard are
  tightened to the same explicit preserved-boundary claim

## Pre-Edit Freeze

Before any retry edit, capture digests for the frozen carried-forward files and
the immutable review snapshot. The final scope guard must prove they stayed
unchanged through attempt 2.

```sh
python3 - <<'PY' >/tmp/round-228-attempt2-frozen.json
import hashlib
import json
from pathlib import Path

paths = [
    "TODO.md",
    "implementation_notes.md",
    "docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md",
    "orchestrator/rounds/round-228/selection.md",
    "orchestrator/rounds/round-228/plan.md",
    "orchestrator/rounds/round-228/review.md",
    "orchestrator/rounds/round-228/reviews/attempt-1.md",
]
print(
    json.dumps(
        {path: hashlib.sha256(Path(path).read_bytes()).hexdigest() for path in paths},
        indent=2,
        sort_keys=True,
    )
)
PY
```

## Sequential Plan

### Task 1: Repair the `CHANGELOG.md` closeout bullet only

**File**

- `CHANGELOG.md`

**Required edits**

- Keep the retry edit inside the existing backend-boundary closeout bullet
  under `## Unreleased` / `### Changed`.
- Preserve the accepted attempt-1 closeout facts already present:
  merged `710c92eb`,
  `all seven backend-boundary mechanism-table rows are now explicitly settled`,
  row 7 is owned by the dedicated
  `backend-boundary mechanism table and closeout ledger stay synchronized`
  guard, and
  no new backend feature or public boundary was introduced.
- Add the three explicit preserved-boundary markers the review requires:
  `one executable eager backend IR`,
  `no public \`LowerableBackend.IR\``, and
  `no lazy STG`.
- Keep the wording bounded to restating the already accepted backend boundary.
  Do not claim new implementation behavior, a reopened architecture decision,
  or any broader public/backend surface.
- Do not edit any other changelog bullet.

### Task 2: Strengthen the row-7 changelog marker set in `test/RepoGuardSpec.hs`

**File**

- `test/RepoGuardSpec.hs`

**Required edits**

- Keep the retry change local to the row-7 closeout guard and its marker
  definitions.
- Strengthen `backendBoundaryCloseoutChangelogMarkers` so
  `assertMarkersPresent "CHANGELOG.md" ...` now requires the same explicit
  preserved-boundary markers added in Task 1:
  `one executable eager backend IR`,
  `no public \`LowerableBackend.IR\``, and
  `no lazy STG`.
- Retain the existing row-7 closeout markers that already proved:
  merged `710c92eb`,
  `all seven backend-boundary mechanism-table rows are now explicitly settled`,
  the dedicated row-7 guard name is cited, and
  the closeout stayed within the no-new-backend-feature boundary.
- Do not weaken the guard to an aggregate grep or indirect proxy check.
- Do not change the focused guard names for rows 1 through 6.
- Do not change `backendBoundaryCloseoutTodoMarkers` or
  `backendBoundaryCloseoutImplementationNoteMarkers`; those surfaces already
  satisfy review.

### Task 3: Refresh round-local attempt-2 evidence only if needed

**File**

- `orchestrator/rounds/round-228/implementation-notes.md`

**Required edits**

- Leave this file untouched unless attempt-2 verification needs a short delta
  note to distinguish the retry from attempt 1.
- If updated, record only the retry-specific delta:
  `CHANGELOG.md` marker repair,
  `test/RepoGuardSpec.hs` marker hardening, and
  the verification results required below.
- Do not restate `TODO.md`, `implementation_notes.md`, or mechanism-table
  edits as newly performed in attempt 2.

## Verification Commands

Run these in order after the retry edits land:

1. Diff hygiene:

```sh
git diff --check
```

2. Focused row-7 guard:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/backend-boundary mechanism table and closeout ledger stay synchronized/"'
```

3. Independent closeout-ledger consistency script from the rejected review:

```sh
python3 - <<'PY'
from pathlib import Path

checks = {
    'TODO.md': [
        '710c92eb',
        'one executable eager backend IR',
        'no public `LowerableBackend.IR`',
        'no lazy STG',
        'No new backend implementation feature',
    ],
    'implementation_notes.md': [
        '710c92eb',
        'rows 1 through 7',
        'one executable eager backend IR',
        'no public `LowerableBackend.IR`',
        'no lazy STG',
        'no new backend implementation feature',
    ],
    'CHANGELOG.md': [
        '710c92eb',
        'all seven backend-boundary mechanism-table rows are now explicitly settled',
        'one executable eager backend IR',
        'no public `LowerableBackend.IR`',
        'no lazy STG',
        'no new backend feature',
    ],
}
failed = False
for path, markers in checks.items():
    src = Path(path).read_text()
    missing = [m for m in markers if m not in src]
    if missing:
        failed = True
        print(f'{path}: MISSING')
        for marker in missing:
            print(f'  - {marker}')
    else:
        print(f'{path}: OK')
if failed:
    raise SystemExit(1)
PY
```

4. Full repo gate required because `test/RepoGuardSpec.hs` is touched:

```sh
cabal build all && cabal test
```

5. Final retry-scope guard:

```sh
python3 - <<'PY'
import hashlib
import json
import subprocess
import sys
from pathlib import Path

baseline = json.loads(Path("/tmp/round-228-attempt2-frozen.json").read_text())
for path, expected in baseline.items():
    current = hashlib.sha256(Path(path).read_bytes()).hexdigest()
    if current != expected:
        print(f'FROZEN_FILE_CHANGED: {path}')
        sys.exit(1)

allowed = {
    "CHANGELOG.md",
    "TODO.md",
    "implementation_notes.md",
    "docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md",
    "test/RepoGuardSpec.hs",
    "orchestrator/rounds/round-228/selection.md",
    "orchestrator/rounds/round-228/plan.md",
    "orchestrator/rounds/round-228/implementation-notes.md",
    "orchestrator/rounds/round-228/review.md",
    "orchestrator/rounds/round-228/reviews/attempt-1.md",
}
controller_owned = {
    "orchestrator/state.json",
    "orchestrator/roadmap.md",
    "orchestrator/verification.md",
    "orchestrator/retry-subloop.md",
}
allowed_prefixes = (
    "orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/",
)

tracked = subprocess.check_output(["git", "diff", "--name-only", "HEAD"], text=True).splitlines()
untracked = subprocess.check_output(
    ["git", "ls-files", "--others", "--exclude-standard"], text=True
).splitlines()
paths = sorted({path for path in tracked + untracked if path})

extra = [
    path for path in paths
    if path not in allowed
    and path not in controller_owned
    and not any(path.startswith(prefix) for prefix in allowed_prefixes)
]
forbidden = [
    path for path in paths
    if path == "mlf2.cabal"
    or path in {
        "AGENTS.md",
        "README.md",
        "Bugs.md",
        "docs/architecture.md",
        "docs/backend-native-pipeline.md",
    }
    or path.startswith("src/")
    or path.startswith("src-public/")
    or path.startswith("app/")
    or (path.startswith("test/") and path != "test/RepoGuardSpec.hs")
]

if forbidden or extra:
    if forbidden:
        print("FORBIDDEN_PATHS:")
        print("\n".join(forbidden))
    if extra:
        print("OUT_OF_SCOPE_PATHS:")
        print("\n".join(extra))
    sys.exit(1)

print("ROUND228_ATTEMPT2_SCOPE_OK")
PY
```

## Success Criteria

- PASS if `CHANGELOG.md` now carries the same preserved-boundary markers that
  already exist in `TODO.md` and `implementation_notes.md`.
- PASS if the row-7 guard in `test/RepoGuardSpec.hs` requires those exact
  changelog markers and therefore fails when any of them are removed.
- PASS if the independent review script reports `TODO.md: OK`,
  `implementation_notes.md: OK`, and `CHANGELOG.md: OK`.
- PASS if `git diff --check`, the focused row-7 guard, `cabal build all && cabal test`,
  and the final retry-scope guard all pass.
- FAIL if attempt 2 reopens the mechanism table, `TODO.md`, the top-level
  `implementation_notes.md`, any backend contract surface, or the immutable
  `reviews/attempt-1.md` snapshot.
