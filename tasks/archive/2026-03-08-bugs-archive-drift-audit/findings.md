# Findings — Bugs + Archive Drift Audit

## 2026-03-08 scan result

- `Bugs.md` has no current task-folder path drift.
- Live docs contained several broken task-folder references; the highest-value ones were patched to either valid surviving plan files, valid archive paths, or explicit historical notes.
- Newly archived task folders also had a few stale self-references to their old `tasks/todo/` locations; these were updated.

- Performed a second-pass cleanup on older implementation-plan docs: direct archive replacements where the archive exists, and explicit historical-note wording where the original task folder was never retained.

## 2026-03-08 final state

- `Bugs.md` is clean with respect to task-folder references.
- The remaining live-doc/task-reference drift identified in this pass was repaired.
- Intentional templates/examples may still mention `tasks/todo/...` patterns, but the live docs audited here no longer contain broken concrete task-folder refs.
