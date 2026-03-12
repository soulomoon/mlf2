# Progress

## 2026-03-13

- Initialized recovery task tracking.
- Next step: reproduce the failing lock-like file creation and inspect directory metadata.
- Captured owner/mode/flags for `/Volumes/src/mlf4/.git`, `.git/logs`, and `.git/refs/heads`; all appear normal and match the current user.
- Reproduced the lock-like creation failure directly in `.git` with both `mktemp` and Python tempfile/open paths.
- Confirmed a control create/unlink succeeds in `/Volumes/src/mlf4/tasks/todo/2026-03-13-git-metadata-recovery`, isolating the denial to the `.git` subtree rather than the volume or repository generally.
- Cleaned up the temporary control probe and verified no `.codex-probe-*` files remain anywhere under `/Volumes/src/mlf4`.
