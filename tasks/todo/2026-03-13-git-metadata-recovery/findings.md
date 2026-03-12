# Findings

## 2026-03-13

- Task started to diagnose `.git` metadata write denial under `/Volumes/src/mlf4`.
- `/Volumes/src/mlf4/.git`, `/Volumes/src/mlf4/.git/logs`, and `/Volumes/src/mlf4/.git/refs/heads` are owned by `ares:staff`, mode `drwxr-xr-x`, with no filesystem flags and no ACL marker in `ls -ldeO@` output.
- The enclosing volume `/Volumes/src` is writable in general; a control probe in `/Volumes/src/mlf4/tasks/todo/2026-03-13-git-metadata-recovery` succeeded and cleaned up.
- Controlled Python probes consistently fail only in the `.git` subtree with `PermissionError: [Errno 1] Operation not permitted`, while succeeding in the non-`.git` control directory on the same repo volume.
- No safe local remediation is justified from the observed metadata, because there is no clear mutable filesystem misconfiguration to correct. The evidence is most consistent with a `.git`-specific runtime/sandbox/policy denial rather than ownership, ACL, immutable flags, or read-only volume state.
