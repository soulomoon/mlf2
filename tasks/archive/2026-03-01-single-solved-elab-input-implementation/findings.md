# Findings

## 2026-03-01
- Implementation plan mandates `executing-plans` workflow and task-by-task commits.
- Migration target is single-solved input (`eeSolved`, `rtcSolved`) while preserving checked-authoritative behavior.
- Explicit anti-scope: no phi replay bridge plan modifications.
- Regression root cause during Task 4: single-solved pipeline wiring initially chose `solvedClean`, which changed elaboration/generalization behavior on bounded/coercion paths.
- Stabilizing choice: use `solvedForGen` as the authoritative single solved snapshot for pipeline elaboration/result-type contexts while retaining single-handle API/records.
- Final migration guard (`rg -n "eeResPhi|eeResReify|eeResGen|rtcSolvedForGen|rtcSolvedClean" src test`) reports no matches.
