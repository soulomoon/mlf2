# Round 018 Implementation Notes (`P3`)

- Applied a same-round repair for the rejected `P3` attempt by removing all direct `P1` token consumption from `Prototype/P3.hs`.
- `P3` now derives stage-input authority only from:
  - `orchestrator/rounds/round-017/review-record.json` (`P2` authoritative attempt `2`, `semantic-negative`);
  - the observed absence of `orchestrator/rounds/round-017/evidence/P2/attempt-2/subject-token.json`.
- Added an explicit guard: if a `P2 -> P3` handoff token exists while authoritative `P2` is `semantic-negative`, `P3` fails with a typed malformed-input error instead of consuming fallback identity.
- Updated `P3` evidence emission to keep `subject_id: null` when no authoritative `P2` handoff token exists (trace bundle and all `check-P3-*` outputs).
- Updated `P3` artifact rendering to remove inherited `P1` token input claims and describe no-token continuity from authoritative `P2` as the bounded stop precondition.
- Preserved rejected `attempt-1` evidence unchanged; regenerated only `attempt-2`.
- Updated tests to lock the repaired contract:
  - `P3` succeeds from authoritative `P2` no-token continuity without seeding a `P1` token fixture;
  - `P3` outputs use `subject_id: null` under no-token continuity;
  - non-pass still emits no `subject-token.json`.

## Verification Run

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 P3 prototype entrypoint"'`
- `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P3-safety-validation --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
- `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P3-safety-validation --scenario-id wrong-scenario --attempt-id 2`
- `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector wrong-stage --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
- `cabal run mlf2`
- `rg -n 'p1AuthoritativeSubjectTokenRelativePath|round-016/evidence/P1/attempt-2/subject-token.json' src/MLF/Research/URI/R2/C1/Prototype/P3.hs src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs` (no matches)
- `find orchestrator/rounds/round-018/evidence/P3/attempt-1 -maxdepth 1 -type f | sort`
- `find orchestrator/rounds/round-018/evidence/P3/attempt-2 -maxdepth 1 -type f | sort`
- `shasum -a 256 orchestrator/rounds/round-018/evidence/P3/attempt-1/*` before/after rerun (identical)
- `shasum -a 256 orchestrator/rounds/round-018/evidence/P3/attempt-2/*` before/after wrong-scenario and wrong-stage checks (identical)
- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-018/state-snapshot.json >/dev/null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-15-00-uri-r2-c1-prototype-evidence-successor-roadmap/rev-003/roadmap.md`
- `python3 -m json.tool` over all `round-018/evidence/P3/attempt-2/*.json`
- `test ! -f orchestrator/rounds/round-018/evidence/P3/attempt-2/subject-token.json`
- `cabal build all && cabal test`

## Outcome

- `attempt-1` is preserved as rejected history and remained byte-stable during this repair.
- `attempt-2` now records the repaired `P3` contract with no direct `P1` token handoff.
- `P3` stage result remains bounded `semantic-negative` with explicit `blocking-stop-condition`.
- No default-path behavior changes and no second executable interface were introduced.
