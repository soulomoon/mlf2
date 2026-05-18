### Source Round
- Round id: `round-258`
- Trigger: `merged-round`
- Merged commit: `dcb12bdc611e52fe27d5187b795b6bf4cb583641`
- Evidence:
  - `orchestrator/rounds/round-258/review-record.json` approved the docs-only readiness audit for `milestone-1` / `direction-1a-evidence-ledger-alignment`.
  - `review-record.json` set `roadmap_closeout.mode` to `semantic-update-required` because the operator requested the roadmap to use the TDD skill for each implementation.
  - `orchestrator/rounds/round-258/review.md` records passing `git diff --check`, roadmap lineage validation, ADR/self-hosting guardrail scans, seed evidence scans, conformance-absence check, manual claim audit, and `./scripts/thesis-conformance-gate.sh`.
  - `orchestrator/rounds/round-258/implementation-notes.md` records the merged docs-only changes to `README.md` and `docs/mlfp-language-reference.md`; no production code, tests, roadmap files, `orchestrator/state.json`, or `test/conformance/mlfp/` were edited in round-258.

### Roadmap Change
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Prior revision: `rev-001`
- Proposed revision: `rev-002`
- Files changed:
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-002/roadmap.md`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-002/roadmap-view.json`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-002/verification.md`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/roadmap-history.md`
  - `orchestrator/roadmap-updates/round-258-roadmap-update.md`

### Rationale

This update changes future coordination and verification meaning, so it cannot
be handled as status-only closeout. Rev-002 keeps the existing Full Self-Boot
staged order and milestone meanings, but adds a roadmap-wide implementation
discipline: behavior-changing implementation rounds must use the `tdd` skill
and advance through vertical RED -> GREEN -> refactor cycles.

The rule is intentionally narrow. It applies to behavior-changing
implementation rounds where the implementer changes compiler, package,
runtime, backend/native, platform, driver, proof, or conformance behavior. It
does not apply to pure docs-only rounds, control-plane-only rounds, review-only
work, semantic roadmap-update authoring, or status-only closeout. Those rounds
still keep their normal diff hygiene, artifact, lineage, and claim-audit
checks.

Rev-002 also carries forward the merged round-258 result: milestone 1 is marked
done with a docs-only completion pointer, and `roadmap-history.md` records the
readiness audit without claiming behavior implementation or self-hosting.

### State Activation
- Requires state.json roadmap metadata update: yes
- New roadmap_dir when applicable: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-002`
