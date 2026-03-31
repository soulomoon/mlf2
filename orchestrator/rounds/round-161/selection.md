# Selection — Round 161

## Selected Item

**item-2**: Fix BUG-2026-03-16-001 (InstBot replay mismatch)

## Rationale

Item-2 is the lowest-numbered unfinished item in the roadmap. It has no
dependencies and is parallel-safe (tier-1). The bug fix targets
`MLF.Elab.Inst.applyInstantiation` where the `InstBot` branch rejects the
non-bottom shape carried by the bounded no-fallback replay path. This is a
known defect tracked in `Bugs.md`.

Item-1 was just completed in round-160 (merged as f7bdf40). Item-3 depends on
item-1 and is now unblocked, but item-2 takes priority as the lower-numbered
unfinished item per guider policy.

## Roadmap Identity

- Roadmap: `2026-03-30-01-codebase-quality-and-coverage-improvements`
- Revision: `rev-001`
- Item: `item-2`
- Round: `round-161`
