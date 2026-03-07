# Task Plan: Narrow `geRes` to Canonical Map

## Metadata
- Date: 2026-03-08
- Execution mode: low-risk thesis-exact cleanup
- Skills in use: using-superpowers, brainstorming, planning-with-files, haskell-pro, verification-before-completion

## Goal
Replace `GeneralizeEnv.geRes :: Solved` with the canonical map it actually uses, remove the local compat builder that only existed to populate that field, and preserve behavior.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Confirm usage shape | completed | `geRes` had one real use and the helper existed only to populate it |
| 2. Narrow env payload | completed | `GeneralizeEnv` now stores a sanitized canonical map directly |
| 3. Remove obsolete helper | completed | Removed `buildSolvedFromPresolutionView` from the planning layer |
| 4. Sync notes | completed | Architecture doc, implementation notes, changelog, and TODO updated |
| 5. Verify | completed | Full gate and focused presolution/generalize slices are green |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## Conclusion
- The planning layer no longer carries a full solved handle where it only needed the canonical map.
- The next cleanup remains relocating the remaining local compatibility builders out of the shared `Solved` surface.
