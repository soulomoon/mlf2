# Progress

- 2026-03-10: Initialized the repo-wide dead-export sweep task.
- 2026-03-10: Ran an automated first pass over `src/` export lists to find lowercase exported identifiers with no external source hits.
- 2026-03-10: Manually validated the strongest candidates against live imports/module intent, separating high-confidence stale exports from likely intentional API surfaces.
- 2026-03-10: Chose `MLF.Elab.Run.ChiQuery` (`chiLookupBindParent`, `chiBindParents`) as the best next bounded cleanup from this sweep.
