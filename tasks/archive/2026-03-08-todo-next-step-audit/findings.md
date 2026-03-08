# Findings

- `Task 58` queue item 3 is still live: `schemeBodyTarget` is defined separately in `src/MLF/Elab/Elaborate.hs` and `src/MLF/Elab/Run/Scope.hs`, and the `Run.Scope` version carries additional scheme-root/body semantics.
- `Task 58` queue item 4 is stale: `freshNameLike` already lives in `src/MLF/Util/Names.hs`, `mapBoundType` already lives in `src/MLF/Elab/Types.hs`, and regression guards assert the duplicate local helpers are gone.
- `Task 52` appears effectively satisfied by newer work: the direct frontend guard tests requested there now exist, and the bounded recursion-schemes follow-up landed in `Task 57`.
- `Task 46` is only partially stale: the fallback ladders and `reifyInst` recovery paths are guarded as removed, but `src/MLF/Elab/Elaborate.hs` still contains the `Left _ -> typeRef root` scope-root fallback called out in the original task.
- `Task 36` is stale: the compatibility names it targeted (`GeneralizeAtWithCompat`, `rtcSolvedCompat`, `eeSolvedCompat`) are absent from production code and covered by later closeout tasks/guards.

## Recommended next step

1. If we follow the current live TODO queue, do `Task 58` item 3 next: semantics-checked consolidation of `schemeBodyTarget`.
2. After that, either close or rewrite the stale TODO entries (`Task 52`, `Task 36`, `Task 58` item 4), and narrow `Task 46` to the remaining scope-root fallback if we still want that strictness cleanup.
