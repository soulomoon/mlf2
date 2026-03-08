# Progress Log

## 2026-03-08
- Started the audit for remaining fallback mechanisms after the solved-facade cleanup loop reached table exhaustion.
- Completed the fallback-family classification: strong removal candidates are the GA/no-GA/reify fallback ladders and the `reifyInst` fallback paths; `checkNoGenFallback` and `NoFallback` reify entry points are not removal targets.
