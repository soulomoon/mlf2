# Findings

## Current Observations

- Passing bounded-alias (b ⩾ a, two-arg) trace still produces an Ω witness with just two `OpGraft` steps followed by a single `OpRaise` (see progress log lines 62-67) and replays to `InstInside (InstBot TBottom)` once Φ sees those steps.
- BUG-003 (triple/dual alias chain) edge-0 trace records five Ω steps, including two additional `OpRaise`s targeting alias binders 0 and 12 plus a canonical copy map that collapses binder 1 into 23 (progress log lines 17-23, 62-70). Those extra ops survive normalization because the failing path keeps their targets inside I(r), and Φ reruns into `InstId`.
