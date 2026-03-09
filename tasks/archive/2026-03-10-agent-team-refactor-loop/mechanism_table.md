# Goal Mechanism Table (Current vs Target)

Last updated (UTC): 2026-03-09
Goal: Stabilize and land the current post-split live tree through one-primary-target loops with strict Builder / Boundary Reviewer / QA gating.
Source of truth: user brief for the agent-team loop, live tree under `src/` + `src-public/`, `papers/these-finale-english.txt`, `TODO.md`, `implementation_notes.md`, and `CHANGELOG.md`.

| Mechanism | Current codebase behavior | Target behavior | Gap summary | Evidence (spec/code/tests) | Gate (YES/NO) | Next action |
|---|---|---|---|---|---|---|
| Loop 0 — Baseline freeze | The large split façades and helper seams already existed in the live tree. | The current tree is snapshotted as the stabilization baseline, `cabal build all && cabal test` is green, and docs treat the tree as already split. | Closed by direct live-tree QA. | `cabal build all && cabal test` | YES | Archived. |
| Loop 1 — Warning-free cleanup | The split tree was already warning-free. | `cabal build all` remains warning-free with no extra production cleanup needed. | No source cleanup remained after the live-tree audit. | `cabal build all && cabal test` | YES | Archived. |
| Loop 2 — Façade ownership guards | Existing guards touched the split owners but did not explicitly lock all five façades as thin child-owned boundaries. | Each split façade has a living thin-façade guard. | Closed by adding explicit source guards in `PipelineSpec` and `RepoGuardSpec`. | `test/PipelineSpec.hs`, `test/RepoGuardSpec.hs`, targeted guard slices | YES | Archived. |
| Loop 3 — Public API hard-cut stabilization | The public hard cut was already live, but docs/topology agreement was not source-guarded. | `PublicSurfaceSpec`, `RepoGuardSpec`, README, architecture docs, and call sites all agree on the hard cut. | Closed by adding doc-agreement guards and revalidating the live caller topology. | `test/PublicSurfaceSpec.hs`, `test/RepoGuardSpec.hs`, `README.md`, `docs/architecture.md`, `app/Main.hs` | YES | Archived. |
| Loop 4 — Cabal and module-graph hygiene | The Cabal graph was already normalized for the split modules, but not explicitly guarded. | Split child modules remain implementation-only Cabal entries. | Closed by adding a `RepoGuardSpec` guard on child-module Cabal ownership. | `mlf2.cabal`, `test/RepoGuardSpec.hs` | YES | Archived. |
| Loop 5 — Split-specific regression sweep | The owner regression slices already existed. | Each split owner passes its owning slices in order and the full gate stays green. | Closed by running the ordered owner sweep through the built `mlf2-test` binary, then rerunning the full gate. | owner-sweep commands logged in `progress.md`, `cabal build all && cabal test` | YES | Archived. |
| Loop 6 — Final landing | The task packet and status docs existed but had not been closed out. | Final docs/task archive reflect the landed stabilization state. | Closed by syncing task/docs and archiving the run folder after a green full gate. | `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, archived task folder | YES | Archived. |

Rules:
- Keep mechanism order fixed.
- Use only `YES` or `NO`.
- Flip `NO -> YES` only with passing verification evidence.
