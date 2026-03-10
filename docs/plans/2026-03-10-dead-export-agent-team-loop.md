# Dead Export Agent-Team Loop Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Resolve the dead-export sweep shortlist one candidate at a time, removing genuinely dead internal exports and explicitly retaining any later shown to be intentional.

**Architecture:** Run a serial mechanism loop with one active export candidate per round. Each round uses the same pattern: revalidate liveness, add a failing source guard, apply the smallest export-surface patch, run targeted verification plus the full Cabal gate, then sync the loop tracker/docs before advancing.

**Tech Stack:** Haskell, Cabal, Hspec, repo task-tracker markdown artifacts

---

## Mechanism Order

1. `MLF.Elab.Run.ChiQuery`: retire `chiLookupBindParent`, `chiBindParents`
2. `MLF.Binding.Validation`: retire exported `validateSingleGenRoot`
3. `MLF.Constraint.Canonicalizer`: retire `canonicalizeRef` if still dead after revalidation
4. `MLF.Elab.Run.Debug`: retire `edgeOrigins` if still dead after revalidation
5. `MLF.Elab.TermClosure`: retire `closeTermWithSchemeSubst` if still dead after revalidation

## Loop Rules

- One active mechanism at a time; no parallel builders.
- Rows 1-2 are expected removals; rows 3-5 require fresh liveness revalidation immediately before any mutation.
- Every landed row must follow red → green with a new guard plus a full `cabal build all && cabal test` gate.
- `TODO.md`, `CHANGELOG.md`, `implementation_notes.md`, and the loop task files must be updated after each landed round.
- Preserve existing unrelated workspace changes, especially the live `AGENTS.md` edit and prior March 10 cleanup/archive work.
