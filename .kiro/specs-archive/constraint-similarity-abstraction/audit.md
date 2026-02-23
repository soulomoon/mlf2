# Audit — constraints up to similarity / abstraction (Thesis Ch. 13)

**Date:** 2026-02-04  
**Spec:** `.kiro/specs/constraint-similarity-abstraction/`

## Summary

This spec stub proposes implementing a similarity/abstraction relation on *constraints* and integrating it into constraint reasoning.

After reviewing `papers/these-finale-english.txt` Chapter 13, that work is **not required for the repo’s current goal** (typing constraints via the gMLF pipeline), and the “abstraction” part would imply a substantially different system than the one implemented here.

## Thesis findings (why this is not currently needed)

### 1) Typing constraints do not need similarity-aware inference

Chapter 13.2 explicitly motivates that *for typing constraints*, it is unnecessary to study inference up to similarity:

- “For typing constraints, this justifies the fact that studying inference up to similarity is not needed. Instead, we can limit ourselves to ⊑ solutions …” (`papers/these-finale-english.txt:14271-14275`)
- Corollary 13.2.5 then characterizes eMLF solutions from the principal gMLF solution (`papers/these-finale-english.txt:14276-14278`).

This repo’s implemented pipeline is the gMLF-style solver/elaboration for typing constraints, so adding constraint-level “≃ up to similarity” comparison/canonicalization does not unlock new typing behavior.

### 2) iMLF “abstraction” does not admit principal presolutions (and is not an inference target)

Chapter 13.3 frames iMLF as an *implicit* system where polymorphism can be “guessed”, and states:

- iMLF “does not permit type inference” and “does not have principal presolutions or solutions” (`papers/these-finale-english.txt:14513-14518`).

So implementing constraint reasoning “up to abstraction” as a drop-in enhancement to the existing principal-presolution solver is not aligned with the thesis: supporting iMLF would require a different story (search/guessing or an alternative interface), which is explicitly beyond this stub’s “don’t replace the solver” scope.

## What to do with this spec

**Recommendation:** treat this spec as **doc-only / deferred** for the current repo.

If we later decide to support iMLF typability (or build proof tooling around Chapter 13), this should be re-scoped into a new spec that:

- states the intended deliverable (e.g., a typability checker/search procedure for iMLF, or proof-oriented constraint transformation tooling),
- makes the non-principal nature of iMLF explicit,
- defines concrete, testable acceptance criteria that match the chosen goal.

## Ralph status

Not recommended to convert to a Ralph implementation task as-is: the current stub does not match what Ch.13 implies is implementable/necessary for the repo’s pipeline.

