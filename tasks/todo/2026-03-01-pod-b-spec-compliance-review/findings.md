# Findings

## Key Discoveries
- No runtime callsites of `stripForNonReplay` remain in `src/` or `test/`.
- Non-replay pruning semantics now live inside `normalizeEdgeWitnessesM` (`WitnessNorm`) and no longer route through a compatibility helper.
- Replay-map fail-fast guards remain active at both producer-side normalization (`validateNormalizedWitness`) and consumer-side Φ/Ω translation.
- Targeted Hspec evidence for changed semantics and fail-fast behavior is present and passed.
