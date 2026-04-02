## Round 181 implementation notes

- Removed the packet-local `preserveC1AuthoritativeRecursiveAlias` /
  `isBlockedC1AliasScheme` shortcut from `src/MLF/Elab/Run/Pipeline.hs`.
- Replayed the bounded `C1` packet after removing that shortcut and confirmed
  the authoritative entrypoints stay recursive anyway, while the fallback
  surface stays on the non-recursive `baseTarget -> baseC` read.
- The surviving recursive authoritative result is carried by the existing
  authoritative root generalization path for this packet, so this round keeps
  `C1` recursive on `runPipelineElab` / `runPipelineElabChecked` and records
  the shortcut removal as the bounded item-5 result.
- Added a focused `PipelineSpec` guard that fails if the deleted packet-local
  shortcut returns to `Run/Pipeline`.
