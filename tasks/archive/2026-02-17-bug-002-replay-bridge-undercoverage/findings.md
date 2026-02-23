# Findings: BUG-002 Replay-Bridge Undercoverage

## Key discoveries
- Pending investigation.

## 2026-02-17 investigation findings
- Deterministic repro is stable for BUG-002 matrix (`--match "BUG-002-V" --seed 1593170056`): 5 examples, 4 failures in this workspace.
- Active failure buckets:
  - `BUG-002-V1`: `PhiTranslatabilityError` (`OpGraft+OpWeaken: binder not found in quantifier spine`, target `NodeId 1`).
  - `BUG-002-V2`: `PhiInvariantError trace/replay binder key-space mismatch` on `OpRaise` raw key `5` (`trace sources=[0,1,3,5]`, replay-map domain `[0,1,3]`).
  - `BUG-002-V3`: same mismatch class (`trace sources=[20,22]`, replay-map domain `[20]`).
  - `PipelineSpec BUG-002-V4` Phase 1-5 guard fails (`OpRaise` target not kept inside `etInterior`).
- Edge-0 V2 presolution evidence (`prEdgeTraces`):
  - `etBinderArgs=[(0,29),(1,30),(3,31),(5,32)]`
  - `etBinderReplayHints={0->33,1->34,3->35,5->36}`
  - `etCopyMap={0->33,1->34,2->37,3->35,4->38,5->36}`
- Φ bridge evidence for V2 with trace enabled:
  - replay scheme subst at entry: `{0->a,3->b,33->a,35->b}`
  - source subst after remap/hydrate: `{0->a,1->b}`
  - bridge result: `traceBinderReplayMap={(0,33),(1,35),(3,35)}` (source key `5` dropped)
  - inferred cause: candidate generation for source key `5` is empty in current bridge logic despite source/hint presence.
- Additional key-space evidence (V2 solved constraint): canonical lookup for keys `0/1/3` is missing (`NodeAccess.lookupNode ... = Nothing`), while `33/34/35/36` are live TyVars. This interacts with bridge filtering and positional seeding.
- Pattern-level diagnosis:
  - `siReplay` and trace metadata currently carry mixed identities (source IDs + replay/copy IDs).
  - bridge construction in `computeTraceBinderReplayBridge` assumes a stable replay key-space and can silently under-cover source binders when candidate pools shrink.
  - Ω fail-fast checks then surface the mismatch at `resolveTraceBinderTarget`.

## 2026-02-17 follow-up findings (post-implementation)
- V2 non-binder regression root cause was confirmed with trace:
  - edge-0 bridge candidates for source key `1` were `[34,35]` and map selected `34`, causing `OpGraft+OpWeaken targets non-binder node`.
- Constraining bridge candidates to replay-binder domain (derived from `siReplay` TyVar keys) removes this class of failures.
- V4 interior guard failure was tied to restored `OpRaise` targets surviving outside source `etInterior`; dropping non-interior raises during restore resolves the guard.
- After contract fixes, BUG-002 slice reduced to one residual failure:
  - `BUG-002-V2` fails with `alias bounds survived scheme finalization` despite replay map + Ω binder lookup being normalized.
- Residual V2 behavior evidence:
  - edge-0 φ now resolves replay keys deterministically and no longer trips non-binder translation checks.
  - failure surface has moved fully into scheme finalization alias-bound cleanup.

## 2026-02-17 completion findings
- Distinguished error source for the residual V2 failure:
  - the `alias bounds survived scheme finalization` error was emitted from `ReifyPlan.bindingFor` (not from `Finalize`).
- Concrete failing shape before final fix:
  - binder reification produced self/near-self bounds (for example `boundTy0 = TVar "a"` for binder `"a"`, and structured self references like `b -> a` where binder is `b`), which later either tripped alias-bound validation or `TCTypeAbsBoundMentionsVar`.
- Effective correction:
  - normalize binder self-references inside reified bounds to `⊥` before extra-quantifier closure and bound admission checks.
  - this preserves structural information needed for BUG-002 V2 (`⊥ -> a` retained) while eliminating illegal self reference.
- Post-fix deterministic results:
  - `BUG-002-V` seed `1593170056`: 5/5 pass.
  - `generalizeAt rejects alias bounds`: still passes.
- Additional observation:
  - cross-linked `BUG-004-V2` with same seed remains failing in this workspace, but in a different class (`TCArgumentMismatch`) than original replay-key mismatch.
