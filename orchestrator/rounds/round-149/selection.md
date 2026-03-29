Selected item: item-4
Title: Result-type: Open fallback reconstruction for recursive types
Reason: It is the lowest-numbered pending item with all dependencies satisfied (no dependencies). Item-5 depends on items 1-4 and cannot proceed until item-4 is done.
Summary: Fallback.hs should recognize μ-types in non-local positions and return the actual recursive surface type instead of the quantified fail-closed shell. The keepTargetFinal / candidate selection logic needs a controlled opening for well-formed recursive type structures, while preserving local fallback behavior for non-recursive types.
