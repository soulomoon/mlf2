# Round 154 Merge — Item 4: Update pipeline test description and survey ElaborationSpec

## Squash-Commit Title

Reclassify non-local proxy pipeline test as TCArgumentMismatch and survey ElaborationSpec

## Summary

Update PipelineSpec:2336 test description from "hits elaboration blocker" to
"elaboration succeeds but produces TCArgumentMismatch" — reflecting that
rounds 152+153 resolved the PhiTranslatabilityError but revealed a deeper
type-check error in the elaborated term.

Survey all 13 ElaborationSpec PhiTranslatabilityError assertion sites:
all are legitimate untranslatable cases, none match the non-local proxy
pattern.

1 file changed (test description only). 1176 examples, 0 failures.

## Follow-Up Notes

- The pipeline still returns Left for the non-local proxy expression, now
  with TCArgumentMismatch instead of PhiTranslatabilityError
- Full upgrade to success requires fixing the elaboration output (missing
  fold/unfold coercions for μ-types) — a deeper issue for future work
- Item-4 is partially complete: survey and reclassification done, but
  test cannot yet assert success
