# Orchestrated Execution Event Log Template (JSONL)

Use `orchestrator-log.jsonl` as the single authoritative orchestrator log.
Keep human-readable narrative summaries in `findings.md` and `progress.md`.

```json
{"event_type":"run_header","date_utc":"<YYYY-MM-DD>","goal":"<GOAL>","prompt":"<PROMPT_PATH>","table":"<TABLE_PATH>"}
{"event_type":"gate","round":1,"selected_mechanism":"<first NO mechanism>","attempt":1,"producing_agent":"Reviewer","gate":"NO","reason_if_no":"<blocking finding>","blocker_class":"<BLOCKER_CLASS>","meaningful_diff":"YES","scope_changed":"NO"}
{"event_type":"gate","round":1,"selected_mechanism":"<first NO mechanism>","attempt":1,"producing_agent":"QA","gate":"YES","reason_if_no":"","blocker_class":"NONE","meaningful_diff":"YES","scope_changed":"NO"}
{"event_type":"gate","round":1,"selected_mechanism":"<first NO mechanism>","attempt":1,"producing_agent":"Verifier","gate":"NO","reason_if_no":"<remaining goal gap>","blocker_class":"<BLOCKER_CLASS>","meaningful_diff":"YES","scope_changed":"NO"}
{"event_type":"final_status","final_status":"<COMPLETED|FAILED|MAXIMUMRETRY>"}
```
