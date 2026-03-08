#!/usr/bin/env bash
set -euo pipefail
LOG_FILE="$1"
ROUND="$2"
PHASE="$3"
AGENT="$4"
IDEA_TITLE="$5"
ATTEMPT="$6"
GATE="$7"
REASON="$8"
FILES_TOUCHED_JSON="$9"
COMMANDS_RUN_JSON="${10}"
COMMIT_HASH="${11}"
jq -nc \
  --argjson round "$ROUND" \
  --arg phase "$PHASE" \
  --arg agent "$AGENT" \
  --arg idea_title "$IDEA_TITLE" \
  --argjson attempt "$ATTEMPT" \
  --arg gate "$GATE" \
  --arg reason "$REASON" \
  --argjson files_touched "$FILES_TOUCHED_JSON" \
  --argjson commands_run "$COMMANDS_RUN_JSON" \
  --arg commit_hash "$COMMIT_HASH" \
  '{round:$round,phase:$phase,agent:$agent,idea_title:$idea_title,attempt:$attempt,gate:$gate,reason:$reason,files_touched:$files_touched,commands_run:$commands_run,commit_hash:$commit_hash}' >> "$LOG_FILE"
