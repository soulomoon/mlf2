#!/usr/bin/env bash
set -euo pipefail
LOG_FILE="$(cd "$(dirname "$0")" && pwd)/orchestrator-log.jsonl"
round="${1:-0}"
phase="${2:-}"
agent="${3:-}"
idea_title="${4:-}"
attempt="${5:-0}"
gate="${6:-}"
reason="${7:-}"
files_touched="${8:-[]}"
commands_run="${9:-[]}"
commit_hash="${10:-}"
if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required" >&2
  exit 1
fi
jq -nc \
  --argjson round "$round" \
  --arg phase "$phase" \
  --arg agent "$agent" \
  --arg idea_title "$idea_title" \
  --argjson attempt "$attempt" \
  --arg gate "$gate" \
  --arg reason "$reason" \
  --argjson files_touched "$files_touched" \
  --argjson commands_run "$commands_run" \
  --arg commit_hash "$commit_hash" \
  '{round:$round,phase:$phase,agent:$agent,idea_title:$idea_title,attempt:$attempt,gate:$gate,reason:$reason,files_touched:$files_touched,commands_run:$commands_run,commit_hash:$commit_hash}' \
  >> "$LOG_FILE"
