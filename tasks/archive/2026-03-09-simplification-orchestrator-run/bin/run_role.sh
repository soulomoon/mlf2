#!/usr/bin/env bash
set -euo pipefail
ROLE="$1"
PROMPT_FILE="$2"
SCHEMA_FILE="$3"
OUT_FILE="$4"
WORKDIR="${5:-/Volumes/src/mlf4}"
REASONING="${CODEX_REASONING:-low}"
TMP_JSON="${OUT_FILE}.jsonl"
rm -f "$OUT_FILE" "$TMP_JSON"
codex exec -c model_reasoning_effort="\"${REASONING}\"" --dangerously-bypass-approvals-and-sandbox --cd "$WORKDIR" --skip-git-repo-check --output-schema "$SCHEMA_FILE" -o "$OUT_FILE" --color never - < "$PROMPT_FILE" > "$TMP_JSON"
