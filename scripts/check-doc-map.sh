#!/usr/bin/env sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

require_file() {
    if [ ! -f "$ROOT/$1" ]; then
        echo "missing required documentation file: $1" >&2
        exit 1
    fi
}

require_text() {
    file=$1
    text=$2
    if ! grep -Fq "$text" "$ROOT/$file"; then
        echo "missing expected text in $file: $text" >&2
        exit 1
    fi
}

require_file "docs/README.md"
require_file "AGENTS.md"
require_file "tasks/readme"
require_file "README.md"
require_file "docs/adr/2026-06-11-canonical-guidance-boundaries.md"

require_text "AGENTS.md" "## Guidance Ownership Map"
require_text "AGENTS.md" "docs/README.md"
require_text "AGENTS.md" "execution evidence"
require_text "README.md" "Documentation map: \`docs/README.md\`"
require_text "README.md" "Active round-execution control plane: \`orchestrator/state.json\`"
require_text "tasks/readme" "the reader-facing documentation map lives in \`docs/README.md\`"
require_text "docs/README.md" "## Execution Evidence"
require_text "docs/README.md" "they do not override canonical references"
require_text "docs/README.md" "\`docs/plans/\`: historical and accepted plan artifacts."

echo "documentation map checks passed"
