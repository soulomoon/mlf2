#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
OUTPUT="${REPO_ROOT}/test/golden/legacy-replay-baseline-v1.json"
GENERATED_ON="$(date -u +%F)"
SOURCE_COMMIT="$(git -C "${REPO_ROOT}" rev-parse HEAD)"

TMP1="$(mktemp)"
TMP2="$(mktemp)"
cleanup() {
    rm -f "${TMP1}" "${TMP2}"
}
trap cleanup EXIT

mkdir -p "$(dirname "${OUTPUT}")"

run_gen() {
    local target="$1"
    cabal run frozen-parity-gen -- \
        --output "${target}" \
        --generated-on "${GENERATED_ON}" \
        --source-commit "${SOURCE_COMMIT}"
}

run_gen "${TMP1}"
run_gen "${TMP2}"

if ! cmp -s "${TMP1}" "${TMP2}"; then
    echo "ERROR: Frozen baseline generation is not deterministic." >&2
    diff -u "${TMP1}" "${TMP2}" || true
    exit 1
fi

mv "${TMP1}" "${OUTPUT}"
cp "${OUTPUT}" "${TMP2}"

echo "Updated ${OUTPUT}"
echo "generated_on=${GENERATED_ON}"
echo "source_commit=${SOURCE_COMMIT}"
