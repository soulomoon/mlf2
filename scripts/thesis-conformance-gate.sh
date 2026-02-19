#!/usr/bin/env bash

set -euo pipefail

run_anchor() {
  local label="$1"
  local matcher="$2"
  local min_examples="$3"
  local tmp_log
  local summary_line
  local examples
  local failures

  tmp_log="$(mktemp)"
  trap 'rm -f "$tmp_log"' RETURN

  echo
  echo "==> [thesis-gate] ${label}"
  if ! cabal test mlf2-test --test-show-details=direct --test-options="--match \"${matcher}\"" >"${tmp_log}" 2>&1; then
    cat "${tmp_log}"
    echo "[thesis-gate] FAILED: matcher run failed for '${label}'"
    exit 1
  fi

  cat "${tmp_log}"

  summary_line="$(grep -E '^[0-9]+ examples?, [0-9]+ failures$' "${tmp_log}" | tail -n 1 || true)"
  if [[ -z "${summary_line}" ]]; then
    echo "[thesis-gate] FAILED: could not parse Hspec summary for '${label}'"
    exit 1
  fi

  examples="$(printf '%s\n' "${summary_line}" | sed -E 's/^([0-9]+) examples?, ([0-9]+) failures$/\1/')"
  failures="$(printf '%s\n' "${summary_line}" | sed -E 's/^([0-9]+) examples?, ([0-9]+) failures$/\2/')"

  if [[ -z "${examples}" || -z "${failures}" ]]; then
    echo "[thesis-gate] FAILED: invalid summary parse for '${label}': ${summary_line}"
    exit 1
  fi

  if (( examples < min_examples )); then
    echo "[thesis-gate] FAILED: '${label}' matched ${examples} examples; expected at least ${min_examples}"
    exit 1
  fi

  if (( failures != 0 )); then
    echo "[thesis-gate] FAILED: '${label}' reported ${failures} failures"
    exit 1
  fi

  rm -f "${tmp_log}"
  trap - RETURN
}

main() {
  echo "[thesis-gate] Running thesis conformance anchors"

  ./scripts/check-thesis-obligations-ledger.sh

  run_anchor "Phi/Omega translatability matrix rows" "R-" 15
  run_anchor "A6 parity regressions" "A6 parity" 3
  run_anchor "A6 strict success regression" "BUG-2026-02-17-002" 1
  run_anchor "Phase 3 atomic wrapping equivalence gates" "Phase 3 atomic wrapping equivalence gates" 7
  run_anchor "Representative theorem baseline" "has type forall a. a -> a" 1

  echo
  echo "[thesis-gate] PASS: thesis conformance anchors are green"
}

main "$@"
