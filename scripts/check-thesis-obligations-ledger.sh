#!/usr/bin/env bash

set -euo pipefail

ROOT="/Volumes/src/mlf4"
LEDGER="${ROOT}/docs/thesis-obligations.yaml"
RENDER="${ROOT}/scripts/render-thesis-obligations-ledger.rb"

if [[ ! -f "${LEDGER}" ]]; then
  echo "[thesis-obligations] FAILED: missing ledger file: ${LEDGER}"
  exit 1
fi

if [[ ! -x "${RENDER}" ]]; then
  echo "[thesis-obligations] FAILED: missing executable renderer: ${RENDER}"
  exit 1
fi

echo "[thesis-obligations] Checking generated markdown drift"
"${RENDER}" --check

tmp_rows="$(mktemp)"
trap 'rm -f "${tmp_rows}"' EXIT

echo "[thesis-obligations] Validating ledger schema, ID set, and anchors"
ruby - "${LEDGER}" >"${tmp_rows}" <<'RUBY'
require 'yaml'

ledger_path = ARGV.fetch(0)
doc = YAML.load_file(ledger_path)

expected_ids = []
# Chapter 4: Binding Trees & Graph Operations
expected_ids.concat(%w[BIND-FLEX-CHILDREN BIND-INTERIOR BIND-ORDER OP-WEAKEN OP-RAISE-STEP OP-RAISE-TO].map { |s| "O04-#{s}" })
# Chapter 5: Inert Node Classification
expected_ids.concat(%w[INERT-NODES INERT-LOCKED WEAKEN-INERT].map { |s| "O05-#{s}" })
# Chapter 7: Unification
expected_ids.concat(%w[UNIF-CORE UNIF-PRESOL REBIND].map { |s| "O07-#{s}" })
# Chapter 8: Reification
expected_ids.concat(%w[REIFY-TYPE REIFY-NAMES].map { |s| "O08-#{s}" })
# Chapter 9: Constraint Generation
expected_ids.concat(%w[CGEN-ROOT CGEN-EXPR].map { |s| "O09-#{s}" })
# Chapter 10: Presolutions & Expansion
expected_ids.concat(%w[EXP-DECIDE EXP-APPLY PROP-SOLVE PROP-WITNESS COPY-SCHEME].map { |s| "O10-#{s}" })
# Chapter 11: Local Transformations
expected_ids.concat(%w[UNIFY-STRUCT WITNESS-NORM WITNESS-COALESCE WITNESS-REORDER].map { |s| "O11-#{s}" })
# Chapter 12: Global Algorithm
expected_ids.concat(%w[SOLVE-UNIFY ACYCLIC-CHECK ACYCLIC-TOPO COPY-INST NORM-GRAFT NORM-MERGE NORM-DROP NORM-FIXPOINT SOLVE-VAR-BASE SOLVE-VAR-VAR SOLVE-HARMONIZE SOLVE-ARROW SOLVE-VALIDATE].map { |s| "O12-#{s}" })
# Chapter 14
expected_ids.concat(%w[EMPTY TVAR VAR].map { |s| "O14-WF-#{s}" })
expected_ids.concat(%w[REFLEX TRANS BOT HYP INNER OUTER QUANT-ELIM QUANT-INTRO].map { |s| "O14-INST-#{s}" })
expected_ids.concat(%w[VAR ABS APP TABS TAPP LET].map { |s| "O14-T-#{s}" })
expected_ids.concat(%w[BETA BETALET REFLEX TRANS QUANT-INTRO QUANT-ELIM INNER OUTER CONTEXT].map { |s| "O14-RED-#{s}" })
expected_ids.concat(%w[N O SEQ INNER OUTER HYP BOT ID].map { |s| "O14-APPLY-#{s}" })
# Chapter 15
expected_ids.concat(%w[NO-INERT-LOCKED SCHEME-ROOT-RIGID ARROW-RIGID NON-INTERIOR-RIGID].map { |s| "O15-TRANS-#{s}" })
expected_ids.concat(%w[REQUIRED IDENTITY].map { |s| "O15-REORDER-#{s}" })
expected_ids.concat(%w[FIND REJECT].map { |s| "O15-CONTEXT-#{s}" })
expected_ids.concat(
  %w[
    SEQ-EMPTY SEQ-CONS RIGID-RAISE RIGID-MERGE RIGID-RAISEMERGE ROOT-GRAFT ROOT-RAISEMERGE
    ROOT-WEAKEN NODE-GRAFT NODE-MERGE NODE-RAISEMERGE NODE-WEAKEN NODE-RAISE
  ].map { |s| "O15-TR-#{s}" }
)
expected_ids << 'O15-EDGE-TRANSLATION'
expected_ids.concat(%w[LAMBDA-VAR LET-VAR ABS APP LET].map { |s| "O15-ELAB-#{s}" })

def fail_grouped!(groups)
  warn "[thesis-obligations] FAILED: ledger validation errors"
  groups.each do |title, entries|
    next if entries.empty?
    warn "- #{title}:"
    entries.each { |e| warn "  - #{e}" }
  end
  exit 1
end

unless doc.is_a?(Hash) && doc['obligations'].is_a?(Array)
  fail_grouped!({ 'schema' => ['root must be a mapping with `obligations` array'] })
end

obligations = doc['obligations']
ids = obligations.map { |o| o['id'] }
id_counts = Hash.new(0)
ids.each { |id| id_counts[id] += 1 }

groups = {
  'count' => [],
  'missing-ids' => [],
  'extra-ids' => [],
  'duplicate-ids' => [],
  'unmapped-rules' => [],
  'status' => [],
  'missing-files' => [],
  'missing-symbols' => [],
  'invalid-code-anchors' => []
}

if obligations.size != 99
  groups['count'] << "expected 99 obligations, got #{obligations.size}"
end

missing = expected_ids - ids
extra = ids - expected_ids
dupes = id_counts.select { |_k, v| v > 1 }.keys
groups['missing-ids'].concat(missing)
groups['extra-ids'].concat(extra)
groups['duplicate-ids'].concat(dupes)

obligations.each do |o|
  id = o['id']
  ta = o['test_anchor']
  matcher = ta.is_a?(Hash) ? ta['matcher'].to_s.strip : ''
  test_file = ta.is_a?(Hash) ? ta['file'].to_s.strip : ''
  status = o['status'].to_s.strip

  if matcher.empty?
    groups['unmapped-rules'] << "#{id}: blank test matcher"
  end
  if test_file.empty?
    groups['unmapped-rules'] << "#{id}: blank test file"
  elsif !File.exist?(test_file)
    groups['missing-files'] << "#{id}: test file not found: #{test_file}"
  end
  if status != 'anchored'
    groups['status'] << "#{id}: expected status=anchored, got #{status.inspect}"
  end

  anchors = o['code_anchors']
  if !anchors.is_a?(Array) || anchors.empty?
    groups['unmapped-rules'] << "#{id}: empty code_anchors"
  else
    anchors.each do |anchor|
      unless anchor.is_a?(String) && anchor.include?('#')
        groups['invalid-code-anchors'] << "#{id}: invalid code anchor #{anchor.inspect}"
        next
      end
      path, symbol = anchor.split('#', 2)
      if path.to_s.empty? || symbol.to_s.empty?
        groups['invalid-code-anchors'] << "#{id}: invalid code anchor #{anchor.inspect}"
        next
      end
      unless File.exist?(path)
        groups['missing-files'] << "#{id}: code file not found: #{path}"
        next
      end
      contents = File.read(path)
      unless contents.include?(symbol)
        groups['missing-symbols'] << "#{id}: symbol #{symbol.inspect} not found in #{path}"
      end
    end
  end
end

if groups.values.any? { |entries| !entries.empty? }
  fail_grouped!(groups)
end

obligations.each do |o|
  puts [o['id'], o.dig('test_anchor', 'matcher'), o.dig('test_anchor', 'file')].join("\t")
end
RUBY

echo "[thesis-obligations] Executing anchor matchers"
command_failures=()
parse_failures=()
zero_examples=()
test_failures=()

while IFS=$'\t' read -r id matcher _file; do
  [[ -n "${id}" ]] || continue
  log_file="$(mktemp)"
  trap 'rm -f "${tmp_rows}" "${log_file}"' EXIT

  echo
  echo "==> [thesis-obligations] ${id} (--match \"${matcher}\")"
  if ! cabal test mlf2-test --test-show-details=direct --test-options="--match \"${matcher}\"" >"${log_file}" 2>&1; then
    cat "${log_file}"
    command_failures+=("${id}")
    rm -f "${log_file}"
    continue
  fi

  cat "${log_file}"

  summary_line="$(grep -E '^[0-9]+ examples?, [0-9]+ failures$' "${log_file}" | tail -n 1 || true)"
  if [[ -z "${summary_line}" ]]; then
    parse_failures+=("${id}")
    rm -f "${log_file}"
    continue
  fi

  examples="$(printf '%s\n' "${summary_line}" | sed -E 's/^([0-9]+) examples?, ([0-9]+) failures$/\1/')"
  failures="$(printf '%s\n' "${summary_line}" | sed -E 's/^([0-9]+) examples?, ([0-9]+) failures$/\2/')"

  if [[ -z "${examples}" || -z "${failures}" ]]; then
    parse_failures+=("${id}")
    rm -f "${log_file}"
    continue
  fi

  if (( examples < 1 )); then
    zero_examples+=("${id}")
  fi
  if (( failures != 0 )); then
    test_failures+=("${id}")
  fi

  rm -f "${log_file}"
done <"${tmp_rows}"

if (( ${#command_failures[@]} > 0 || ${#parse_failures[@]} > 0 || ${#zero_examples[@]} > 0 || ${#test_failures[@]} > 0 )); then
  echo
  echo "[thesis-obligations] FAILED: executable anchor checks"
  if (( ${#command_failures[@]} > 0 )); then
    echo "- matcher command failures:"
    printf '  - %s\n' "${command_failures[@]}"
  fi
  if (( ${#parse_failures[@]} > 0 )); then
    echo "- unparseable hspec summary:"
    printf '  - %s\n' "${parse_failures[@]}"
  fi
  if (( ${#zero_examples[@]} > 0 )); then
    echo "- zero matched examples:"
    printf '  - %s\n' "${zero_examples[@]}"
  fi
  if (( ${#test_failures[@]} > 0 )); then
    echo "- matcher failures:"
    printf '  - %s\n' "${test_failures[@]}"
  fi
  exit 1
fi

echo
echo "[thesis-obligations] PASS: all obligations are mapped and green"
