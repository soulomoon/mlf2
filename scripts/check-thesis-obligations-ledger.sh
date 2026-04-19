#!/usr/bin/env bash

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
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
ruby - "${LEDGER}" "${ROOT}" >"${tmp_rows}" <<'RUBY'
require 'date'
require 'yaml'

ledger_path = ARGV.fetch(0)
doc = YAML.safe_load(
  File.read(ledger_path),
  permitted_classes: [Date],
  aliases: true,
  filename: ledger_path
)
root = ARGV.fetch(1)

expected_ids = []
# Chapter 4: Binding Trees & Graph Operations
expected_ids.concat(%w[BIND-FLEX-CHILDREN BIND-INTERIOR BIND-ORDER OP-WEAKEN OP-RAISE-STEP OP-RAISE-TO].map { |s| "O04-#{s}" })
# Chapter 5: Inert Node Classification
expected_ids.concat(%w[INERT-NODES INERT-LOCKED WEAKEN-INERT].map { |s| "O05-#{s}" })
# Chapter 7: Unification
expected_ids.concat(%w[UNIF-CORE UNIF-PRESOL REBIND GENUNIF].map { |s| "O07-#{s}" })
# Chapter 8: Reification
expected_ids.concat(%w[REIFY-TYPE REIFY-NAMES BIND-MONO SYN-TO-GRAPH REIFY-INLINE INLINE-PRED].map { |s| "O08-#{s}" })
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
expected_ids.concat(%w[LAMBDA LET WF].map { |s| "O15-ENV-#{s}" })
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
  'matcher-id-mismatch' => [],
  'evidence-kind' => [],
  'min-success' => [],
  'status' => [],
  'missing-files' => [],
  'invalid-code-anchors' => []
}

if obligations.size != 107
  groups['count'] << "expected 107 obligations, got #{obligations.size}"
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
  kind = ta.is_a?(Hash) ? ta['kind'].to_s.strip : ''
  min_success = ta.is_a?(Hash) ? ta['min_success'] : nil
  status = o['status'].to_s.strip

  if matcher.empty?
    groups['unmapped-rules'] << "#{id}: blank test matcher"
  elsif matcher != id
    groups['matcher-id-mismatch'] << "#{id}: test matcher must be the obligation id, got #{matcher.inspect}"
  end
  if kind != 'quickcheck'
    groups['evidence-kind'] << "#{id}: expected test_anchor.kind=quickcheck, got #{kind.inspect}"
  end
  unless min_success.is_a?(Integer) && min_success.positive?
    groups['min-success'] << "#{id}: expected positive integer test_anchor.min_success, got #{min_success.inspect}"
  end
  if test_file.empty?
    groups['unmapped-rules'] << "#{id}: blank test file"
  elsif !File.exist?(File.join(root, test_file))
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
      path, fragment = anchor.split('#', 2)
      if path.to_s.empty? || fragment.to_s.empty?
        groups['invalid-code-anchors'] << "#{id}: invalid code anchor #{anchor.inspect}"
        next
      end
      full_path = File.join(root, path)
      unless File.exist?(full_path)
        groups['missing-files'] << "#{id}: code file not found: #{path}"
        next
      end
      # Code anchors are navigational source references. The executable matcher
      # checks below are the semantic gate; a function-name substring is not
      # evidence that an obligation remains implemented.
    end
  end
end

if groups.values.any? { |entries| !entries.empty? }
  fail_grouped!(groups)
end

obligations.each do |o|
  puts [o['id'], o.dig('test_anchor', 'matcher'), o.dig('test_anchor', 'file'), o.dig('test_anchor', 'min_success')].join("\t")
end
RUBY

echo "[thesis-obligations] Executing anchor matchers"
command_failures=()
parse_failures=()
zero_examples=()
test_failures=()
missing_property_success=()
insufficient_property_success=()

hspec_summary_line() {
  ruby -pe '$_.gsub!(/\e\[[0-9;?]*[ -\/]*[@-~]/, "")' "$1" |
    grep -E '^[0-9]+ examples?, [0-9]+ failures$' |
    tail -n 1 || true
}

quickcheck_pass_count() {
  ruby - "$1" <<'RUBY'
path = ARGV.fetch(0)
max = 0
File.foreach(path) do |line|
  line = line.gsub(/\e\[[0-9;?]*[ -\/]*[@-~]/, '')
  if line =~ /\+\+\+ OK, passed ([0-9]+) tests?/
    count = Regexp.last_match(1).to_i
    max = count if count > max
  end
end
puts max
RUBY
}

while IFS=$'\t' read -r id matcher _file min_success; do
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

  summary_line="$(hspec_summary_line "${log_file}")"
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
  quickcheck_passes="$(quickcheck_pass_count "${log_file}")"
  if (( quickcheck_passes < 1 )); then
    missing_property_success+=("${id}")
  elif (( quickcheck_passes < min_success )); then
    insufficient_property_success+=("${id}: passed ${quickcheck_passes}, required ${min_success}")
  fi

  rm -f "${log_file}"
done <"${tmp_rows}"

if (( ${#command_failures[@]} > 0 || ${#parse_failures[@]} > 0 || ${#zero_examples[@]} > 0 || ${#test_failures[@]} > 0 || ${#missing_property_success[@]} > 0 || ${#insufficient_property_success[@]} > 0 )); then
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
  if (( ${#missing_property_success[@]} > 0 )); then
    echo "- missing QuickCheck success lines:"
    printf '  - %s\n' "${missing_property_success[@]}"
  fi
  if (( ${#insufficient_property_success[@]} > 0 )); then
    echo "- insufficient QuickCheck success counts:"
    printf '  - %s\n' "${insufficient_property_success[@]}"
  fi
  exit 1
fi

echo
echo "[thesis-obligations] PASS: all obligations are mapped and green"
