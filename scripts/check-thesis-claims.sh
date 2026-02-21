#!/usr/bin/env bash

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CLAIMS="${ROOT}/docs/thesis-claims.yaml"
DEVIATIONS="${ROOT}/docs/thesis-deviations.yaml"
OBLIGATIONS="${ROOT}/docs/thesis-obligations.yaml"

if [[ ! -f "${CLAIMS}" ]]; then
  echo "[thesis-claims] FAILED: missing claims file: ${CLAIMS}"
  exit 1
fi
if [[ ! -f "${DEVIATIONS}" ]]; then
  echo "[thesis-claims] FAILED: missing deviations file: ${DEVIATIONS}"
  exit 1
fi
if [[ ! -f "${OBLIGATIONS}" ]]; then
  echo "[thesis-claims] FAILED: missing obligations file: ${OBLIGATIONS}"
  exit 1
fi

echo "[thesis-claims] Validating claims, deviations, and cross-links"
ruby - "${CLAIMS}" "${DEVIATIONS}" "${OBLIGATIONS}" "${ROOT}" <<'RUBY'
require 'yaml'
require 'set'

claims_path = ARGV.fetch(0)
deviations_path = ARGV.fetch(1)
obligations_path = ARGV.fetch(2)
root = ARGV.fetch(3)

claims_doc = YAML.load_file(claims_path)
deviations_doc = YAML.load_file(deviations_path)
obligations_doc = YAML.load_file(obligations_path)

errors = Hash.new { |h, k| h[k] = [] }

# ── 1. Basic schema validation ──────────────────────────────────────────

unless claims_doc.is_a?(Hash) && claims_doc['claims'].is_a?(Array)
  errors['schema'] << 'claims root must be a mapping with `claims` array'
end

unless deviations_doc.is_a?(Hash) && deviations_doc['deviations'].is_a?(Array)
  errors['schema'] << 'deviations root must be a mapping with `deviations` array'
end

unless obligations_doc.is_a?(Hash) && obligations_doc['obligations'].is_a?(Array)
  errors['schema'] << 'obligations root must be a mapping with `obligations` array'
end

if errors.values.any? { |e| !e.empty? }
  errors.each do |title, entries|
    next if entries.empty?
    warn "[thesis-claims] FAILED: #{title}"
    entries.each { |e| warn "  - #{e}" }
  end
  exit 1
end

claims = claims_doc['claims']
deviations = deviations_doc['deviations']
obligations = obligations_doc['obligations']

# ── 2. Claim count matches scope ────────────────────────────────────────

expected_count = claims_doc.dig('scope', 'claim_count')
if expected_count && claims.size != expected_count
  errors['claim-count'] << "scope.claim_count=#{expected_count} but found #{claims.size} claims"
end

# ── 3. No duplicate claim IDs ───────────────────────────────────────────

claim_ids = claims.map { |c| c['id'] }
claim_id_set = Set.new
claim_ids.each do |id|
  if claim_id_set.include?(id)
    errors['duplicate-claim-ids'] << id
  end
  claim_id_set.add(id)
end

# ── 4. Required fields per claim ────────────────────────────────────────

required_claim_fields = %w[id type chapter section thesis_ref statement evidence status]
claims.each_with_index do |c, idx|
  unless c.is_a?(Hash)
    errors['claim-schema'] << "claim ##{idx + 1} is not a mapping"
    next
  end
  required_claim_fields.each do |field|
    unless c.key?(field)
      errors['claim-schema'] << "claim #{c['id'] || "##{idx + 1}"} missing `#{field}`"
    end
  end
end

# ── 5. Defended claims must have evidence ───────────────────────────────

claims.each do |c|
  next unless c.is_a?(Hash)
  id = c['id']
  status = c['status'].to_s
  evidence = c['evidence'] || {}
  obligations_list = evidence['obligations'] || []
  property_tests = evidence['property_tests'] || []
  devs = c['deviations'] || []
  notes = c['notes'].to_s.strip

  if status == 'defended' && obligations_list.empty? && property_tests.empty?
    errors['defended-no-evidence'] << "#{id}: status=defended but no obligations or property_tests"
  end

  if status == 'undefended' && devs.empty? && notes.empty?
    errors['undefended-no-rationale'] << "#{id}: status=undefended with no deviations or notes"
  end
end

# ── 6. No duplicate deviation IDs ───────────────────────────────────────

deviation_ids = deviations.map { |d| d['id'] }
deviation_id_set = Set.new
deviation_ids.each do |id|
  if deviation_id_set.include?(id)
    errors['duplicate-deviation-ids'] << id
  end
  deviation_id_set.add(id)
end

# ── 7. Obligation IDs index ─────────────────────────────────────────────

obligation_id_set = Set.new(obligations.map { |o| o['id'] })

# ── 8. Cross-link: claim obligations exist in obligations ledger ────────

claims.each do |c|
  next unless c.is_a?(Hash)
  id = c['id']
  (c.dig('evidence', 'obligations') || []).each do |oid|
    unless obligation_id_set.include?(oid)
      errors['claim-obligation-missing'] << "#{id} references obligation #{oid} not in ledger"
    end
  end
end

# ── 9. Cross-link: claim deviations exist in deviation register ─────────

claims.each do |c|
  next unless c.is_a?(Hash)
  id = c['id']
  (c['deviations'] || []).each do |did|
    unless deviation_id_set.include?(did)
      errors['claim-deviation-missing'] << "#{id} references deviation #{did} not in register"
    end
  end
end

# ── 10. Cross-link: obligation supports_claims exist in claims ──────────

obligations.each do |o|
  next unless o.is_a?(Hash)
  oid = o['id']
  (o['supports_claims'] || []).each do |cid|
    unless claim_id_set.include?(cid)
      errors['obligation-claim-missing'] << "#{oid} supports_claims references #{cid} not in claims"
    end
  end
end

# ── 11. Deviation register: no status=open ──────────────────────────────

deviations.each do |d|
  next unless d.is_a?(Hash)
  if d['status'].to_s == 'open'
    errors['deviation-open'] << "#{d['id']}: status=open"
  end
end

# ── 12. Deviation register: no orphans ──────────────────────────────────

referenced_deviations = Set.new
claims.each do |c|
  next unless c.is_a?(Hash)
  (c['deviations'] || []).each { |did| referenced_deviations.add(did) }
end

deviations.each do |d|
  next unless d.is_a?(Hash)
  did = d['id']
  unless referenced_deviations.include?(did)
    errors['deviation-orphan'] << "#{did}: not referenced by any claim"
  end
end

# ── 13. Code path validation ────────────────────────────────────────────

claims.each do |c|
  next unless c.is_a?(Hash)
  id = c['id']
  (c.dig('evidence', 'code_paths') || []).each do |cp|
    unless cp.is_a?(String) && cp.include?('#')
      errors['code-path-format'] << "#{id}: invalid code_path format: #{cp.inspect}"
      next
    end
    path, symbol = cp.split('#', 2)
    full_path = File.join(root, path)
    unless File.exist?(full_path)
      errors['code-path-file-missing'] << "#{id}: file not found: #{full_path}"
      next
    end
    contents = File.read(full_path)
    unless contents.include?(symbol)
      errors['code-path-symbol-missing'] << "#{id}: symbol #{symbol.inspect} not found in #{full_path}"
    end
  end
end

# ── Report ──────────────────────────────────────────────────────────────

if errors.values.any? { |e| !e.empty? }
  warn "[thesis-claims] FAILED: validation errors"
  errors.each do |title, entries|
    next if entries.empty?
    warn "- #{title}:"
    entries.each { |e| warn "  - #{e}" }
  end
  exit 1
end

puts "[thesis-claims] Schema: #{claims.size} claims, #{deviations.size} deviations"
puts "[thesis-claims] Cross-links: all obligation/deviation references valid"
puts "[thesis-claims] Code paths: all files and symbols found"
puts "[thesis-claims] Deviations: no open, no orphans"
RUBY

echo "[thesis-claims] PASS: all validations green"
