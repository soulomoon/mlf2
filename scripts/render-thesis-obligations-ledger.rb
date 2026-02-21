#!/usr/bin/env ruby
# frozen_string_literal: true

require 'optparse'
require 'yaml'

ROOT = '/Volumes/src/mlf4'
LEDGER_PATH = File.join(ROOT, 'docs/thesis-obligations.yaml')
OUTPUT_PATH = File.join(ROOT, 'docs/thesis-obligations.md')

def expected_ids
  ids = []
  ids.concat(%w[EMPTY TVAR VAR].map { |s| "O14-WF-#{s}" })
  ids.concat(%w[REFLEX TRANS BOT HYP INNER OUTER QUANT-ELIM QUANT-INTRO].map { |s| "O14-INST-#{s}" })
  ids.concat(%w[VAR ABS APP TABS TAPP LET].map { |s| "O14-T-#{s}" })
  ids.concat(%w[BETA BETALET REFLEX TRANS QUANT-INTRO QUANT-ELIM INNER OUTER CONTEXT].map { |s| "O14-RED-#{s}" })
  ids.concat(%w[N O SEQ INNER OUTER HYP BOT ID].map { |s| "O14-APPLY-#{s}" })
  ids.concat(%w[NO-INERT-LOCKED SCHEME-ROOT-RIGID ARROW-RIGID NON-INTERIOR-RIGID].map { |s| "O15-TRANS-#{s}" })
  ids.concat(%w[REQUIRED IDENTITY].map { |s| "O15-REORDER-#{s}" })
  ids.concat(%w[FIND REJECT].map { |s| "O15-CONTEXT-#{s}" })
  ids.concat(
    %w[
      SEQ-EMPTY SEQ-CONS RIGID-RAISE RIGID-MERGE RIGID-RAISEMERGE ROOT-GRAFT ROOT-RAISEMERGE
      ROOT-WEAKEN NODE-GRAFT NODE-MERGE NODE-RAISEMERGE NODE-WEAKEN NODE-RAISE
    ].map { |s| "O15-TR-#{s}" }
  )
  ids << 'O15-EDGE-TRANSLATION'
  ids.concat(%w[LAMBDA-VAR LET-VAR ABS APP LET].map { |s| "O15-ELAB-#{s}" })
  ids
end

def fail_validation(message)
  warn("thesis-obligations render: #{message}")
  exit(1)
end

def validate_schema!(doc)
  fail_validation('root must be a mapping') unless doc.is_a?(Hash)
  obligations = doc['obligations']
  fail_validation('missing `obligations` list') unless obligations.is_a?(Array)

  required_fields = %w[
    id chapter section figure_or_definition thesis_rule_label judgment_or_equation code_anchors test_anchor status
  ]
  required_test_anchor_fields = %w[matcher file rationale]

  obligations.each_with_index do |o, idx|
    fail_validation("obligation ##{idx + 1} must be a mapping") unless o.is_a?(Hash)
    required_fields.each do |field|
      fail_validation("obligation ##{idx + 1} missing `#{field}`") unless o.key?(field)
    end
    fail_validation("obligation ##{idx + 1} has blank id") if o['id'].to_s.strip.empty?
    fail_validation("obligation #{o['id']} has empty code_anchors") unless o['code_anchors'].is_a?(Array) && !o['code_anchors'].empty?
    o['code_anchors'].each do |anchor|
      fail_validation("obligation #{o['id']} has invalid code anchor `#{anchor}`") unless anchor.is_a?(String) && anchor.include?('#')
    end
    ta = o['test_anchor']
    fail_validation("obligation #{o['id']} has invalid test_anchor") unless ta.is_a?(Hash)
    required_test_anchor_fields.each do |field|
      fail_validation("obligation #{o['id']} test_anchor missing `#{field}`") unless ta[field].is_a?(String) && !ta[field].strip.empty?
    end
  end
end

def render_markdown(obligations)
  sorted = obligations.sort_by { |o| [o['chapter'].to_s, o['section'].to_s, o['id'].to_s] }
  by_chapter = sorted.group_by { |o| o['chapter'] }
  status_counts = sorted.group_by { |o| o['status'] }.transform_values(&:size)

  lines = []
  lines << '# Thesis Obligations Ledger'
  lines << ''
  lines << "Generated from `#{LEDGER_PATH}` by `scripts/render-thesis-obligations-ledger.rb`."
  lines << ''
  lines << '## Summary'
  lines << ''
  lines << "- Total obligations: **#{sorted.size}**"
  lines << "- Status counts: #{status_counts.map { |k, v| "`#{k}`=#{v}" }.join(', ')}"
  lines << "- Chapters covered: #{by_chapter.keys.sort.join(', ')}"
  lines << ''

  by_chapter.keys.sort.each do |chapter|
    lines << "## Chapter #{chapter}"
    lines << ''
    lines << '| ID | Section | Figure/Def | Rule | Test Matcher | Test File |'
    lines << '|---|---|---|---|---|---|'
    by_chapter[chapter].sort_by { |o| [o['section'].to_s, o['id'].to_s] }.each do |o|
      lines << [
        "| `#{o['id']}`",
        "`#{o['section']}`",
        "`#{o['figure_or_definition']}`",
        o['thesis_rule_label'],
        "`#{o.dig('test_anchor', 'matcher')}`",
        "`#{o.dig('test_anchor', 'file')}` |"
      ].join(' | ')
    end
    lines << ''
  end

  lines << '## Validation Notes'
  lines << ''
  lines << '- This file is generated; edit the YAML source instead.'
  lines << '- Gate enforcement additionally verifies id set, mapping completeness, file/symbol anchors, and executable test anchors.'
  lines << ''
  lines.join("\n")
end

options = {
  write: true,
  check: false
}

OptionParser.new do |opts|
  opts.banner = 'Usage: scripts/render-thesis-obligations-ledger.rb [--check] [--stdout]'
  opts.on('--check', 'Fail if generated markdown differs from the committed markdown') do
    options[:check] = true
  end
  opts.on('--stdout', 'Print generated markdown to stdout instead of writing file') do
    options[:write] = false
  end
end.parse!

doc = YAML.load_file(LEDGER_PATH)
validate_schema!(doc)
obligations = doc['obligations']

md = render_markdown(obligations)

if options[:check]
  current = File.exist?(OUTPUT_PATH) ? File.read(OUTPUT_PATH) : ''
  if current != md
    warn("thesis-obligations render: markdown is out of date: #{OUTPUT_PATH}")
    exit(1)
  end
  options[:write] = false
end

if options[:write]
  File.write(OUTPUT_PATH, md)
  puts("wrote #{OUTPUT_PATH}")
else
  puts(md)
end
