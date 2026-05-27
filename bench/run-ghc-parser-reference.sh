#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: bench/run-ghc-parser-reference.sh [--runs N] [--source PATH] [--defs N] [--output PATH] [--ghc PATH] [--regenerate]

Run the persisted GHC parser-shape reference and write a TSV timing artifact.

Defaults:
  --runs 1
  --source bench/ghc-reference/ParserShape914.hs
  --defs 914
  --output bench/results/ghc-per-def-reference.tsv
  --ghc ghc
USAGE
}

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"

runs=1
defs=914
source_path="$repo_root/bench/ghc-reference/ParserShape914.hs"
output_path="$repo_root/bench/results/ghc-per-def-reference.tsv"
ghc_bin="ghc"
regenerate=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --runs)
      [[ $# -ge 2 ]] || { echo "missing value for --runs" >&2; exit 2; }
      runs="$2"
      shift 2
      ;;
    --source)
      [[ $# -ge 2 ]] || { echo "missing value for --source" >&2; exit 2; }
      source_path="$2"
      shift 2
      ;;
    --defs)
      [[ $# -ge 2 ]] || { echo "missing value for --defs" >&2; exit 2; }
      defs="$2"
      shift 2
      ;;
    --output)
      [[ $# -ge 2 ]] || { echo "missing value for --output" >&2; exit 2; }
      output_path="$2"
      shift 2
      ;;
    --ghc)
      [[ $# -ge 2 ]] || { echo "missing value for --ghc" >&2; exit 2; }
      ghc_bin="$2"
      shift 2
      ;;
    --regenerate)
      regenerate=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

case "$runs" in
  ''|*[!0-9]*)
    echo "--runs must be a positive integer" >&2
    exit 2
    ;;
esac

case "$defs" in
  ''|*[!0-9]*)
    echo "--defs must be a positive integer" >&2
    exit 2
    ;;
esac

if (( runs < 1 )); then
  echo "--runs must be at least 1" >&2
  exit 2
fi

if (( defs < 1 )); then
  echo "--defs must be at least 1" >&2
  exit 2
fi

if [[ "$source_path" != /* ]]; then
  source_path="$repo_root/$source_path"
fi

if [[ "$output_path" != /* ]]; then
  output_path="$repo_root/$output_path"
fi

if (( regenerate )); then
  module_name="$(basename -- "$source_path" .hs)"
  "$repo_root/bench/ghc-reference/generate-parser-shape.sh" \
    --defs "$defs" \
    --output "$source_path" \
    --module "$module_name"
fi

if [[ ! -f "$source_path" ]]; then
  echo "missing GHC reference source: $source_path" >&2
  echo "rerun with --regenerate to create it" >&2
  exit 1
fi

mkdir -p "$(dirname -- "$output_path")"

version="$("$ghc_bin" --numeric-version)"
mode="-fforce-recomp -fno-code -O0 -v0 +RTS -A64m"
tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/ghc-parser-reference.XXXXXX")"
trap 'rm -rf "$tmp_dir"' EXIT

printf 'tool\tversion\tmode\tdefs\tsource\trun\treal_ms\tper_def_ms\tstatus\n' > "$output_path"

for (( run = 1; run <= runs; run++ )); do
  stderr_path="$tmp_dir/run-$run.stderr"
  set +e
  /usr/bin/time -p "$ghc_bin" -fforce-recomp -fno-code -O0 -v0 "$source_path" +RTS -A64m >"$tmp_dir/run-$run.stdout" 2>"$stderr_path"
  status=$?
  set -e

  real_seconds="$(awk '$1 == "real" { print $2 }' "$stderr_path")"
  if [[ -z "$real_seconds" ]]; then
    echo "failed to parse /usr/bin/time output for run $run" >&2
    sed -n '1,120p' "$stderr_path" >&2
    exit 1
  fi

  real_ms="$(awk -v seconds="$real_seconds" 'BEGIN { printf "%.3f", seconds * 1000 }')"
  per_def_ms="$(awk -v ms="$real_ms" -v defs="$defs" 'BEGIN { printf "%.3f", ms / defs }')"
  rel_source="${source_path#$repo_root/}"
  printf 'ghc\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$version" "$mode" "$defs" "$rel_source" "$run" "$real_ms" "$per_def_ms" "$status" >> "$output_path"

  if (( status != 0 )); then
    echo "GHC reference failed on run $run" >&2
    sed -n '1,120p' "$stderr_path" >&2
    exit "$status"
  fi
done
