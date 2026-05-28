#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: bench/run-benchmarks.sh [--runs N] [--output PATH] [--bin PATH] [--no-build]
                               [--allow-status STATUS]
                               [--benchmark NAME PATH]

Run focused .mlfp package benchmarks and write summary TSV output.

Defaults:
  --runs 1
  --output bench/results/latest.tsv
  --allow-status 0
  --benchmark fixture-cross-module-let test/programs/packages/cross-module-let

The script also writes raw per-run metrics next to the summary as
<output>.runs.tsv.
USAGE
}

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"

runs=1
output_path="$repo_root/bench/results/latest.tsv"
bin_path=""
build_first=1
benchmarks=()
allowed_statuses=(0)

while [[ $# -gt 0 ]]; do
  case "$1" in
    --runs)
      [[ $# -ge 2 ]] || { echo "missing value for --runs" >&2; exit 2; }
      runs="$2"
      shift 2
      ;;
    --output)
      [[ $# -ge 2 ]] || { echo "missing value for --output" >&2; exit 2; }
      output_path="$2"
      shift 2
      ;;
    --bin)
      [[ $# -ge 2 ]] || { echo "missing value for --bin" >&2; exit 2; }
      bin_path="$2"
      build_first=0
      shift 2
      ;;
    --no-build)
      build_first=0
      shift
      ;;
    --allow-status)
      [[ $# -ge 2 ]] || { echo "missing value for --allow-status" >&2; exit 2; }
      allowed_statuses+=("$2")
      shift 2
      ;;
    --benchmark)
      [[ $# -ge 3 ]] || { echo "missing NAME PATH for --benchmark" >&2; exit 2; }
      benchmarks+=("$2"$'\t'"$3")
      shift 3
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

if (( runs < 1 )); then
  echo "--runs must be at least 1" >&2
  exit 2
fi

for allowed_status in "${allowed_statuses[@]}"; do
  case "$allowed_status" in
    ''|*[!0-9]*)
      echo "--allow-status must be a non-negative integer" >&2
      exit 2
      ;;
  esac
done

if [[ "$output_path" != /* ]]; then
  output_path="$repo_root/$output_path"
fi

mkdir -p "$(dirname -- "$output_path")"
raw_path="${output_path%.tsv}.runs.tsv"

if (( ${#benchmarks[@]} == 0 )); then
  benchmarks=(
    "fixture-cross-module-let"$'\t'"test/programs/packages/cross-module-let"
  )
fi

if (( build_first )); then
  cabal build exe:mlf2
fi

if [[ -z "$bin_path" ]]; then
  bin_path="$(cabal list-bin exe:mlf2)"
elif [[ "$bin_path" != /* ]]; then
  bin_path="$repo_root/$bin_path"
fi

tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/mlf-bench.XXXXXX")"
trap 'rm -rf "$tmp_dir"' EXIT

printf 'benchmark\tpath\trun\tmetric\tvalue_ms\tstatus\n' > "$raw_path"

parse_metrics() {
  local stderr_path="$1"
  awk '
    /^\[MLF_PROGRAM_TIMING\] done  / {
      metric = $3
      value = $4
      sub(/ms$/, "", value)
      print metric "\t" value
    }
    /^\[MLF_PROGRAM_TIMING\] metric / {
      metric = $3
      value = $4
      print metric "\t" value
    }
    /^real / { printf "real\t%.3f\n", $2 * 1000 }
    /^user / { printf "user\t%.3f\n", $2 * 1000 }
    /^sys / { printf "sys\t%.3f\n", $2 * 1000 }
  ' "$stderr_path"
}

is_allowed_status() {
  local status="$1"
  local allowed
  for allowed in "${allowed_statuses[@]}"; do
    if [[ "$status" == "$allowed" ]]; then
      return 0
    fi
  done
  return 1
}

for benchmark_entry in "${benchmarks[@]}"; do
  benchmark="${benchmark_entry%%$'\t'*}"
  rel_path="${benchmark_entry#*$'\t'}"
  fixture_path="$repo_root/$rel_path"
  if [[ ! -d "$fixture_path" ]]; then
    echo "missing benchmark fixture: $rel_path" >&2
    exit 1
  fi

  for (( run = 1; run <= runs; run++ )); do
    stdout_path="$tmp_dir/$benchmark.$run.stdout"
    stderr_path="$tmp_dir/$benchmark.$run.stderr"
    set +e
    /usr/bin/time -p env MLF_PROGRAM_TIMING_DETAIL=1 MLF_PROGRAM_TIMING_OPERATIONS=1 "$bin_path" check-program "$fixture_path" >"$stdout_path" 2>"$stderr_path"
    status=$?
    set -e

    while IFS=$'\t' read -r metric value_ms; do
      [[ -n "$metric" ]] || continue
      printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$benchmark" "$rel_path" "$run" "$metric" "$value_ms" "$status" >> "$raw_path"
    done < <(parse_metrics "$stderr_path")
    printf '%s\t%s\t%s\tcommand.status\t%s\t%s\n' "$benchmark" "$rel_path" "$run" "$status" "$status" >> "$raw_path"

    if ! is_allowed_status "$status"; then
      echo "benchmark failed: $benchmark run $run" >&2
      sed -n '1,120p' "$stderr_path" >&2
      exit "$status"
    fi
  done
done

awk -F '\t' '
  NR == 1 { next }
  {
    key = $1 SUBSEP $2 SUBSEP $4
    if (!(key in seen)) {
      seen[key] = 1
      keys[++key_count] = key
      names[key] = $1 "\t" $2 "\t" $4
    }
    count[key] += 1
    sum[key] += $5
    values[key] = values[key] (values[key] == "" ? "" : sep) $5
  }
  function sort_numeric(values_text, out,    parts, n, i, j, x) {
    n = split(values_text, parts, sep)
    for (i = 1; i <= n; i++) {
      out[i] = parts[i] + 0
    }
    for (i = 2; i <= n; i++) {
      x = out[i]
      j = i - 1
      while (j >= 1 && out[j] > x) {
        out[j + 1] = out[j]
        j--
      }
      out[j + 1] = x
    }
    return n
  }
  function median(sorted, n) {
    if (n % 2 == 1) {
      return sorted[(n + 1) / 2]
    }
    return (sorted[n / 2] + sorted[n / 2 + 1]) / 2
  }
  BEGIN {
    sep = "\034"
    print "benchmark\tpath\tmetric\truns\tmin_ms\tmedian_ms\tmax_ms\tmean_ms\tvalues_ms"
  }
  END {
    for (k = 1; k <= key_count; k++) {
      key = keys[k]
      delete sorted
      n = sort_numeric(values[key], sorted)
      values_csv = ""
      for (i = 1; i <= n; i++) {
        values_csv = values_csv (i == 1 ? "" : ",") sprintf("%.3f", sorted[i])
      }
      printf "%s\t%d\t%.3f\t%.3f\t%.3f\t%.3f\t%s\n", names[key], count[key], sorted[1], median(sorted, n), sorted[n], sum[key] / count[key], values_csv
    }
  }
' "$raw_path" > "$output_path"

echo "wrote $output_path"
echo "wrote $raw_path"
