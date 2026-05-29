#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: bench/run-many-defs-comparison.sh [--defs N] [--runs N]
                                         [--output PATH] [--ghc PATH]
                                         [--batch-size N]
                                         [--operations]
                                         [--no-build] [--no-regenerate]

Generate matched mixed many-definition .mlfp and Haskell modules, then compare
MLF check-program timing against GHC -fforce-recomp -fno-code -O0 timing.

Defaults:
  --defs 1000
  --runs 1
  --batch-size 160
  --output bench/results/many-defs-comparison.tsv
  --ghc ghc
USAGE
}

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"

defs=1000
runs=1
output_path="$repo_root/bench/results/many-defs-comparison.tsv"
ghc_bin="ghc"
build_first=1
regenerate=1
timing_operations=0
module_batch_size=160

while [[ $# -gt 0 ]]; do
  case "$1" in
    --defs)
      [[ $# -ge 2 ]] || { echo "missing value for --defs" >&2; exit 2; }
      defs="$2"
      shift 2
      ;;
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
    --ghc)
      [[ $# -ge 2 ]] || { echo "missing value for --ghc" >&2; exit 2; }
      ghc_bin="$2"
      shift 2
      ;;
    --batch-size)
      [[ $# -ge 2 ]] || { echo "missing value for --batch-size" >&2; exit 2; }
      module_batch_size="$2"
      shift 2
      ;;
    --operations|--timing-operations)
      timing_operations=1
      shift
      ;;
    --no-build)
      build_first=0
      shift
      ;;
    --no-regenerate)
      regenerate=0
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

case "$defs" in
  ''|*[!0-9]*)
    echo "--defs must be a positive integer" >&2
    exit 2
    ;;
esac

case "$runs" in
  ''|*[!0-9]*)
    echo "--runs must be a positive integer" >&2
    exit 2
    ;;
esac

if (( defs < 1 )); then
  echo "--defs must be at least 1" >&2
  exit 2
fi

if (( runs < 1 )); then
  echo "--runs must be at least 1" >&2
  exit 2
fi

case "$module_batch_size" in
  ''|*[!0-9]*)
    echo "--batch-size must be a positive integer" >&2
    exit 2
    ;;
esac

if (( module_batch_size < 1 )); then
  echo "--batch-size must be at least 1" >&2
  exit 2
fi

if [[ "$output_path" != /* ]]; then
  output_path="$repo_root/$output_path"
fi

mlfp_rel="bench/benchmarks/many-defs-$defs"
mlfp_dir="$repo_root/$mlfp_rel"
haskell_module="ManyDefs$defs"
haskell_rel="bench/ghc-reference/$haskell_module.hs"
haskell_source="$repo_root/$haskell_rel"
top_defs=$((defs + 3))

mkdir -p "$(dirname -- "$output_path")" "$repo_root/bench/results"

if (( regenerate )); then
  "$repo_root/bench/generate-many-defs-comparison.sh" \
    --defs "$defs" \
    --mlfp-dir "$mlfp_dir" \
    --haskell-source "$haskell_source" \
    --haskell-module "$haskell_module"
fi

if (( build_first )); then
  cabal build exe:mlf2
fi

output_stem="${output_path%.tsv}"
mlfp_output="$output_stem.mlfp.tsv"
ghc_output="$output_stem.ghc.tsv"

mlfp_bench_args=(
  --runs "$runs"
  --no-build
  --benchmark "many-defs-$defs" "$mlfp_rel"
  --output "$mlfp_output"
)
if (( timing_operations )); then
  mlfp_bench_args+=(--operations)
fi

MLF_MODULE_DEF_BATCH_SIZE="$module_batch_size" "$repo_root/bench/run-benchmarks.sh" \
  "${mlfp_bench_args[@]}"

"$repo_root/bench/run-ghc-parser-reference.sh" \
  --runs "$runs" \
  --defs "$top_defs" \
  --source "$haskell_source" \
  --output "$ghc_output" \
  --ghc "$ghc_bin"

printf 'tool\tmode\tdefs\tsource\tmetric\truns\ttotal_ms\tper_def_ms\tstatus\n' > "$output_path"

awk -F '\t' -v defs="$top_defs" -v batch="$module_batch_size" '
  NR == 1 { next }
  $3 == "program.check.module.Main.constructor-bindings" ||
  $3 == "program.check.module.Main.def-bindings" ||
  $3 == "program.check.module.Main" ||
  $3 == "program.check.modules" ||
  $3 == "real" {
    per_def = $5 / defs
    printf "mlf2\tcheck-program +RTS -A64m batch%s\t%d\t%s\t%s\t%s\t%.3f\t%.3f\t0\n", batch, defs, $2, $3, $4, $5, per_def
  }
' "$mlfp_output" >> "$output_path"

awk -F '\t' '
  NR == 1 { next }
  {
    printf "ghc\t%s\t%s\t%s\treal\t1\t%s\t%s\t%s\n", $3, $4, $5, $7, $8, $9
  }
' "$ghc_output" >> "$output_path"

printf 'wrote %s\n' "$output_path"
printf 'wrote %s\n' "$mlfp_output"
printf 'wrote %s\n' "$ghc_output"
