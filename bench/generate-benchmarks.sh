#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: bench/generate-benchmarks.sh [--name NAME] [--let-depth N] [--output-dir DIR]

Generate a local .mlfp package benchmark fixture.

Defaults:
  --name cross-module-let-baseline
  --let-depth 1
  --output-dir bench/benchmarks
USAGE
}

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"

name="cross-module-let-baseline"
let_depth=1
output_dir="$repo_root/bench/benchmarks"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --name)
      [[ $# -ge 2 ]] || { echo "missing value for --name" >&2; exit 2; }
      name="$2"
      shift 2
      ;;
    --let-depth)
      [[ $# -ge 2 ]] || { echo "missing value for --let-depth" >&2; exit 2; }
      let_depth="$2"
      shift 2
      ;;
    --output-dir)
      [[ $# -ge 2 ]] || { echo "missing value for --output-dir" >&2; exit 2; }
      output_dir="$2"
      shift 2
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

case "$name" in
  ""|*/*|.*|*..*)
    echo "invalid benchmark name: $name" >&2
    exit 2
    ;;
esac

case "$let_depth" in
  ''|*[!0-9]*)
    echo "--let-depth must be a positive integer" >&2
    exit 2
    ;;
esac

if (( let_depth < 1 )); then
  echo "--let-depth must be at least 1" >&2
  exit 2
fi

if [[ "$output_dir" != /* ]]; then
  output_dir="$repo_root/$output_dir"
fi

fixture_dir="$output_dir/$name"
mkdir -p "$fixture_dir"

expr="id${let_depth} 1"
for (( i = let_depth; i >= 1; i-- )); do
  if (( i == 1 )); then
    rhs="λx x"
  else
    prev=$((i - 1))
    rhs="λx id${prev} x"
  fi
  expr="let id${i} = ${rhs} in ${expr}"
done

cat > "$fixture_dir/Core.mlfp" <<EOF
module Core export (applyId) {
  def applyId : Int = ${expr};
}
EOF

cat > "$fixture_dir/Main.mlfp" <<'EOF'
module Main export (main) {
  import Core exposing (applyId);

  def main : Int = applyId;
}
EOF

printf 'generated %s (let-depth=%s)\n' "$fixture_dir" "$let_depth"
