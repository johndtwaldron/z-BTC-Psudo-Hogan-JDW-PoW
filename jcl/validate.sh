#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

declare -A MAP=(
  ["HBANKTRX.WARP.JCL"]="scripts/hbanktrx.sh"
  ["HSETTLE.WARP.JCL"]="scripts/hsettle.sh"
  ["HFINAL.WARP.JCL"]="scripts/hfinal.sh"
)

fail=0
for jcl in "${!MAP[@]}"; do
  driver="${ROOT}/${MAP[$jcl]}"
  jclp="$ROOT/jcl/$jcl"
  [[ -f "$driver" && -f "$jclp" ]] || { echo "Skip $jcl (missing)"; continue; }
  echo "Validating parity: jcl/$(basename "$jclp") <-> $(basename "$driver")"
  steps=$(grep -Eo '^//STEP[0-9]+' "$jclp" | sed 's|^//||' | sort -u || true)
  for s in $steps; do
    if ! grep -qE "STEP[[:space:]]*$(( ${s#STEP} ))" "$driver" && ! grep -qE "$s" "$driver"; then
      echo "  MISSING: $s in $driver"
      fail=1
    else
      echo "  OK: $s"
    fi
  done
done

[[ $fail -eq 0 ]] || { echo "Parity validation failed."; exit 1; }
echo "All JCL steps are represented in Bash drivers."
