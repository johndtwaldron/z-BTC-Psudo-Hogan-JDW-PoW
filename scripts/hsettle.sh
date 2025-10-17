#!/usr/bin/env bash
set -euo pipefail
echo "=== HSETTLE JOB START ==="
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$ROOT/cobol/data"

echo "STEP1: Settlement service (or stub)"
SETTLE_JAR="$ROOT/java/settle/target/settle.jar"
CONFIRM="$ROOT/cobol/data/confirm.csv"
if [[ -f "$SETTLE_JAR" ]]; then
  java -jar "$SETTLE_JAR" settle --confirmations 2
else
  echo "INFO: $SETTLE_JAR not found; creating stub $CONFIRM"
  WORK="$ROOT/cobol/data/work.csv"
  {
    echo "#tx_id,status,settled_block"
    awk -F',' 'NR>1 && $1 !~ /^#/ {print $1 ",SETTLED,100"}' "$WORK"
  } > "$CONFIRM"
fi
echo "=== HSETTLE JOB END ==="