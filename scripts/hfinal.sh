#!/usr/bin/env bash
set -euo pipefail

echo "=== HFINAL JOB START ==="
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$ROOT/cobol/data"

## STEP1: COBOL ledger_finalize confirm.csv -> ledger.dat -> audit_report.txt
echo "STEP1: COBOL ledger_finalize"
LEDGER_FIN="$(find "$ROOT/cobol/bin" -maxdepth 1 -type f -name 'ledger_finalize*' | head -n1 || true)"
if [[ -z "${LEDGER_FIN}" ]]; then
  echo "ERROR: ledger_finalize binary not found in $ROOT/cobol/bin" >&2
  exit 12
fi
CONFIRM="$ROOT/cobol/data/confirm.csv"
LEDGER="$ROOT/cobol/data/ledger.dat"
REPORT="$ROOT/cobol/data/audit_report.txt"
: > "$REPORT"
"$LEDGER_FIN" "$CONFIRM" "$LEDGER" "$REPORT"

echo "Audit report: $REPORT"
echo "=== HFINAL JOB END ==="
