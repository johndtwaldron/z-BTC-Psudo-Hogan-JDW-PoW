#!/usr/bin/env bash
set -euo pipefail

echo "=== HFINAL JOB START ==="
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

## STEP1: COBOL ledger_finalize confirm.csv -> update ledger.dat -> audit_report.txt
echo "STEP1: COBOL ledger_finalize"
LEDGER_FIN="$(find "$ROOT/cobol/bin" -maxdepth 1 -type f -name 'ledger_finalize*' | head -n1 || true)"
if [[ -z "${LEDGER_FIN}" ]]; then
  echo "ERROR: ledger_finalize binary not found in $ROOT/cobol/bin" >&2
  exit 12
fi
mkdir -p "$ROOT/cobol/data"
: > "$ROOT/cobol/data/audit_report.txt"
"$LEDGER_FIN" "$ROOT/cobol/data/confirm.csv" "$ROOT/cobol/data/ledger.dat" "$ROOT/cobol/data/audit_report.txt"

echo "Audit report: $ROOT/cobol/data/audit_report.txt"
echo "=== HFINAL JOB END ==="
