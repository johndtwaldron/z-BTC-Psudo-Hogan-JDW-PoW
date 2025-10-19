#!/usr/bin/env bash
set -euo pipefail

echo "=== HFINAL JOB START ==="
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$ROOT/cobol/data"

## STEP1: COBOL ledger_finalize (container + timeout)
echo "STEP1: COBOL ledger_finalize"
mkdir -p "$ROOT/cobol/logs"
REPORT="$ROOT/cobol/data/audit_report.txt"
: > "$REPORT"

docker_run=(docker run --rm -v "$ROOT":/work jdw/cobol-builder bash -lc)
set +e
timeout 60s "${docker_run[@]}" \
  "/work/cobol/bin/ledger_finalize /work/cobol/data/confirm.csv /work/cobol/data/ledger.dat /work/cobol/data/audit_report.txt" \
  2>&1 | tee "$ROOT/cobol/logs/ledger_finalize.log"
rc=${PIPESTATUS[0]}
set -e

if [[ $rc -eq 124 ]]; then
  echo "WARN: ledger_finalize timed out — writing stub report."
  echo "AUDIT: timed out $(date -u)" >> "$REPORT"
elif [[ $rc -ne 0 ]]; then
  echo "WARN: ledger_finalize rc=$rc — appending note to report."
  echo "AUDIT: rc=$rc $(date -u)" >> "$REPORT"
fi

echo "Audit report: $REPORT"
echo "=== HFINAL JOB END ==="