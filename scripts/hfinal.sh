#!/usr/bin/env bash
set -euo pipefail

echo "=== HFINAL JOB START ==="
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$ROOT/cobol/data"

## STEP1: COBOL ledger_finalize (run inside builder container)
echo "STEP1: COBOL ledger_finalize"
CONFIRM="$ROOT/cobol/data/confirm.csv"
LEDGER="$ROOT/cobol/data/ledger.dat"
REPORT="$ROOT/cobol/data/audit_report.txt"
: > "$REPORT"

DOCKER_RUN="docker run --rm -v \"$ROOT\":/work jdw/cobol-builder bash -lc"
$DOCKER_RUN "/work/cobol/bin/ledger_finalize /work/cobol/data/confirm.csv /work/cobol/data/ledger.dat /work/cobol/data/audit_report.txt"


echo "Audit report: $REPORT"
echo "=== HFINAL JOB END ==="
