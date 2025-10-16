#!/usr/bin/env bash
set -euo pipefail

echo "=== HBANKTRX JOB START ==="
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

## STEP0: Ensure services are up (MQ + bitcoind)
echo "STEP0: docker compose up (MQ + bitcoind)"
docker compose -f "$ROOT/compose/docker-compose.yaml" up -d

## STEP1: Build COBOL (dockerized GnuCOBOL) -> ./cobol/bin/*
echo "STEP1: Build COBOL"
if [[ -f "$ROOT/cobol/build.sh" ]]; then
  bash "$ROOT/cobol/build.sh"
else
  echo "WARN: $ROOT/cobol/build.sh not found (skipping compile)"
fi

## STEP2: Java core MQGET -> write work.csv
echo "STEP2: Java core -> generate work file"
if [[ ! -f "$ROOT/java/core/target/core.jar" ]]; then
  (cd "$ROOT/java" && mvn -q -DskipTests package)
fi
java -jar "$ROOT/java/core/target/core.jar" generate-work "$ROOT/cobol/data/work.csv"

## STEP3: COBOL ledger_update work.csv -> ledger.dat
echo "STEP3: COBOL ledger_update"
LEDGER_UPD="$(find "$ROOT/cobol/bin" -maxdepth 1 -type f -name 'ledger_update*' | head -n1 || true)"
if [[ -z "${LEDGER_UPD}" ]]; then
  echo "ERROR: ledger_update binary not found in $ROOT/cobol/bin" >&2
  exit 8
fi
"$LEDGER_UPD" "$ROOT/cobol/data/work.csv" "$ROOT/cobol/data/ledger.dat"

echo "=== HBANKTRX JOB END ==="
