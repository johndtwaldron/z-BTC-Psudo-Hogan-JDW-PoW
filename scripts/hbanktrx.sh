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

mkdir -p "$ROOT/cobol/data"

## STEP2: Java core -> generate work file (or stub if Java missing)
echo "STEP2: Generate work file"
CORE_JAR="$ROOT/java/core/target/core.jar"
WORK="$ROOT/cobol/data/work.csv"
if [[ -f "$CORE_JAR" ]]; then
  java -jar "$CORE_JAR" generate-work "$WORK"
else
  echo "INFO: $CORE_JAR not found; creating stub $WORK"
  cat > "$WORK" <<EOF
#tx_id,from,to,amount,currency
TX001,A,B,50.00,GBP
TX002,B,C,12.34,GBP
EOF
fi

## STEP3: COBOL ledger_update (run inside builder container)
echo "STEP3: COBOL ledger_update"
docker_run=(docker run --rm -v "$ROOT":/work jdw/cobol-builder bash -lc)
"${docker_run[@]}" "/work/cobol/bin/ledger_update /work/cobol/data/work.csv /work/cobol/data/ledger.dat"

echo "=== HBANKTRX JOB END ==="