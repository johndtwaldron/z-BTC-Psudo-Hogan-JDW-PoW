#!/usr/bin/env bash
set -euo pipefail

echo "=== HSETTLE JOB START ==="
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

## STEP1: Settlement microservice MQGET SETTLE.NOTIFY -> Bitcoin RPC -> MQPUT SETTLE.CONFIRM
echo "STEP1: Settlement service"
if [[ ! -f "$ROOT/java/settle/target/settle.jar" ]]; then
  (cd "$ROOT/java" && mvn -q -DskipTests package)
fi
java -jar "$ROOT/java/settle/target/settle.jar" settle --confirmations 2

echo "=== HSETTLE JOB END ==="
