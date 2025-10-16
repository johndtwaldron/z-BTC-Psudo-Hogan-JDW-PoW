#!/usr/bin/env bash
set -euo pipefail
docker build -t jdw/cobol-builder ./cobol
docker run --rm -v "$PWD":/work jdw/cobol-builder bash -lc '
  mkdir -p /work/cobol/bin
  cobc -x -o /work/cobol/bin/ledger_update     /work/cobol/src/ledger_update.cob
  cobc -x -o /work/cobol/bin/ledger_finalize   /work/cobol/src/ledger_finalize.cob
  echo "Built COBOL binaries to ./cobol/bin"
'
