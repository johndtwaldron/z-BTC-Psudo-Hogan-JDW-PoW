#!/bin/bash
#
# COBOL Build Script
# Builds COBOL programs using dockerized GnuCOBOL
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$SCRIPT_DIR/src"
BIN_DIR="$SCRIPT_DIR/bin"

echo "Building COBOL programs..."

# Create bin directory if it doesn't exist
mkdir -p "$BIN_DIR"

# Build Docker image if it doesn't exist
if ! docker image inspect gnucobol-builder >/dev/null 2>&1; then
    echo "Building GnuCOBOL Docker image..."
    docker build -t gnucobol-builder "$SCRIPT_DIR"
fi

# Compile COBOL programs
echo "Compiling ledger_update.cob..."
docker run --rm \
    -v "$SRC_DIR:/src" \
    -v "$BIN_DIR:/bin" \
    gnucobol-builder \
    cobc -x -o /bin/ledgerupd /src/ledger_update.cob

echo "Compiling ledger_finalize.cob..."
docker run --rm \
    -v "$SRC_DIR:/src" \
    -v "$BIN_DIR:/bin" \
    gnucobol-builder \
    cobc -x -o /bin/ledgerfin /src/ledger_finalize.cob

echo "COBOL build complete."
echo "Binaries available in: $BIN_DIR"
