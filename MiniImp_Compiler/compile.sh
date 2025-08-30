#!/bin/bash
# Usage: ./run_miniimp.sh [-D] [-O] <num_of_registers> <program.miniimp> <output.minirisc>

# Parse flags
D_FLAG="false"
O_FLAG="false"

while [[ "$1" == -* ]]; do
  case "$1" in
    -D) D_FLAG="true" ; shift ;;
    -O) O_FLAG="true" ; shift ;;
    *) echo "Unknown flag: $1"; exit 1 ;;
  esac
done

if [ $# -ne 3 ]; then
  echo "Usage: $0 [-D] [-O] <num_of_registers> <program.miniimp> <output.minirisc>"
  exit 1
fi

NUM_REGS="$1"
PROG_FILE="$2"
OUT_FILE="$3"

# Run dune exec with flags encoded as first two args
dune build
dune exec MiniImp_Compiler $D_FLAG $O_FLAG "$NUM_REGS" "$PROG_FILE" "$OUT_FILE"
