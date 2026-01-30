#!/bin/bash
# mpc_designation_cli.sh - CLI wrapper for MPC Designation Converter (AWK)
#
# Usage: ./mpc_designation_cli.sh <designation> [designation ...]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AWK_LIB="$SCRIPT_DIR/mpc_designation.awk"
AWK_MAIN="$SCRIPT_DIR/mpc_designation_main.awk"

if [ $# -eq 0 ] || [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    echo "Usage: $(basename "$0") <designation> [designation ...]"
    echo "Converts between packed and unpacked MPC designations."
    echo ""
    echo "Examples:"
    echo "  $(basename "$0") 00001          # -> 1"
    echo "  $(basename "$0") '1995 XA'      # -> J95X00A"
    echo "  $(basename "$0") 1P             # -> 0001P"
    exit 0
fi

for arg in "$@"; do
    echo "$arg" | awk -f "$AWK_LIB" -f "$AWK_MAIN"
done
