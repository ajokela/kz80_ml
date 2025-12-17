#!/bin/bash
# ML Test Runner for kz80_ml
# Compiles each test file and runs it through the emulator

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
EMULATOR="$PROJECT_DIR/../emulator/retroshield"
COMPILER="$PROJECT_DIR/target/release/kz80_ml"
TMP_BIN="/tmp/kz80_ml_test.bin"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Counters
PASSED=0
FAILED=0
SKIPPED=0

# Build compiler if needed
if [ ! -f "$COMPILER" ]; then
    echo "Building compiler..."
    (cd "$PROJECT_DIR" && cargo build --release)
fi

# Check emulator
if [ ! -f "$EMULATOR" ]; then
    echo -e "${RED}Error: Emulator not found at $EMULATOR${NC}"
    exit 1
fi

echo "Running ML tests..."
echo "===================="
echo

# Process each test file
for test_file in "$SCRIPT_DIR"/test_*.ml; do
    test_name=$(basename "$test_file" .ml)

    # Extract expected value from comment: (* Expected: N *)
    expected=$(grep -o 'Expected: -\?[0-9]*' "$test_file" | head -1 | sed 's/Expected: //')

    if [ -z "$expected" ]; then
        echo -e "${YELLOW}SKIP${NC} $test_name (no expected value)"
        ((SKIPPED++))
        continue
    fi

    # Compile
    if ! "$COMPILER" "$test_file" -o "$TMP_BIN" 2>/dev/null; then
        echo -e "${RED}FAIL${NC} $test_name (compilation error)"
        ((FAILED++))
        continue
    fi

    # Run through emulator with timeout
    output=$(timeout 3 "$EMULATOR" -l "$TMP_BIN" 2>&1 || true)

    # Extract the numeric result (last number in output)
    # The emulator prints the result followed by a newline
    actual=$(echo "$output" | grep -o '\-\?[0-9]*$' | tail -1)

    if [ -z "$actual" ]; then
        # Try to find any number in the output
        actual=$(echo "$output" | grep -o '\-\?[0-9]\+' | tail -1)
    fi

    if [ "$actual" == "$expected" ]; then
        echo -e "${GREEN}PASS${NC} $test_name (expected $expected, got $actual)"
        ((PASSED++))
    else
        echo -e "${RED}FAIL${NC} $test_name (expected $expected, got '$actual')"
        echo "       Output: $output"
        ((FAILED++))
    fi
done

# Cleanup
rm -f "$TMP_BIN"

echo
echo "===================="
echo -e "Results: ${GREEN}$PASSED passed${NC}, ${RED}$FAILED failed${NC}, ${YELLOW}$SKIPPED skipped${NC}"
echo

if [ $FAILED -gt 0 ]; then
    exit 1
fi
