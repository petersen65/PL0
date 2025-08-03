#!/bin/bash
# Copyright 2024-2025 Michael Petersen. All rights reserved.
# Use of this source code is governed by an Apache license that can be found in the LICENSE file.

set -e

# check for debug/release mode
MODE=${1:-debug}  # default to debug mode

if [[ "$MODE" = "debug" ]]; then
    CFLAGS="-std=c23 -m64 -ggdb -g3 -O0 -DDEBUG -fno-omit-frame-pointer"
    ASFLAGS="-m64 -ggdb"  # 64-bit assembly with debug info
    LDFLAGS="-m64 -ggdb -fno-omit-frame-pointer"  # 64-bit linking with debug info
    echo "Compiling in debug mode with DWARF symbols"
elif [[ "$MODE" = "release" ]]; then
    CFLAGS="-std=c23 -m64 -O2 -DNDEBUG"
    ASFLAGS="-m64"  # 64-bit assembly
    LDFLAGS="-m64 -s"  # 64-bit linking, strip symbols in release mode
    echo "Compiling in release mode with optimizations"
else
    echo "Usage: $0 [debug|release]"
    exit 1
fi

# check for GCC-15 and 64-bit target
echo "Verifying GCC-15 and 64-bit target"
if ! command -v gcc-15 &> /dev/null; then
    echo "Error: gcc-15 not found"
    exit 1
fi

# check if GCC-15 supports 64-bit
GCC_MACHINE=$(gcc-15 -dumpmachine)
echo "GCC-15 target: $GCC_MACHINE"

if [[ ! "$GCC_MACHINE" == *"x86_64"* ]]; then
    echo "Warning: GCC-15 may not be configured for x86_64"
fi

# file locations
BUILD_DIR="build"
STANDARD_SRC="standard/standard.c"
STANDARD_OBJ="$BUILD_DIR/standard.o"
OUT_ASM="$BUILD_DIR/out.s"
OUT_OBJ="$BUILD_DIR/out.o"
OUT_RT_ASM="$BUILD_DIR/out.rt.s"
OUT_RT_OBJ="$BUILD_DIR/out.rt.o"
OUTPUT_BIN="$BUILD_DIR/out"

# perform C and assembly compilation with a final linking step
mkdir --parents $BUILD_DIR  # ensure build directory exists
gcc-15 -no-pie $CFLAGS -c -o $STANDARD_OBJ $STANDARD_SRC
gcc-15 -no-pie $ASFLAGS -c -o $OUT_OBJ $OUT_ASM
gcc-15 -no-pie $ASFLAGS -c -o $OUT_RT_OBJ $OUT_RT_ASM
gcc-15 -no-pie $LDFLAGS -o $OUTPUT_BIN $OUT_OBJ $OUT_RT_OBJ $STANDARD_OBJ
echo "Compilation complete: $OUTPUT_BIN"