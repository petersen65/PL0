#!/bin/bash
# Copyright 2024-2025 Michael Petersen. All rights reserved.
# Use of this source code is governed by an Apache license that can be found in the LICENSE file.

set -e

# check for debug/release mode
MODE=${1:-debug}  # default to debug mode

if [ "$MODE" = "debug" ]; then
    CFLAGS="-std=c23 -ggdb -g3 -O0 -DDEBUG -fno-omit-frame-pointer"
    ASFLAGS="-ggdb"  # just debug info for assembly
    LDFLAGS="-ggdb -fno-omit-frame-pointer"  # debug info and frame pointers for linking
    echo "Compiling in debug mode with DWARF symbols"
elif [ "$MODE" = "release" ]; then
    CFLAGS="-std=c23 -O2 -DNDEBUG"
    ASFLAGS=""  # no special flags for release assembly
    LDFLAGS="-s"  # strip symbols in release mode
    echo "Compiling in release mode with optimizations"
else
    echo "Usage: $0 [debug|release]"
    exit 1
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
gcc -no-pie $CFLAGS -c -o $STANDARD_OBJ $STANDARD_SRC
gcc -no-pie $ASFLAGS -c -o $OUT_OBJ $OUT_ASM
gcc -no-pie $ASFLAGS -c -o $OUT_RT_OBJ $OUT_RT_ASM
gcc -no-pie $LDFLAGS -o $OUTPUT_BIN $OUT_OBJ $OUT_RT_OBJ $STANDARD_OBJ
echo "Compilation complete: $OUTPUT_BIN"