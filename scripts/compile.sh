#!/bin/bash
# Copyright 2024-2025 Michael Petersen. All rights reserved.
# Use of this source code is governed by an Apache license that can be found in the LICENSE file.

set -e

gcc -no-pie -c -o build/standard.o standard/standard.c
gcc -no-pie -c -o build/out.o build/out.s
gcc -no-pie -c -o build/out.rt.o build/out.rt.s
gcc -no-pie -o build/out build/out.o build/out.rt.o build/standard.o