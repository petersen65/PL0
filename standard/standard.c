// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in
// the LICENSE file.

// The standard library for PL/0 provides basic I/O functions written in C23.

#include <inttypes.h> // PRId64, SCNd64
#include <stdint.h>   // int64_t, INT64_MIN
#include <stdio.h>    // printf, scanf

/// @brief Read a signed 64-bit integer from stdin.
/// @param None
/// @return The parsed int64_t value or INT64_MIN on failure.
/// @details Skips leading whitespace, parses optional '+' or '-' sign, and reads decimal digits.
int64_t pl0_read(void) {
  int64_t value = 0;

  // SCNd64 specifier consumes whitespace and parses sign + digits
  if (scanf("%" SCNd64, &value) != 1) {
    return INT64_MIN;
  }

  return value;
}

/// @brief Write a signed 64-bit integer to stdout.
/// @param value The int64_t value to write.
/// @details Uses printf with the PRId64 format specifier for portability.
void pl0_write(int64_t value) {
  // use PRId64 for portable int64 formatting
  printf("%" PRId64 "\n", value);
}
