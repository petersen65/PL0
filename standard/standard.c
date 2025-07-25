/**
 * pl0_io.c
 *
 * C23-based I/O routines for the PL/0 standard library.
 * These functions leverage the standard C library (GCC 15+) and the
 * System V AMD64 ABI under Linux to perform basic integer and UTF-32
 * string I/O with minimal dependencies.
 */

#include <stdio.h>      // printf, scanf, fwrite
#include <inttypes.h>   // PRId64, SCNd64
#include <uchar.h>      // char32_t, c32rtomb
#include <locale.h>     // setlocale, LC_CTYPE
#include <string.h>     // memset
#include <limits.h>     // MB_LEN_MAX

/**
 * pl0_write - Write a signed 64-bit integer to stdout in decimal format.
 * @value: The int64_t value to write.
 *
 * Relies on printf and the C runtime; no manual syscall.
 */
void pl0_write(int64_t value) {
    // Use PRId64 for portable int64 formatting.
    printf("%" PRId64 "\n", value);
}

/**
 * pl0_read - Read a signed 64-bit integer from stdin.
 *
 * Skips leading whitespace, parses optional '+' or '-' sign,
 * and reads decimal digits. If input fails or no digits are found,
 * returns 0.
 *
 * Returns: The parsed int64_t value.
 */
int64_t pl0_read(void) {
    int64_t value = 0;
    // SCNd64 specifier consumes whitespace and parses sign + digits.
    if (scanf("%" SCNd64, &value) != 1) {
        return 0;
    }
    return value;
}

/**
 * pl0_print - Write a UTF-32 zero-terminated string to stdout as UTF-8.
 * @s: Pointer to a NUL-terminated array of char32_t (UTF-32 codepoints).
 *
 * Converts each UTF-32 codepoint to its multibyte (UTF-8) sequence using
 * c32rtomb and writes via fwrite. Relies on the current C locale for
 * correct encoding—call setlocale(LC_CTYPE, "") at program start if needed.
 */
void pl0_print(const char32_t *s) {
    // Ensure multibyte conversion uses correct locale settings.
    // Caller should set locale appropriately, e.g., setlocale(LC_CTYPE, "");
    mbstate_t state;
    memset(&state, 0, sizeof(state));

    // Buffer to hold resulting UTF-8 bytes; MB_LEN_MAX from <limits.h>.
    char buf[MB_LEN_MAX];

    // Process each UTF-32 codepoint until NUL.
    while (*s) {
        // Convert to multibyte; returns length or (size_t)-1 on error.
        size_t len = c32rtomb(buf, *s++, &state);
        if (len == (size_t)-1) {
            // On conversion error, reset state and skip this codepoint.
            memset(&state, 0, sizeof(state));
            continue;
        }
        // Write the UTF-8 sequence to stdout.
        fwrite(buf, 1, len, stdout);
    }
}

// gcc -no-pie -c -o build/standard.o stdlib/standard.c
// gcc -no-pie -c -o build/out.o build/out.s
// gcc -no-pie -c -o build/out.rt.o build/out.rt.s
// gcc -no-pie -o build/out build/out.o build/out.rt.o build/standard.o