#include <stdio.h>
#include <stdint.h>

int64_t pl0_read(void) {
    int64_t val;
    if (scanf("%ld", &val) != 1) {
        return 0; // fallback or error handling
    }
    return val;
}

void pl0_write(int64_t val) {
    printf("Value is: %ld\n", val);
}

// gcc -no-pie -c -o build/pl0_stdlib.o stdlib/pl0_stdlib.c
// gcc -no-pie -c -o build/out.o build/out.s
// gcc -no-pie -c -o build/out.rt.o build/out.rt.s
// gcc -no-pie -o build/out build/out.o build/out.rt.o build/pl0_stdlib.o