#include "stdio.h"

double putchard(double x) {
    putchar((char)x);
    fflush(stdout);
    return 0;
}

double printd(double x) {
    printf("%f\n", x);
    return 0;
}
