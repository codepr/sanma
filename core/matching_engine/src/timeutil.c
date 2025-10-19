#include <time.h>

int64_t current_millis(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);

    return ts.tv_sec * 1e3 + ts.tv_nsec / 1e6;
}
