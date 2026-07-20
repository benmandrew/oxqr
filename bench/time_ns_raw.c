#define _POSIX_C_SOURCE 200809L
#include <time.h>
#include <caml/mlvalues.h>

/* Returns CLOCK_MONOTONIC nanoseconds as an unboxed OCaml int.
   Current epoch (~1.75e18 ns) fits in a 63-bit signed int (max 4.6e18).
   [@@noalloc] on the OCaml side: we never call into the runtime here. */
value bench_time_ns(value unit)
{
    (void)unit;
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return Val_long((long long)ts.tv_sec * 1000000000LL + ts.tv_nsec);
}
