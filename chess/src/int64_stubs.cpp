#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <random>

/* Bitwise intrinsics */

extern "C" {

intnat ml_int64_popcount_unboxed(int64_t v) { return __builtin_popcountll(v); }

CAMLprim value ml_int64_popcount(value v) {
  return Val_int(ml_int64_popcount_unboxed(Int64_val(v)));
}

} // extern "C"

/* Randomness */

static std::mt19937_64 &rng() {
  static std::mt19937_64 m(std::random_device{}());
  return m;
}

extern "C" {

value ml_int64_random_seed(value seed) {
  CAMLparam1(seed);
  rng().seed(Int64_val(seed));
  CAMLreturn(Val_unit);
}

value ml_int64_random(value low, value high) {
  CAMLparam2(low, high);

  auto dist =
      std::uniform_int_distribution<int64_t>(Int64_val(low), Int64_val(high));

  CAMLreturn(copy_int64(dist(rng())));
}

} // extern "C"
