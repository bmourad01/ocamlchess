#include <caml/mlvalues.h>

intnat ml_int64_popcount_unboxed(int64_t v) { return __builtin_popcountll(v); }

CAMLprim value ml_int64_popcount(value v) {
  return Val_int(ml_int64_popcount_unboxed(Int64_val(v)));
}
