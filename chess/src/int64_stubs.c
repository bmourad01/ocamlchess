#include <caml/mlvalues.h>

/* Cast two 64-bit numbers to 128-bit numbers, multiply them together, and grab
   the high 64 bits as the result.

   On an AMD64 machine, this is already computed for us in hardware:

      mov    rax,rdi
      mul    rsi
      mov    rax,rdx
      ret

   Where the upper 64 bits of the multiplication is stored in RDX.
*/

intnat ocamlchess_mul_hi64_unboxed(uint64_t a, uint64_t b) {
#if defined(IS_64BIT)
  return ((__uint128_t)a * (__uint128_t)b) >> 64;
#else
  uint64_t al = (uint32_t)a;
  uint64_t ah = a >> 32;
  uint64_t bl = (uint32_t)b;
  uint64_t bh = b >> 32;
  uint64_t c1 = (al * bl) >> 32;
  uint64_t c2 = ah * bl + c1;
  uint64_t c3 = al * bh + (uint32_t)c2;
  return ah * bh + (c2 >> 32) + (c3 >> 32);
#endif
}

CAMLprim value ocamlchess_mul_hi64(value a, value b) {
  return Val_int(ocamlchess_mul_hi64_unboxed(Int64_val(a), Int64_val(b)));
}
