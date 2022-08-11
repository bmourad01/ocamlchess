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
  return ((__uint128_t)a * (__uint128_t)b) >> 64;
}

CAMLprim value ocamlchess_mul_hi64(value a, value b) {
  return Val_int(
      ocamlchess_mul_hi64_unboxed(Int64_val(a), (uint64_t)Int_val(b)));
}
