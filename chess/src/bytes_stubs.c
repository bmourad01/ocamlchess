#include <caml/alloc.h>
#include <caml/mlvalues.h>

/* The system C compiler is usually smart enough to turn this code into
   a normal memory read, followed by a byte swap (e.g. `bswap` on x86)
   if the native arch is little-endian. */

CAMLprim value ml_bytes_int64_be(value v, value pos) {
  uint8_t *b = Bytes_val(v) + Int_val(pos);
  uint64_t result = ((uint64_t)b[0] << 56) | ((uint64_t)b[1] << 48) |
                    ((uint64_t)b[2] << 40) | ((uint64_t)b[3] << 32) |
                    ((uint64_t)b[4] << 24) | ((uint64_t)b[5] << 16) |
                    ((uint64_t)b[6] << 8)  | ((uint64_t)b[7]);
  return caml_copy_int64(result);
}

CAMLprim value ml_bytes_int32_be(value v, value pos) {
  uint8_t *b = Bytes_val(v) + Int_val(pos);
  uint32_t result = ((uint32_t)b[0] << 24) | ((uint32_t)b[1] << 16) |
                    ((uint32_t)b[2] << 8)  | ((uint32_t)b[3]);
  return caml_copy_int32(result);
}

CAMLprim value ml_bytes_int16_be(value v, value pos) {
  uint8_t *b = Bytes_val(v) + Int_val(pos);
  uint16_t result = ((uint16_t)b[0] << 8) | ((uint16_t)b[1]);
  return Val_int(result);
}
