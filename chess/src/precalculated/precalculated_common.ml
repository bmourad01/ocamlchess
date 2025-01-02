module Bb = Bitboard
module Cr = Castling_rights

let clz = Ocaml_intrinsics.Int64.count_leading_zeros
let ctz = Ocaml_intrinsics.Int64.count_trailing_zeros

let msb x = Square.last - clz x
let lsb = ctz

module Shift = Precalculated_shift
module Magic = Precalculated_magic
module Simple = Precalculated_simple
module Mask = Precalculated_mask
