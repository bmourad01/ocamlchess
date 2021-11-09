open Core
open Base

let bits = 6

module T = struct type t = int [@@deriving compare, equal, hash, sexp] end
include T
include Comparable.Make (T)

let of_int_exn i =
  if i < 0 || i > 63 then
    invalid_arg (sprintf "Invalid square integer '%d'" i)
  else i

let of_int i = Option.try_with (fun () -> of_int_exn i)
let to_int = ident

let of_rank_and_file_exn ~rank ~file =
  if rank < 0 || rank > 7 then
    invalid_arg (sprintf "Invalid rank index '%d'" rank)
  else if file < 0 || file > 7 then
    invalid_arg (sprintf "Invalid file index '%d'" file)
  else (rank lsl 3) lor file

let of_rank_and_file ~rank ~file =
  Option.try_with (fun () -> of_rank_and_file_exn ~rank ~file)

module Bits = struct
  (* Valid squares *)

  let a1 = 0b000_000
  let b1 = 0b000_001
  let c1 = 0b000_010
  let d1 = 0b000_011
  let e1 = 0b000_100
  let f1 = 0b000_101
  let g1 = 0b000_110
  let h1 = 0b000_111
  let a2 = 0b001_000
  let b2 = 0b001_001
  let c2 = 0b001_010
  let d2 = 0b001_011
  let e2 = 0b001_100
  let f2 = 0b001_101
  let g2 = 0b001_110
  let h2 = 0b001_111
  let a3 = 0b010_000
  let b3 = 0b010_001
  let c3 = 0b010_010
  let d3 = 0b010_011
  let e3 = 0b010_100
  let f3 = 0b010_101
  let g3 = 0b010_110
  let h3 = 0b010_111
  let a4 = 0b011_000
  let b4 = 0b011_001
  let c4 = 0b011_010
  let d4 = 0b011_011
  let e4 = 0b011_100
  let f4 = 0b011_101
  let g4 = 0b011_110
  let h4 = 0b011_111
  let a5 = 0b100_000
  let b5 = 0b100_001
  let c5 = 0b100_010
  let d5 = 0b100_011
  let e5 = 0b100_100
  let f5 = 0b100_101
  let g5 = 0b100_110
  let h5 = 0b100_111
  let a6 = 0b101_000
  let b6 = 0b101_001
  let c6 = 0b101_010
  let d6 = 0b101_011
  let e6 = 0b101_100
  let f6 = 0b101_101
  let g6 = 0b101_110
  let h6 = 0b101_111
  let a7 = 0b110_000
  let b7 = 0b110_001
  let c7 = 0b110_010
  let d7 = 0b110_011
  let e7 = 0b110_100
  let f7 = 0b110_101
  let g7 = 0b110_110
  let h7 = 0b110_111
  let a8 = 0b111_000
  let b8 = 0b111_001
  let c8 = 0b111_010
  let d8 = 0b111_011
  let e8 = 0b111_100
  let f8 = 0b111_101
  let g8 = 0b111_110
  let h8 = 0b111_111

  (* Extract the bits *)

  let rank sq = sq lsr 3
  let file sq = sq land 0b111
end

let rank_char =
  let ranks = "12345678" in
  fun sq -> ranks.[Bits.rank sq]

let file_char =
  let files = "abcdefgh" in
  fun sq -> files.[Bits.file sq]

let of_string =
  let tbl =
    Hashtbl.of_alist_exn
      (module String)
      Bits.
        [ ("a1", a1); ("b1", b1); ("c1", c1); ("d1", d1); ("e1", e1)
        ; ("f1", f1); ("g1", g1); ("h1", h1); ("a2", a2); ("b2", b2)
        ; ("c2", c2); ("d2", d2); ("e2", e2); ("f2", f2); ("g2", g2)
        ; ("h2", h2); ("a3", a3); ("b3", b3); ("c3", c3); ("d3", d3)
        ; ("e3", e3); ("f3", f3); ("g3", g3); ("h3", h3); ("a4", a4)
        ; ("b4", b4); ("c4", c4); ("d4", d4); ("e4", e4); ("f4", f4)
        ; ("g4", g4); ("h4", h4); ("a5", a5); ("b5", b5); ("c5", c5)
        ; ("d5", d5); ("e5", e5); ("f5", f5); ("g5", g5); ("h5", h5)
        ; ("a6", a6); ("b6", b6); ("c6", c6); ("d6", d6); ("e6", e6)
        ; ("f6", f6); ("g6", g6); ("h6", h6); ("a7", a7); ("b7", b7)
        ; ("c7", c7); ("d7", d7); ("e7", e7); ("f7", f7); ("g7", g7)
        ; ("h7", h7); ("a8", a8); ("b8", b8); ("c8", c8); ("d8", d8)
        ; ("e8", e8); ("f8", f8); ("g8", g8); ("h8", h8) ] in
  Hashtbl.find tbl

let of_string_exn s =
  match of_string s with
  | None -> invalid_arg (sprintf "Invalid square string '%s'" s)
  | Some sq -> sq

let to_string sq = sprintf "%c%c" (file_char sq) (rank_char sq)

include Bits
