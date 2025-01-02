(* Pawn and king placements change less often than other pieces, so we
   can cache the related information.

   See: https://www.chessprogramming.org/Pawn_Hash_Table
*)

open Core_kernel [@@warning "-D"]
open Eval_common

type entry = {
  key     : Zobrist.key;
  weval   : score;
  beval   : score;
  wsafety : score;
  bsafety : score;
  wpassed : Bb.t;
  bpassed : Bb.t;
}

let len = 1 lsl 18
let table = Option_array.create ~len
let same e k = Zobrist.equal_key e.key k

external mul_hi64 :
  (int64[@unboxed]) ->
  (int64[@unboxed]) ->
  (int [@untagged]) =
  "ocamlchess_mul_hi64" "ocamlchess_mul_hi64_unboxed" [@@noalloc]

let slot k = mul_hi64 k @@ Int64.of_int len

let find pos =
  let k = Position.pawn_king_hash pos in
  slot k |> Option_array.unsafe_get table |>
  Option.bind ~f:(fun e -> Option.some_if (same e k) e)
