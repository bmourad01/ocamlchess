(* Adapted from:

   https://github.com/AndyGrant/Ethereal/blob/master/src/evaluate.c
*)

open Core_kernel [@@warning "-D"]

module Bb = Bitboard
module Pre = Precalculated
module Score = Eval_score

type score = Score.t

(* Branchless integer comparisons. *)

let[@inline] isign x = x asr (Caml.Sys.int_size - 1)
let[@inline] isign2 x = Bool.to_int (x > 0) - Bool.to_int (x < 0)

let[@inline] imax x y =
  let m = x - y in
  x - (m land (isign m))

let[@inline] imin x y =
  let m = x - y in
  y + (m land (isign m))

let[@inline] iabs x =
  let m = isign x in
  (x lxor m) + (m land 1)

(* Helpers. *)

let pawn_advance = function
  | Piece.White -> fun b -> Bb.(b << 8)
  | Piece.Black -> fun b -> Bb.(b >> 8)

let pawn_captures p = function
  | Piece.White -> Bb.((p << 7) - file_h, (p << 9) - file_a)
  | Piece.Black -> Bb.((p >> 7) - file_a, (p >> 9) - file_h)

let rammed ours theirs = function
  | Piece.White -> Bb.((theirs >> 8) & ours)
  | Piece.Black -> Bb.((theirs << 8) & ours)

let relative_rank = function
  | Piece.White -> Square.rank
  | Piece.Black -> fun sq -> 7 - Square.rank sq

let relative_square sq c =
  let r = relative_rank c sq in
  let sq = Square.with_rank_exn sq r in
  Square.to_int sq

let backmost = function
  | Piece.White -> Bb.first_set_exn
  | Piece.Black -> Bb.first_set_rev_exn

let square_fwd = function
  | Piece.White -> fun sq -> Square.(of_int_exn (to_int sq + 8))
  | Piece.Black -> fun sq -> Square.(of_int_exn (to_int sq - 8))

let third_rank = function
  | Piece.White -> Bb.rank_3
  | Piece.Black -> Bb.rank_6

let uget = Array.unsafe_get
let uset = Array.unsafe_set
