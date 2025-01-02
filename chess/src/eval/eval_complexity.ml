(* Evaluate the how "complex" the position is. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

let total_pawns = 0 $ 8
let pawn_flanks = 0 $ 82
let pawn_endgame = 0 $ 76
let adjustment = 0 $ -157

let[@inline] calculate eval complexity =
  let ceg = Score.eg complexity in
  let eg = Score.eg eval in
  let lb = -(iabs eg) in
  let s = isign2 eg in
  0 $ s * imax ceg lb

let[@inline] go pos eval =
  let open Bb in
  let pawn = Position.pawn pos in
  let both_flanks =
    (pawn & (file_a + file_b + file_c + file_d)) <> empty &&
    (pawn & (file_e + file_f + file_g + file_h)) <> empty in
  let npw = Position.has_non_pawn_material pos White in
  let npb = Position.has_non_pawn_material pos Black in
  let base =
    (total_pawns  ** count pawn)                     +$
    (pawn_flanks  ** Bool.to_int both_flanks)        +$
    (pawn_endgame ** Bool.to_int (not (npw || npb))) in
  calculate eval (base +$ adjustment)
