(* Evaluate the spaces on the board itself. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

let restrict_occupied = -4 $ -1
let restrict_empty = -4 $ -2
let center_control = 3 $ 0

let[@inline] go info c =
  let open Bb in
  let acc = 0 $ 0 in
  let c' = Piece.Color.opposite c in
  let our_info = Eval_info.color info c in
  let their_info = Eval_info.color info c' in
  let all = Position.all_board info.pos in
  let us = Position.board_of_color info.pos c in
  let uncontrolled =
    their_info.attacks2 &
    our_info.attacks -
    our_info.attacks2 -
    Eval_info.attacked_by our_info Pawn in
  (* Occupied squares. *)
  let acc = acc +$ (restrict_occupied ** count (uncontrolled & all)) in
  (* Empty squares. *)
  let acc = acc +$ (restrict_empty ** count (uncontrolled - all)) in
  (* Uncontested central squares. *)
  let acc =
    let knight = Position.knight info.pos in
    let bishop = Position.bishop info.pos in
    let rook = Position.rook info.pos in
    let queen = Position.queen info.pos in
    let minor = count (knight + bishop) in
    let major = count (rook + queen) in
    if Int.((minor + 2 * major) > 12) then
      let b = ~~(their_info.attacks) & (our_info.attacks + us) & bigcenter in
      acc +$ (center_control ** count b)
    else acc in
  acc
