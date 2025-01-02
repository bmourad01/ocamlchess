(* The evaluation function. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

let tempo = 20

let[@inline] pawn_and_king info =
  if Option.is_none info.Eval_info.pkentry then begin
    Eval_pawn.go info White;
    Eval_pawn.go info Black;
    Eval_pawn_king.go info White;
    Eval_pawn_king.go info Black;
  end

let[@inline] terms ?(init = 0 $ 0) info =
  let eval = init +$ (Eval_knight.go info White -$ Eval_knight.go info Black) in
  let eval = eval +$ (Eval_bishop.go info White -$ Eval_bishop.go info Black) in
  let eval = eval +$ (  Eval_rook.go info White -$   Eval_rook.go info Black) in
  let eval = eval +$ ( Eval_queen.go info White -$  Eval_queen.go info Black) in
  let eval = eval +$ (  Eval_king.go info White -$   Eval_king.go info Black) in
  let eval = eval +$ (Eval_passed.go info White -$ Eval_passed.go info Black) in
  let eval = eval +$ (Eval_threat.go info White -$ Eval_threat.go info Black) in
  let eval = eval +$ ( Eval_space.go info White -$  Eval_space.go info Black) in
  let eval = eval +$ Eval_info.pawn_king_eval info in
  let eval = eval +$ Eval_psqt.go info.pos in
  let eval = eval +$ Eval_closedness.go info in
  let eval = eval +$ Eval_complexity.go info.pos eval in
  eval
