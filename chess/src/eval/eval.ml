(* Adapted from:

   https://github.com/AndyGrant/Ethereal/blob/master/src/evaluate.c
*)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

let go pos =
  let info = Eval_info.create pos in
  Eval_main.pawn_and_king info;
  let eval = Eval_main.terms info in
  Eval_info.save_pkentry info;
  let score = Eval_phase.interpolate pos eval in
  Eval_main.tempo + match Position.active pos with
  | Piece.White -> score
  | Piece.Black -> -score
