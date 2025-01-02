(* Safety evaluation of pieces, notably king safety. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

(* These are for piece-specific evaluations, to be later used in the
   king safety evaluation. *)
let knight = 48 $ 41
let bishop = 24 $ 35
let rook = 37 $ 8
let queen = 30 $ 6

(* King safety. *)
let attack_value = 45 $ 34
let weak_squares = 42 $ 41
let no_enemy_queens = -237 $ -259
let safe_queen_check = 93 $ 83
let safe_rook_check = 90 $ 98
let safe_bishop_check = 59 $ 59
let safe_knight_check = 112 $ 117
let adjustment = -74 $ -26

module Shelter = struct
  let t = [|
    (* Different file. *)
    -2 $ 7;
    -1 $ 13;
    0 $ 8;
    4 $ 7;
    6 $ 2;
    -1 $ 0;
    2 $ 0;
    0 $ -13;
    (* Same file. *)
    0 $ 0;
    -2 $ 13;
    -2 $ 9;
    4 $ 5;
    3 $ 1;
    -3 $ 0;
    -2 $ 0;
    -1 $ -9;
  |]

  let get same_file file =
    let i = file + (Bool.to_int same_file * Square.File.count) in
    uget t i
end

module Storm = struct
  let t = [|
    (* Not blocked. *)
    -4 $ -1;
    -8 $ 3;
    0 $ 5;
    1 $ -1;
    3 $ 6;
    -2 $ 20;
    -2 $ 18;
    2 $ -12;
    (* Blocked. *)
    0 $ 0;
    1 $ 0;
    -1 $ 4;
    0 $ 0;
    0 $ 5;
    -1 $ 1;
    1 $ 0;
    1 $ 0;
  |]

  let get blocked rank =
    let i = rank + (Bool.to_int blocked * Square.Rank.count) in
    uget t i
end
