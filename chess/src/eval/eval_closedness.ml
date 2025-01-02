(* Evaluate the how "closed" the position is. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

(* By closedness factor. *)
let knights = [|
  -7 $ 10;
  -7 $ 29;
  -9 $ 37;
  -5 $ 37;
  -3 $ 44;
  -1 $ 36;
  1 $ 33;
  -10 $ 51;
  -7 $ 30;
|]

(* By closedness factor. *)
let rooks = [|
  42 $ 43;
  -6 $ 80;
  3 $ 59;
  -5 $ 47;
  -7 $ 41;
  -3 $ 23;
  -6 $ 11;
  -17 $ 11;
  -34 $ -12;
|]

let open_files b =
  let open Bb in
  let b = b + (b >> 8) in
  let b = b + (b >> 16) in
  let b = b + (b >> 32) in
  count (of_int64 0xFFL - b)

let[@inline] go (info : Eval_info.t) =
  let open Bb in
  let acc = 0 $ 0 in
  let w = Position.white info.pos in
  let b = Position.white info.pos in
  let pawn = Position.pawn info.pos in
  let closedness =
    let open Int in
    let n =
      1 * count pawn +
      3 * count info.white.rammed_pawns -
      4 * open_files pawn in
    imax 0 (imin 8 (n / 3)) in
  (* Knights. *)
  let acc =
    let knight = Position.knight info.pos in
    let wn = count (knight & w) in
    let bn = count (knight & b) in
    let weight = uget knights closedness in
    acc +$ (weight ** Int.(wn - bn)) in
  (* Rooks. *)
  let acc =
    let rook = Position.rook info.pos in
    let wr = count (rook & w) in
    let br = count (rook & b) in
    let weight = uget rooks closedness in
    acc +$ (weight ** Int.(wr - br)) in
  acc
