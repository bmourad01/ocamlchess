open Core_kernel [@@warning "-D"]
open Position_common

type t = {
  move          : Move.t;
  parent        : position;
  self          : position Lazy.t;
  capture       : Piece.kind Uopt.t;
  is_en_passant : bool;
  castle_side   : Cr.side Uopt.t;
} [@@deriving fields]

let self child = Lazy.force child.self

let same x y =
  same_hash  x.parent y.parent &&
  Move.equal x.move   y.move

let is_move child m = Move.(m = child.move)
let is_capture child = Uopt.is_some child.capture
let is_castle child = Uopt.is_some child.castle_side
let capture child = Uopt.to_option child.capture
let castle_side child = Uopt.to_option child.castle_side

let gives_check child =
  if Lazy.is_val child.self then in_check @@ self child
  else gives_check child.parent child.move

let capture_square child =
  if Uopt.is_none child.capture then None
  else Option.some @@
    let dst = Move.dst child.move in
    if not child.is_en_passant then dst
    else Fn.flip en_passant_pawn_aux dst @@ inactive @@ self child

let new_threats {move; parent = pos; _} =
  let c = active pos in
  let all = all_board pos in
  let them = Bb.(all - board_of_color pos c) in
  let major = Bb.(pos.rook + pos.queen) in
  let minor = Bb.(pos.knight + pos.bishop) in
  let src, dst, promote = Move.decomp move in
  match promote with
  | None -> begin
      match Piece.kind @@ piece_at_square_exn pos src with
      | Pawn   -> Bb.(Pre.pawn_capture dst c & them & (minor + major))
      | Knight -> Bb.(Pre.knight dst & them & major)
      | Bishop -> Bb.(Pre.(bishop dst all - bishop src all) & them & major)
      | Rook   -> Bb.(Pre.(rook dst all - rook src all) & them & pos.queen)
      | Queen  -> Bb.empty
      | King   -> Bb.empty
    end
  | Some k ->
    let p = Pre.pawn_capture src c in
    match k with
    | Knight -> Bb.((Pre.knight dst - p) & them & major)
    | Bishop -> Bb.((Pre.bishop dst all - p) & them & major)
    | Rook   -> Bb.((Pre.rook dst all - p) & them & pos.queen)
    | Queen  -> Bb.empty

let is_passed_pawn {move; parent; _} =
  let open Bb in
  let src = Move.src move in
  let dst = Move.dst move in
  let us = board_of_color parent parent.active in
  let mask = Pre.passed_pawns dst parent.active in
  let our_pawns = parent.pawn & us in
  let their_pawns = parent.pawn - our_pawns in
  src @ our_pawns && (their_pawns & mask) = empty
