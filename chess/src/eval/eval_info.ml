(* Common info used by the evaluation features. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

type info = {
  pawn_attacks               : Bb.t;
  pawn_attacks2              : Bb.t;
  rammed_pawns               : Bb.t;
  blocked_pawns              : Bb.t;
  king_square                : Square.t;
  king_area                  : Bb.t;
  mobility_area              : Bb.t;
  no_bishop                  : Bb.t;
  no_rook                    : Bb.t;
  attacked_by                : Bb.t array;
  mutable passed_pawns       : Bb.t;
  mutable attacks            : Bb.t;
  mutable attacks2           : Bb.t;
  mutable num_king_attacks   : int;
  mutable num_king_attackers : int;
  mutable king_attackers     : score;
  mutable pawn_king_eval     : score;
  mutable pawn_king_safety   : score;
}

let extract c = function
  | None -> (0 $ 0), (0 $ 0), Bb.empty
  | Some (e : Eval_pkcache.entry) -> match c with
    | Piece.White -> e.weval, e.wsafety, e.wpassed
    | Piece.Black -> e.beval, e.bsafety, e.bpassed

let create_info pos c pke =
  let open Bb in
  let c' = Piece.Color.opposite c in
  let all = Position.all_board pos in
  let us = Position.board_of_color pos c in
  let them = all - us in
  let pawn = Position.pawn pos in
  let our_king = Position.king pos & us in
  let our_queens = Position.queen pos & us in
  let our_pawns = pawn & us in
  let their_pawns = pawn & them in
  let pawn_left, pawn_right = pawn_captures our_pawns c in
  let pawn_left', pawn_right' = pawn_captures their_pawns c' in
  let blocked_pawns = rammed our_pawns all c in
  let their_pawn_attacks = pawn_left' + pawn_right' in
  let king_square = Bb.first_set_exn our_king in
  let king_attacks = Pre.king king_square in
  let pawn_king_eval, pawn_king_safety, passed_pawns = extract c pke in
  let attacked_by = Array.create Bb.empty ~len:Piece.Kind.count in
  let pawn_attacks = pawn_left + pawn_right in
  let pawn_attacks2 = pawn_left & pawn_right in
  attacked_by.(Piece.Kind.pawn) <- pawn_attacks;
  attacked_by.(Piece.Kind.king) <- king_attacks; {
    pawn_attacks;
    pawn_attacks2;
    rammed_pawns = rammed our_pawns their_pawns c;
    blocked_pawns;
    king_square;
    king_area = Pre.king_area king_square c;
    mobility_area = ~~(their_pawn_attacks + our_king + blocked_pawns);
    no_bishop = all ^ ((Position.bishop pos & us) + our_queens);
    no_rook = all ^ ((Position.rook pos & us) + our_queens);
    attacked_by;
    passed_pawns;
    attacks = pawn_attacks + king_attacks;
    attacks2 = pawn_attacks2 + (pawn_attacks & king_attacks);
    num_king_attacks = 0;
    num_king_attackers = 0;
    king_attackers = 0 $ 0;
    pawn_king_eval;
    pawn_king_safety;
  }

let attacked_by info k =
  uget info.attacked_by @@ Piece.Kind.to_int k

let update_attacks info a k =
  let open Bb.Syntax in
  let k = Piece.Kind.to_int k in
  let ak = uget info.attacked_by k in
  uset info.attacked_by k (a + ak);
  info.attacks2 <- info.attacks2 + (a & info.attacks);
  info.attacks <- info.attacks + a

let update_king_safety info a w =
  let a = Bb.(a & (info.king_area - info.pawn_attacks2)) in
  if Bb.(a <> empty) then begin
    info.num_king_attacks <- info.num_king_attacks + Bb.count a;
    info.num_king_attackers <- info.num_king_attackers + 1;
    info.king_attackers <- info.king_attackers +$ w;
  end

type t = {
  pos     : Position.t;
  white   : info;
  black   : info;
  pkentry : Eval_pkcache.entry option;
}

let create pos =
  let pke = Eval_pkcache.find pos in {
    pos;
    white = create_info pos White pke;
    black = create_info pos Black pke;
    pkentry = pke;
  }

let color t = function
  | Piece.White -> t.white
  | Piece.Black -> t.black

let pawn_king_eval t =
  t.white.pawn_king_eval -$ t.black.pawn_king_eval

let save_pkentry t = match t.pkentry with
  | Some _ -> ()
  | None ->
    let open Eval_pkcache in
    let key = Position.pawn_king_hash t.pos in
    Option_array.unsafe_set_some table (slot key) {
      key;
      weval = t.white.pawn_king_eval;
      beval = t.black.pawn_king_eval;
      wsafety = t.white.pawn_king_safety;
      bsafety = t.black.pawn_king_safety;
      wpassed = t.white.passed_pawns;
      bpassed = t.black.passed_pawns;
    }
