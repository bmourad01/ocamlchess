(* Evaluation of rook placement. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

let open_file = 10 $ 9
let semi_open_file = 34 $ 8
let seventh_rank = -1 $ 42

(* By number of attacked squares in our mobility area *)
let mobility = [|
  -127 $ -148;
  -56 $ -127;
  -25 $ -85;
  -12 $ -28;
  -10 $ 2;
  -12 $ 27;
  -11 $ 42;
  -4 $ 46;
  4 $ 52;
  9 $ 55;
  11 $ 64;
  19 $ 68;
  19 $ 73;
  37 $ 60;
  97 $ 15;
|]

let[@inline] go info c =
  let open Bb in
  let c' = Piece.Color.opposite c in
  let our_info = Eval_info.color info c in
  let their_info = Eval_info.color info c' in
  let all = Position.all_board info.pos in
  let us = Position.board_of_color info.pos c in
  let them = all - us in
  let pawn = Position.pawn info.pos in
  let rook = Position.rook info.pos in
  let our_pawns = pawn & us in
  let our_rooks = rook & us in
  let their_pawns = pawn & them in
  let relr = relative_rank c in
  let kr6 = Int.(relr their_info.king_square >= Square.Rank.seven) in
  Bb.fold our_rooks ~init:(0 $ 0) ~f:(fun acc sq ->
      let att = Pre.rook sq our_info.no_rook in
      Eval_info.update_attacks our_info att Rook;
      (* Open or semi-open file. *)
      let acc =
        let f = file_exn @@ Square.file sq in
        if (our_pawns & f) = empty then
          if (their_pawns & f) <> empty
          then acc +$ semi_open_file
          else acc +$ open_file
        else acc in
      (* Seventh rank. *)
      let acc =
        if kr6 && Int.((relr sq) = Square.Rank.seven)
        then acc +$ seventh_rank else acc in
      (* Mobility. *)
      let acc =
        let c = count (our_info.mobility_area & att) in
        acc +$ uget mobility c in
      (* King safety. *)
      Eval_info.update_king_safety their_info att Eval_safety.rook;
      acc)
