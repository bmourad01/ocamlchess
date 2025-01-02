open Core_kernel [@@warning "-D"]
open Position_common

let make_move parent move =
  let src, dst, promote = Move.decomp move in
  let self, capture, is_en_passant, castle_side =
    let piece = piece_at_square_uopt parent src in
    if Uopt.is_none piece then
      lazy (copy parent), Uopt.none, false, Uopt.none
    else
      let piece = Uopt.unsafe_value piece in
      let en_passant_pawn =
        if Uopt.is_some parent.en_passant then
          let ep = Uopt.unsafe_value parent.en_passant in
          if has_pawn_threat parent ep
          then Uopt.some @@ en_passant_pawn_aux parent.active ep
          else Uopt.none
        else Uopt.none in
      Position_movegen.run_makemove parent
        ~src ~dst ~promote ~piece ~en_passant_pawn in
  Position_child.Fields.create
    ~move ~parent ~self ~capture ~is_en_passant ~castle_side

let[@inline] null_move pos =
  let pos = copy pos in
  Position_makemove.flip_active pos;
  Position_makemove.update_hash pos ~f:(Position_hash.Update.en_passant pos);
  set_en_passant pos Uopt.none;
  set_halfmove pos 0;
  pos

let[@inline] is_en_passant pos m =
  is_en_passant_square pos (Move.dst m) &&
  let p = piece_at_square_uopt pos (Move.src m) in
  Uopt.is_some p &&
  let p = Uopt.unsafe_value p in
  Piece.is_pawn p &&
  Piece.(Color.equal (color p) pos.active)

let[@inline] is_capture pos m =
  let open Bb.Syntax in
  (Move.src m @ active_board pos &&
   Move.dst m @ inactive_board pos) ||
  is_en_passant pos m

let is_castle pos m =
  let src = Move.src m in
  let p = piece_at_square_uopt pos src in
  Uopt.is_some p &&
  let c, k = Piece.decomp @@ Uopt.unsafe_value p in
  Piece.Color.(c = pos.active) &&
  Piece.Kind.(k = King) &&
  is_castle src (Move.dst m) c
