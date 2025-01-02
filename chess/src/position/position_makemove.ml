(* Handling moves.

   This is likely to be the biggest source of bugs, as it is quite easy to
   incorrectly implement the rules of chess.

   NOTE: We originally used a state monad for the Makemove module, and a reader
   monad for the Movegen module. For performance reasons, we then switched to a
   reader monad for Makemove, with mutable fields in the position datatype.
   This pattern acted as a "pseudo" state monad. However, we've gotten rid of
   the monadic code entirely since the compiler had trouble with inlining and
   specialization, as there were a lot of calls to anonymous functions left in
   the compiled code. While the monadic style was much more concise and elegant,
   our priority for this code weighs far more in favor of performance.
*)

open Core_kernel [@@warning "-D"]
open Position_common

(* Collection of precomputed info that is useful for generating the new
   position. We have the following fields:

   - en_passant: the en passant aquare. This field is only valid if
     the square is threatened with capture.

   - en_passant_pawn: the pawn that is "in front" of the en passant
     square. This field is only valid if the move we are making is in
     fact an en passant capture.

   - castle: the side on which castling occurred, if any.

   - piece: the piece being moved (which belongs to the active color).

   - direct_capture: the inactive piece, if any, that will be captured
     by the "direct" move (e.g. it is not an en passant capture).
*)
type info = {
  en_passant      : Square.t Uopt.t;
  en_passant_pawn : Square.t Uopt.t;
  castle_side     : Cr.side Uopt.t;
  piece           : Piece.t;
  direct_capture  : Piece.t Uopt.t;
} [@@deriving fields]

let[@inline] update_hash pos ~f = set_hash pos @@ f pos.hash

let[@inline] update_pawn_king_hash sq c k pos =
  set_pawn_king_hash pos @@ Position_hash.Update.piece c k sq pos.pawn_king_hash

let[@inline] map_color c pos ~f = match c with
  | Piece.White -> set_white pos @@ f @@ white pos
  | Piece.Black -> set_black pos @@ f @@ black pos

let[@inline] map_kind k pos ~f = match k with
  | Piece.Pawn   -> set_pawn pos   @@ f @@ pawn pos
  | Piece.Knight -> set_knight pos @@ f @@ knight pos
  | Piece.Bishop -> set_bishop pos @@ f @@ bishop pos
  | Piece.Rook   -> set_rook pos   @@ f @@ rook pos
  | Piece.Queen  -> set_queen pos  @@ f @@ queen pos
  | Piece.King   -> set_king pos   @@ f @@ king pos

(* Helper for setting both the color and the kind fields of the board. *)
let[@inline] map_piece p sq pos ~f = 
  let c, k = Piece.decomp p in
  map_color c pos ~f;
  map_kind k pos ~f;
  update_hash pos ~f:(Position_hash.Update.piece c k sq);
  begin match k with
    | Piece.Pawn | Piece.King -> update_pawn_king_hash sq c k pos
    | _ -> ()
  end;
  k

let set = Fn.flip Bb.set
let clr = Fn.flip Bb.clear

let[@inline] set_square p sq pos = map_piece p sq pos ~f:(set sq) |> ignore
let[@inline] clear_square p sq pos = map_piece p sq pos ~f:(clr sq) |> ignore

(* Assume that if `p` exists, then it occupies square `sq`. *)
let[@inline] clear_square_capture sq direct_capture pos =
  if Uopt.is_some direct_capture then
    let p = Uopt.unsafe_value direct_capture in
    map_piece p sq pos ~f:(clr sq) |> ignore

(* The halfmove clock is reset after captures and pawn moves, and incremented
   otherwise. *)
let[@inline] update_halfmove info pos = set_halfmove pos @@
  if Uopt.is_some info.en_passant_pawn
  || Uopt.is_some info.direct_capture
  || Piece.is_pawn info.piece
  then 0 else succ pos.halfmove

(* Castling rights change monotonically, so the only time we update the hash
   is when we take away rights. *)
let[@inline] castle_hash c s pos =
  if Cr.mem pos.castle c s then update_hash pos ~f:(Position_hash.Update.castle c s)

let[@inline] clear_white_castling_rights pos =
  castle_hash White Kingside pos;
  castle_hash White Queenside pos;
  set_castle pos Cr.(pos.castle - white)

let[@inline] clear_black_castling_rights pos =
  castle_hash Black Kingside pos;
  castle_hash Black Queenside pos;
  set_castle pos Cr.(pos.castle - black)

let[@inline] white_kingside_castle pos =
  clear_square Piece.white_rook Square.h1 pos;
  set_square Piece.white_rook Square.f1 pos;
  clear_white_castling_rights pos

let[@inline] white_queenside_castle pos =
  clear_square Piece.white_rook Square.a1 pos;
  set_square Piece.white_rook Square.d1 pos;
  clear_white_castling_rights pos

let[@inline] black_kingside_castle pos =
  clear_square Piece.black_rook Square.h8 pos;
  set_square Piece.black_rook Square.f8 pos;
  clear_black_castling_rights pos

let[@inline] black_queenside_castle pos =
  clear_square Piece.black_rook Square.a8 pos;
  set_square Piece.black_rook Square.d8 pos;
  clear_black_castling_rights pos

let castles = [|
  white_kingside_castle;
  white_queenside_castle;
  black_kingside_castle;
  black_queenside_castle;
|]

let[@inline] select_castle pos s =
  (Array.unsafe_get castles
     (Ocaml_intrinsics.Int.count_trailing_zeros
        Cr.(to_int (pos.active --> s)))) pos

(* If we're castling our king on this move, then we need to move the rook as
   well as clear our rights. *)
let[@inline] king_moved_or_castled castle pos =
  if Uopt.is_some castle then
    select_castle pos @@ Uopt.unsafe_value castle
  else match pos.active with
    | Piece.White -> clear_white_castling_rights pos
    | Piece.Black -> clear_black_castling_rights pos

(* If we're moving or capturing a rook, then clear the castling rights for
   that particular side. *)
let[@inline] rook_moved_or_was_captured sq c pos = match c with
  | Piece.White when Square.(sq = h1) ->
    castle_hash White Kingside pos;
    set_castle pos Cr.(pos.castle - white_kingside)
  | Piece.White when Square.(sq = a1) ->
    castle_hash White Queenside pos;
    set_castle pos Cr.(pos.castle - white_queenside)
  | Piece.Black when Square.(sq = h8) ->
    castle_hash Black Kingside pos;
    set_castle pos Cr.(pos.castle - black_kingside)
  | Piece.Black when Square.(sq = a8) ->
    castle_hash Black Queenside pos;
    set_castle pos Cr.(pos.castle - black_queenside)
  | _ -> ()

let[@inline] update_castle info src dst pos =
  (* Check if our king or rook moved. *)
  begin match Piece.kind info.piece with
    | Rook -> rook_moved_or_was_captured src pos.active pos
    | King -> king_moved_or_castled info.castle_side pos
    | _ -> ()
  end;
  (* Check if an enemy rook was captured. *)
  if Uopt.is_some info.direct_capture then
    let p = Uopt.unsafe_value info.direct_capture in
    if Piece.is_rook p then
      rook_moved_or_was_captured dst (Piece.color p) pos

(* Reset the en passant hash and return the new en passant square if a pawn
   double push occurred. *) 
let[@inline] update_en_passant info src dst pos =
  if Uopt.is_some info.en_passant then begin
    let ep = Uopt.unsafe_value info.en_passant in
    update_hash pos ~f:(Position_hash.Update.en_passant_sq ep)
  end;
  set_en_passant pos @@ if Piece.is_pawn info.piece then
    let src_rank = Square.rank src in
    let dst_rank = Square.rank dst in
    match pos.active with
    | Piece.White when dst_rank - src_rank = 2 ->
      Uopt.some @@ Square.(with_rank_unsafe dst Rank.three)
    | Piece.Black when src_rank - dst_rank = 2 ->
      Uopt.some @@ Square.(with_rank_unsafe dst Rank.six)
    | _ -> Uopt.none
  else Uopt.none

let[@inline] flip_active pos =
  update_hash pos ~f:Position_hash.Update.active_player;
  set_active pos @@ inactive pos

(* Since white moves first, increment the fullmove clock after black
   has moved. *)
let[@inline] update_fullmove pos = match pos.active with
  | Black -> set_fullmove pos @@ succ pos.fullmove
  | White -> ()

(* Update the piece for the destination square if we're promoting. *)
let[@inline] do_promote p promote pos = match promote with
  | Some k -> Piece.with_kind p @@ Move.Promote.to_piece_kind k
  | None -> p

(* Clear the square of the pawn captured by en passant. *)
let[@inline] en_passant_capture pw pos =
  if Uopt.is_some pw then
    let sq = Uopt.unsafe_value pw in
    let p = Piece.create (inactive pos) Pawn in
    clear_square p sq pos

let[@inline] go src dst promote pos info =
  (* Do the stuff that relies on the initial state. *)
  update_en_passant info src dst pos;
  update_halfmove info pos;
  update_castle info src dst pos;
  (* Clear the old placement. *)
  clear_square info.piece src pos;
  clear_square_capture dst info.direct_capture pos;
  (* Set the new placement. *)
  let p = do_promote info.piece promote pos in
  set_square p dst pos;
  en_passant_capture info.en_passant_pawn pos;
  (* Prepare for the next move. *)
  update_fullmove pos;
  flip_active pos;
  (* If en passant state changed, then update the hash. *)
  update_hash pos ~f:(Position_hash.Update.en_passant pos)
