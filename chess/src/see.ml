open Core_kernel

module Bb = Bitboard
module Child = Position.Child
module Pre = Precalculated

(* Get the set of squares from sliding pieces that are attacking the
   destination square. *)
let[@inline] sliders pos dst occupied =
  let open Bb.Syntax in
  let diag = Pre.bishop dst occupied in
  let orth = Pre.rook dst occupied in
  let bishop = Position.bishop pos in
  let rook = Position.rook pos in
  let queen = Position.queen pos in
  let b = diag & bishop in
  let r = orth & rook in
  let q = (diag + orth) & queen in
  b + r + q

(* Get the set of squares that are attacking the destination square. *)
let[@inline] attackers pos dst occupied =
  let open Bb.Syntax in
  let white = Position.white pos in
  let black = Position.black pos in
  let pawn = Position.pawn pos in
  let king = Position.king pos in
  let knight = Position.knight pos in
  let wp = Pre.pawn_capture dst Black & white & pawn in
  let bp = Pre.pawn_capture dst White & black & pawn in
  let n = Pre.knight dst & knight in
  let k = Pre.king dst & king in
  wp + bp + n + k + sliders pos dst occupied

type state = {
  mutable from       : Square.t;
  mutable attackers  : Bb.t;
  mutable occupation : Bb.t;
  mutable target_val : int;
  mutable depth      : int;
  mutable side       : Piece.color;
}

(* Maximum number of pieces in a legal position. *)
let swap_len = 32

let[@inline] init is_en_passant from dst pos victim =
  let depth = 1 in
  let attacker = Piece.kind @@ Position.piece_at_square_exn pos from in
  let target_val = Piece.Kind.value attacker in
  let swap = Array.create ~len:swap_len 0 in
  swap.(0) <- Piece.Kind.value victim;
  (* Compute the initial set of attackers at the destination square. *)
  let all = Position.all_board pos in
  let attackers = attackers pos dst all in
  (* Compute the mask for squares that we will consider to be occupied.
     As the evaluation continues, less squares will be available since
     the pieces at these squares will be exchanged. *)
  let occupation =
    if is_en_passant then
      let sq = Option.value_exn (Position.en_passant_pawn pos) in
      Bb.(full -- sq)
    else Bb.full in
  (* Remove the initial attacker from the board. *)
  let attackers = Bb.(attackers -- from) in
  let occupation = Bb.(occupation -- from) in
  (* Now that the initial attacker is removed, compute sliding attacks
     that may have been previously blocked by him. *)
  let active_board = Position.active_board pos in
  let side = Position.inactive pos in
  let sliders = sliders pos dst Bb.(all & occupation) in
  let attackers = Bb.((attackers + (sliders & active_board)) & occupation) in
  {from; attackers; occupation; target_val; depth; side}, swap

(* Simple negamax search with a branching factor of 1. *)
let[@inline] rec evaluate swap depth =
  let depth = depth - 1 in
  if depth > 0 then
    let s = swap.(depth) in
    let s1 = swap.(depth - 1) in
    swap.(depth - 1) <- -(max (-s1) s);
    evaluate swap depth
  else swap.(0)

let[@inline] see m pos is_en_passant victim =
  let src = Move.src m in
  let dst = Move.dst m in
  let st, swap = init is_en_passant src dst pos victim in
  let all = Position.all_board pos in
  (* The order from least to most valuable attacker. *)
  let lva_order = [
    Position.pawn   pos, Piece.Pawn;
    Position.knight pos, Piece.Knight;
    Position.bishop pos, Piece.Bishop;
    Position.rook   pos, Piece.Rook;
    Position.queen  pos, Piece.Queen;
    Position.king   pos, Piece.King;
  ] in
  (* Get the least valuable piece that is currently able to attack the
     square. *)
  let[@inline] lva () =
    let default = false in
    let us = Position.board_of_color pos st.side in
    let them = Bb.(all - us) in
    let mask = Bb.(st.attackers & us) in
    Bb.(mask <> empty) && List.exists lva_order ~f:(fun (b, k) -> match k with
        (* We can't attack with the king if our opponent still has pieces left
           to exchange. Since we've used up our remaining pieces, we terminate
           the evaluation here. *)
        | Piece.King when Bb.((st.attackers & them) <> empty) -> false
        | _ -> Option.value_map Bb.(first_set (mask & b)) ~default ~f:(fun sq ->
            st.target_val <- Piece.Kind.value k;
            st.from <- sq;
            true)) in
  (* Main loop. *)
  let[@inline] rec loop () =
    (* Update the swap list. *)
    swap.(st.depth) <- st.target_val - swap.(st.depth - 1);
    (* Find the least valuable attacker, if they exist. *)
    if lva () then begin
      (* Update the board. *)
      st.attackers <- Bb.(st.attackers -- st.from);
      st.occupation <- Bb.(st.occupation -- st.from);
      let sliders = sliders pos dst Bb.(all & st.occupation) in
      let mask = Position.board_of_color pos st.side in
      st.attackers <- Bb.((st.attackers + (sliders & mask)) & st.occupation);
      (* Other side to move. *)
      st.side <- Piece.Color.opposite st.side;
      st.depth <- st.depth + 1;
      loop ()
    end in
  loop ();
  (* Evaluate the material gains/losses. *)
  evaluate swap st.depth

let go child =
  Child.capture child |>
  Option.map ~f:(fun victim ->
      let m = Child.move child in
      let pos = Child.parent child in
      let is_en_passant = Child.is_en_passant child in
      see m pos is_en_passant victim)

let go_unsafe pos m =
  let dst = Move.dst m in
  let them = Position.inactive_board pos in
  let is_en_passant = Position.Unsafe.is_en_passant pos m in
  if Position.Unsafe.is_en_passant pos m || Bb.(dst @ them) then
    let victim =
      if is_en_passant then Piece.Pawn
      else Piece.kind @@ Position.piece_at_square_exn pos dst in
    Some (see m pos is_en_passant victim)
  else None
