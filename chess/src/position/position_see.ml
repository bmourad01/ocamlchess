(* Static Exchange Evaluation. *)

open Core_kernel [@@warning "-D"]
open Position_common

(* Get the set of squares from sliding pieces that are attacking the
     destination square. *)
let[@inline] sliders pos dst occupied =
  let open Bb.Syntax in
  let diag = Pre.bishop dst occupied in
  let orth = Pre.rook dst occupied in
  let b = diag & (pos.bishop + pos.queen) in
  let r = orth & (pos.rook + pos.queen) in
  b + r

(* Get the set of squares that are attacking the destination square. *)
let[@inline] attackers pos dst occupied =
  let open Bb.Syntax in
  let wp = Pre.pawn_capture dst Black & pos.white & pos.pawn in
  let bp = Pre.pawn_capture dst White & pos.black & pos.pawn in
  let n = Pre.knight dst & pos.knight in
  let k = Pre.king dst & pos.king in
  wp + bp + n + k + sliders pos dst occupied

type state = {
  mutable from       : Square.t;
  mutable attackers  : Bb.t;
  mutable occupation : Bb.t;
  mutable target_val : int;
  mutable depth      : int;
  mutable side       : Piece.color;
}

let[@inline] init is_en_passant from dst pos victim =
  let side = pos.active in
  let attackers = attackers pos dst @@ all_board pos in
  let occupation =
    let sq =
      if is_en_passant then
        en_passant_pawn_aux pos.active @@
        Uopt.unsafe_value pos.en_passant
      else dst in
    Bb.(full -- sq) in
  let p = piece_at_square_exn pos from in
  let target_val = Piece.(Kind.value @@ kind p) in
  {from; attackers; occupation; target_val; depth = 0; side}

(* Simple negamax search with a branching factor of 1. *)
let[@inline] rec evaluate swap depth =
  let depth = depth - 1 in
  if depth > 0 then
    let s = Array.unsafe_get swap depth in
    let s1 = Array.unsafe_get swap (depth - 1) in
    Array.unsafe_set swap (depth - 1) (-(max (-s1) s));
    evaluate swap depth
  else Array.unsafe_get swap 0

(* We're using `Base.Array.exists`, which starts from the end
   of the list. *)
let[@inline] lva_order pos = [|
  pos.king;
  pos.queen;
  pos.rook;
  pos.bishop;
  pos.knight;
  pos.pawn;
|]

(* Calculate the mask for attackers on this turn. Pinned pieces shouldn't
   be able to attack if the pinners haven't moved yet. *)
let[@inline] attacker_mask pos all us st =
  let mask = Bb.(st.attackers & us) in
  let pinners = pinners pos @@ Piece.Color.opposite st.side in
  if Bb.((all & st.occupation & pinners) <> empty)
  then Bb.(mask - pinned pos st.side)
  else mask

(* Get the least valuable piece that is currently able to attack the
   square.

   Note that if we try to attack with the king, it can only be when the
   opposite side to move has no attackers left.
*)
let[@inline] lva pos all order st =
  let us = board_of_color pos st.side in
  let mask = attacker_mask pos all us st in
  Bb.(mask <> empty) &&
  let them = Bb.(all - us) in
  (Array.existsi [@specialised]) order ~f:(fun i b ->
      match Piece.Kind.(of_int_exn (count - 1 - i)) with
      | King when Bb.((st.attackers & them) <> empty) -> false
      | k -> match Bb.(first_set (mask & b)) with
        | None -> false
        | Some sq ->
          st.target_val <- Piece.Kind.value k;
          st.from <- sq;
          true)

let see m pos is_en_passant victim =
  let src = Move.src m in
  let dst = Move.dst m in
  let st = init is_en_passant src dst pos victim in
  let value = Option.value_map victim ~default:0 ~f:Piece.Kind.value in
  let swap = Array.create value ~len:Square.count in
  let all = all_board pos in
  let order = lva_order pos in
  let continue = ref true in
  while !continue do
    (* Remove the attacker from the board. *)
    st.attackers <- Bb.(st.attackers -- st.from);
    st.occupation <- Bb.(st.occupation -- st.from);
    (* Now that the attacker is removed, compute sliding attacks that may have
       been previously blocked by him. *)
    let sliders = sliders pos dst Bb.(all & st.occupation) in
    let mask = board_of_color pos st.side in
    st.attackers <- Bb.((st.attackers + (sliders & mask)) & st.occupation);
    (* Next iteration. *)
    st.side <- Piece.Color.opposite st.side;
    st.depth <- st.depth + 1;
    let v = st.target_val - Array.unsafe_get swap (st.depth - 1) in
    Array.unsafe_set swap st.depth v;
    continue := lva pos all order st
  done;
  (* Evaluate the material gains/losses. *)
  evaluate swap st.depth

let go (child : Position_child.t) =
  let victim = Uopt.to_option child.capture in
  let m = Position_child.move child in
  let pos = Position_child.parent child in
  let is_en_passant = Position_child.is_en_passant child in
  see m pos is_en_passant victim

let go_unsafe pos m =
  let dst = Move.dst m in
  let them = inactive_board pos in
  let is_en_passant = Position_unsafe.is_en_passant pos m in
  let victim =
    if is_en_passant || Bb.(dst @ them) then
      if is_en_passant then Some Piece.Pawn
      else Some (Piece.kind @@ piece_at_square_exn pos dst)
    else None in
  see m pos is_en_passant victim
