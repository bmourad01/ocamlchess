open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal
module Pre = Precalculated

(* Get the set of squares that are attacking the destination square. *)
let[@inline] attackers pos dst occupied =
  let open Bb.Syntax in
  let white = Position.white pos in
  let black = Position.black pos in
  let pawn = Position.pawn pos in
  let knight = Position.knight pos in
  let bishop = Position.bishop pos in
  let rook = Position.rook pos in
  let queen = Position.queen pos in
  let king = Position.king pos in
  let diag = Pre.bishop dst occupied in
  let orth = Pre.rook dst occupied in
  let pw = Pre.pawn_capture dst Black & white & pawn in
  let pb = Pre.pawn_capture dst White & black & pawn in
  let n = Pre.knight dst & knight in
  let b = diag & bishop in
  let r = orth & rook in
  let q = (diag + orth) & queen in
  let k = Pre.king dst & king in
  pw + pb + n + b + r + q + k

(* Get the set of squares from sliding pieces that are attacking the
   destination square. *)
let[@inline] sliders pos dst occupied c =
  let open Bb.Syntax in
  let diag = Pre.bishop dst occupied in
  let orth = Pre.rook dst occupied in
  let bishop = Position.bishop pos in
  let rook = Position.rook pos in
  let queen = Position.queen pos in
  let b = diag & bishop in
  let r = orth & rook in
  let q = (diag + orth) & queen in
  (b + r + q) & Position.board_of_color pos c

type state = {
  mutable from       : Square.t;
  mutable attackers  : Bb.t;
  mutable occupation : Bb.t;
  mutable target_val : int;
  mutable depth      : int;
  mutable side       : Piece.color;
  mutable attacker   : Piece.kind;
}

(* Maximum number of pieces in a legal position. *)
let swap_len = 32

let[@inline] init legal from dst pos victim =
  let depth = 1 in
  let attacker = Piece.kind @@ Position.piece_at_square_exn pos from in
  let target_val = Piece.Kind.value attacker in
  let swap = Array.create ~len:swap_len 0 in
  swap.(0) <- Piece.Kind.value victim;
  (* Compute the initial set of attackers at the destination square. *)
  let attackers = attackers pos dst @@ Position.all_board pos in
  (* Compute the mask for squares that we will consider to be occupied.
     As the evaluation continues, less squares will be available since
     the pieces at these squares will be exchanged. *)
  let occupation =
    if Legal.is_en_passant legal then
      let sq = Option.value_exn (Position.en_passant_pawn pos) in
      Bb.(full -- sq)
    else Bb.full in
  (* Remove the initial attacker from the board. *)
  let attackers = Bb.(attackers -- from) in
  let occupation = Bb.(occupation -- from) in
  (* Now that the initial attacker is removed, compute sliding attacks
     that may have been previously blocked by him. *)
  let all = Position.all_board pos in
  let active = Position.active pos in
  let side = Piece.Color.opposite active in
  let sliders = sliders pos dst Bb.(all & occupation) active in
  let attackers = Bb.((attackers + sliders) & occupation) in
  {from; attackers; occupation; target_val; depth; side; attacker}, swap

let[@inline] rec evaluate swap depth =
  let depth = depth - 1 in
  if depth > 0 then
    let s = swap.(depth) in
    let s1 = swap.(depth - 1) in
    if s > -s1 then
      swap.(depth - 1) <- -s;
    evaluate swap depth
  else swap.(0)

let[@inline] see legal victim =
  let exception Stop in
  let m = Legal.move legal in
  let src = Move.src m in
  let dst = Move.dst m in
  let pos = Legal.parent legal in
  let st, swap = init legal src dst pos victim in
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
    let mask = Bb.(st.attackers & Position.board_of_color pos st.side) in
    if not @@ (List.exists [@specialised]) lva_order ~f:(fun (b, k) ->
        match Bb.(first_set (mask & b)) with
        | None -> false
        | Some sq ->
          st.from <- sq;
          st.attacker <- k;
          true)
    then raise Stop in
  (* Main loop. *)
  let rec loop () =
    (* Any attackers left? *)
    if Bb.(st.attackers <> empty) then begin
      (* Find the least valuable attacker, if they exist. *)
      lva ();
      (* Update the swap list. *)
      let s1 = swap.(st.depth - 1) in
      let s = st.target_val - s1 in
      swap.(st.depth) <- s;
      (* The exchange is clearly losing, so abort. This is a simple pruning
         mechanism as it does not change the final evaluation. *)
      if max (-s1) s < 0 then raise Stop;
      st.depth <- st.depth + 1;
      st.target_val <- Piece.Kind.value st.attacker;
      (* Update the board. *)
      st.attackers <- Bb.(st.attackers -- st.from);
      st.occupation <- Bb.(st.occupation -- st.from);
      let sliders = sliders pos dst Bb.(all & st.occupation) st.side in
      st.attackers <- Bb.((st.attackers + sliders) & st.occupation);
      (* Other side to move. *)
      st.side <- Piece.Color.opposite st.side;
      loop ()
    end in
  begin try loop () with Stop -> () end;
  (* Evaluate the material gains/losses. *)
  evaluate swap st.depth

let go legal = match Legal.capture legal with
  | Some victim -> Some (see legal victim)
  | None -> None
