(* Relevant info about the position for generating moves, as well as performing
   sanity checks. *)

open Core_kernel [@@warning "-D"]
open Position_common

module T = struct
  type t = {
    pos                   : position;
    king_sq               : Square.t;
    en_passant_pawn       : Square.t Uopt.t;
    occupied              : Bb.t;
    active_board          : Bb.t;
    inactive_board        : Bb.t;
    inactive_attacks      : Bb.t;
    pin_masks             : Bb.t Map.M(Square).t;
    num_checkers          : int;
    check_mask            : Bb.t;
    en_passant_check_mask : Bb.t;
  } [@@deriving fields]
end

(* Calculate the set of pinned pieces and pinners. *)
let[@inline] pins pos sliders sq c =
  let open Bb in
  let c = board_of_color pos c in
  let bq = Pre.bishop sq empty & (pos.queen + pos.bishop) in
  let rq = Pre.rook sq empty & (pos.queen + pos.rook) in
  let snipers = (bq + rq) & sliders in
  let occupied = all_board pos ^ snipers in
  let init = empty, empty in
  Bb.fold snipers ~init ~f:(fun ((pinned, pinners) as acc) ssq ->
      let b = Pre.between sq ssq & occupied in
      if Int.equal 1 @@ count b then
        let pinned = pinned + b in
        let pinners = if (b & c) <> empty then pinners ++ ssq else pinners in
        pinned, pinners
      else acc)

let calculate_checks pos = set_checks pos @@ lazy begin
    let occupied = all_board pos in
    let wksq = Bb.(first_set_exn (pos.king & pos.white)) in
    let bksq = Bb.(first_set_exn (pos.king & pos.black)) in
    let[@specialise] squares c =
      let ksq = match c with
        | Piece.White -> wksq
        | Piece.Black -> bksq in
      let squares = Array.create ~len:Piece.Kind.count Bb.empty in
      let p = Pre.pawn_capture ksq c in
      let n = Pre.knight ksq in
      let b = Pre.bishop ksq occupied in
      let r = Pre.rook ksq occupied in
      Array.unsafe_set squares Piece.Kind.pawn   p;
      Array.unsafe_set squares Piece.Kind.knight n;
      Array.unsafe_set squares Piece.Kind.bishop b;
      Array.unsafe_set squares Piece.Kind.rook   r;
      Array.unsafe_set squares Piece.Kind.queen  Bb.(b + r);
      squares in
    let wpinned, bpinners = pins pos pos.black wksq White in
    let bpinned, wpinners = pins pos pos.white bksq Black in
    let wsquares = squares White in
    let bsquares = squares Black in
    {wpinned; bpinned; wpinners; bpinners; wsquares; bsquares}
  end

(* For each pinned piece, calculate a pin mask to restrict its movement. *)
let[@inline] pin_masks pos ~king_sq =
  let init = Map.empty (module Square) in
  pinned pos pos.active |> Bb.fold ~init ~f:(fun m sq ->
      Map.set m ~key:sq ~data:(Pre.line sq king_sq))

(* Generate the masks which may restrict movement in the event of a check. *)
let[@inline] checks pos ~en_passant_pawn ~num_checkers ~checkers ~king_sq =
  if num_checkers = 1 then
    (* Test if the checker is a sliding piece. If so, then we can try to
       block the attack. Otherwise, they may only be captured. *)
    let open Bb.Syntax in
    let sq = Bb.first_set_exn checkers in
    match which_kind_exn pos sq with
    | Bishop | Rook | Queen -> checkers + Pre.between king_sq sq, Bb.empty
    | Pawn when Uopt.is_some en_passant_pawn ->
      (* Edge case for being able to get out of check via en passant
         capture. *)
      let ep = Uopt.unsafe_value pos.en_passant in
      let pw = Uopt.unsafe_value en_passant_pawn in
      if Square.(sq = pw) then checkers, !!ep else checkers, Bb.empty
    |  _ -> checkers, Bb.empty
  else Bb.(full, empty)

(* Populate info needed for generating legal moves. *)
let[@inline] create pos =
  let open Bb.Syntax in
  (* First, find our king. *)
  let king_sq = Bb.first_set_exn (pos.king & active_board pos) in
  (* Square of the en passant pawn. For purposes of analysis, we only care
     if this pawn is actually threatened with an en passant capture. *)
  let en_passant_pawn =
    if Uopt.is_some pos.en_passant then
      let ep = Uopt.unsafe_value pos.en_passant in
      if has_pawn_threat pos ep
      then Uopt.some @@ en_passant_pawn_aux pos.active ep
      else Uopt.none
    else Uopt.none in
  (* Most general info. *)
  let inactive = inactive pos in
  let occupied = all_board pos in
  let active_board = active_board pos in
  let inactive_board = inactive_board pos in
  let inactive_pieces = collect_color pos inactive in
  (* We're considering attacked squares only for king moves. For sliding
     moves, we pretend that the king is removed from the board. Thus,
     when calculating king moves we will be able to exclude moves where
     the king doesn't escape from the attack ray (if he is in check). *)
  let inactive_attacks =
    let occupied = occupied -- king_sq in
    List.fold inactive_pieces ~init:Bb.empty ~f:(fun acc (sq, k) ->
        acc + Pre.attacks sq occupied inactive k) in
  (* Pinned pieces. *)
  let pin_masks = pin_masks pos ~king_sq in
  (* Pieces checking our king. *)
  let checkers = checkers pos in
  (* Number of checkers is important for how we can decide to get out of
     check. *)
  let num_checkers = Bb.count checkers in
  (* Masks which will may allow us to escape check. *)
  let check_mask, en_passant_check_mask =
    checks pos ~en_passant_pawn ~num_checkers ~checkers ~king_sq in
  (* Construct the analyzed position. *)
  T.Fields.create
    ~pos ~king_sq ~en_passant_pawn ~occupied ~active_board
    ~inactive_board ~inactive_attacks ~pin_masks ~num_checkers
    ~check_mask ~en_passant_check_mask

include T
