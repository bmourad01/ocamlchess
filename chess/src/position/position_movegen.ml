open Core_kernel [@@warning "-D"]
open Position_common
open Position_analysis

module Pieces = struct
  module Pawn = struct
    let[@inline] push sq {pos; occupied; _} =
      Bb.(Pre.pawn_advance sq pos.active - occupied)

    (* Double pawn push. *)
    let[@inline] push2 rank file {pos; occupied; _} =
      let open Bb.Syntax in
      match pos.active with
      | Piece.White when Square.Rank.(rank = two) ->
        !!(Square.create_unsafe ~rank:Square.Rank.four ~file) - occupied
      | Piece.Black when Square.Rank.(rank = seven) ->
        !!(Square.create_unsafe ~rank:Square.Rank.five ~file) - occupied
      | _ -> Bb.empty

    (* Check if our pawn or the captured pawn is along a pin ray. If so,
       then this capture would be illegal, since it would lead to a discovery
       on the king. En passant moves arise rarely across all chess positions,
       so we can do a bit of heavy calculation here. *)
    let[@inline] en_passant sq ep pw diag a =
      let open Bb in
      (* Remove our pawn and the captured pawn from the board, but pretend
         that the en passant square is occupied. This covers the case where
         we can capture the pawn, but it may leave our pawn pinned. *)
      let occupied = a.occupied -- sq -- pw ++ ep in
      (* Check if an appropriate diagonal attack from the king would reach
         that corresponding piece. *)
      let {bishop; rook; queen; _} = a.pos in
      let bq = Pre.bishop a.king_sq occupied & (bishop + queen) in
      let rq = Pre.rook   a.king_sq occupied & (rook   + queen) in
      if ((bq + rq) & a.inactive_board) <> empty then diag else diag ++ ep

    let[@inline] capture sq a =
      let open Bb.Syntax in
      let capture = Pre.pawn_capture sq a.pos.active in
      let diag = capture & a.inactive_board in
      if Uopt.is_some a.en_passant_pawn then
        let ep = Uopt.unsafe_value a.pos.en_passant  in
        let pw = Uopt.unsafe_value a.en_passant_pawn in
        if ep @ capture then en_passant sq ep pw diag a else diag
      else diag

    let[@inline] promote src dst =
      let f = Move.create_with_promote src dst in
      [f Knight; f Bishop; f Rook; f Queen]
  end

  module Knight = struct
    let[@inline] jump sq {active_board; _} = Bb.(Pre.knight sq - active_board)
  end

  module Bishop = struct
    let[@inline] slide sq {occupied; active_board; _} =
      Bb.(Pre.bishop sq occupied - active_board)
  end

  module Rook = struct
    let[@inline] slide sq {occupied; active_board; _} =
      Bb.(Pre.rook sq occupied - active_board)
  end

  module Queen = struct
    let[@inline] slide sq {occupied; active_board; _} =
      Bb.(Pre.queen sq occupied - active_board)
  end

  module King = struct
    (* Note that `inactive_attacks` includes squares occupied by inactive
       pieces. Therefore, the king may not attack those squares. *)
    let[@inline] move sq {active_board; inactive_attacks; _} =
      Bb.(Pre.king sq - (active_board + inactive_attacks))

    let castle {pos; occupied; inactive_attacks; num_checkers; _} =
      let open Bb in
      (* Cannot castle out of check. *)
      if Int.(num_checkers = 0) then
        let active = pos.active in
        let m = occupied + inactive_attacks in
        let[@inline] ks_castle sq =
          let b = Pre.castle pos.castle active Kingside in
          if Int.equal 2 @@ count (b - m) then !!sq else empty in
        let[@inline] qs_castle sq bsq =
          if not (bsq @ occupied) then
            let b = Pre.castle pos.castle active Queenside in
            if Int.equal 2 @@ count (b - m) then !!sq else empty
          else empty in
        let ks, qs, bsq = match active with
          | Piece.White -> Square.(g1, c1, b1)
          | Piece.Black -> Square.(g8, c8, b8) in
        ks_castle ks + qs_castle qs bsq
      else empty
  end

  (* Use this mask to restrict the movement of pinned pieces. *)
  let[@inline] pin_mask sq a =
    Option.value ~default:Bb.full @@ Map.find a.pin_masks sq

  (* Special case for pawns, with en passant capture being an option to
     escape check. *)
  let[@inline] pawn_check_mask capture a =
    if a.num_checkers <> 1 then a.check_mask
    else Bb.(a.check_mask + (capture & a.en_passant_check_mask))

  (* All other pieces (except the king). *)
  let[@inline] make sq a b = Bb.(b & pin_mask sq a & a.check_mask)

  let[@inline] pawn sq a =
    let open Pawn in
    let open Bb.Syntax in
    let push = push sq a in
    (* Only allow double push if a single push is available. *)
    let push = if Bb.(push <> empty) then
        let rank, file = Square.decomp sq in
        push + push2 rank file a
      else push in
    let capture = capture sq a in
    (push + capture) & pin_mask sq a & pawn_check_mask capture a

  let[@inline] knight sq a = make sq a @@ Knight.jump  sq a
  let[@inline] bishop sq a = make sq a @@ Bishop.slide sq a
  let[@inline] rook   sq a = make sq a @@ Rook.slide   sq a
  let[@inline] queen  sq a = make sq a @@ Queen.slide  sq a
  let[@inline] king   sq a = Bb.(King.(move sq a + castle a))
end

let[@inline] castle_side piece src dst =
  if Piece.is_king piece then
    let file = Square.file src in
    if Square.File.(file = e) then
      let file' = Square.file dst in
      if Square.File.(file' = c) then Uopt.some Cr.Queenside
      else if Square.File.(file' = g) then Uopt.some Cr.Kingside
      else Uopt.none
    else Uopt.none
  else Uopt.none

let[@inline] run_makemove pos ~src ~dst ~promote ~piece ~en_passant_pawn =
  let is_en_passant = Piece.is_pawn piece && is_en_passant_square pos dst in
  let castle_side = castle_side piece src dst in
  let direct_capture =
    if is_en_passant then Uopt.none
    else piece_at_square_uopt pos dst in
  let capture =
    if is_en_passant then Uopt.some Piece.Pawn
    else if Uopt.is_some direct_capture then
      Uopt.some @@ Piece.kind @@ Uopt.unsafe_value direct_capture
    else Uopt.none in
  (* We will defer running makemove until the new position is actually
     needed. Unsurprisingly, this gives a significant speedup during
     alpha-beta search. *)
  let self = lazy begin
    let self = copy pos in
    let en_passant =
      if Uopt.is_some en_passant_pawn then pos.en_passant else Uopt.none in
    let en_passant_pawn =
      if is_en_passant then en_passant_pawn else Uopt.none in
    Position_makemove.go src dst promote self @@
    Position_makemove.Fields_of_info.create
      ~en_passant ~en_passant_pawn ~piece
      ~castle_side ~direct_capture;
    calculate_checkers self;
    calculate_checks self;
    Position_attacks.calculate self;
    self
  end in
  self, capture, is_en_passant, castle_side

let[@inline] accum_makemove acc move ~parent ~en_passant_pawn ~piece =
  let src, dst, promote = Move.decomp move in
  let self, capture, is_en_passant, castle_side =
    run_makemove parent ~src ~dst ~promote ~piece ~en_passant_pawn in
  Position_child.Fields.create
    ~move ~parent ~self ~capture ~is_en_passant ~castle_side :: acc

let[@inline] is_promote_rank b = function
  | Piece.White -> Bb.((b & rank_8) = b)
  | Piece.Black -> Bb.((b & rank_1) = b)

let[@inline] bb_to_moves src k ~init ~a b = match k with
  | Piece.Pawn when is_promote_rank b a.pos.active ->
    (Bb.fold [@specialised]) b ~init
      ~f:(fun acc dst -> List.rev_append (Pieces.Pawn.promote src dst) acc)
  | _ ->
    (Bb.fold [@specialised]) b ~init
      ~f:(fun acc dst -> Move.create src dst :: acc)

let[@inline] bb_to_children src k ~init ~a:{pos; en_passant_pawn; _} b =
  let active = pos.active in
  let piece = Piece.create active k in
  let f = accum_makemove ~parent:pos ~en_passant_pawn ~piece in
  match k with
  | Piece.Pawn when is_promote_rank b active -> 
    (Bb.fold [@specialised]) b ~init ~f:(fun init dst ->
        Pieces.Pawn.promote src dst |>
        (List.fold [@specialised]) ~init ~f)
  | _ ->
    (Bb.fold [@specialised]) b ~init
      ~f:(fun acc dst -> Move.create src dst |> f acc)

(* Piece-specific bitboards for legal moves. *)
let[@inline] bb_of_kind sq k a = match k with
  | Piece.Pawn   -> Pieces.pawn   sq a
  | Piece.Knight -> Pieces.knight sq a
  | Piece.Bishop -> Pieces.bishop sq a
  | Piece.Rook   -> Pieces.rook   sq a
  | Piece.Queen  -> Pieces.queen  sq a
  | Piece.King   -> Pieces.king   sq a

let[@inline][@specialise] go f a =
  if a.num_checkers <= 1 then
    (List.fold [@speciailised]) ~init:[] ~f:(fun init (sq, k) ->
        bb_of_kind sq k a |> f sq k ~init ~a) @@ collect_active a.pos
  else Pieces.king a.king_sq a |> f a.king_sq Piece.King ~init:[] ~a

let[@inline][@specialise] go_captures f a =
  if a.num_checkers <= 1 then
    (List.fold [@speciailised]) ~init:[] ~f:(fun init (sq, k) ->
        let b = bb_of_kind sq k a in
        let b = match k with
          | Piece.Pawn when Uopt.is_some a.en_passant_pawn ->
            let ep = Uopt.unsafe_value a.pos.en_passant in
            Bb.(b & (a.inactive_board ++ ep))
          | _ -> Bb.(b & a.inactive_board) in
        f sq k ~init ~a b) @@ collect_active a.pos
  else
    Bb.(Pieces.king a.king_sq a & a.inactive_board) |>
    f a.king_sq Piece.King ~init:[] ~a

let[@inline][@specialise] go_promotions f a =
  if a.num_checkers <= 1 then
    let mask = match a.pos.active with
      | Piece.White -> Bb.rank_8
      | Piece.Black -> Bb.rank_1 in
    Bb.(a.pos.pawn & a.active_board) |>
    (Bb.fold [@speciailised]) ~init:[] ~f:(fun init sq ->
        Bb.(Pieces.pawn sq a & mask) |> f sq Piece.Pawn ~init ~a)
  else []

let[@inline][@specialise] go_quiet f a =
  if a.num_checkers <= 1 then
    (List.fold [@speciailised]) ~init:[] ~f:(fun init (sq, k) ->
        let b = bb_of_kind sq k a in
        let b = match k with
          | Piece.Pawn when Uopt.is_some a.en_passant_pawn ->
            let ep = Uopt.unsafe_value a.pos.en_passant in
            Bb.(b - (a.inactive_board ++ ep))
          | _ -> Bb.(b - a.inactive_board) in
        f sq k ~init ~a b) @@ collect_active a.pos
  else
    Bb.(Pieces.king a.king_sq a - a.inactive_board) |>
    f a.king_sq Piece.King ~init:[] ~a
