open Core_kernel
open Monads.Std

module Pre = Precalculated
module Bb = Bitboard

module T = struct
  type t = {
    white : Bb.t;
    black : Bb.t;
    pawn : Bb.t;
    knight : Bb.t;
    bishop : Bb.t;
    rook : Bb.t;
    queen : Bb.t;
    king : Bb.t;
    active : Piece.color;
    castle : Castling_rights.t;
    en_passant : Square.t option;
    halfmove : int;
    fullmove : int
  } [@@deriving compare, equal, fields, hash, sexp]
end

include T

(* Bb accessors *)

let all_board pos = Bb.(pos.white + pos.black)

let board_of_color pos = function
  | Piece.White -> pos.white
  | Piece.Black -> pos.black

let active_board pos = board_of_color pos pos.active

let board_of_kind pos = function
  | Piece.Pawn -> pos.pawn
  | Piece.Knight -> pos.knight
  | Piece.Bishop -> pos.bishop
  | Piece.Rook -> pos.rook
  | Piece.Queen -> pos.queen
  | Piece.King -> pos.king

let board_of_piece pos p =
  let c, k = Piece.decomp p in
  Bb.(board_of_color pos c & board_of_kind pos k)

let is_en_passant pos sq = Option.exists pos.en_passant ~f:(Square.equal sq)

(* Piece lookup *)

let which_color pos sq =
  let open Bb.Syntax in
  if sq @ pos.white then Some Piece.White
  else if sq @ pos.black then Some Piece.Black
  else None

let which_kind pos sq =
  let open Bb.Syntax in
  if sq @ pos.pawn then Some Piece.Pawn
  else if sq @ pos.knight then Some Piece.Knight
  else if sq @ pos.bishop then Some Piece.Bishop
  else if sq @ pos.rook then Some Piece.Rook
  else if sq @ pos.queen then Some Piece.Queen
  else if sq @ pos.king then Some Piece.King
  else None

let find_color pos c =
  board_of_color pos c |> Bb.fold ~init:[] ~f:(fun acc sq ->
      which_kind pos sq |> Option.value_map ~default:acc
        ~f:(fun k -> (sq, k) :: acc))

let find_active pos = find_color pos pos.active

let find_kind pos k =
  board_of_kind pos k |> Bb.fold ~init:[] ~f:(fun acc sq ->
      which_color pos sq |> Option.value_map ~default:acc
        ~f:(fun c -> (sq, c) :: acc))

let find_piece pos p =
  board_of_piece pos p |> Bb.fold ~init:[] ~f:(fun acc sq -> sq :: acc)

let piece_at_square pos sq =
  let open Option.Monad_infix in
  which_color pos sq >>= fun c -> which_kind pos sq >>| Piece.create c

let all_pieces pos =
  all_board pos |> Bb.fold ~init:[] ~f:(fun acc sq ->
      piece_at_square pos sq |> Option.value_map ~default:acc
        ~f:(fun p -> (sq, p) :: acc))

(* FEN parsing/unparsing *)

module Fen = struct
  let start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  let parse_placement s =
    let color_tbl = Array.create Bb.empty ~len:Piece.Color.count in
    let kind_tbl = Array.create Bb.empty ~len:Piece.Kind.count in
    let rec f (rank, file) sym =
      if rank < 0 then invalid_arg @@
        sprintf "Invalid number of ranks %d" (Square.Rank.count - rank)
      else if Char.equal sym '/' then rank_separator rank file
      else if Char.is_digit sym  then skip_file rank file sym
      else if Char.is_alpha sym  then place_piece rank file sym
      else invalid_arg @@
        sprintf "Unexpected symbol '%c' in piece placement string '%s'"
          sym s
    and rank_separator rank file =
      if file <> Square.File.count then invalid_arg @@
        sprintf "Invalid separation at rank %d with %d files remaining"
          (succ rank) (Square.File.count - file)
      else pred rank, 0
    and skip_file rank file sym =
      let inc = Char.(to_int sym - to_int '0') in
      let file' = file + inc in
      if file' > Square.File.count then invalid_arg @@
        sprintf "Invalid increment %d at file %d" inc file
      else rank, file'
    and place_piece rank file sym =
      if file > Square.File.h then invalid_arg @@
        sprintf "Invalid piece placement on full rank %d" (succ rank)
      else
        let sq = Square.create_exn ~rank ~file in
        match Piece.of_fen sym with
        | Some p ->
          let open Bb.Syntax in
          let c = Piece.(color p |> Color.to_int) in
          let k = Piece.(kind p |> Kind.to_int) in
          color_tbl.(c) <- color_tbl.(c) ++ sq;
          kind_tbl.(k) <- kind_tbl.(k) ++ sq;
          rank, succ file
        | None -> invalid_arg @@
          sprintf "Invalid piece '%c' placed at square '%s'"
            sym (Square.to_string sq) in
    ignore @@ String.fold s ~init:(Square.Rank.eight, Square.File.a) ~f;
    Piece.(
      color_tbl.(Color.white),
      color_tbl.(Color.black),
      kind_tbl.(Kind.pawn),
      kind_tbl.(Kind.knight),
      kind_tbl.(Kind.bishop),
      kind_tbl.(Kind.rook),
      kind_tbl.(Kind.queen),
      kind_tbl.(Kind.king))

  let parse_active = function
    | "w" -> Piece.White
    | "b" -> Piece.Black
    | s -> invalid_arg @@ sprintf "Invalid active color '%s'" s

  let parse_castle = Castling_rights.of_string_exn

  let parse_en_passant = function
    | "-" -> None
    | s -> Some (Square.of_string_exn s)

  let parse_halfmove s =
    try
      let halfmove = Int.of_string s in
      if halfmove < 0 then invalid_arg @@
        sprintf "Invalid halfmove count '%d'" halfmove
      else halfmove
    with Failure _ -> invalid_arg (sprintf "Invalid halfmove count '%s'" s)

  let parse_fullmove s =
    try
      let fullmove = Int.of_string s in
      if fullmove < 0 then invalid_arg @@
        sprintf "Invalid fullmove count '%d'" fullmove
      else fullmove
    with Failure _ -> invalid_arg @@ sprintf "Invalid halfmove count '%s'" s

  (* Reject positions that are impossible to arise from any game. *)
  let validate pos =
    let white_kings = Bb.(count (pos.king & pos.white)) in
    if white_kings < 1 then
      invalid_arg "Piece placement is missing a white king"
    else if white_kings > 1 then
      invalid_arg "Piece placement has more than one white king";
    let white_pawns = Bb.(count (pos.pawn & pos.white)) in
    if white_pawns > 8 then
      invalid_arg "Piece placement has more than eight white pawns"; 
    let black_kings = Bb.(count (pos.king & pos.black)) in
    if black_kings < 1 then
      invalid_arg "Piece placement is missing a black king"
    else if black_kings > 1 then
      invalid_arg "Piece placement has more than one black king";
    let black_pawns = Bb.(count (pos.pawn & pos.black)) in
    if black_pawns > 8 then
      invalid_arg "Piece placement has more than eight black pawns"

  let of_string_exn ?(reject_invalid = false) s =
    match String.split s ~on:' ' with
    | [placement; active; castle; en_passant; halfmove; fullmove] ->
      let white, black, pawn, knight, bishop, rook, queen, king =
        parse_placement placement in
      let pos = Fields.create
          ~white ~black ~pawn ~knight ~bishop ~rook ~queen ~king
          ~active:(parse_active active)
          ~castle:(parse_castle castle)
          ~en_passant:(parse_en_passant en_passant)
          ~halfmove:(parse_halfmove halfmove)
          ~fullmove:(parse_fullmove fullmove) in
      if reject_invalid then validate pos; pos
    | _ -> invalid_arg @@
      sprintf "Invalid number of sections in FEN string '%s'" s
 
  let of_string ?(reject_invalid = false) s =
    Option.try_with @@ fun () -> of_string_exn s ~reject_invalid

  let string_of_placement pos =
    let rec aux rank file skip acc =
      if rank < 0 then acc
      else if file > 7 then
        let acc = if skip > 0 then acc ^ Int.to_string skip else acc in
        let acc = if rank > 0 then acc ^ "/" else acc in
        aux (rank - 1) 0 0 acc
      else match piece_at_square pos @@ Square.create_exn ~rank ~file with
        | None -> aux rank (file + 1) (skip + 1) acc
        | Some p ->
          let acc = if skip > 0 then acc ^ Int.to_string skip else acc in
          let acc = acc ^ String.of_char @@ Piece.to_fen p in
          aux rank (file + 1) 0 acc in
    aux 7 0 0 ""

  let string_of_active = function
    | Piece.White -> "w"
    | Piece.Black -> "b"

  let string_of_castle = Castling_rights.to_string
  let string_of_en_passant = Option.value_map ~default:"-" ~f:Square.to_string

  let to_string (pos : t) =
    sprintf "%s %s %s %s %d %d" (string_of_placement pos)
      (string_of_active pos.active)
      (string_of_castle pos.castle)
      (string_of_en_passant pos.en_passant)
      pos.halfmove pos.fullmove
end

let start = Fen.(of_string_exn start)

module Attacks = struct
  (* Useful when excluding squares that are occupied by our color. *)
  let ignore_color pos c b = Bb.(b - board_of_color pos c)

  (* Generate for a particular color and kind *)
  let gen ?(ignore_same = true) pos c k f =
    let open Bb.Syntax in
    Piece.create c k |> board_of_piece pos |> Bb.fold
      ~init:Bb.empty ~f:(fun acc sq -> acc + f sq) |>
    fun b -> if ignore_same then ignore_color pos c b else b

  let pawn ?(ignore_same = true) pos c =
    gen pos c Pawn ~ignore_same @@ fun sq -> Pre.pawn_capture sq c

  let knight ?(ignore_same = true) pos c =
    gen pos c Knight Pre.knight ~ignore_same

  (* Get the occupied squares for the board. `king_danger` indicates that the
     king of the opposite color should be ignored, so that sliding attacks
     can "see through" the enemy king. This is useful when the king is blocking
     the attack of a sliding piece. *)
  let occupied pos c king_danger =
    let open Bb.Syntax in
    if king_danger then
      let p = Piece.(create (Color.opposite c) King) in
      all_board pos - board_of_piece pos p
    else all_board pos

  let bishop ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Bishop ~ignore_same @@ fun sq -> Pre.bishop sq occupied

  let rook ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Rook ~ignore_same @@ fun sq -> Pre.rook sq occupied

  let queen ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Queen ~ignore_same @@ fun sq -> Pre.queen sq occupied

  let king ?(ignore_same = true) pos c =
    gen pos c King Pre.king ~ignore_same

  let pre_of_kind sq occupied c = function
    | Piece.Pawn -> Pre.pawn_capture sq c
    | Piece.Knight -> Pre.knight sq
    | Piece.Bishop -> Pre.bishop sq occupied
    | Piece.Rook -> Pre.rook sq occupied
    | Piece.Queen -> Pre.queen sq occupied
    | Piece.King -> Pre.king sq

  let aux ?(ignore_same = true) ?(king_danger = false) pos c ~f =
    let open Bb.Syntax in
    let occupied = occupied pos c king_danger in
    find_color pos c |> List.fold ~init:Bb.empty ~f:(fun acc (sq, k) ->
        if f k then acc + pre_of_kind sq occupied c k else acc) |>
    fun b -> if ignore_same then ignore_color pos c b else b

  let all ?(ignore_same = true) ?(king_danger = false) pos c =
    aux pos c ~ignore_same ~king_danger ~f:(fun _ -> true)

  let sliding ?(ignore_same = true) ?(king_danger = false) pos c =
    aux pos c ~ignore_same ~king_danger ~f:(function
        | Piece.(Bishop | Rook | Queen) -> true
        | _ -> false)

  let non_sliding ?(ignore_same = true) pos c =
    aux pos c ~ignore_same ~f:(function
        | Piece.(Bishop | Rook | Queen) -> false
        | _ -> true)
end

(* Helpers for updating fields. *)
module Update = struct
  (* P for Position *)
  module P = Monad.State.Make(T)(Monad.Ident)
  open P.Syntax

  let (>>) m n = m >>= fun _ -> n

  let color_field = function
    | Piece.White -> Fields.white
    | Piece.Black -> Fields.black

  let kind_field = function
    | Piece.Pawn -> Fields.pawn
    | Piece.Knight -> Fields.knight
    | Piece.Bishop -> Fields.bishop
    | Piece.Rook -> Fields.rook
    | Piece.Queen -> Fields.queen
    | Piece.King -> Fields.king

  let piece_fields p =
    let c, k = Piece.decomp p in
    color_field c, kind_field k

  (* A piece can be optionally provided. If not, then we will look up
     the piece at that square. *)
  let handle_piece ?p sq = match p with
    | None -> P.gets @@ Fn.flip piece_at_square @@ sq
    | Some _ -> P.return p

  let map_field field ~f = P.update @@ Field.map field ~f

  (* Helper for setting both the color and the kind fields of the board. *)
  let map_square ?p sq ~f = handle_piece sq ?p >>= function
    | None -> P.return ()
    | Some p ->
      let c, k = piece_fields p in
      map_field c ~f >> map_field k ~f

  let set_square ?p sq = map_square sq ?p ~f:Bb.((+) !!sq)
  let clear_square ?p sq = map_square sq ?p ~f:Bb.(fun b -> b - !!sq)

  let is_pawn_or_capture sq sq' = P.gets @@ fun pos ->
    let open Bb.Syntax in
    let is_pawn = sq @ pos.pawn in
    let is_capture =
      (sq' @ all_board pos) || (is_pawn && is_en_passant pos sq') in
    is_pawn, is_capture

  (* The halfmove clock is reset after captures or pawn moves, and
     incremented otherwise. *)
  let update_halfmove sq sq' = is_pawn_or_capture sq sq' >>=
    fun (is_pawn, is_capture) -> map_field Fields.halfmove ~f:(fun n ->
      if is_pawn || is_capture then 0 else succ n)

  module CR = Castling_rights

  let white_kingside_castle =
    clear_square Square.h1 >> set_square Square.f1 ~p:Piece.white_rook >>
    P.update @@ Field.map Fields.castle ~f:(fun x -> CR.(diff x white))

  let white_queenside_castle =
    clear_square Square.a1 >> set_square Square.d1 ~p:Piece.white_rook >>
    P.update @@ Field.map Fields.castle ~f:(fun x -> CR.(diff x white))

  let black_kingside_castle =
    clear_square Square.h8 >> set_square Square.f8 ~p:Piece.black_rook >>
    P.update @@ Field.map Fields.castle ~f:(fun x -> CR.(diff x black))

  let black_queenside_castle =
    clear_square Square.a8 >> set_square Square.d8 ~p:Piece.black_rook >>
    P.update @@ Field.map Fields.castle ~f:(fun x -> CR.(diff x black))

  (* If this move is actually a castling, then we need to move the rook
     as well as clear our rights. *)
  let king_castled sq sq' = P.gets active >>= function
    | Piece.White when Square.(sq = e1 && sq' = g1) -> white_kingside_castle
    | Piece.White when Square.(sq = e1 && sq' = c1) -> white_queenside_castle
    | Piece.Black when Square.(sq = e8 && sq' = g8) -> black_kingside_castle
    | Piece.Black when Square.(sq = e8 && sq' = c8) -> black_queenside_castle
    | _ -> P.return ()

  (* If we're moving or capturing a rook, then clear the castling rights for
     that particular side. *)
  let rook_moved_or_captured sq = function
    | Piece.White when Square.(sq = h1) -> P.update @@
      Field.map Fields.castle ~f:(fun x -> CR.(diff x white_kingside))
    | Piece.White when Square.(sq = a1) -> P.update @@
      Field.map Fields.castle ~f:(fun x -> CR.(diff x white_queenside))
    | Piece.Black when Square.(sq = h8) -> P.update @@
      Field.map Fields.castle ~f:(fun x -> CR.(diff x black_kingside))
    | Piece.Black when Square.(sq = a8) -> P.update @@
      Field.map Fields.castle ~f:(fun x -> CR.(diff x black_queenside))
    | _ -> P.return ()

  (* Rook moved from a square. *)
  let rook_moved sq = P.gets active >>= rook_moved_or_captured sq

  (* Rook was captured at a square. Assume that it is the enemy's color. *)
  let rook_captured sq = handle_piece sq >>= function
    | Some p when Piece.is_rook p -> rook_moved_or_captured sq @@ Piece.color p
    | _ -> P.return ()

  (* Handle castling-related details. *)
  let update_castle ?p sq sq' = handle_piece sq ?p >>= function
    | Some p when Piece.is_king p -> king_castled sq sq'
    | Some p when Piece.is_rook p -> rook_moved sq >> rook_captured sq
    | Some _ -> rook_captured sq'
    | _ -> P.return ()

  (* Update the en passant square if a pawn double push occurred. We're
     skipping the check on whether the file changed, since our assumption is
     that the move is legal. For the check if `p` is a pawn or not, we assume
     that it belongs to the active color.  *)
  let update_en_passant ?p sq sq' = handle_piece sq ?p >>= begin function
      | None -> P.return None
      | Some p when not @@ Piece.is_pawn p -> P.return None
      | Some _ ->
        let rank = Square.rank sq and rank', file = Square.decomp sq' in
        P.gets active >>| function
        | Piece.White when Square.Rank.(rank = two && rank' = four) ->
          Some (Square.create_exn ~rank:(pred rank') ~file)
        | Piece.Black when Square.Rank.(rank = seven && rank' = five) ->
          Some (Square.create_exn ~rank:(succ rank') ~file)
        | _ -> None
    end >>= fun ep -> map_field Fields.en_passant ~f:(const ep)

  (* After each halfmove, give the turn to the other player. *)
  let flip_active = map_field Fields.active ~f:Piece.Color.opposite

  (* Since white moves first, increment the fullmove clock after black
     has moved. *)
  let update_fullmove = P.gets active >>= function
    | White -> P.return ()
    | Black -> map_field Fields.fullmove ~f:succ

  (* Update the piece for the destination square if we're promoting. *)
  let do_promote ?p = function
    | Some k -> P.gets @@ fun pos -> Some (Piece.create pos.active k)
    | None -> P.return p

  (* Perform a halfmove `m` for piece `p`. Assume it has already been checked
     for legality. *)
  let move p m =
    Move.decomp m |> fun (sq, sq', promote) ->
    (* Do the stuff that relies on the initial state. *)
    update_halfmove sq sq' >>
    update_en_passant sq sq' ~p >>
    update_castle sq sq' ~p >>
    (* Move the piece. *)
    clear_square sq ~p >> clear_square sq' >>
    do_promote promote ~p >>= fun p -> set_square sq' ?p >>
    (* Prepare for the next move. *)
    update_fullmove >> flip_active
end

module Moves = struct
  (* General information about the position that is needed for generating
     moves. *)
  module Info = struct
    type t = {
      pos : T.t;
      occupied : Bb.t;
      active_board : Bb.t;
      enemy_board : Bb.t;
      enemy_attacks : Bb.t;
      king_slide : Bb.t;
      enemy_slide : Bb.t;
      pinned : Bb.t;
      num_checkers : int;
      check_mask : Bb.t;
    } [@@deriving fields]
  end

  let create_info pos =
    let king_sq =
      List.hd_exn @@ find_piece pos @@ Piece.create pos.active King in
    let enemy = Piece.Color.opposite pos.active in
    let occupied = all_board pos in
    let active_board = active_board pos in
    let enemy_board = board_of_color pos enemy in
    (* We're considering attacked squares only for king moves. These squares
       should include enemy pieces which may block an enemy attack, since it
       would be illegal for the king to attack those squares. *)
    let enemy_attacks =
      Attacks.all pos enemy ~ignore_same:false ~king_danger:true in
    let king_knight = Pre.knight king_sq in
    let king_slide, enemy_slide, pinned =
      (* Calculate the intersection of the following bitboards:
         1) The rays of sliding moves that move outward from the king's square.
         2) All sliding attacks from enemy pieces.
         If a piece that is not the king is in this intersection, then it is
         pinned. *)
      let open Bb.Syntax in
      let king_slide = Pre.queen king_sq occupied in
      let enemy_slide = Attacks.sliding pos enemy ~ignore_same:false in
      let pinned = king_slide & enemy_slide & (active_board -- king_sq) in
      king_slide, enemy_slide, pinned in
    (* Attacks of all piece kinds, starting from the king, intersected with the
       squares occupied by enemy pieces. *)
    let checkers = Bb.((king_knight + king_slide) & enemy_board) in
    let num_checkers = Bb.count checkers in
    let check_mask =
      if num_checkers = 1 then
        (* Test if the checker is a sliding piece. If so, then we can try to
           block the attack. Otherwise, they may only be captured. *)
        Bb.fold_until checkers ~init:checkers ~finish:ident
          ~f:(fun acc sq -> match which_kind pos sq with
              | Some Piece.(Bishop | Rook | Queen) -> Stop king_slide
              | _ -> Continue acc)
      else Bb.full in
    Info.Fields.create
      ~pos
      ~occupied
      ~active_board
      ~enemy_board
      ~enemy_attacks
      ~king_slide
      ~enemy_slide
      ~pinned
      ~num_checkers
      ~check_mask

  (* I for Info *)
  module I = Monad.Reader.Make(Info)(Monad.Ident)
  open I.Syntax

  (* Standalone calculation of pinned pieces. *)
  let pinned_pieces pos = (create_info pos).pinned

  let default_accum src acc dst = Move.create src dst :: acc

  module Pawn = struct
    let push sq = I.read () >>| fun {pos; occupied; _} ->
      let open Bb.Syntax in
      Pre.pawn_advance sq pos.active - occupied

    let push2 rank file = I.read () >>| fun {pos; _} ->
      let open Bb.Syntax in
      match pos.active with
      | Piece.White when Square.Rank.(rank = two) ->
        !!(Square.create_exn ~rank:Square.Rank.four ~file)
      | Piece.Black when Square.Rank.(rank = seven) ->
        !!(Square.create_exn ~rank:Square.Rank.five ~file)
      | _ -> Bb.empty

    (* We need to check if an en passant capture will lead to a discovery.
       There are the following scenarios:

       1) Our pawn is on the king's side of the pin, and the enemy pawn is
          on the enemy's side of the pin.
       2) Same as (1), but reversed.
       3) The enemy pawn is on both sides of the pin.

       We don't have to check if our pawn is on both sides of the pin (see
       the use of `pin_mask`). This function checks for the special case of
       en passant, since it is the only kind of move where we capture a piece
       by moving to a square that is not occupied by that piece. *)
    let en_passant sq ep diag = I.read () >>|
      fun {king_slide; enemy_slide; _} -> Bb.(
        let sq = !!sq and ep = !!ep in
        let sq_king  = (sq & king_slide)  = sq
        and sq_enemy = (sq & enemy_slide) = sq
        and ep_king  = (ep & king_slide)  = ep
        and ep_enemy = (ep & enemy_slide) = ep in
        if (ep_king && ep_enemy)
        || (sq_king && ep_enemy)
        || (ep_king && sq_enemy)
        then diag else diag + ep)

    let capture sq = I.read () >>= fun {pos; enemy_board; _} ->
      let open Bb.Syntax in
      let diag = Pre.pawn_capture sq pos.active & enemy_board in
      match pos.en_passant with
      | Some ep when not (ep @ diag) -> en_passant sq ep diag
      | _ -> I.return diag

    (* We need to multiply the move by the number of pieces we can
       promote to. *)
    let promote =
      let kinds = Piece.[Knight; Bishop; Rook; Queen] in
      fun src dst -> List.map kinds ~f:(fun k ->
          Move.create src dst ~promote:(Some k))

    (* Accumulator function for all the squares we can move to. We need this
       in case we have a promotion. *)
    let move_accum src rank = I.read () >>| fun {pos; _} ->
      if (Piece.Color.(pos.active = White) && Square.Rank.(rank = seven))
      || (Piece.Color.(pos.active = Black) && Square.Rank.(rank = two))
      then fun acc dst -> promote src dst @ acc
      else default_accum src
  end

  module Knight = struct
    let jump sq = I.read () >>| fun {active_board; _} ->
      Bb.(Pre.knight sq - active_board)
  end

  module Bishop = struct
    let slide sq = I.read () >>| fun {occupied; active_board; _} ->
      Bb.(Pre.bishop sq occupied - active_board)
  end

  module Rook = struct
    let slide sq = I.read () >>| fun {occupied; active_board; _} ->
      Bb.(Pre.rook sq occupied - active_board)
  end

  module Queen = struct
    let slide sq = I.read () >>| fun {occupied; active_board; _} ->
      Bb.(Pre.queen sq occupied - active_board)
  end

  module King = struct
    (* Note that `enemy_attacks` includes squares occupied by enemy pieces.
       Therefore, the king may not attack those squares. *)
    let move sq = I.read () >>| fun {active_board; enemy_attacks; _} ->
      Bb.(Pre.king sq - active_board - enemy_attacks)

    let castle = I.read () >>|
      fun {pos; occupied; enemy_attacks; num_checkers; _} ->
      if num_checkers > 0 then Bb.empty
      else
        let open Bb.Syntax in
        let c sq s =
          let b =
            Pre.castle pos.castle pos.active s - occupied - enemy_attacks in
          if sq @ b then !!sq else Bb.empty in
        match pos.active with
        | Piece.White -> c Square.g1 `king + c Square.c1 `queen
        | Piece.Black -> c Square.g8 `king + c Square.c8 `queen
  end

  (* Use this mask to restrict the movement of pinned pieces. *)
  let pin_mask sq = I.read () >>|
    fun {king_slide; enemy_slide; pinned; _} ->
    Bb.(if sq @ pinned then king_slide & enemy_slide else full)

  (* Use this mask to restrict the movement of pieces when we are in check. *)
  let check_mask = I.read () >>| Info.check_mask

  (* It is technically illegal to actually capture the enemy king, so let's
     mask it out. We shouldn't need to check this for our king, because it
     would be illegal to even reach a position where our king can capture
     the enemy king. *)
  let king_mask = I.read () >>| fun {pos = {king; _}; enemy_board; _} ->
    Bb.(~~(king & enemy_board))

  let make sq b ~f =
    pin_mask sq >>= fun pin ->
    check_mask >>= fun chk ->
    king_mask >>| fun k ->
    Bb.(fold (b & pin & chk & k) ~init:[] ~f)

  (* King cannot be pinned, so do not use the pin mask. *)
  let make_king sq = Bb.fold ~init:[] ~f:(default_accum sq)

  let pawn sq =
    let open Pawn in
    let open Bb.Syntax in
    let rank, file = Square.decomp sq in
    push sq >>= fun push ->
    push2 rank file >>= fun push2 ->
    capture sq >>= fun capture ->
    move_accum sq rank >>= fun f ->
    make sq (push + push2 + capture) ~f

  let knight sq = Knight.jump sq >>= make sq ~f:(default_accum sq)
  let bishop sq = Bishop.slide sq >>= make sq ~f:(default_accum sq)
  let rook sq = Rook.slide sq >>= make sq ~f:(default_accum sq)
  let queen sq = Queen.slide sq >>= make sq ~f:(default_accum sq)

  let king sq =
    let open King in
    let open Bb.Syntax in
    move sq >>= fun move ->
    castle >>| fun castle ->
    make_king sq (move + castle)

  (* Get the new positions from the list of moves. *)
  let exec k moves = I.read () >>| fun {pos; _} ->
    let p = Piece.create pos.active k in
    List.map moves ~f:(fun m -> m, Monad.State.exec (Update.move p m) pos)

  let any sq = function
    | Piece.Pawn -> pawn sq
    | Piece.Knight -> knight sq
    | Piece.Bishop -> bishop sq
    | Piece.Rook -> rook sq
    | Piece.Queen -> queen sq
    | Piece.King -> king sq

  let piece sq k = I.read () >>= fun {num_checkers; _} -> begin
      (* If the king has more than one attacker, then it is the only piece
         we can move. *)
      if num_checkers > 1 then king sq else any sq k
    end >>= exec k

  let legal pos =
    let info = create_info pos in
    find_active pos |> List.map ~f:(fun (sq, k) ->
        Monad.Reader.run (piece sq k) info) |> List.concat
end
