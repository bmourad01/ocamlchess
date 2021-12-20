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
    fullmove : int;
  } [@@deriving compare, equal, fields, sexp]
end

include T

let enemy pos = Piece.Color.opposite pos.active

(* Bitboard accessors *)

let[@inline] all_board pos = Bb.(pos.white + pos.black)

let[@inline] board_of_color pos = function
  | Piece.White -> pos.white
  | Piece.Black -> pos.black

let[@inline] active_board pos = board_of_color pos pos.active
let[@inline] enemy_board pos = board_of_color pos @@ enemy pos

let[@inline] board_of_kind pos = function
  | Piece.Pawn -> pos.pawn
  | Piece.Knight -> pos.knight
  | Piece.Bishop -> pos.bishop
  | Piece.Rook -> pos.rook
  | Piece.Queen -> pos.queen
  | Piece.King -> pos.king

let[@inline] board_of_piece pos p =
  let c, k = Piece.decomp p in
  Bb.(board_of_color pos c & board_of_kind pos k)

let[@inline] is_en_passant pos sq = match pos.en_passant with
  | Some sq' -> Square.(sq = sq')
  | None -> false

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
        let sq = Square.create_unsafe ~rank ~file in
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

  let of_string_exn s =
    match String.split s ~on:' ' with
    | [placement; active; castle; en_passant; halfmove; fullmove] ->
      let white, black, pawn, knight, bishop, rook, queen, king =
        parse_placement placement in
      Fields.create
        ~white ~black ~pawn ~knight ~bishop ~rook ~queen ~king
        ~active:(parse_active active)
        ~castle:(parse_castle castle)
        ~en_passant:(parse_en_passant en_passant)
        ~halfmove:(parse_halfmove halfmove)
        ~fullmove:(parse_fullmove fullmove)
    | _ -> invalid_arg @@
      sprintf "Invalid number of sections in FEN string '%s'" s

  let of_string s = Option.try_with @@ fun () -> of_string_exn s

  let string_of_placement pos =
    let rec aux rank file skip acc =
      if rank < 0 then acc
      else if file > 7 then
        let acc = if skip > 0 then acc ^ Int.to_string skip else acc in
        let acc = if rank > 0 then acc ^ "/" else acc in
        aux (rank - 1) 0 0 acc
      else match piece_at_square pos @@ Square.create_unsafe ~rank ~file with
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

(* Handling moves *)

module Attacks = struct
  (* Useful when excluding squares that are occupied by our color. *)
  let[@inline] ignore_color pos c b = Bb.(b - board_of_color pos c)

  (* Generate for a particular color and kind *)
  let[@inline] gen ?(ignore_same = true) pos c k f =
    let open Bb.Syntax in
    Piece.create c k |> board_of_piece pos |> Bb.fold
      ~init:Bb.empty ~f:(fun acc sq -> acc + f sq) |>
    fun b -> if ignore_same then ignore_color pos c b else b

  let[@inline] pawn ?(ignore_same = true) pos c =
    gen pos c Pawn ~ignore_same @@ fun sq -> Pre.pawn_capture sq c

  let[@inline] knight ?(ignore_same = true) pos c =
    gen pos c Knight Pre.knight ~ignore_same

  (* Get the occupied squares for the board. `king_danger` indicates that the
     king of the opposite color should be ignored, so that sliding attacks
     can "see through" the enemy king. This is useful when the king is blocking
     the attack of a sliding piece. *)
  let[@inline] occupied pos c king_danger =
    let open Bb.Syntax in
    if king_danger then
      let p = Piece.(create (Color.opposite c) King) in
      all_board pos - board_of_piece pos p
    else all_board pos

  let[@inline] bishop ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Bishop ~ignore_same @@ fun sq -> Pre.bishop sq occupied

  let[@inline] rook ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Rook ~ignore_same @@ fun sq -> Pre.rook sq occupied

  let[@inline] queen ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Queen ~ignore_same @@ fun sq -> Pre.queen sq occupied

  let[@inline] king ?(ignore_same = true) pos c =
    gen pos c King Pre.king ~ignore_same

  let[@inline] pre_of_kind sq occupied c = function
    | Piece.Pawn -> Pre.pawn_capture sq c
    | Piece.Knight -> Pre.knight sq
    | Piece.Bishop -> Pre.bishop sq occupied
    | Piece.Rook -> Pre.rook sq occupied
    | Piece.Queen -> Pre.queen sq occupied
    | Piece.King -> Pre.king sq

  let[@inline] aux ?(ignore_same = true) ?(king_danger = false) pos c ~f =
    let open Bb.Syntax in
    let occupied = occupied pos c king_danger in
    find_color pos c |> List.fold ~init:Bb.empty ~f:(fun acc (sq, k) ->
        if f k then acc + pre_of_kind sq occupied c k else acc) |>
    fun b -> if ignore_same then ignore_color pos c b else b

  let[@inline] all ?(ignore_same = true) ?(king_danger = false) pos c =
    aux pos c ~ignore_same ~king_danger ~f:(fun _ -> true)

  let[@inline] sliding ?(ignore_same = true) ?(king_danger = false) pos c =
    aux pos c ~ignore_same ~king_danger ~f:(function
        | Piece.(Bishop | Rook | Queen) -> true
        | _ -> false)

  let[@inline] non_sliding ?(ignore_same = true) pos c =
    aux pos c ~ignore_same ~f:(function
        | Piece.(Bishop | Rook | Queen) -> false
        | _ -> true)
end

let in_check pos =
  let active_board = active_board pos in
  let attacks = Attacks.all pos (enemy pos) ~ignore_same:true in
  Bb.((active_board & pos.king & attacks) <> empty)

(* P for Position *)
module P = Monad.State.Make(T)(Monad.Ident)

module Apply = struct
  open P.Syntax

  let[@inline] (>>) m n = m >>= fun _ -> n

  let[@inline] color_field = function
    | Piece.White -> Fields.white
    | Piece.Black -> Fields.black

  let[@inline] kind_field = function
    | Piece.Pawn -> Fields.pawn
    | Piece.Knight -> Fields.knight
    | Piece.Bishop -> Fields.bishop
    | Piece.Rook -> Fields.rook
    | Piece.Queen -> Fields.queen
    | Piece.King -> Fields.king

  let[@inline] piece_fields p =
    let c, k = Piece.decomp p in
    color_field c, kind_field k

  (* A piece can be optionally provided. If not, then we will look up
     the piece at that square. *)
  let[@inline] handle_piece ?p sq = match p with
    | None -> P.gets @@ Fn.flip piece_at_square @@ sq
    | Some _ -> P.return p

  let[@inline] map_field field ~f = P.update @@ Field.map field ~f

  (* Helper for setting both the color and the kind fields of the board. *)
  let map_square ?p sq ~f = handle_piece sq ?p >>= function
    | None -> P.return ()
    | Some p ->
      let c, k = piece_fields p in
      map_field c ~f >> map_field k ~f

  let[@inline] set_square ?p sq = map_square sq ?p ~f:Bb.(fun b -> b ++ sq)
  let[@inline] clear_square ?p sq = map_square sq ?p ~f:Bb.(fun b -> b -- sq)

  let[@inline] is_pawn_or_capture sq sq' = P.gets @@ fun pos ->
    let open Bb.Syntax in
    let is_pawn = sq @ pos.pawn in
    let is_capture =
      (sq' @ all_board pos) || (is_pawn && is_en_passant pos sq') in
    is_pawn || is_capture

  (* The halfmove clock is reset after captures or pawn moves, and
     incremented otherwise. *)
  let[@inline] update_halfmove sq sq' =
    is_pawn_or_capture sq sq' >>= fun cnd ->
    map_field Fields.halfmove ~f:(fun n -> if cnd then 0 else succ n)

  module CR = Castling_rights

  let clear_white_castling_rights =
    P.update @@ Field.map Fields.castle ~f:(fun x -> CR.(diff x white))

  let clear_black_castling_rights =
    P.update @@ Field.map Fields.castle ~f:(fun x -> CR.(diff x black))

  let white_kingside_castle =
    clear_square Square.h1 >>
    set_square Square.f1 ~p:Piece.white_rook >>
    clear_white_castling_rights

  let white_queenside_castle =
    clear_square Square.a1 >>
    set_square Square.d1 ~p:Piece.white_rook >>
    clear_white_castling_rights

  let black_kingside_castle =
    clear_square Square.h8 >>
    set_square Square.f8 ~p:Piece.black_rook >>
    clear_black_castling_rights

  let black_queenside_castle =
    clear_square Square.a8 >>
    set_square Square.d8 ~p:Piece.black_rook >>
    clear_black_castling_rights

  (* If this move is actually a castling, then we need to move the rook
     as well as clear our rights. *)
  let[@inline] king_moved_or_castled sq sq' = P.gets active >>= function
    | Piece.White when Square.(sq = e1 && sq' = g1) -> white_kingside_castle
    | Piece.White when Square.(sq = e1 && sq' = c1) -> white_queenside_castle
    | Piece.Black when Square.(sq = e8 && sq' = g8) -> black_kingside_castle
    | Piece.Black when Square.(sq = e8 && sq' = c8) -> black_queenside_castle
    | Piece.White -> clear_white_castling_rights
    | Piece.Black -> clear_black_castling_rights

  (* If we're moving or capturing a rook, then clear the castling rights for
     that particular side. *)
  let[@inline] rook_moved_or_captured sq = function
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
  let[@inline] rook_moved sq = P.gets active >>= rook_moved_or_captured sq

  (* Rook was captured at a square. Assume that it is the enemy's color. *)
  let[@inline] rook_captured sq = handle_piece sq >>= function
    | Some p when Piece.is_rook p -> rook_moved_or_captured sq @@ Piece.color p
    | _ -> P.return ()

  (* Handle castling-related details. *)
  let[@inline] update_castle ?p sq sq' = handle_piece sq ?p >>= function
    | Some p when Piece.is_king p -> king_moved_or_castled sq sq'
    | Some p when Piece.is_rook p -> rook_moved sq >> rook_captured sq
    | Some _ -> rook_captured sq'
    | _ -> P.return ()

  (* Update the en passant square if a pawn double push occurred. We're
     skipping the check on whether the file changed, since our assumption is
     that the move is legal. For the check if `p` is a pawn or not, we assume
     that it belongs to the active color. *)
  let[@inline] update_en_passant ?p sq sq' =
    handle_piece sq ?p >>= begin function
      | None -> P.return None
      | Some p when not @@ Piece.is_pawn p -> P.return None
      | Some _ ->
        let rank = Square.rank sq and rank', file = Square.decomp sq' in
        P.gets active >>| function
        | Piece.White when Square.Rank.(rank = two && rank' = four) ->
          Some (Square.create_unsafe ~rank:(pred rank') ~file)
        | Piece.Black when Square.Rank.(rank = seven && rank' = five) ->
          Some (Square.create_unsafe ~rank:(succ rank') ~file)
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
  let[@inline] do_promote ?p = function
    | Some k -> P.gets @@ fun pos -> Some (Piece.create pos.active k)
    | None -> P.return p

  let[@inline] move_or_capture ?p sq' ep =
    set_square sq' ?p >>
    (* Check if this was an en passant capture. *)
    if Option.exists ep ~f:(Square.equal sq')
    && Option.exists p ~f:Piece.is_pawn
    then
      let rank, file = Square.decomp sq' in
      begin P.gets @@ fun {active; _} -> match active with
        | Piece.White -> Square.create_unsafe ~rank:(rank - 1) ~file
        | Piece.Black -> Square.create_unsafe ~rank:(rank + 1) ~file
      end >>= clear_square
    else P.return ()

  (* Perform a halfmove `m` for piece `p`. Assume it has already been checked
     for legality. *)
  let[@inline] move p m =
    Move.decomp m |> fun (sq, sq', promote) ->
    P.gets en_passant >>= fun ep ->
    (* Do the stuff that relies on the initial state. *)
    update_halfmove sq sq' >>
    update_en_passant sq sq' ~p >>
    update_castle sq sq' ~p >>
    (* Move the piece. *)
    clear_square sq ~p >> clear_square sq' >>
    do_promote promote ~p >>= fun p ->
    move_or_capture sq' ep ?p >>
    (* Prepare for the next move. *)
    update_fullmove >> flip_active
end


(* Relevant info about the position for generating moves. *)
module Info = struct
  type t = {
    pos : T.t;
    king_sq : Square.t;
    king_mask : Bb.t;
    occupied : Bb.t;
    active_board : Bb.t;
    enemy_board : Bb.t;
    enemy_attacks : Bb.t;
    pinners : Bb.t Map.M(Square).t;
    num_checkers : int;
    check_mask : Bb.t;
    en_passant_check_mask : Bb.t;
    enemy_sliders : (Square.t * Piece.kind) list;
  } [@@deriving fields]
end

(* I for Info *)
module I = Monad.Reader.Make(Info)(Monad.Ident)

module Moves = struct
  open I.Syntax

  (* Accumulate moves with a cons. We use this for every kind of move except
     for a pawn promotion. *)
  let[@inline] default_accum src acc dst = Move.create src dst :: acc

  module Pawn = struct
    let[@inline] push sq = I.read () >>| fun {pos; occupied; _} ->
      let open Bb.Syntax in
      Pre.pawn_advance sq pos.active - occupied

    let[@inline] push2 rank file = I.read () >>| fun {pos; occupied; _} ->
      let open Bb.Syntax in
      match pos.active with
      | Piece.White when Square.Rank.(rank = two) ->
        !!(Square.create_unsafe ~rank:Square.Rank.four ~file) - occupied
      | Piece.Black when Square.Rank.(rank = seven) ->
        !!(Square.create_unsafe ~rank:Square.Rank.five ~file) - occupied
      | _ -> Bb.empty

    (* Check if our pawn or the captured pawn are along a pin ray. If so,
       then this capture would be illegal, since it would lead to a discovery
       on the king.

       En passant moves arise rarely across all chess positions, so we can
       do a bit of heavy calculation here. *)
    let[@inline] en_passant sq ep diag = I.read () >>|
      fun {pos; king_sq; occupied; enemy_sliders; _} ->
      (* Get the position of the pawn which made a double advance. *)
      let pw =
        let rank, file = Square.decomp ep in
        match pos.active with
        | Piece.White -> Square.create_unsafe ~rank:(rank - 1) ~file
        | Piece.Black -> Square.create_unsafe ~rank:(rank + 1) ~file in
      let open Bb in
      (* Remove our pawn and the captured pawn from the board. *)
      let occupied = occupied -- sq -- pw in
      let init = diag ++ ep and finish = ident in
      List.fold_until enemy_sliders ~init ~finish ~f:(fun acc (sq, k) ->
          (* Check if an appropriate diagonal attack from the king would reach
             that corresponding piece. *)
          match k with
          | Piece.Bishop when sq @ (Pre.bishop king_sq occupied) -> Stop diag
          | Piece.Rook when sq @ (Pre.rook king_sq occupied) -> Stop diag
          | Piece.Queen when sq @ (Pre.queen king_sq occupied) -> Stop diag
          | _ -> Continue acc)

    let[@inline] capture sq = I.read () >>= fun {pos; enemy_board; _} ->
      let open Bb.Syntax in
      let diag' = Pre.pawn_capture sq pos.active in
      let diag = diag' & enemy_board in
      match pos.en_passant with
      | Some ep when ep @ diag' -> en_passant sq ep diag
      | _ -> I.return diag

    let promote_kinds = Piece.[Knight; Bishop; Rook; Queen]

    (* We need to multiply the move by the number of pieces we can
       promote to. *)
    let[@inline] promote src dst = List.map promote_kinds ~f:(fun k ->
        Move.create src dst ~promote:(Some k))

    (* Accumulator function for all the squares we can move to. We need this
       in case we have a promotion. *)
    let[@inline] move_accum src rank = I.read () >>| fun {pos; _} ->
      if (Piece.Color.(pos.active = White) && Square.Rank.(rank = seven))
      || (Piece.Color.(pos.active = Black) && Square.Rank.(rank = two))
      then fun acc dst -> promote src dst @ acc
      else default_accum src
  end

  module Knight = struct
    let[@inline] jump sq = I.read () >>| fun {active_board; _} ->
      Bb.(Pre.knight sq - active_board)
  end

  module Bishop = struct
    let[@inline] slide sq = I.read () >>| fun {occupied; active_board; _} ->
      Bb.(Pre.bishop sq occupied - active_board)
  end

  module Rook = struct
    let[@inline] slide sq = I.read () >>| fun {occupied; active_board; _} ->
      Bb.(Pre.rook sq occupied - active_board)
  end

  module Queen = struct
    let[@inline] slide sq = I.read () >>| fun {occupied; active_board; _} ->
      Bb.(Pre.queen sq occupied - active_board)
  end

  module King = struct
    (* Note that `enemy_attacks` includes squares occupied by enemy pieces.
       Therefore, the king may not attack those squares. *)
    let[@inline] move sq =
      I.read () >>| fun {active_board; enemy_attacks; _} ->
      Bb.(Pre.king sq - active_board - enemy_attacks)

    let castle = I.read () >>|
      fun {pos; occupied; enemy_attacks; num_checkers; _} ->
      if num_checkers > 0 then Bb.empty
      else
        let open Bb.Syntax in
        let c sq s =
          let m, b = Pre.castle pos.castle pos.active s in
          (* Check the actual squares to move our pieces to. *)
          let ok = Bb.count (b - occupied - enemy_attacks) = 2 in
          let ok = match s with
            | `king -> ok
            | `queen ->
              (* For queenside, the extra b-file square needs to be
                 unoccupied, so check the mask. *)
              ok && Bb.count (m - occupied) = 3 in
          if ok then !!sq else Bb.empty in
        match pos.active with
        | Piece.White -> c Square.g1 `king + c Square.c1 `queen
        | Piece.Black -> c Square.g8 `king + c Square.c8 `queen
  end

  (* Use this mask to restrict the movement of pinned pieces. *)
  let[@inline] pin_mask sq = I.read () >>| fun {king_sq; pinners; _} ->
    match Map.find pinners sq with
    | None -> Bb.full
    | Some p ->
      if Bb.count p > 1 then Bb.empty
      else
        let sq' = Bb.first_set_exn p in
        let mask = Pre.between king_sq sq' in
        Bb.(mask ++ sq')

  (* Use this mask to restrict the movement of pieces when we are in check. *)
  let[@inline] check_mask ?(capture = Bb.empty) k = I.read () >>|
    fun {num_checkers; check_mask; en_passant_check_mask; _} ->
    if num_checkers <> 1 then check_mask
    else match k with
      | Piece.Pawn -> Bb.(check_mask + (capture & en_passant_check_mask))
      | _ -> check_mask      

  (* It is technically illegal to actually capture the enemy king, so let's
     mask it out. We shouldn't need to check this for our king, because it
     would be illegal to even reach a position where our king can capture
     the enemy king. *)
  let king_mask = I.read () >>| Info.king_mask

  let[@inline] make ?(capture = Bb.empty) sq k b ~f =
    pin_mask sq >>= fun pin ->
    check_mask k ~capture >>= fun chk ->
    king_mask >>| fun k ->
    Bb.(fold (b & pin & chk & k) ~init:[] ~f)

  (* King cannot be pinned, so do not use the pin mask. *)
  let[@inline] make_king sq = Bb.fold ~init:[] ~f:(default_accum sq)

  let[@inline] pawn sq =
    let open Pawn in
    let open Bb.Syntax in
    let rank, file = Square.decomp sq in
    push sq >>= fun push -> begin
      (* Only allow double push if a single push is available. *)
      if Bb.(push = empty) then I.return push
      else push2 rank file >>| (+) push
    end >>= fun push ->
    capture sq >>= fun capture ->
    move_accum sq rank >>= fun f ->
    make sq Pawn (push + capture) ~f ~capture

  let[@inline] knight sq =
    Knight.jump sq >>= make sq Knight ~f:(default_accum sq)

  let[@inline] bishop sq =
    Bishop.slide sq >>= make sq Bishop ~f:(default_accum sq)

  let[@inline] rook sq = Rook.slide sq >>= make sq Rook ~f:(default_accum sq)

  let[@inline] queen sq =
    Queen.slide sq >>= make sq Queen ~f:(default_accum sq)

  let[@inline] king sq =
    let open King in
    let open Bb.Syntax in
    move sq >>= fun move ->
    castle >>| fun castle ->
    make_king sq (move + castle)

  (* Get the new positions from the list of moves. *)
  let[@inline] exec k moves = I.read () >>| fun {pos; _} ->
    let p = Piece.create pos.active k in
    List.map moves ~f:(fun m -> m, Monad.State.exec (Apply.move p m) pos)

  let[@inline] any sq = function
    | Piece.Pawn -> pawn sq
    | Piece.Knight -> knight sq
    | Piece.Bishop -> bishop sq
    | Piece.Rook -> rook sq
    | Piece.Queen -> queen sq
    | Piece.King -> king sq

  let go = I.read () >>= fun {pos; king_sq; num_checkers; _} ->
    (* If the king has more than one attacker, then it is the only piece
       we can move. *)
    if num_checkers > 1 then king king_sq >>= exec King
    else
      find_active pos |> I.List.fold ~init:[] ~f:(fun acc (sq, k) ->
          any sq k >>= exec k >>| fun moves -> moves @ acc)
end

(* Attacks of all piece kinds, starting from the king, intersected with
   the squares occupied by enemy pieces. *)
let[@inline] checkers pos ~king_sq ~enemy_board ~occupied =
  let open Bb.Syntax in
  let p = Pre.pawn_capture king_sq pos.active & pos.pawn in
  let n = Pre.knight king_sq & pos.knight in
  let bishop = Pre.bishop king_sq occupied in
  let rook = Pre.rook king_sq occupied in
  let bq = bishop & (pos.bishop + pos.queen) in
  let rq = rook & (pos.rook + pos.queen) in
  let k = Pre.king king_sq & pos.king in
  (p + n + bq + rq + k) & enemy_board

(* For each enemy sliding piece, calculate its attack set. Then,
   intersect it with the same attack set from our king's square.
   Then, intersect with the squares between the sliding piece and our
   king. Any of our pieces that are in this intersection are thus
   pinned. *)
let[@inline] pinners ~active_board ~king_sq ~enemy_sliders ~occupied =
  let open Bb.Syntax in
  let update pinners sq p k mask =
    match Bb.first_set (p & k & mask) with
    | None -> pinners
    | Some sq' -> Map.update pinners sq' ~f:(function
        | Some b -> b ++ sq
        | None -> !!sq)
  and mask = active_board -- king_sq
  and init = Map.empty (module Square) in
  List.fold enemy_sliders ~init ~f:(fun pinners (sq, k) ->
      let mask = mask & Pre.between king_sq sq in
      match k with
      | Piece.Bishop ->
        let b, k = Pre.(bishop sq occupied, bishop king_sq occupied) in
        update pinners sq b k mask
      | Piece.Rook ->
        let r, k = Pre.(rook sq occupied, rook king_sq occupied) in
        update pinners sq r k mask
      | Piece.Queen ->
        let q, k = Pre.(queen sq occupied, queen king_sq occupied) in
        update pinners sq q k mask
      | _ -> pinners) 

(* Generate the masks which may restrict movement in the event of a check. *)
let[@inline] check_masks pos ~num_checkers ~checkers ~king_sq ~enemy =
  if num_checkers <> 1
  then Bb.full, Bb.empty
  else
    (* Test if the checker is a sliding piece. If so, then we can try to
       block the attack. Otherwise, they may only be captured. *)
    let open Bb.Syntax in
    let sq = Bb.first_set_exn checkers in
    match which_kind pos sq with
    | Some (Bishop | Rook | Queen) ->
      checkers + Pre.between king_sq sq, Bb.empty
    | Some Pawn ->
      (* Edge case for being able to get out of check via en passant
         capture. *)
      let rank, file = Square.decomp sq in
      let ep = match (enemy : Piece.color) with
        | White -> Square.create ~rank:(pred rank) ~file
        | Black -> Square.create ~rank:(succ rank) ~file in
      checkers, Option.value_map ep ~default:Bb.empty ~f:(fun ep ->
          if Option.exists pos.en_passant ~f:(Square.equal ep)
          then !!ep else Bb.empty)
    | Some _ -> checkers, Bb.empty
    | None -> failwith @@
      sprintf "Expected to find first set square in bitboard %016LX"
        (checkers :> int64)

(* Populate info needed for generating legal moves. *)
let[@inline] create_info pos =
  let open Bb.Syntax in
  (* First, find our king. *)
  let king_sq = Bitboard.(first_set_exn (pos.king & active_board pos)) in
  (* Most general info. *)
  let enemy = Piece.Color.opposite pos.active in
  let occupied = all_board pos in
  let active_board = active_board pos in
  let enemy_board = enemy_board pos in
  let king_mask = ~~(pos.king & enemy_board) in
  (* We're considering attacked squares only for king moves. These squares
     should include enemy pieces which may block an enemy attack, since it
     would be illegal for the king to attack those squares. *)
  let enemy_attacks =
    Attacks.all pos enemy ~ignore_same:false ~king_danger:true in
  let enemy_pieces = find_color pos enemy in
  let enemy_sliders =
    List.filter enemy_pieces ~f:(fun (_, k) -> Piece.Kind.is_sliding k) in
  let pinners = pinners ~active_board ~king_sq ~enemy_sliders ~occupied in
  let checkers = checkers pos ~king_sq ~enemy_board ~occupied in
  (* Number of checkers is important for how we can decide to get out of
     check. *)
  let num_checkers = Bb.count checkers in
  let check_mask, en_passant_check_mask =
    check_masks pos ~num_checkers ~checkers ~king_sq ~enemy in
  Info.Fields.create
    ~pos ~king_sq ~king_mask ~occupied ~active_board ~enemy_board
    ~enemy_attacks  ~pinners ~num_checkers ~check_mask
    ~en_passant_check_mask ~enemy_sliders

type legal_move = Move.t * T.t [@@deriving compare, equal, sexp]

module Legal_move = struct
  module T = struct
    type t = legal_move [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make(T)

  let[@inline] move m = fst m
  let[@inline] position m = snd m
  let[@inline] decomp m = m
end

(* Generate all legal moves from the position. *)
let legal_moves pos = create_info pos |> Monad.Reader.run Moves.go

include Comparable.Make(T)
