open Core_kernel
open Monads.Std

module Pre = Precalculated

module T = struct
  type t = {
    white : Bitboard.t;
    black : Bitboard.t;
    pawn : Bitboard.t;
    knight : Bitboard.t;
    bishop : Bitboard.t;
    rook : Bitboard.t;
    queen : Bitboard.t;
    king : Bitboard.t;
    active : Piece.color;
    castle : Castling_rights.t;
    en_passant : Square.t option;
    halfmove : int;
    fullmove : int
  } [@@deriving compare, equal, fields, hash, sexp]
end

include T

(* Bitboard accessors *)

let all_board pos = Bitboard.(pos.white + pos.black)

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
  Bitboard.(board_of_color pos c & board_of_kind pos k)

let is_en_passant pos sq = Option.exists pos.en_passant ~f:(Square.equal sq)

(* Piece lookup *)

let which_color pos sq =
  let open Bitboard.Syntax in
  if sq @ pos.white then Some Piece.White
  else if sq @ pos.black then Some Piece.Black
  else None

let which_kind pos sq =
  let open Bitboard.Syntax in
  if sq @ pos.pawn then Some Piece.Pawn
  else if sq @ pos.knight then Some Piece.Knight
  else if sq @ pos.bishop then Some Piece.Bishop
  else if sq @ pos.rook then Some Piece.Rook
  else if sq @ pos.queen then Some Piece.Queen
  else if sq @ pos.king then Some Piece.King
  else None

let find_color pos c =
  board_of_color pos c |> Bitboard.fold ~init:[] ~f:(fun acc sq ->
      which_kind pos sq |> Option.value_map ~default:acc
        ~f:(fun k -> (sq, k) :: acc))

let find_kind pos k =
  board_of_kind pos k |> Bitboard.fold ~init:[] ~f:(fun acc sq ->
      which_color pos sq |> Option.value_map ~default:acc
        ~f:(fun c -> (sq, c) :: acc))

let find_piece pos p =
  board_of_piece pos p |> Bitboard.fold ~init:[] ~f:(fun acc sq -> sq :: acc)

let piece_at_square pos sq =
  let open Option.Monad_infix in
  which_color pos sq >>= fun c -> which_kind pos sq >>| Piece.create c

let all_pieces pos =
  all_board pos |> Bitboard.fold ~init:[] ~f:(fun acc sq ->
      piece_at_square pos sq |> Option.value_map ~default:acc
        ~f:(fun p -> (sq, p) :: acc))

(* FEN parsing/unparsing *)

module Fen = struct
  let start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  let parse_placement s =
    let color_tbl = Array.create Bitboard.empty ~len:Piece.Color.count in
    let kind_tbl = Array.create Bitboard.empty ~len:Piece.Kind.count in
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
          let open Bitboard.Syntax in
          let c = Piece.(color p |> Color.to_int) in
          let k = Piece.(kind p |> Kind.to_int) in
          color_tbl.(c) <- color_tbl.(c) <-- sq;
          kind_tbl.(k) <- kind_tbl.(k) <-- sq;
          rank, succ file
        | None -> invalid_arg @@
          sprintf "Invalid piece '%c' placed at square '%s'"
            sym (Square.to_string sq) in
    ignore @@ String.fold s ~init:(7, 0) ~f;
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
      Fields.create ~white ~black ~pawn ~knight ~bishop ~rook ~queen ~king
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
      else
        let sq = Square.create_exn ~rank ~file in
        match piece_at_square pos sq with
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

(* Generating attacked squares *)
module Attacks = struct
  (* Useful when excluding squares that are occupied by our color.  *)
  let ignore_color pos c b = Bitboard.(b - board_of_color pos c)

  (* Generate for a particular color and kind *)
  let gen pos c k f =
    let open Bitboard.Syntax in
    Piece.create c k |> board_of_piece pos |> Bitboard.fold
      ~init:Bitboard.empty ~f:(fun acc sq -> acc + f sq) |>
    ignore_color pos c

  let pawn pos c = gen pos c Pawn @@ fun sq -> Pre.pawn_capture sq c
  let knight pos c = gen pos c Knight Pre.knight

  (* Get the occupied squares for the board. `king_danger` indicates that the
     king of the opposite color should be ignored, so that sliding attacks
     can "see through" the enemy king. This is useful when the king is blocking
     the attack of a sliding piece. *)
  let occupied pos c king_danger =
    let open Bitboard.Syntax in
    if king_danger then
      let p = Piece.(create (Color.opposite c) King) in
      all_board pos - board_of_piece pos p
    else all_board pos

  let bishop ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Bishop @@ fun sq -> Pre.bishop sq occupied

  let rook ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Rook @@ fun sq -> Pre.rook sq occupied

  let queen ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Queen @@ fun sq -> Pre.queen sq occupied

  let king pos c = gen pos c King Pre.king

  let pre_of_kind sq occupied c = function
    | Piece.Pawn -> Pre.pawn_capture sq c
    | Piece.Knight -> Pre.knight sq
    | Piece.Bishop -> Pre.bishop sq occupied
    | Piece.Rook -> Pre.rook sq occupied
    | Piece.Queen -> Pre.queen sq occupied
    | Piece.King -> Pre.king sq 

  let all ?(king_danger = false) pos c =
    let open Bitboard.Syntax in
    let occupied = occupied pos c king_danger in
    find_color pos c |> List.fold ~init:Bitboard.empty ~f:(fun acc (sq, k) ->
        acc + pre_of_kind sq occupied c k) |> ignore_color pos c
end

module State = struct
  include Monad.State.T1(T)(Monad.Ident)
  include Monad.State.Make(T)(Monad.Ident)
end

open State.Syntax

(* Helpers for updating fields. *)
module Update = struct
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
  let handle_piece p sq = State.gets @@ fun pos -> match p with
    | None -> piece_at_square pos sq
    | Some _ -> p

  let map_field field ~f = State.update @@ Field.map field ~f

  (* Helper for setting both the color and the kind fields of the board. *)
  let map_square ?p sq ~f = handle_piece p sq >>= function
    | None -> State.return ()
    | Some p ->
      let c, k = piece_fields p in
      map_field c ~f >>= fun () ->
      map_field k ~f

  let set_square ?p sq = map_square sq ?p ~f:Bitboard.((+) !!sq)
  let clear_square ?p sq = map_square sq ?p ~f:Bitboard.((-) !!sq)

  let is_pawn_or_capture sq sq' = State.gets @@ fun pos ->
    let open Bitboard.Syntax in
    let is_pawn = sq @ pos.pawn in
    let is_capture =
      (sq' @ all_board pos) || (is_pawn && is_en_passant pos sq') in
    is_pawn, is_capture

  (* The haalfmove clock is reset after captures or pawn moves, and
     incremented otherwise. *)
  let update_halfmove sq sq' = is_pawn_or_capture sq sq' >>=
    fun (is_pawn, is_capture) -> map_field Fields.halfmove ~f:(fun n ->
      if is_pawn || is_capture then 0 else succ n)

  (* Update the en passant square if a pawn double advance occurred. *)
  let update_en_passant sq sq' = State.update @@ fun pos ->
    Field.map Fields.en_passant pos ~f:(fun _ ->
        let open Bitboard.Syntax in
        if not (sq @ (pos.pawn & active_board pos)) then None
        else
          let rank, file = Square.decomp sq
          and rank', file' = Square.decomp sq' in
          if file <> file' then None
          else
            let open Square.Rank in
            match pos.active with
            | Piece.White when rank = two && rank' = four ->
              Some (Square.create_exn ~rank:(pred rank') ~file)
            | Piece.Black when rank = seven && rank' = five ->
              Some (Square.create_exn ~rank:(succ rank') ~file)
            | _ -> None)

  (* After each halfmove, give the turn to the other player. *)
  let flip_active = map_field Fields.active ~f:Piece.Color.opposite

  (* Since white moves first, increment the fullmove clock after black
     has moved. *)
  let update_fullmove = State.get () >>= function
    | {active = White; _} -> State.return ()
    | {active = Black; _} -> map_field Fields.fullmove ~f:succ

  (* Update the piece for the destination square if we're promoting. *)
  let do_promote ?p = function
    | Some k -> State.gets @@ fun pos -> Some (Piece.create pos.active k)
    | None -> State.return p

  (* Perform a halfmove. Assume it has already been checked for legality. *)
  let _move ?p m =
    Move.decomp m |> fun (sq, sq', promote) ->
    update_halfmove sq sq' >>= fun () ->
    update_en_passant sq sq' >>= fun () ->
    clear_square sq ?p >>= fun () ->
    clear_square sq' >>= fun () ->
    do_promote promote ?p >>= fun p ->
    set_square sq' ?p >>= fun () ->
    update_fullmove >>= fun () ->
    flip_active
end
