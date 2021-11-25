open Core_kernel

module Pre = Precalculated

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

(* Bitboard accessors *)

let all_board pos = Bitboard.(pos.white + pos.black)

let board_of_color pos = function
  | Piece.White -> pos.white
  | Piece.Black -> pos.black

let board_of_kind pos = function
  | Piece.Pawn -> pos.pawn
  | Piece.Knight -> pos.knight
  | Piece.Bishop -> pos.bishop
  | Piece.Rook -> pos.rook
  | Piece.Queen -> pos.queen
  | Piece.King -> pos.king

let board_of_piece pos p =
  let c = Piece.color p and k = Piece.kind p in
  Bitboard.(board_of_color pos c & board_of_kind pos k)

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
        sprintf "Invalid number of ranks %d" (8 - rank)
      else if Char.equal sym '/' then rank_separator rank file
      else if Char.is_digit sym  then skip_file rank file sym
      else if Char.is_alpha sym  then place_piece rank file sym
      else invalid_arg @@
        sprintf "Unexpected symbol '%c' in piece placement string '%s'"
          sym s
    and rank_separator rank file =
      if file <> 8 then invalid_arg @@
        sprintf "Invalid separation at rank %d with %d files remaining"
          (succ rank) (8 - file)
      else pred rank, 0
    and skip_file rank file sym =
      let inc = Char.(to_int sym - to_int '0') in
      let file' = file + inc in
      if file' > 8 then invalid_arg @@
        sprintf "Invalid increment %d at file %d" inc file
      else rank, file'
    and place_piece rank file sym =
      if file > 7 then invalid_arg @@
        sprintf "Invalid piece placement on full rank %d" (rank + 1)
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
      color_tbl.(Color.to_int White),
      color_tbl.(Color.to_int Black),
       kind_tbl.(Kind.to_int Pawn),
       kind_tbl.(Kind.to_int Knight),
       kind_tbl.(Kind.to_int Bishop),
       kind_tbl.(Kind.to_int Rook),
       kind_tbl.(Kind.to_int Queen),
       kind_tbl.(Kind.to_int King))

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

(* Attacked squares *)

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
