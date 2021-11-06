open Core_kernel

type t =
  { placement: Piece.t Map.M(Square).t
  ; active: Piece.color
  ; queenside_castle: Set.M(Piece.Color).t
  ; kingside_castle: Set.M(Piece.Color).t
  ; en_passant: Square.t option
  ; halfmove: int
  ; fullmove: int }
[@@deriving compare, equal, hash, sexp]

let parse_placement s =
  String.fold s
    ~init:(7, 0, Map.empty (module Square))
    ~f:(fun (rank, file, placement) sym ->
      if Char.equal sym '/' then (rank - 1, 0, placement)
      else if Char.is_digit sym then
        (rank, file + (Char.to_int sym - Char.to_int '0'), placement)
      else if Char.is_alpha sym then
        let key = Square.of_rank_and_file_idx_exn ~rank ~file in
        let data = Piece.of_fen_exn sym in
        (rank, file + 1, Map.set placement ~key ~data)
      else
        invalid_arg
          (sprintf "Unexpected symbol '%c' in piece placement string '%s'"
             sym s ) )
  |> trd3

let parse_active = function
  | "w" -> Piece.White
  | "b" -> Piece.Black
  | s -> invalid_arg (sprintf "Invalid active color '%s'" s)

let parse_castle s =
  let queenside_castle = Set.empty (module Piece.Color) in
  let kingside_castle = Set.empty (module Piece.Color) in
  if String.equal s "-" then (queenside_castle, kingside_castle)
  else
    String.fold s ~init:(queenside_castle, kingside_castle)
      ~f:(fun (queenside_castle, kingside_castle) sym ->
        match sym with
        | 'K' -> (queenside_castle, Set.add kingside_castle Piece.White)
        | 'Q' -> (Set.add queenside_castle Piece.White, kingside_castle)
        | 'k' -> (queenside_castle, Set.add kingside_castle Piece.Black)
        | 'q' -> (Set.add queenside_castle Piece.Black, kingside_castle)
        | _ ->
            invalid_arg
              (sprintf
                 "Unexpected symbol '%c' in castling rights string '%s'" sym
                 s ) )

let parse_en_passant s =
  if String.equal s "-" then None else Some (Square.of_string_exn s)

let parse_halfmove s =
  try
    let halfmove = Int.of_string s in
    if halfmove < 0 then
      invalid_arg (sprintf "Invalid halfmove count '%d'" halfmove)
    else halfmove
  with Failure _ -> invalid_arg (sprintf "Invalid halfmove count '%s'" s)

let parse_fullmove s =
  try
    let fullmove = Int.of_string s in
    if fullmove < 0 then
      invalid_arg (sprintf "Invalid fullmove count '%d'" fullmove)
    else fullmove
  with Failure _ -> invalid_arg (sprintf "Invalid halfmove count '%s'" s)

let of_string_exn s =
  let sections = Array.of_list @@ String.split s ~on:' ' in
  if Array.length sections <> 6 then
    invalid_arg (sprintf "Invalid number of sections in FEN string '%s'" s)
  else
    let placement = parse_placement sections.(0) in
    let active = parse_active sections.(1) in
    let queenside_castle, kingside_castle = parse_castle sections.(2) in
    let en_passant = parse_en_passant sections.(3) in
    let halfmove = parse_halfmove sections.(4) in
    let fullmove = parse_fullmove sections.(5) in
    { placement
    ; active
    ; queenside_castle
    ; kingside_castle
    ; en_passant
    ; halfmove
    ; fullmove }

let of_string s = Option.try_with (fun () -> of_string_exn s)

let string_of_placement placement =
  let rec aux rank file skip acc =
    if rank < 0 then acc
    else if file > 7 then
      let acc = if skip > 0 then acc ^ Int.to_string skip else acc in
      let acc = if rank > 0 then acc ^ "/" else acc in
      aux (rank - 1) 0 0 acc
    else
      let sq = Square.of_rank_and_file_idx_exn ~rank ~file in
      match Map.find placement sq with
      | None -> aux rank (file + 1) (skip + 1) acc
      | Some p ->
          let acc = if skip > 0 then acc ^ Int.to_string skip else acc in
          let acc = acc ^ String.of_char @@ Piece.to_fen p in
          aux rank (file + 1) 0 acc
  in
  aux 7 0 0 ""

let string_of_active = function
  | Piece.White -> "w"
  | Piece.Black -> "b"

let string_of_castle queenside_castle kingside_castle =
  let acc = "" in
  let acc = if Set.mem kingside_castle Piece.White then acc ^ "K" else acc in
  let acc =
    if Set.mem queenside_castle Piece.White then acc ^ "Q" else acc
  in
  let acc = if Set.mem kingside_castle Piece.Black then acc ^ "k" else acc in
  let acc =
    if Set.mem queenside_castle Piece.Black then acc ^ "q" else acc
  in
  if String.is_empty acc then "-" else acc

let string_of_en_passant = Option.value_map ~default:"-" ~f:Square.to_string

let to_string fen =
  sprintf "%s %s %s %s %d %d"
    (string_of_placement fen.placement)
    (string_of_active fen.active)
    (string_of_castle fen.queenside_castle fen.kingside_castle)
    (string_of_en_passant fen.en_passant)
    fen.halfmove fen.fullmove
