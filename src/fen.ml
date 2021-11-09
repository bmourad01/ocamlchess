open Core_kernel

let start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

type t =
  { placement: Piece.t Map.M(Square).t
  ; active: Piece.color
  ; castle: Castling_rights.t
  ; en_passant: Square.t option
  ; halfmove: int
  ; fullmove: int }
[@@deriving compare, equal, hash, sexp]

let parse_placement s =
  String.fold s
    ~init:(7, 0, Map.empty (module Square))
    ~f:(fun (rank, file, placement) sym ->
      if rank < 0 then
        invalid_arg (sprintf "Invalid number of ranks %d" (8 - rank))
      else if Char.equal sym '/' then
        if file <> 8 then
          invalid_arg
            (sprintf "Invalid separation at rank %d with %d files remaining"
               (rank + 1) (8 - file) )
        else (rank - 1, 0, placement)
      else if Char.is_digit sym then
        let inc = Char.(to_int sym - to_int '0') in
        let file' = file + inc in
        if file' > 8 then
          invalid_arg (sprintf "Invalid increment %d at file %d" inc file)
        else (rank, file', placement)
      else if Char.is_alpha sym then
        if file > 7 then
          invalid_arg
            (sprintf "Invalid piece placement on full rank %d" (rank + 1))
        else
          let key = Square.of_rank_and_file_exn ~rank ~file in
          match Piece.of_fen sym with
          | None ->
            invalid_arg
              (sprintf "Invalid piece '%c' placed at square '%s'" sym
                 (Square.to_string key) )
          | Some data -> (rank, file + 1, Map.set placement ~key ~data)
      else
        invalid_arg
          (sprintf "Unexpected symbol '%c' in piece placement string '%s'"
             sym s ) )
  |> trd3

let parse_active = function
  | "w" -> Piece.White
  | "b" -> Piece.Black
  | s -> invalid_arg (sprintf "Invalid active color '%s'" s)

let parse_castle = Castling_rights.of_string_exn

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
  match String.split s ~on:' ' with
  | [placement; active; castle; en_passant; halfmove; fullmove] ->
    { placement= parse_placement placement
    ; active= parse_active active
    ; castle= parse_castle castle
    ; en_passant= parse_en_passant en_passant
    ; halfmove= parse_halfmove halfmove
    ; fullmove= parse_fullmove fullmove }
  | _ ->
    invalid_arg (sprintf "Invalid number of sections in FEN string '%s'" s)

let of_string s = Option.try_with (fun () -> of_string_exn s)
let create () = of_string_exn start

let string_of_placement placement =
  let rec aux rank file skip acc =
    if rank < 0 then acc
    else if file > 7 then
      let acc = if skip > 0 then acc ^ Int.to_string skip else acc in
      let acc = if rank > 0 then acc ^ "/" else acc in
      aux (rank - 1) 0 0 acc
    else
      let sq = Square.of_rank_and_file_exn ~rank ~file in
      match Map.find placement sq with
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

let to_string fen =
  sprintf "%s %s %s %s %d %d"
    (string_of_placement fen.placement)
    (string_of_active fen.active)
    (string_of_castle fen.castle)
    (string_of_en_passant fen.en_passant)
    fen.halfmove fen.fullmove
