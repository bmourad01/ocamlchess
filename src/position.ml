open Core_kernel

type t =
  { white: Bitboard.t
  ; black: Bitboard.t
  ; pawn: Bitboard.t
  ; knight: Bitboard.t
  ; bishop: Bitboard.t
  ; rook: Bitboard.t
  ; queen: Bitboard.t
  ; king: Bitboard.t
  ; active: Piece.color
  ; castle: Castling_rights.t
  ; en_passant: Square.t option
  ; halfmove: int
  ; fullmove: int }
[@@deriving compare, equal, hash, sexp]

let board_of_color pos (c : Piece.color) =
  match c with
  | White -> pos.white
  | Black -> pos.black

let board_of_kind pos (k : Piece.kind) =
  match k with
  | Pawn -> pos.pawn
  | Knight -> pos.knight
  | Bishop -> pos.bishop
  | Rook -> pos.rook
  | Queen -> pos.queen
  | King -> pos.king

let board_of_piece pos p =
  let c = Piece.color p and k = Piece.kind p in
  Bitboard.(board_of_color pos c & board_of_kind pos k)

let which_color pos sq : Piece.color option =
  let open Bitboard.Syntax in
  if sq @ pos.white then Some White
  else if sq @ pos.black then Some Black
  else None

let which_kind pos sq : Piece.kind option =
  let open Bitboard.Syntax in
  if sq @ pos.pawn then Some Pawn
  else if sq @ pos.knight then Some Knight
  else if sq @ pos.bishop then Some Bishop
  else if sq @ pos.rook then Some Rook
  else if sq @ pos.queen then Some Queen
  else if sq @ pos.king then Some King
  else None

let find_color pos c =
  board_of_color pos c
  |> Bitboard.fold ~init:[] ~f:(fun acc sq ->
       which_kind pos sq
       |> Option.value_map ~default:acc ~f:(fun k -> (sq, k) :: acc) )

let find_kind pos k =
  board_of_kind pos k
  |> Bitboard.fold ~init:[] ~f:(fun acc sq ->
       which_color pos sq
       |> Option.value_map ~default:acc ~f:(fun c -> (sq, c) :: acc) )

let find_piece pos p =
  board_of_piece pos p |> Bitboard.fold ~init:[] ~f:(fun acc sq -> sq :: acc)

let piece_at_square pos sq =
  let open Option.Monad_infix in
  which_color pos sq >>= fun c -> which_kind pos sq >>| Piece.create c
