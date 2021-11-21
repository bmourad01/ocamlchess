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

let board_of_color b (c : Piece.color) =
  match c with
  | White -> b.white
  | Black -> b.black

let board_of_kind b (k : Piece.kind) =
  match k with
  | Pawn -> b.pawn
  | Knight -> b.knight
  | Bishop -> b.bishop
  | Rook -> b.rook
  | Queen -> b.queen
  | King -> b.king

let board_of_piece b p =
  let c = Piece.color p and k = Piece.kind p in
  Bitboard.(board_of_color b c & board_of_kind b k)

let which_color b sq : Piece.color option =
  let open Bitboard.Syntax in
  if sq @ b.white then Some White
  else if sq @ b.black then Some Black
  else None

let which_kind b sq : Piece.kind option =
  let open Bitboard.Syntax in
  if sq @ b.pawn then Some Pawn
  else if sq @ b.knight then Some Knight
  else if sq @ b.bishop then Some Bishop
  else if sq @ b.rook then Some Rook
  else if sq @ b.queen then Some Queen
  else if sq @ b.king then Some King
  else None

let find_color b c =
  board_of_color b c
  |> Bitboard.fold ~init:[] ~f:(fun acc sq ->
       which_kind b sq
       |> Option.value_map ~default:acc ~f:(fun k -> (sq, k) :: acc) )

let find_kind b k =
  board_of_kind b k
  |> Bitboard.fold ~init:[] ~f:(fun acc sq ->
       which_color b sq
       |> Option.value_map ~default:acc ~f:(fun c -> (sq, c) :: acc) )

let find_piece b p =
  board_of_piece b p |> Bitboard.fold ~init:[] ~f:(fun acc sq -> sq :: acc)

let piece_at_square b sq =
  let open Option.Monad_infix in
  which_color b sq >>= fun c -> which_kind b sq >>| Piece.create c
