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

(* Bitboard accessors *)

let all_board pos = Bitboard.(pos.white + pos.black)

let board_of_color pos c =
  match (c : Piece.color) with
  | White -> pos.white
  | Black -> pos.black

let board_of_kind pos k =
  match (k : Piece.kind) with
  | Pawn -> pos.pawn
  | Knight -> pos.knight
  | Bishop -> pos.bishop
  | Rook -> pos.rook
  | Queen -> pos.queen
  | King -> pos.king

let board_of_piece pos p =
  let c = Piece.color p and k = Piece.kind p in
  Bitboard.(board_of_color pos c & board_of_kind pos k)

(* Piece lookup *)

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

let all_pieces pos =
  all_board pos
  |> Bitboard.fold ~init:[] ~f:(fun acc sq ->
       piece_at_square pos sq
       |> Option.value_map ~default:acc ~f:(fun p -> (sq, p) :: acc) )

(* Attacked squares *)

module Attacks = struct
  (* Generate for a particular color and kind *)
  let gen pos c k f =
    let open Bitboard.Syntax in
    let b =
      Piece.create c k |> board_of_piece pos
      |> Bitboard.fold ~init:Bitboard.empty ~f:(fun acc sq -> acc + f sq)
    in
    (* If the square is occupied by a piece of our color, then we cannot
       attack it. *)
    b - board_of_color pos c

  let pawn pos c =
    gen pos c Pawn @@ fun sq -> Precalculated.pawn_capture sq c

  let knight pos c = gen pos c Knight Precalculated.knight

  (* Get the occupied squares for the board. `king_danger` indicates that the
     king of the opposite color should be ignored, so that sliding attacks
     can "see through" the enemy king. *)
  let occupied pos c king_danger =
    let open Bitboard.Syntax in
    if king_danger then
      let p = Piece.(create (Color.opposite c) King) in
      all_board pos - board_of_piece pos p
    else all_board pos

  let bishop ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Bishop @@ fun sq -> Precalculated.bishop sq occupied

  let rook ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Rook @@ fun sq -> Precalculated.rook sq occupied

  let queen ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Queen @@ fun sq -> Precalculated.queen sq occupied

  let king pos c = gen pos c King Precalculated.king

  let all ?(king_danger = false) pos c =
    let open Bitboard.Syntax in
    let occupied = occupied pos c king_danger in
    let b =
      find_color pos c
      |> List.fold ~init:Bitboard.empty ~f:(fun acc (sq, k) ->
           let b =
             match (k : Piece.kind) with
             | Pawn -> Precalculated.pawn_capture sq c
             | Knight -> Precalculated.knight sq
             | Bishop -> Precalculated.bishop sq occupied
             | Rook -> Precalculated.rook sq occupied
             | Queen -> Precalculated.queen sq occupied
             | King -> Precalculated.king sq in
           acc + b ) in
    (* If the square is occupied by a piece of our color, then we cannot
       attack it. *)
    b - board_of_color pos c
end
