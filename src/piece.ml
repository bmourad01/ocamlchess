open Core_kernel

type color = White | Black [@@deriving compare, equal, hash, sexp]

module Color = struct
  module T = struct
    type t = color [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

type kind = Pawn | Knight | Bishop | Rook | Queen | King
[@@deriving compare, equal, hash, sexp]

module Kind = struct
  module T = struct
    type t = kind [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

type t = int [@@deriving compare, equal, hash, sexp]

module Bits = struct
  (* Valid colors *)

  let white = 0b0

  let black = 0b1

  (* Valid kinds *)

  let pawn = 0b000

  let knight = 0b001

  let bishop = 0b010

  let rook = 0b011

  let queen = 0b100

  let king = 0b101

  (* Valid encodings *)

  module Pieces = struct
    let white_pawn = (white lsl 3) lor pawn

    let white_knight = (white lsl 3) lor knight

    let white_bishop = (white lsl 3) lor bishop

    let white_rook = (white lsl 3) lor rook

    let white_queen = (white lsl 3) lor queen

    let white_king = (white lsl 3) lor king

    let black_pawn = (black lsl 3) lor pawn

    let black_knight = (black lsl 3) lor knight

    let black_bishop = (black lsl 3) lor bishop

    let black_rook = (black lsl 3) lor rook

    let black_queen = (black lsl 3) lor queen

    let black_king = (black lsl 3) lor king
  end

  (* Extract the bits *)

  let color p = p lsr 3

  let kind p = p land 0b111
end

include Bits.Pieces

(* Converting to/from the ADTs *)

let color =
  let colors = [|White; Black|] in
  fun p -> colors.(Bits.color p)

let kind =
  let kinds = [|Pawn; Knight; Bishop; Rook; Queen; King|] in
  fun p -> kinds.(Bits.kind p)

let create color kind =
  let color =
    match color with
    | White -> Bits.white
    | Black -> Bits.black
  in
  let kind =
    match kind with
    | Pawn -> Bits.pawn
    | Knight -> Bits.knight
    | Bishop -> Bits.bishop
    | Rook -> Bits.rook
    | Queen -> Bits.queen
    | King -> Bits.king
  in
  (color lsl 3) lor kind

(* Testing membership *)

let is_white p = Bits.(color p = white)

let is_black p = Bits.(color p = black)

let is_pawn p = Bits.(kind p = pawn)

let is_knight p = Bits.(kind p = knight)

let is_bishop p = Bits.(kind p = bishop)

let is_rook p = Bits.(kind p = rook)

let is_queen p = Bits.(kind p = queen)

let is_king p = Bits.(kind p = king)

(* Integer representation *)

let of_int_exn i =
  let color = Bits.color i in
  if color < 0 || color > 1 then
    invalid_arg (sprintf "Invalid color index '%d'" color)
  else
    let kind = Bits.kind i in
    if kind < 0 || kind > 5 then
      invalid_arg (sprintf "Invalid kind index '%d'" kind)
    else i

let of_int i = Option.try_with (fun () -> of_int_exn i)

let to_int = ident

(* FEN string representation *)

let of_fen_exn = function
  | 'P' -> white_pawn
  | 'N' -> white_knight
  | 'B' -> white_bishop
  | 'R' -> white_rook
  | 'Q' -> white_queen
  | 'K' -> white_king
  | 'p' -> black_pawn
  | 'n' -> black_knight
  | 'b' -> black_bishop
  | 'r' -> black_rook
  | 'q' -> black_queen
  | 'k' -> black_king
  | c -> invalid_arg (sprintf "Invalid FEN piece '%c'" c)

let of_fen c = Option.try_with (fun () -> of_fen_exn c)

let to_fen =
  let fen = [|"PNBRQK"; "pnbrqk"|] in
  fun p -> fen.(Bits.color p).[Bits.kind p]
