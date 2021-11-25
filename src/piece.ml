open Core_kernel

let color_bits = 1
let kind_bits = 3
let bits = color_bits + kind_bits

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
    let white_pawn = (white lsl kind_bits) lor pawn
    let white_knight = (white lsl kind_bits) lor knight
    let white_bishop = (white lsl kind_bits) lor bishop
    let white_rook = (white lsl kind_bits) lor rook
    let white_queen = (white lsl kind_bits) lor queen
    let white_king = (white lsl kind_bits) lor king
    let black_pawn = (black lsl kind_bits) lor pawn
    let black_knight = (black lsl kind_bits) lor knight
    let black_bishop = (black lsl kind_bits) lor bishop
    let black_rook = (black lsl kind_bits) lor rook
    let black_queen = (black lsl kind_bits) lor queen
    let black_king = (black lsl kind_bits) lor king
  end

  (* Extract the bits *)

  let color p = p lsr kind_bits
  let kind p = p land 0b111
end

type color = White | Black [@@deriving compare, equal, hash, sexp]

module Color = struct
  module T = struct
    type t = color [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let count = 2

  let of_int_exn = function
    | 0b0 -> White
    | 0b1 -> Black
    | i -> invalid_arg @@ sprintf "Integer %d is not a valid color" i

  let of_int i = Option.try_with @@ fun () -> of_int_exn i

  let to_int = function
    | White -> Bits.white
    | Black -> Bits.black

  let opposite = function
    | White -> Black
    | Black -> White
end

type kind = Pawn | Knight | Bishop | Rook | Queen | King
[@@deriving compare, equal, hash, sexp]

module Kind = struct
  module T = struct
    type t = kind [@@deriving compare, equal, hash, sexp]
  end
  
  include T
  include Comparable.Make (T)

  let count = 6

  let of_int_exn = function
    | 0b000 -> Pawn
    | 0b001 -> Knight
    | 0b010 -> Bishop
    | 0b011 -> Rook
    | 0b100 -> Queen
    | 0b101 -> King
    | i -> invalid_arg @@ sprintf "Integer %d is not a valid piece" i

  let of_int i = Option.try_with @@ fun () -> of_int_exn i

  let to_int = function
    | Pawn -> Bits.pawn
    | Knight -> Bits.knight
    | Bishop -> Bits.bishop
    | Rook -> Bits.rook
    | Queen -> Bits.queen
    | King -> Bits.king
end

module T = struct
  type t = int [@@deriving compare, equal, hash, sexp]
end

include T
include Comparable.Make (T)
include Bits.Pieces

(* Converting to/from the ADTs *)

let color p = Color.of_int_exn @@ Bits.color p
let kind p = Kind.of_int_exn @@ Bits.kind p

let create color kind =
  (Color.to_int color lsl kind_bits) lor Kind.to_int kind

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
  if color < 0 || color > 1 then invalid_arg @@
    sprintf "Invalid color index '%d'" color
  else
    let kind = Bits.kind i in
    if kind < 0 || kind > 5 then invalid_arg @@
      sprintf "Invalid kind index '%d'" kind
    else i

let of_int i = Option.try_with @@ fun () -> of_int_exn i
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
  | c -> invalid_arg @@ sprintf "Invalid FEN piece '%c'" c

let of_fen c = Option.try_with @@ fun () -> of_fen_exn c

let fen_white = "PNBRQK"
let fen_black = "pnbrqk"

let to_fen p = match color p with
  | White -> fen_white.[Bits.kind p]
  | Black -> fen_black.[Bits.kind p]
           
