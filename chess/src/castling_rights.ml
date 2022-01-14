open Core_kernel

let bits = 4
let nmask = lnot 0b1111

module T = struct
  type t = int [@@deriving compare, equal, hash, sexp]
end

include T
include Comparable.Make(T)

type side = Kingside | Queenside [@@deriving compare, equal, sexp]

module Side = struct
  module T = struct
    type t = side [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make(T)

  let count = 2
  let is_kingside = equal Kingside
  let is_queenside = equal Queenside

  let nmask = lnot 0b1

  let of_int_exn i =
    if Int.(i land nmask <> 0)
    then invalid_argf "Invalid side integer %d" i ()
    else ((Obj.magic i) : t)

  let of_int i =
    if Int.(i land nmask <> 0) then None
    else Some ((Obj.magic i) : t)

  let of_int_unsafe i = ((Obj.magic i) : t)
  let to_int s = ((Obj.magic (Obj.repr s)) : int)
end

(* Integer conversion. *)

let of_int_exn i =
  if i land nmask <> 0
  then invalid_argf "Invalid integer value '%d' for castling rights" i ()
  else i

let[@inline] of_int_unsafe i = i
let[@inline] of_int i = Option.some_if (i land nmask = 0) i
let[@inline] to_int cr = cr

(* Predefined constants. *)

let none = 0b0000
let white_kingside = 0b0001
let white_queenside = 0b0010
let white = white_kingside lor white_queenside
let black_kingside = 0b0100
let black_queenside = 0b1000
let black = black_kingside lor black_queenside
let kingside = white_kingside lor black_kingside
let queenside = white_queenside lor black_queenside
let all = white lor black

(* Constructor *)

let[@inline] singleton c s = match c, s with
  | Piece.White, Kingside -> white_kingside
  | Piece.White, Queenside -> white_queenside
  | Piece.Black, Kingside -> black_kingside
  | Piece.Black, Queenside -> black_queenside

(* Logical operators. *)

let[@inline] inter x y = x land y
let[@inline] union x y = x lor y
let[@inline] compl x = all land lnot x
let[@inline] minus x y = inter x @@ compl y

(* Testing membership. *)

let[@inline] mem x color side = match color, side with
  | Piece.White, Kingside -> inter x white_kingside <> none
  | Piece.White, Queenside -> inter x white_queenside <> none
  | Piece.Black, Kingside -> inter x black_kingside <> none
  | Piece.Black, Queenside -> inter x black_queenside <> none

(* String operations. *)

let string_pairs =
  List.zip_exn (List.init bits ~f:Int.((lsl) 1)) ["K"; "Q"; "k"; "q"]

let to_string = function
  | 0 -> "-"
  | x -> List.fold string_pairs ~init:"" ~f:(fun acc (y, s) ->
      if inter x y <> none then acc ^ s else acc)

let of_string_exn = function
  | "-" -> none
  | "" -> invalid_arg "Empty string is invalid for castling rights"
  | s -> String.fold s ~init:none ~f:(fun acc -> function
      | 'K' -> union acc white_kingside
      | 'Q' -> union acc white_queenside
      | 'k' -> union acc black_kingside
      | 'q' -> union acc black_queenside
      | sym -> invalid_argf "Invalid symbol '%c' in castling rights \
                             string '%s'" sym s ())

let of_string s = Option.try_with @@ fun () -> of_string_exn s
