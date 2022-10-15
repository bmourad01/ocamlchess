open Core_kernel [@@warning "-D"]

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
    else (Obj.magic i : t)

  let of_int i =
    if Int.(i land nmask <> 0) then None
    else Some (Obj.magic i : t)

  let of_int_unsafe i = (Obj.magic i : t)
  let to_int s = (Obj.(magic @@ repr s) : int)
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

let none            = 0b0000
let white_kingside  = 0b0001
let white_queenside = 0b0010
let black_kingside  = 0b0100
let black_queenside = 0b1000

let white     = white_kingside  lor white_queenside
let black     = black_kingside  lor black_queenside
let kingside  = white_kingside  lor black_kingside
let queenside = white_queenside lor black_queenside
let all       = white           lor black

(* Constructor *)

let[@inline] singleton c s =
  let c = Piece.Color.to_int c in
  let s = Side.to_int s in
  (succ s) lsl (c * succ c)

(* Logical operators. *)

let[@inline] inter x y = x land y
let[@inline] union x y = x lor y
let[@inline] compl x = all land lnot x
let[@inline] minus x y = inter x @@ compl y

(* Testing membership. *)

let[@inline] mem x color side =
  inter x (singleton color side) <> none

(* String operations. *)

let char_pairs =
  List.zip_exn (List.init bits ~f:Int.((lsl) 1)) ['K'; 'Q'; 'k'; 'q']

let pp ppf = function
  | 0 -> Format.fprintf ppf "-%!"
  | x -> List.iter char_pairs ~f:(fun (y, c) ->
      if inter x y <> none then Format.fprintf ppf "%c%!" c)

let to_string cr = Format.asprintf "%a%!" pp cr

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

module Syntax = struct
  let (&) = inter
  let (+) = union
  let (~~) = compl
  let (-) = minus
  let (-->) = singleton
end

include Syntax
