open Core_kernel

let bits = 4

module T = struct type t = int [@@deriving compare, equal, hash, sexp] end
include T
include Comparable.Make (T)

(* Integer conversion. *)

let of_int_exn i =
  if i < 0 || i > 15 then
    invalid_arg (sprintf "Invalid integer value '%d' for castling rights" i)
  else i

let of_int i = Option.try_with (fun () -> of_int_exn i)
let to_int = ident

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

(* Logical operators. *)

let inter x y = x land y
let union x y = x lor y
let compl x = all land lnot x
let diff x y = inter x @@ lnot y

(* Testing membership. *)

let mem x (color : Piece.color) side =
  match (color, side) with
  | White, `king -> inter x white_kingside <> none
  | White, `queen -> inter x white_queenside <> none
  | Black, `king -> inter x black_kingside <> none
  | Black, `queen -> inter x black_queenside <> none

(* String operations. *)

let to_string x =
  let wk = if mem x White `king then "K" else "" in
  let wq = if mem x White `queen then "Q" else "" in
  let bk = if mem x Black `king then "k" else "" in
  let bq = if mem x Black `queen then "q" else "" in
  let s = String.concat ~sep:"" [wk; wq; bk; bq] in
  if String.is_empty s then "-" else s

let of_string_exn = function
  | "" ->
    invalid_arg
      "Empty string is invalid for castling rights, use \"-\" instead."
  | "-" -> none
  | s ->
    String.fold s ~init:none ~f:(fun acc sym ->
      match sym with
      | 'K' -> union acc white_kingside
      | 'Q' -> union acc white_queenside
      | 'k' -> union acc black_kingside
      | 'q' -> union acc black_queenside
      | _ ->
        invalid_arg
          (sprintf "Invalid symbol '%c' in castling rights string '%s'" sym s) )

let of_string s = Option.try_with (fun () -> of_string_exn s)
