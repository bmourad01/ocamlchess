open Core_kernel

module T = struct
  type t = Int64.t [@@deriving compare, equal, hash, sexp]
end

include T
include Comparable.Make (T)

(* Conversions. *)

let of_int64 = ident
let to_int64 = ident

(* Generic constants. *)

let empty = Int64.zero
let full = Int64.bit_not empty

(* Chess-specific constants. *)

let rank_1 = 0x00000000000000FFL
let rank_2 = 0x000000000000FF00L
let rank_3 = 0x0000000000FF0000L
let rank_4 = 0x00000000FF000000L
let rank_5 = 0x000000FF00000000L
let rank_6 = 0x0000FF0000000000L
let rank_7 = 0x00FF000000000000L
let rank_8 = 0xFF00000000000000L
let file_a = 0x0101010101010101L
let file_b = 0x0202020202020202L
let file_c = 0x0404040404040404L
let file_d = 0x0808080808080808L
let file_e = 0x1010101010101010L
let file_f = 0x2020202020202020L
let file_g = 0x4040404040404040L
let file_h = 0x8080808080808080L
let edges = 0xFF818181818181FFL

(* Helpers to access rank/file by index. *)

let rank_exn =
  let ranks =
    [|rank_1; rank_2; rank_3; rank_4; rank_5; rank_6; rank_7; rank_8|] in
  Array.get ranks

let file_exn =
  let files =
    [|file_a; file_b; file_c; file_d; file_e; file_f; file_g; file_h|] in
  Array.get files

let rank i = Option.try_with (fun () -> rank_exn i)
let file i = Option.try_with (fun () -> file_exn i)

(* Bitwise operators. *)

let inter = Int64.bit_and
let union = Int64.bit_or
let compl = Int64.bit_not
let diff x y = inter x @@ compl y
let singleton sq = Int64.(one lsl Square.to_int sq)
let set b sq = union b @@ singleton sq
let clear b sq = diff b @@ singleton sq
let mem b sq = empty <> inter b @@ singleton sq
let count = Int64.popcount

(* Higher-order functions. *)

let fold ?(rev = false) b ~init ~f =
  let next = if rev then fun b -> 63 - Int64.clz b else Int64.ctz in
  let rec aux b acc =
    if b = empty then acc
    else
      let sq = Square.of_int_exn @@ next b in
      aux (clear b sq) @@ f acc sq in
  aux b init

let fold_until ?(rev = false) b ~init ~f ~finish =
  let open Continue_or_stop in
  let next = if rev then fun b -> 63 - Int64.clz b else Int64.ctz in
  let rec aux b acc =
    if b = empty then finish acc
    else
      let sq = Square.of_int_exn @@ next b in
      match f acc sq with
      | Stop x -> x
      | Continue acc -> aux (clear b sq) acc in
  aux b init

let iter ?(rev = false) b ~f = fold b ~init:() ~f:(fun () sq -> f sq) ~rev

let iter_until ?(rev = false) b ~f =
  fold_until b ~init:()
    ~f:(fun () sq -> if f sq then Stop () else Continue ())
    ~finish:ident ~rev

let filter b ~f =
  fold b ~init:b ~f:(fun acc sq -> if f sq then acc else clear acc sq)

(* Infix operators. *)

module Syntax = struct
  let ( & ) = inter
  let ( + ) = union
  let ( ~~ ) = compl
  let ( - ) = diff
  let ( ! ) = singleton
  let ( <-- ) = set
  let ( --> ) = clear
  let ( @ ) sq b = mem b sq
  let ( $ ) = count
end

include Syntax
