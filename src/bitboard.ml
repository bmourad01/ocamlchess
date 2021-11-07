open Core_kernel

module T = struct
  type t = Int64.t [@@deriving compare, equal, hash, sexp]
end

include T
include Comparable.Make (T)

(* Conversions. *)

let of_int64 = ident

let to_int64 = ident

(* Constants. *)

let empty = Int64.zero

let full = Int64.bit_not empty

(* Bitwise operators. *)

let inter = Int64.bit_and

let union = Int64.bit_or

let compl = Int64.bit_not

let diff x y = inter x @@ compl y

let singleton sq = Int64.(one lsl Square.to_int sq)

let set b sq = union b @@ singleton sq

let clear b sq = inter b @@ compl @@ singleton sq

let mem b sq = empty <> inter b @@ singleton sq

let count = Int64.popcount

(* Higher-order functions. *)

let fold ?(rev = false) b ~init ~f =
  let next = if rev then fun b -> 63 - Int64.clz b else Int64.ctz in
  let rec aux b acc =
    if b = empty then acc
    else
      let sq = Square.of_int_exn @@ next b in
      aux (clear b sq) @@ f acc sq
  in
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
      | Continue acc -> aux (clear b sq) acc
  in
  aux b init

let iter ?(rev = false) b ~f = fold b ~init:() ~f:(fun () sq -> f sq) ~rev

let iter_until ?(rev = false) b ~f =
  fold_until b ~init:()
    ~f:(fun () sq -> if f sq then Stop () else Continue ())
    ~finish:ident ~rev

(* Infix operators. *)

let ( & ) = inter

let ( + ) = union

let ( ~~ ) = compl

let ( - ) = diff

let ( ! ) = singleton

let ( <-- ) = set

let ( --> ) = clear

let ( @ ) sq b = mem b sq

let ( $ ) = count
