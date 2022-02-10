open Core_kernel

module T = struct
  type t = int64 [@@deriving compare, equal, hash, sexp]
end

include T
include Comparable.Make(T)

(* Conversions. *)

let[@inline] of_int64 b = b
let[@inline] to_int64 b = b

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
let edges  = 0xFF818181818181FFL

(* Helpers to access rank/file by index. *)

let ranks = [|rank_1; rank_2; rank_3; rank_4; rank_5; rank_6; rank_7; rank_8|]
let files = [|file_a; file_b; file_c; file_d; file_e; file_f; file_g; file_h|]

let rank_exn i =
  if Int.(i land Square.Rank.nmask <> 0)
  then invalid_argf "Integer %d is not a valid rank" i ()
  else Array.unsafe_get ranks i

let file_exn i =
  if Int.(i land Square.File.nmask <> 0)
  then invalid_argf "Integer %d is not a valid file" i ()
  else Array.unsafe_get files i

let rank i =
  if Int.(i land Square.Rank.nmask <> 0) then None
  else Some (Array.unsafe_get ranks i)

let file i =
  if Int.(i land Square.File.nmask <> 0) then None
  else Some (Array.unsafe_get files i)

(* Bitwise operators. *)

let inter = Int64.bit_and
let union = Int64.bit_or
let compl = Int64.bit_not
let diff  = Int64.bit_xor

let[@inline] minus x y = inter x @@ compl y
let[@inline] singleton sq = Int64.(one lsl Square.to_int sq)
let[@inline] set b sq = union b @@ singleton sq
let[@inline] clear b sq = minus b @@ singleton sq
let[@inline] mem b sq = Int64.(singleton sq land b <> zero)

external count : (int64[@unboxed]) -> (int[@untagged]) =
  "ml_int64_popcount" "ml_int64_popcount_unboxed" [@@noalloc]

let[@inline] next_square b = Square.of_int_unsafe @@ Int64.ctz b

let[@inline] next_square_rev b =
  Square.of_int_unsafe (Square.last lxor Int64.clz b)

(* Higher-order functions. *)

(* We can more efficiently pop the LSB from the board than with the MSB. *)
let[@inline] clear_fast_fwd b = Int64.(b land pred b)

let fold b ~init ~f =
  let[@inline] rec aux b acc =
    if b = empty then acc
    else aux (clear_fast_fwd b) @@ f acc @@ next_square b in
  aux b init

let fold_rev b ~init ~f =
  let[@inline] rec aux b acc =
    if b = empty then acc
    else
      let sq = next_square_rev b in
      aux (clear b sq) @@ f acc sq in
  aux b init

let fold_until b ~init ~f ~finish =
  let open Continue_or_stop in
  let[@inline] rec aux b acc =
    if b = empty then finish acc
    else match f acc @@ next_square b with
      | Continue acc -> aux (clear_fast_fwd b) acc 
      | Stop x -> x in
  aux b init

let fold_until_rev b ~init ~f ~finish =
  let open Continue_or_stop in
  let[@inline] rec aux b acc =
    if b = empty then finish acc
    else
      let sq = next_square_rev b in
      match f acc sq with
      | Continue acc -> aux (clear b sq) acc 
      | Stop x -> x in
  aux b init

let iter b ~f =
  let[@inline] rec aux b =
    if b <> empty then begin
      f @@ next_square b;
      aux @@ clear_fast_fwd b;
    end in
  aux b

let iter_rev b ~f =
  let[@inline] rec aux b =
    if b <> empty then begin
      let sq = next_square_rev b in
      f sq;
      aux @@ clear b sq
    end in
  aux b

let iter_until b ~f =
  let[@inline] rec aux b =
    if b <> empty && not @@ f @@ next_square b
    then aux @@ clear_fast_fwd b in
  aux b

let iter_until_rev b ~f =
  let[@inline] rec aux b =
    if b <> empty then
      let sq = next_square_rev b in
      if not @@ f sq then aux @@ clear b sq in
  aux b

let[@inline] filter b ~f =
  fold b ~init:b ~f:(fun acc sq -> if f sq then acc else clear acc sq)

let find b ~f =
  let[@inline] rec aux b =
    if b = empty then None
    else
      let sq = next_square b in
      if f @@ next_square b then Some sq else aux @@ clear_fast_fwd b in
  aux b

let find_rev b ~f =
  let[@inline] rec aux b =
    if b = empty then None
    else
      let sq = next_square_rev b in
      if f sq then Some sq else aux @@ clear b sq in
  aux b

let first_set_exn b =
  if b = empty then invalid_arg "Find first set on an empty bitboard"
  else next_square b

let first_set_rev_exn b =
  if b = empty then invalid_arg "Find first set on an empty bitboard"
  else next_square b

let first_set b =
  if b = empty then None 
  else Some (next_square b)

let first_set_rev b =
  if b = empty then None 
  else Some (next_square_rev b)

let to_list b =
  let[@inline] rec aux acc b =
    if b = empty then acc
    else
      let sq = next_square_rev b in
      aux (sq :: acc) @@ clear b sq in
  aux [] b

let to_list_rev b =
  let[@inline] rec aux acc b =
    if b = empty then acc
    else
      let sq = next_square b in
      aux (sq :: acc) @@ clear_fast_fwd b in
  aux [] b

(* Infix operators. *)

module Syntax = struct
  let (&) = inter
  let (+) = union
  let (~~) = compl
  let (^) = diff
  let (-) = minus
  let (!!) = singleton
  let (++) = set
  let (--) = clear
  let[@inline] (@) sq b = mem b sq
end

include Syntax
