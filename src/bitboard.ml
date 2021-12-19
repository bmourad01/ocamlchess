open Core_kernel

module T = struct
  type t = int64 [@@deriving compare, equal, hash, sexp]
end

include T
include Comparable.Make (T)

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
  if Int.(i < 0 || i >= Square.Rank.count) then invalid_arg @@
    sprintf "Integer %d is not a valid rank" i
  else Array.unsafe_get ranks i

let file_exn i =
  if Int.(i < 0 || i >= Square.File.count) then invalid_arg @@
    sprintf "Integer %d is not a valid file" i
  else Array.unsafe_get files i

let rank i =
  if Int.(i < 0 || i >= Square.Rank.count) then None
  else Some (Array.unsafe_get ranks i)

let file i =
  if Int.(i < 0 || i >= Square.File.count) then None
  else Some (Array.unsafe_get files i)

(* Bitwise operators. *)

let inter = Int64.bit_and
let union = Int64.bit_or
let compl = Int64.bit_not
let[@inline] diff x y = inter x @@ compl y
let[@inline] singleton sq = Int64.(one lsl Square.to_int sq)
let[@inline] set b sq = union b @@ singleton sq
let[@inline] clear b sq = diff b @@ singleton sq
let[@inline] mem b sq = empty <> inter b @@ singleton sq
let count = Int64.popcount

(* Higher-order functions. *)

let[@inline] next_square ?(rev = false) =
  (* These are intrinsics which hopefully map to hardware instructions
     (such as `lzcnt/tzcnt` on x86). Note that the results are undefined
     if `b` is zero. *)
  if rev then fun b -> Square.last - Int64.clz b
  else Int64.ctz

let fold ?(rev = false) b ~init ~f =
  let next = next_square ~rev in
  let rec aux b acc =
    if b = empty then acc
    else
      let sq = Square.of_int_unsafe @@ next b in
      aux (clear b sq) @@ f acc sq in
  aux b init

let fold_until ?(rev = false) b ~init ~f ~finish =
  let open Continue_or_stop in
  let next = next_square ~rev in
  let rec aux b acc =
    if b = empty then finish acc
    else
      let sq = Square.of_int_unsafe @@ next b in
      match f acc sq with
      | Stop x -> x
      | Continue acc -> aux (clear b sq) acc in
  aux b init

let iter ?(rev = false) b ~f =
  let next = next_square ~rev in
  let rec aux b =
    if b <> empty then begin
      let sq = Square.of_int_unsafe @@ next b in
      f sq;
      aux @@ clear b sq
    end in
  aux b

let iter_until ?(rev = false) b ~f =
  let next = next_square ~rev in
  let rec aux b =
    if b <> empty then
      let sq = Square.of_int_unsafe @@ next b in
      if not @@ f sq then aux @@ clear b sq in
  aux b

let[@inline] filter b ~f =
  fold b ~init:b ~f:(fun acc sq -> if f sq then acc else clear acc sq)

let find ?(rev = false) b ~f =
  let next = next_square ~rev in
  let rec aux b =
    if b = empty then None
    else
      let sq = Square.of_int_unsafe @@ next b in
      if f sq then Some sq else aux @@ clear b sq in
  aux b

let first_set_exn ?(rev = false) b =
  if b = empty then invalid_arg "Find first set on an empty bitboard"
  else Square.of_int_unsafe @@ next_square b ~rev

let first_set ?(rev = false) b =
  if b = empty then None 
  else Some (Square.of_int_unsafe @@ next_square b ~rev)

(* Infix operators. *)

module Syntax = struct
  let (&) = inter
  let (+) = union
  let (~~) = compl
  let (-) = diff
  let (!!) = singleton
  let (++) = set
  let (--) = clear
  let[@inline] (@) sq b = mem b sq
end

include Syntax
