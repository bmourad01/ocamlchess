(* Transposition table for caching search results. *)

open Core_kernel [@@warning "-D"]
open Search_common

type bound = Uci.Send.Info.bound [@@deriving equal]

module Entry = struct
  type t = {
    key   : Zobrist.key;
    depth : int;
    score : int;
    eval  : int Uopt.t;
    best  : Child.t Uopt.t;
    bound : bound;
  } [@@deriving fields]

  let eval e = Uopt.to_option e.eval
  let best e = Uopt.to_option e.best

  (* There is some extra space being used for heap-allocated values,
     but we will ignore it. *)
  let size = 6 * (Caml.Sys.word_size / 8)
end

type entry = Entry.t

(* We're going to use a fixed-size flat hash table. I've found that
   searches to a depth of >=12 will cache too many positions using
   the standard library's hash table.

   Thus, with a fixed size we need to decide which entries to evict,
   and when.
*)
type t = entry Oa.t

let create ?(mb = 32) () =
  if mb <= 0 then
    invalid_argf
      "Invalid transposition table size %dmb, must be greater than 0"
      mb ()
  else Oa.create ~len:((mb * 0x100000) / Entry.size)

let clear tt = Oa.clear tt

external mul_hi64 :
  (int64[@unboxed]) ->
  (int64[@unboxed]) ->
  (int [@untagged]) =
  "ocamlchess_mul_hi64" "ocamlchess_mul_hi64_unboxed" [@@noalloc]

let slot tt key = mul_hi64 key @@ Int64.of_int @@ Oa.length tt

let find tt pos =
  let key = Position.hash pos in
  let i = slot tt key in
  Oa.unsafe_get tt i |> Option.bind ~f:(fun entry ->
      Option.some_if (Zobrist.equal_key key entry.Entry.key) entry)

let of_score score ply =
  if is_mate score then score + ply
  else if is_mated score then score - ply
  else score

(* Decide whether we should keep an existing entry, based on the
   parameters of the new entry. *)
let keep (e : entry) depth bound pv =
  let pv = b2i pv * 2 in
  let ex = b2i @@ equal_bound bound Exact in
  depth + pv + ex <= e.depth

(* Store the evaluation results for the position. *)
let store tt pos ~ply ~depth ~score ~eval ~best ~bound ~pv =
  let key = Position.hash pos in
  let i = slot tt key in
  match Oa.unsafe_get tt i with
  | Some e when keep e depth bound pv -> ()
  | None | Some _ ->
    let e = Entry.Fields.create ~key ~depth ~score ~eval ~best ~bound in
    Oa.unsafe_set_some tt i e

let to_score score ply =
  if is_mate score then score - ply
  else if is_mated score then score + ply
  else score

(* Check for a previous evaluation of the position at a comparable depth.
   For non-PV nodes, we can cut off the search early based on the bounds
   of the score for the entry. *)
let lookup tt ~pos ~depth ~ply ~alpha ~beta ~pv = match find tt pos with
  | None -> Second None
  | Some entry when pv || ply <= 0 || Entry.depth entry < depth ->
    Second (Some entry)
  | Some entry ->
    let score = to_score entry.score ply in
    match entry.bound with
    | Exact -> First score
    | Lower when score >= beta -> First score
    | Upper when score <= alpha -> First score
    | _ -> Second (Some entry)
