(* Move ordering is critical for optimizing the performance of alpha-beta
   pruning. We use some heuristics to determine which moves are likely
   to be the best, and then search those first, hoping that the worse
   moves get pruned more effectively. *)

open Core_kernel [@@warning "-D"]
open Monads.Std
open Bap_future.Std
open Search_common

module Tt = Search_tt
module State = Search_state
module Iterator = Search_iterator

let hash_offset = inf
let good_capture_offset = 100
let bad_capture_offset = -100
let promote_offset = 96
let killer1_offset = 95
let killer2_offset = 94
let countermove_offset = 93
let castle_offset = 92
let history_offset = -90
let history_scale = 180

let is_hash ttentry = match (ttentry : Tt.entry option) with
  | None -> fun _ -> false
  | Some {best; _} ->
    if Uopt.is_none best then fun _ -> false
    else
      let best = Uopt.unsafe_value best in
      fun m -> same_move best m

let promote m =
  let open Move.Promote in
  Child.move m |> Move.promote |>
  Option.value_map ~default:0 ~f:(function
      | Queen -> promote_offset + 3
      | Rook -> promote_offset + 2
      | Bishop -> promote_offset + 1
      | Knight -> promote_offset)

(* This is exactly what the OCaml standard library implementation does,
   except we fuse it with a `map` operation so that we avoid allocating
   a new list or array. *)
let array_of_list_map ~f = function
  | [] -> [||]
  | (x :: xs) as l ->
    let a = Array.create ~len:(List.length l) @@ f x in
    let rec fill i = function
      | [] -> a
      | x :: xs ->
        Array.unsafe_set a i @@ f x;
        fill (succ i) xs in
    fill 1 xs

(* Score each move according to `f`. *)
let score_aux moves ~f =
  Iterator.create @@ array_of_list_map moves ~f:(fun m -> m, f m)

(* Score the moves for normal search. Priority (from best to worst):

   1. Moves that were cached in the TT.
   2. Captures that produced a non-negative SEE score.
   3. Promotions.
   4. Refutations.
   5. Castling moves.
   6. Move history score (the "distance" heuristic).
   7. Captures that produced a negative SEE score.
*)
let score st moves ~ply ~pos ~ttentry =
  let is_hash = is_hash ttentry in
  let refutation =
    let k1 = State.killer1 st ply in
    let k2 = State.killer2 st ply in
    let cm = State.countermove st pos ply in
    fun m -> match k1 with
      | Some k when same_move m k -> killer1_offset
      | _ -> match k2 with
        | Some k when same_move m k -> killer2_offset
        | _ -> match cm with
          | Some c when same_move m c -> countermove_offset
          | _ -> 0 in
  let move_history =
    let max = State.move_history_max st pos in
    let maxf = Float.of_int max in
    fun m ->
      (* We "squish" the history score so that it fits between bad
         captures and castling moves. *)
      let h = State.move_history st m in
      let m = (h * history_scale) + max - 1 in
      Int.of_float_unchecked (Int.to_float m /. maxf) + history_offset in
  score_aux moves ~f:(fun m ->
      if is_hash m then hash_offset
      else if Child.is_capture m then
        let see = See.go m in
        if see >= 0 then good_capture_offset + see
        else bad_capture_offset + see
      else
        let promote = promote m in
        if promote <> 0 then promote
        else
          let refute = refutation m in
          if refute <> 0 then refute
          else match Child.castle_side m with
            | Some _ -> castle_offset
            | None -> move_history m)

(* Score the moves for quiescence search. *)
let qscore moves ~ttentry ~check =
  let is_hash = is_hash ttentry in
  let f m = is_noisy m ~check || is_hash m in
  List.filter moves ~f |> function
  | [] -> None
  | moves -> Some (score_aux moves ~f:(fun m ->
      if is_hash m then hash_offset
      else if Child.is_capture m then
        let see = See.go m in
        if see >= 0 then good_capture_offset + see
        else bad_capture_offset + see
      else promote m))

(* Score the moves for quiescence search when we generate
   quiet check evasions. *)
let qescore (st : State.t) pos moves =
  score_aux moves ~f:(State.move_history st)

(* Score the moves for ProbCut. *)
let pcscore moves ~ttentry =
  let is_hash = is_hash ttentry in
  let f m = Child.is_capture m || is_hash m in
  List.filter moves ~f |> function
  | [] -> None
  | moves -> Some (score_aux moves ~f:(fun m ->
      if is_hash m then hash_offset
      else See.go m))
