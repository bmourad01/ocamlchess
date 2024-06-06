(* Aspiration window.

   The idea is that if we have the best score from a shallower search,
   then we can use it as the basis for an initial window to search
   around (the initial values of alpha and beta).

   Then, we can recursively use the resulting scores of each search to
   adjust the window based on whether we failed low or high.
*)

open Core_kernel [@@warning "-D"]
open Monads.Std
open Bap_future.Std
open Search_common

module State = Search_state
module Main = Search_main

let min_depth = 6
let initial_delta = 10

let set_bound (st : State.t) bound =
  let rm = Array.unsafe_get st.root_moves st.pv_index in
  rm.bound <- bound

let update (st : State.t) =
  if st.multipv = 1 then
    let time = State.elapsed st in
    if time > update_time then ignore @@ State.make_result st ~time

let rec loop st moves depth ~alpha ~beta ~delta =
  let score =
    Main.with_moves st st.root moves ~alpha ~beta ~depth
      ~check:st.check ~ply:0 ~pv:true in
  State.sort_root_moves st st.pv_index;
  if not st.stopped then
    let new_delta = delta * 2 in
    if score >= beta then
      let beta = min inf (score + delta) in
      set_bound st Lower;
      update st;
      loop st moves depth ~alpha ~beta ~delta:new_delta
    else if score <= alpha then
      let beta = (alpha + beta) / 2 in
      let alpha = max (-inf) (score - delta) in
      set_bound st Upper;
      update st;
      loop st moves depth ~alpha ~beta ~delta:new_delta
    else set_bound st Exact
  else set_bound st Exact

let go st moves depth basis =
  if depth < min_depth then
    (* It shouldn't be necessary to enter the loop at lower depths.
       We will get a more accurate "best" score if we just keep
       searching a bit deeper before entering the loop. *)
    let _score = Main.with_moves st st.root moves ~depth
        ~alpha:(-inf) ~beta:inf ~check:st.check ~ply:0 ~pv:true in
    State.sort_root_moves st st.pv_index;
    set_bound st Exact
  else
    let delta = initial_delta in
    let alpha = max (-inf) (basis - delta) in
    let beta = min inf (basis + delta) in
    loop st moves depth ~alpha ~beta ~delta
