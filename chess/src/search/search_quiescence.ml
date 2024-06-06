(* Quiescence search is used when we reach our maximum depth for the main
   search. The goal is then to keep searching only "noisy" positions, until
   we reach one that is "quiet", and then return our evaluation. *)

open Core_kernel [@@warning "-D"]
open Search_common

module Tt = Search_tt
module Ply = Search_ply
module State = Search_state
module Order = Search_order
module Iterator = Search_iterator

(* Delta pruning.

   Skip searching this position if we're not in endgame and the static
   evaluation is significantly lower than alpha.
*)
let delta =
  let margin = 900 in
  fun pos eval alpha ->
    let active = Position.active pos in
    Position.has_non_pawn_material pos active &&
    eval + margin < alpha

let static_eval st pos ~ply ~(ttentry : Tt.entry option) = match ttentry with
  | None when ply <= 0 -> Eval.go pos
  | None when not (State.is_null_move st (ply - 1)) -> Eval.go pos
  | None -> -(State.lookup_eval_unsafe st (ply - 1))
  | Some {eval; _} when Uopt.is_none eval -> Eval.go pos
  | Some {eval; } -> Uopt.unsafe_value eval

let evaluate st pos ~alpha ~beta ~ply ~depth ~check ~ttentry ~pv =
  if not check then
    let eval = static_eval st pos ~ply ~ttentry in
    State.update_eval st ~ply ~eval |> ignore;
    let score = match ttentry with
      | Some {score; bound = Lower; _} -> max eval @@ Tt.to_score score ply
      | Some {score; bound = Upper; _} -> min eval @@ Tt.to_score score ply
      | None | Some _ -> eval in
    if score >= beta then begin
      if Option.is_none ttentry then
        Tt.store st.tt pos ~ply ~depth ~score
          ~eval:(Uopt.some eval) ~best:Uopt.none
          ~bound:Lower ~pv:false;
      First score
    end else if delta pos score alpha then First score
    else Second ((if pv then max score alpha else alpha), Some eval)
  else begin
    State.clear_eval st ply;
    Second (alpha, None)
  end

(* TT entries for quiescence search have a depth of either 0 or -1,
   depending on whether this is the first ply or we're in check. *)
let tt_depth ~check ~init = b2i (check || init) - 1

(* To avoid state explosion, only generate quiet checks at the root. *)
let order st moves pos ~ply ~check ~init ~ttentry =
  match Order.qscore moves ~check:init ~ttentry with
  | Some it -> Some (it, false)
  | None when not check -> None
  | None -> Some (Order.qescore st pos moves, true)

(* Search a position until it becomes "quiet". *)
let rec go ?(init = true) st pos ~alpha ~beta ~ply ~pv =
  let check = Position.in_check pos in
  if State.drawn st pos then 0
  else if ply >= max_ply || State.check_limits st then end_of_line pos ~check
  else Position.children pos |> function
    | [] -> if check then mated ply else 0
    | moves -> with_moves st pos moves ~alpha ~beta ~ply ~check ~pv ~init

(* Search the available moves for a given position, but only if they are
   "noisy". *)
and with_moves ?(init = true) st pos moves ~alpha ~beta ~ply ~check ~pv =
  let depth = tt_depth ~check ~init in
  match State.lookup st pos ~depth ~ply ~alpha ~beta ~pv with
  | First score -> score
  | Second ttentry ->
    match evaluate st pos ~alpha ~beta ~ply ~depth ~check ~ttentry ~pv with
    | First score -> score
    | Second (alpha, score) ->
      match order st moves pos ~ply ~check ~init ~ttentry with
      | None -> Option.value_exn score
      | Some (it, quiet_evasion) ->
        (* The `state` field is the number of quiet evasions that have
           occurred so far. *)
        let t = Ply.create ~alpha ?score 0 in
        let finish () = t.score in
        let score =
          (Iterator.fold_until [@specialised]) it ~init:() ~finish ~f:(fun u e ->
              child st t u e ~beta ~ply ~pv ~check ~quiet_evasion) in
        Ply.store st t pos ~depth ~ply ~score ~pv;
        score

(* Search a child of the current node. *)
and child st t ~beta ~ply ~pv ~check ~quiet_evasion = fun () (m, order) ->
  let open Continue_or_stop in
  should_skip t m ~order ~check ~quiet_evasion |> function
  | true -> Continue ()
  | false ->
    let pos = Child.self m in
    State.set_move st ply m;
    State.incr st pos;
    State.inc_nodes st;
    let score = Int.neg @@ go st pos ~pv
        ~init:false
        ~ply:(ply + 1)
        ~alpha:(-beta)
        ~beta:(-t.alpha) in
    State.decr st pos;
    if Ply.qcutoff st t m ~score ~beta ~ply ~pv then Stop t.score
    else if st.stopped then Stop t.score
    else Continue ()

and should_skip t m ~order ~check ~quiet_evasion =
  t.score > mated max_ply && begin
    (* If we didn't generate quiet check evasions, then ignore moves
       with negative SEE values. *)
    (not quiet_evasion && order <= Order.bad_capture_offset) ||
    (* If we're in check, then allow only one non-capturing evasion
       to be searched. *)
    (check && not (Child.is_capture m) && begin
        let result = t.state > 1 in
        t.state <- t.state + 1;
        result
      end)
  end
