(* Helpers for the search. *)

open Core_kernel [@@warning "-D"]
open Search_common

module Tt = Search_tt
module State = Search_state
module Root_move = Search_root_move

(* The results for a single ply. *)
type 'a t = {
  mutable best  : Child.t Uopt.t;
  mutable alpha : int;
  mutable score : int;
  mutable bound : Tt.bound;
  mutable state : 'a;
}

let create ?(alpha = -inf) ?score state = {
  best = Uopt.none;
  alpha;
  score = Option.value score ~default:(-inf);
  bound = Upper;
  state;
}

let update_root_move (st : State.t) t i m score =
  State.find_root_move st m |>
  Option.iter ~f:(fun (rm : Root_move.t) ->
      Root_move.update_avg rm score;
      if i = 0 || score > t.alpha then
        let child_pv = Array.unsafe_get st.pv 1 in
        rm.score <- score;
        rm.seldepth <- st.seldepth;
        State.update_pv_aux rm.pv child_pv
      else rm.score <- (-inf))

(* Update the results of the search and return true if we fail high. *)
let cutoff ?(q = false) st t m ~score ~beta ~ply ~depth ~pv =
  let result = ref false in
  if score > t.score then begin
    t.score <- score;
    if score > t.alpha then begin
      t.best <- Uopt.some m;
      if pv && (q || ply > 0) then State.update_pv st ply m;
      if pv && score < beta then begin
        t.alpha <- score;
        if not q then t.bound <- Exact;
      end else begin
        if is_quiet m then begin
          State.update_killer st ply m;
          State.update_move_history st m depth;
          State.update_countermove st ply m;
        end;
        t.bound <- Lower;
        result := true
      end
    end
  end;
  !result

let qcutoff st (t : 'a t) = cutoff ~q:true ~depth:0 st t

(* Cache an evaluation. *)
let store (st : State.t) t pos ~depth ~ply ~score ~pv =
  let eval = Uopt.of_option @@ State.lookup_eval st ply in
  Tt.store st.tt pos ~ply ~depth ~score ~eval ~best:t.best ~bound:t.bound ~pv
