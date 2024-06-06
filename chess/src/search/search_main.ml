(* The main search of the game tree. The core of it is the negamax algorithm
   with alpha-beta pruning (and other enhancements). *)

open Core_kernel [@@warning "-D"]
open Monads.Std
open Bap_future.Std
open Search_common

module Limits = Search_limits
module Tt = Search_tt
module Result = Search_result
module State = Search_state
module Iterator = Search_iterator
module Order = Search_order
module Ply = Search_ply
module Quiescence = Search_quiescence

let static_eval st pos ~(ttentry : Tt.entry option) = match ttentry with
  | Some {eval; _} when Uopt.is_none eval -> Eval.go pos
  | Some {eval; _} -> Uopt.unsafe_value eval
  | None -> Eval.go pos

let evaluate st pos ~ply ~check ~ttentry =
  if not check then
    let eval = static_eval st pos ~ttentry in
    let improving = State.update_eval st ~ply ~eval in
    let score = match ttentry with
      | Some {score; bound = Lower; _} -> max eval @@ Tt.to_score score ply
      | Some {score; bound = Upper; _} -> min eval @@ Tt.to_score score ply
      | None | Some _ -> eval in
    Some score, improving
  else begin
    State.clear_eval st ply;
    None, false
  end

(* Search from a new position. *)
let rec go (st : State.t) pos ~alpha ~beta ~ply ~depth ~pv =
  let check = Position.in_check pos in
  if pv then st.seldepth <- max st.seldepth ply;
  if State.drawn st pos then 0
  else if ply >= max_ply || State.check_limits st then end_of_line pos ~check
  else match Position.children pos with
    | [] -> if check then mated ply else 0
    | moves -> match mdp ~alpha ~beta ~ply with
      | First alpha -> alpha
      | Second (alpha, beta) ->
        (* Check + single reply extension. *)
        let single = match moves with [_] -> true | _ -> false in
        let depth = depth + b2i (check || single) in
        if depth <= 0 then
          Quiescence.with_moves st pos moves ~alpha ~beta ~ply ~check ~pv
        else with_moves st pos moves ~alpha ~beta ~ply ~depth ~check ~pv

(* Search the available moves for the given position. *)
and with_moves st pos moves ~alpha ~beta ~ply ~depth ~check ~pv =
  match State.lookup st pos ~depth ~ply ~alpha ~beta ~pv with
  | First score -> score
  | Second ttentry ->
    let score, improving = evaluate st pos ~ply ~check ~ttentry in
    prune_non_pv_node st pos moves ~score ~alpha ~beta
      ~ply ~depth ~pv ~improving ~ttentry |> function
    | Some score -> score
    | None ->
      let it = Order.score st moves ~ply ~pos ~ttentry in
      let t = Ply.create ~alpha () in
      let finish _ = t.score in
      let score =
        (Iterator.fold_until [@specialised]) it ~init:0 ~finish ~f:(fun i e ->
            child st t pos i e ~beta ~depth ~ply ~check ~pv ~improving ~ttentry) in
      if should_store st ply then Ply.store st t pos ~depth ~ply ~score ~pv;
      score

(* Mate distance pruning.

   If we've already found a shorter path to checkmate than our current
   distance from the root, then just cut off the search here.
*)
and mdp ~alpha ~beta ~ply =
  if ply > 0 then
    let alpha = max alpha @@ mated ply in
    let beta = min beta @@ mating (ply + 1) in
    if alpha >= beta then First alpha else Second (alpha, beta)
  else Second (alpha, beta)

(* Don't store the results in the TT if:

   - We're in a Singular Extension search.
   - We're in the root position and we're searching a line that isn't
     the PV.
*)
and should_store (st : State.t) ply =
  not (State.has_excluded st ply || (ply = 0 && st.pv_index > 0))

(* If we're not in a PV search and we're not in check, then try a
   variety of pruning heuristics before we search the children of
   this node.

   Note that `score` should be `None` if the position is in check.
*)
and prune_non_pv_node st pos moves ~score ~alpha
    ~beta ~ply ~depth ~pv ~improving ~ttentry =
  match score with
  | None -> None
  | Some _ when pv -> None
  | Some score ->
    razor st pos moves ~score ~alpha ~ply ~depth >>? fun () ->
    rfp ~depth ~score ~beta ~improving >>? fun () ->
    nmp st pos ~score ~beta ~ply ~depth ~ttentry >>? fun () ->
    probcut st pos moves ~depth ~ply ~beta ~ttentry ~improving

(* Search a child of the current node. *)
and child st t pos ~beta ~depth ~ply ~check ~pv
    ~improving ~ttentry = fun i (m, order) ->
  let open Continue_or_stop in
  let[@inline] next () = Continue (i + 1) in
  if no_root_move st m ~ply then next () else begin
    emit_update st m ~i ~ply ~depth;
    if should_skip st t m ~i ~beta ~depth ~ply ~order then next()
    else match semc st pos m ~depth ~ply ~beta ~check ~ttentry with
      | First score -> Stop score
      | Second _ when st.stopped -> Stop t.score
      | Second ext ->
        let r = lmr st m ~i ~order ~ply ~depth ~check ~pv ~improving in
        let pos = Child.self m in
        State.set_move st ply m;
        State.inc_nodes st;
        State.incr st pos;
        let score = pvs st t pos ~i ~r ~beta ~ply ~depth:(depth + ext) ~pv in
        State.decr st pos;
        if ply = 0 then Ply.update_root_move st t i m score;
        if Ply.cutoff st t m ~score ~beta ~ply ~depth ~pv then Stop t.score
        else if st.stopped then Stop t.score
        else next ()
  end

and emit_update st m ~i ~ply ~depth =
  if ply = 0 && State.elapsed st >= update_time then
    st.currmove m ~n:(i + 1) ~depth

and should_skip st t m ~i ~beta ~depth ~ply ~order =
  State.is_excluded st ply m || begin
    i > 0 && t.score > mated max_ply && begin
      see m ~order ~depth ||
      futile st t m ~beta ~ply ~depth
    end
  end

(* For multi-PV searching, ignore moves that are ordered before the current
   PV index. *)
and no_root_move (st : State.t) m ~ply =
  ply = 0
  && st.multipv > 1
  && Option.is_none (State.find_root_move st m ~pos:st.pv_index)

(* Futility pruning.

   We want to skip searching quiet moves if our static evaluation of the
   position is within a margin below alpha, since it is unlikely that such
   moves will improve alpha.

   This won't apply if the position is in check (since we don't perform
   static evaluations in this situation).
*)
and futile st t m ~beta ~ply ~depth = match State.lookup_eval st ply with
  | None -> false
  | Some eval ->
    ply > 0
    && depth <= futile_max_depth
    && is_quiet m
    && eval + futile_margin depth < t.alpha

and futile_margin depth =
  let m = 100 in
  m + m * depth

and futile_max_depth = 6

(* Bad captures have a particular offset in the move ordering, corresponding
   to a negative SEE value.

   If this move is unlikely to allow us compensation given the current depth
   of the search, then just skip the move.
*)
and see m ~order ~depth =
  order <= Order.bad_capture_offset &&
  order - Order.bad_capture_offset < depth * see_margin

and see_margin = -(Piece.Kind.value Pawn * 2)

(* Reverse futility pruning.

   If our score is within a margin above beta, then it is likely too
   good, and should cause a cutoff.
*)
and rfp ~depth ~score ~beta ~improving =
  Option.some_if begin
    depth <= rfp_max_depth &&
    score - rfp_margin depth improving >= beta
  end score

and rfp_margin depth improving =
  let r = b2i improving in
  134 * (depth - r)

and rfp_max_depth = 8

(* Null move pruning.

   If we forfeit our right to play a move and our opponent's best
   response still produces a beta cutoff, then we know this position
   is unlikely.

   This should not be called when the position is in check.
*)
and nmp st pos ~score ~beta ~ply ~depth ~ttentry =
  let active = Position.active pos in
  if not (State.is_null_move st (ply - 1))
  && not (State.has_excluded st ply)
  && score >= beta
  && score >= State.lookup_eval_unsafe st ply
  && depth >= nmp_min_depth
  && Position.has_non_pawn_material pos active
  && nmp_check_ttentry beta ttentry then
    let r = if depth <= 6 then 2 else 3 in
    State.null_move st ply;
    let score =
      Position.Unsafe.null_move pos |> go st
        ~alpha:(-beta)
        ~beta:(-beta + 1)
        ~ply:(ply + 1)
        ~depth:(depth - r - 1)
        ~pv:false |> Int.neg in
    Option.some_if (score >= beta) score
  else None

(* If the TT entry is available, its contents may suggest that the
   search would fail immediately. *)
and nmp_check_ttentry beta = function
  | Some Tt.Entry.{bound = Upper; score; _} -> score >= beta
  | Some _ | None -> true

and nmp_min_depth = 3

(* Razoring.

   When approaching the horizon, if our evaluation is significantly
   lower than alpha, then drop down to quiescence search to see if
   the position can be improved.
*)
and razor st pos moves ~score ~alpha ~ply ~depth =
  if depth <= razor_max_depth && score + razor_margin depth < alpha then
    let score = Quiescence.with_moves st pos moves ~ply
        ~pv:false ~check:false ~alpha:(alpha - 1) ~beta:alpha in
    Option.some_if (score < alpha) score
  else None

and razor_max_depth = 7

and razor_margin =
  let p = 100 in
  let n = 300 in
  let base = n + (p / 2) in
  let mult = n - (p / 2) in
  fun depth -> base + mult * depth * depth

(* ProbCut

   If we have any threats and a reduced search gives us a score within
   a margin above beta, then we can safely prune this branch.
*)
and probcut st pos moves ~depth ~ply ~beta ~ttentry ~improving =
  let beta_cut = beta + probcut_margin improving in
  if depth >= probcut_min_depth
  && not (is_mate beta || is_mated beta)
  && probcut_tt ~ply ~depth ~beta_cut ~ttentry then
    Order.pcscore moves ~ttentry |> Option.bind ~f:(fun it ->
        let init = None and finish = Fn.id in
        let eval = State.lookup_eval_unsafe st ply in
        (Iterator.fold_until [@specialised]) it ~init ~finish ~f:(fun k e ->
            probcut_child st pos k e ~eval ~depth ~ply ~beta_cut))
  else None

(* Confirm the move with quiescence search before dropping down to the
   full search. *)
and probcut_child st pos ~eval ~depth ~ply ~beta_cut = fun k (m, order) ->
  let open Continue_or_stop in
  if not @@ probcut_skip st m ~eval ~beta_cut ~ply ~order then
    let alpha = -beta_cut in
    let beta = -beta_cut + 1 in
    let child = Child.self m in
    State.set_move st ply m;
    State.incr st child;
    State.inc_nodes st;
    let score = Int.neg @@ Quiescence.go st child
        ~alpha ~beta ~ply:(ply + 1) ~pv:false in
    let score =
      if score >= beta_cut then
        let depth = depth - (probcut_min_depth - 1) in
        Int.neg @@ go st child ~alpha ~beta ~depth
          ~ply:(ply + 1) ~pv:false
      else score in
    State.decr st child;
    if score >= beta_cut then
      let depth = depth - (probcut_min_depth - 2) in
      Tt.store st.tt pos ~ply ~depth ~score
        ~best:(Uopt.some m) ~eval:(Uopt.some eval)
        ~bound:Lower ~pv:false;
      Stop (Some score)
    else if st.stopped then Stop None
    else Continue None
  else Continue None

(* Excluded moves as well as moves whose SEE values are below our
   threshold should be skipped. Note that we convert the score to
   centipawns.
*)
and probcut_skip st m ~eval ~beta_cut ~ply ~order =
  State.is_excluded st ply m || begin
    order <> Order.hash_offset &&
    order * 100 < beta_cut - eval
  end

(* If the evaluation was cached then make sure that it doesn't refute
   our hypothesis. *)
and probcut_tt ~ply ~depth ~beta_cut ~ttentry = match ttentry with
  | Some (entry : Tt.entry) ->
    entry.depth < depth - (probcut_min_depth - 2) ||
    Tt.to_score entry.score ply >= beta_cut
  | None -> true

and probcut_margin improving =
  let p = 100 in
  let n = 300 in
  (n / 2) - ((p / 2) * b2i improving)

and probcut_min_depth = 5

(* Singular extensions + multi-cut.

   If we have a TT entry with the best move, then try a reduced search of
   the position where we search every move except this one. If the result
   is within a margin below the score for this TT entry, then we say that
   this move is "singularly" good, and thus we should extend the search
   depth.

   On the other hand, if this entry has a score within a margin above beta,
   then it probably isn't singular and we can either cut off the search
   early or reduce the depth.
*)
and semc st pos m ~depth ~ply ~beta ~ttentry ~check = match ttentry with
  | None -> Second 0
  | Some (entry : Tt.entry) ->
    let ttscore = Tt.(to_score entry.score ply) in
    if not check
    && ply > 0
    && ply < st.root_depth * 2
    && Tt.equal_bound entry.bound Lower
    && not (is_mate ttscore || is_mated ttscore)
    && depth >= se_min_depth
    && not (State.has_excluded st ply)
    && Uopt.is_some entry.best
    && same_move m (Uopt.unsafe_value entry.best)
    && entry.depth >= se_min_ttdepth depth then
      let target = ttscore - depth * 3 in
      let depth = (depth - 1) / 2 in
      State.set_excluded st ply @@ Child.move m;
      let score = go st pos ~depth ~ply
          ~alpha:(target - 1)
          ~beta:target
          ~pv:false in
      State.clear_excluded st ply;
      if score < target then Second 1
      else if target >= beta then First target
      else if ttscore >= beta then Second (-2)
      else Second 0
    else Second 0

and se_min_depth = 4
and se_min_ttdepth depth = depth - (se_min_depth - 1)

(* Late move reduction.

   For quiet moves that are ordered later in the list, try reducing the
   depth of the search. We should avoid doing this for the first couple
   of moves, or if the depth is too low.

   Note that singular extensions will only happen for TT moves, which
   are always ordered first, so we don't need to worry about it interacting
   with the results of LMR.
*)
and lmr st m ~i ~order ~ply ~depth ~check ~pv ~improving =
  if not check
  && i >= lmr_min_index
  && depth >= lmr_min_depth
  && order < Order.promote_offset then
    let r = ref @@ Array.unsafe_get lmr_table (depth * max_moves + i) in
    if Child.gives_check m then decr r;
    if Bb.(Child.new_threats m <> empty) then decr r;
    if improving then decr r;
    if not pv then incr r;
    if order > Order.bad_capture_offset then begin
      if Child.is_passed_pawn m then decr r;
      if See.go m < 0 then incr r;
      r := !r - min 2 (State.move_history st m / 5000);
    end;
    if State.is_maximizing_player st m then incr r;
    max 0 !r
  else 0

and lmr_min_depth = 3
and lmr_min_index = 2

and lmr_table =
  let t = Array.create ~len:(max_ply * max_moves) 0 in
  for depth = 1 to max_ply - 1 do
    for i = 1 to max_moves - 1 do
      let fd = Float.of_int depth in
      let fi = Float.of_int i in
      let f = 0.75 +. log fd *. log fi  /. 2.25 in
      t.(depth * max_moves + i) <- Float.to_int f
    done
  done;
  t

(* Principal variation search.

   Attempt to search with a zero window around alpha, and do a full
   search if the score is within our normal window. This can allow us
   to skip lines that are unlikely to be part of the PV.
*)
and pvs st t pos ~i ~r ~beta ~ply ~depth ~pv =
  let[@specialise] go ?(r = 0) alpha ~pv =
    Int.neg @@ go st pos ~pv ~alpha
      ~beta:(-t.alpha)
      ~ply:(ply + 1)
      ~depth:(depth - r - 1) in
  (* Zero and full window for alpha. *)
  let zw = -t.alpha - 1 and fw = -beta in
  if pv then
    (* First move in a PV node should always search the full window. *)
    if i = 0 then go fw ~pv:true
    else
      let score = go zw ~r ~pv:false in
      (* Ignore beta if this is the root node. *)
      if score > t.alpha && (ply = 0 || score < beta)
      then go fw ~pv:true else score
  else
    let score = go zw ~r ~pv:false in
    (* Ignore beta if we're reducing the depth. *)
    if score > t.alpha && (r > 0 || score < beta)
    then go fw ~pv:false else score
