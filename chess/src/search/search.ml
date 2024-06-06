open Core_kernel [@@warning "-D"]
open Monads.Std
open Bap_future.Std
open Search_common

module Limits = Search_limits
module Tt = Search_tt
module Result = Search_result
module Root_move = Search_root_move
module State = Search_state
module Main = Search_main
module Aspiration = Search_aspiration

type limits = Limits.t
type tt = Tt.t
type result = Result.t
type state = State.t

let rec iterdeep (st : state) moves =
  if st.pv_index < st.multipv then
    let rm = State.current_root_move st in
    State.new_line st;
    Aspiration.go st moves st.root_depth rm.avg_score;
    let time = State.elapsed st in
    st.pv_index <- st.pv_index + 1;
    State.sort_root_moves st 0 ~last:st.pv_index;
    if not @@ Limits.stopped st.limits
    then iterdeep st moves
    else State.make_result st ~time
  else next st moves

(* Decide whether to continue iterating. *)
and next st moves =
  let time = State.elapsed st in
  let result = State.make_result st ~time in
  let best = State.best_root_move st in
  let score, _ = Root_move.real_score best in
  let mate = is_mate score in
  let mated = is_mated score in
  let no_ponder = not @@ State.pondering st in
  (* If movetime was specified, check if we've reached the limit. *)
  let movetime_done =
    no_ponder &&
    Limits.movetime st.limits |>
    Option.value_map ~default:false ~f:(fun n -> time >= n) in
  (* Last iteration may have eaten up at least half the allocated time,
     so the next (deeper) iteration is likely to take longer without
     having completed. Thus, we should abort the search. *)
  let too_long =
    no_ponder &&
    Limits.max_time st.limits |>
    Option.value_map ~default:false ~f:(fun n -> time * 2 >= n) in
  (* Stop searching once we've reached the depth limit. *)
  let max_depth =
    Limits.depth st.limits |>
    Option.value_map ~default:false ~f:(fun n -> st.root_depth >= n) in
  (* Stop searching once we've reached the node limit. *)
  let max_nodes =
    no_ponder &&
    Limits.nodes st.limits |>
    Option.value_map ~default:false ~f:(fun n -> st.nodes >= n) in
  (* Stop searching once we've found a mate/mated in X (if applicable). *)
  let mate_in_x = no_ponder && match Limits.mate st.limits with
    | Some n when n < 0 && mated ->
      ply_to_moves (mate_score + score) <= -n
    | Some n when n >= 0 && mate ->
      ply_to_moves (mate_score - score) <= n
    | Some _ | None -> false in
  (* Continue iterating? *)
  if movetime_done
  || too_long
  || max_nodes
  || max_depth
  || mate_in_x
  then result else begin
    State.new_iter st;
    iterdeep st moves
  end

(* Either checkmate or stalemate. *)
let no_moves ?(mzero = false) root iter =
  let score =
    let open Uci.Send.Info in
    if not mzero && Position.in_check root then Mate 0 else Cp (0, Exact) in
  let lines = [Result.Line.Fields.create ~pv:[] ~score ~seldepth:0] in
  let r = Result.Fields.create ~lines ~nodes:0 ~depth:0 ~time:0 in
  iter r;
  r

(* Of the available moves, filter out those that the user
   didn't ask us to search (if they exist). *)
let moves pos limits =
  let moves = Position.children pos in
  match Limits.moves limits with
  | [] -> moves
  | searchmoves ->
    List.filter moves ~f:(fun m ->
        List.exists searchmoves ~f:(Child.is_move m))

(* Did the user ask for checkmate? If so, we can just abort
   immediately when there's no moves for us to search. *)
let mate_in_zero limits = match Limits.mate limits with
  | Some n -> n = 0
  | None -> false

let default_currmove _ ~n:_ ~depth:_ = ()

let go
    ?(iter = ignore)
    ?(currmove = default_currmove)
    ?(ponder = None)
    ?(histogram = Position.Histogram.empty)
    ~root
    ~limits
    ~tt
    () =
  match moves root limits with
  | [] -> no_moves root iter
  | _ when mate_in_zero limits -> no_moves root iter ~mzero:true
  | moves ->
    let st =
      State.create moves ~root ~limits ~histogram
        ~tt ~iter ~currmove ~ponder in
    iterdeep st moves
