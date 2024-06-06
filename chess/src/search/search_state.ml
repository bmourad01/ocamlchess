(* Our state for the entirety of the search. *)

open Core_kernel [@@warning "-D"]
open Bap_future.Std
open Search_common

module Tt = Search_tt
module Limits = Search_limits
module Result = Search_result
module Root_move = Search_root_move

type t = {
  limits                     : Limits.t;
  root                       : Position.t;
  root_moves                 : Root_move.t array;
  check                      : bool;
  histogram                  : (Zobrist.key, int) Hashtbl.t;
  tt                         : Tt.t;
  start_time                 : Time.t;
  evals                      : int Oa.t;
  excluded                   : Move.t Oa.t;
  pv                         : Child.t Oa.t array;
  moves                      : Child.t Oa.t;
  countermoves               : Child.t Oa.t;
  killer1                    : Child.t Oa.t;
  killer2                    : Child.t Oa.t;
  move_history               : int array;
  mutable move_history_max_w : int;
  mutable move_history_max_b : int;
  mutable stopped            : bool;
  mutable seldepth           : int;
  mutable nodes              : int;
  mutable root_depth         : int;
  mutable pv_index           : int;
  multipv                    : int;
  iter                       : Result.t -> unit;
  currmove                   : Child.t -> n:int -> depth:int -> unit;
  ponder                     : unit future option;
}

let pv_size = max_ply + 2
let killer_size = max_ply
let move_history_size = Piece.Color.count * Square.(count * count)
let countermove_size = Piece.Color.count * Piece.Kind.count * Square.count

let create_frequency_table h root =
  let tbl = Hashtbl.create (module Int64) in
  Position.Histogram.to_sequence h |>
  Sequence.iter ~f:(fun (key, data) -> Hashtbl.set tbl ~key ~data);
  Position.hash root |> Hashtbl.update tbl ~f:(function
      | Some n -> n | None -> 1);
  tbl

let create
    moves
    ~limits
    ~root
    ~histogram
    ~tt
    ~iter
    ~currmove
    ~ponder =
  let root_moves = List.to_array moves |> Array.map ~f:Root_move.create in
  let histogram = create_frequency_table histogram root in {
    limits;
    root;
    root_moves;
    check = Position.in_check root;
    histogram;
    tt;
    start_time = Time.now ();
    evals = Oa.create ~len:max_ply;
    excluded = Oa.create ~len:max_ply;
    pv = Array.init pv_size ~f:(fun _ -> Oa.create ~len:pv_size);
    moves = Oa.create ~len:max_ply;
    countermoves = Oa.create ~len:countermove_size;
    killer1 = Oa.create ~len:killer_size;
    killer2 = Oa.create ~len:killer_size;
    move_history = Array.create ~len:move_history_size 0;
    move_history_max_w = 1;
    move_history_max_b = 1;
    stopped = false;
    seldepth = 1;
    nodes = 0;
    root_depth = 1;
    pv_index = 0;
    multipv = min limits.multipv @@ Array.length root_moves;
    iter;
    currmove;
    ponder;
  }

let elapsed st =
  int_of_float @@ Time.(Span.to_ms @@ diff (now ()) st.start_time)

(* Start a new iteration. *)
let new_iter st =
  st.pv_index <- 0;
  st.stopped <- false;
  st.root_depth <- st.root_depth + 1;
  Array.iter st.root_moves ~f:(fun rm -> rm.prev_score <- rm.score)

(* Searching a new line. *)
let new_line st =
  st.seldepth <- 1;
  Array.iter st.pv ~f:Oa.clear

(* If we've found a mate that's shorter than the root depth, then we will
   want to cut off the PV extraction at that point. *)
let pv_depth st score ~mate ~mated =
  if mate then min (mate_score - score) st.root_depth
  else if mated then min (mate_score + score) st.root_depth
  else st.root_depth

(* Increment the number of nodes we've evaluated. *)
let inc_nodes st = st.nodes <- st.nodes + 1

(* Return true if the move was played by the active player of the
   root node. *)
let is_maximizing_player st child =
  let active = Position.active st.root in
  let parent = Child.parent child in
  let current = Position.active parent in
  Piece.Color.equal active current

(* Get the first killer move. *)
let killer1 st ply = Oa.get st.killer1 ply

(* Get the second killer move. *)
let killer2 st ply = Oa.get st.killer2 ply

(* Is `m` a killer move? *)
let is_killer st m ply = match killer1 st ply with
  | Some k when same_move k m -> true
  | _ -> match killer2 st ply with
    | Some k -> same_move k m
    | None -> false

(* Update the killer move for a particular ply. *)
let update_killer st ply m =
  match killer1 st ply with
  | Some k when same_move k m -> ()
  | None -> Oa.set_some st.killer1 ply m
  | Some k ->
    Oa.set_some st.killer2 ply k;
    Oa.set_some st.killer1 ply m

let move_history_idx m =
  let c = Piece.Color.to_int @@ Position.active @@ Child.parent m in
  let m = Child.move m in
  let src = Square.to_int @@ Move.src m in
  let dst = Square.to_int @@ Move.dst m in
  ((src lsl 1) lor c) * Square.count + dst

let move_history st m =
  Array.unsafe_get st.move_history @@ move_history_idx m

(* Update the move history heuristic. *)
let update_move_history st m depth =
  if depth > 0 then
    let i = move_history_idx m in
    let d = Array.unsafe_get st.move_history i + (depth * depth) in
    Array.unsafe_set st.move_history i d;
    match Position.active @@ Child.parent m with
    | White ->
      st.move_history_max_w <- max d st.move_history_max_w
    | Black ->
      st.move_history_max_b <- max d st.move_history_max_b

let move_history_max st pos = match Position.active pos with
  | White -> st.move_history_max_w
  | Black -> st.move_history_max_b

(* Update the evaluation history and return whether our position is
   improving or not.

   We use an idea from Stockfish where, if our previous position was
   in check, we can try looking at the one before that.
*)
let update_eval st ~ply ~eval =
  Oa.unsafe_set_some st.evals ply eval;
  ply < 2 || match Oa.unsafe_get st.evals (ply - 2) with
  | Some e -> eval > e
  | None when ply < 4 -> true
  | None -> match Oa.unsafe_get st.evals (ply - 4) with
    | Some e -> eval > e
    | None -> true

let lookup_eval st ply = Oa.unsafe_get st.evals ply
let lookup_eval_unsafe st ply = Oa.unsafe_get_some_exn st.evals ply
let clear_eval st ply = Oa.unsafe_set_none st.evals ply
let stop st = st.stopped <- true
let incr st pos = Hashtbl.incr st.histogram @@ Position.hash pos

let decr st pos =
  Hashtbl.decr ~remove_if_zero:true st.histogram @@ Position.hash pos

let pondering st = match st.ponder with
  | Some p -> not @@ Future.is_decided p
  | None -> false

let excluded st ply = Oa.unsafe_get st.excluded ply
let has_excluded st ply = Oa.unsafe_is_some st.excluded ply
let set_excluded st ply m = Oa.unsafe_set_some st.excluded ply m
let clear_excluded st ply = Oa.unsafe_set_none st.excluded ply

let is_excluded st ply m =
  excluded st ply |> Option.exists ~f:(Child.is_move m)

let update_pv_aux pv child_pv =
  let[@inline] rec append_child i =
    match Oa.unsafe_get child_pv (i - 1) with
    | None -> Oa.unsafe_set_none pv i
    | Some m ->
      Oa.unsafe_set_some pv i m;
      append_child (i + 1) in
  append_child 1

let update_pv st ply m =
  let pv = Array.unsafe_get st.pv ply in
  let child_pv = Array.unsafe_get st.pv (ply + 1) in
  Oa.unsafe_set_some pv 0 m;
  update_pv_aux pv child_pv

let extract_lines st =
  Array.foldi st.root_moves ~init:[] ~f:(fun i acc rm ->
      if i < st.multipv then
        let score, updated = Root_move.real_score rm in
        if st.root_depth > 1 || updated || i = 0 then
          let bound = rm.bound in
          let mate = is_mate score in
          let mated = is_mated score in
          let pv =
            let depth = pv_depth st score ~mate ~mated in
            let init = 0, [] and finish = Fn.compose List.rev snd in
            Oa.fold_until rm.pv ~init ~finish ~f:(fun (i, l) -> function
                | Some _ when i >= depth -> Stop (List.rev l)
                | Some m -> Continue (i + 1, m :: l)
                | None -> Stop (List.rev l)) in
          let line = Result.Line.Fields.create ~pv
              ~score:(convert_score score ~bound ~mate ~mated)
              ~seldepth:rm.seldepth in
          line :: acc
        else acc
      else acc) |> List.rev

let current_move st ply = Oa.unsafe_get st.moves ply
let set_move st ply m = Oa.unsafe_set_some st.moves ply m
let null_move st ply = Oa.unsafe_set_none st.moves ply
let is_null_move st ply = ply >= 0 && Oa.is_none st.moves ply

let countermove_idx p sq =
  Piece.to_int p * Square.count + Square.to_int sq

let countermove st pos ply =
  if ply > 0 then match Oa.unsafe_get st.moves (ply - 1) with
    | None -> None
    | Some prev ->
      let dst = Move.dst @@ Child.move prev in
      match Position.piece_at_square pos dst with
      | None -> None
      | Some p ->
        let i = countermove_idx p dst in
        Oa.unsafe_get st.countermoves i
  else None

let update_countermove st ply m =
  if ply > 0 then match Oa.unsafe_get st.moves (ply - 1) with
    | None -> ()
    | Some prev ->
      let pos = Child.parent m in
      let dst = Move.dst @@ Child.move prev in
      match Position.piece_at_square pos dst with
      | None -> ()
      | Some p ->
        let i = countermove_idx p dst in
        Oa.unsafe_set_some st.countermoves i m

let sort_root_moves ?last st first =
  let total = Array.length st.root_moves in
  let last = match last with
    | Some last -> min total last
    | None -> total in
  let len = max 0 (last - first) in
  if len > 1 then
    let compare = Root_move.order in
    if len < total then
      let a = Array.sub st.root_moves ~pos:first ~len in
      Array.stable_sort a ~compare;
      Array.unsafe_blit ~src:a ~src_pos:0 ~dst:st.root_moves ~dst_pos:first ~len
    else Array.stable_sort st.root_moves ~compare

let find_root_move ?(pos = 0) st m =
  let rec aux i =
    if i < Array.length st.root_moves then
      let rm = Array.unsafe_get st.root_moves i in
      if Root_move.same_move rm m then Some rm else aux (i + 1)
    else None in
  aux pos

let best_root_move st = Array.unsafe_get st.root_moves 0
let current_root_move st = Array.unsafe_get st.root_moves st.pv_index

let has_reached_limits st =
  let elapsed = elapsed st in
  not (pondering st) && begin
    Limits.stopped st.limits || begin
      match Limits.nodes st.limits with
      | Some n when st.nodes >= n -> true
      | _ -> match Limits.max_time st.limits with
        | Some t -> elapsed >= t
        | None -> match Limits.movetime st.limits with
          | Some t -> elapsed >= t
          | None -> false
    end
  end

let check_limits st =
  let result = has_reached_limits st in
  if result then stop st;
  result

(* Will playing this position likely lead to a repetition draw? *)
let check_repetition st pos =
  Position.hash pos |> Hashtbl.find st.histogram |>
  Option.value_map ~default:false ~f:(fun n -> n >= 2)

(* Will the position lead to a draw? *)
let drawn st pos =
  check_repetition st pos ||
  Position.halfmove pos >= 100 ||
  Position.is_insufficient_material pos

(* Find a cached evaluation of the position. *)
let lookup st pos ~depth ~ply ~alpha ~beta ~pv =
  Tt.lookup st.tt ~pos ~depth ~ply ~alpha ~beta ~pv

let make_result st ~time =
  let lines = extract_lines st in
  let nodes = st.nodes and depth = st.root_depth in
  let r = Result.Fields.create ~lines ~time ~nodes ~depth in
  st.iter r;
  r
