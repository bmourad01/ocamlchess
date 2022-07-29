open Core_kernel
open Monads.Std
open Bap_future.Std

module Bb = Bitboard
module Pre = Precalculated
module Legal = Position.Legal
module Threats = Position.Threats

module Limits = struct
  type kind =
    | Infinite
    | Depth of int
    | Time of int

  type t = {
    infinite : bool;
    nodes    : int option;
    mate     : int option;
    depth    : int option;
    movetime : int option;
    max_time : int option;
    stop     : unit future;
    moves    : Move.t list;
  } [@@deriving fields]

  let stopped limits = Future.is_decided @@ stop limits

  let check_nodes = function
    | None -> ()
    | Some n when n >= 1 -> ()
    | Some n ->
      invalid_argf "Invalid node limit %d, must be greater than 0" n ()

  let check_mate = function
    | None -> ()
    | Some n when n >= 1 -> ()
    | Some n ->
      invalid_argf "Invalid mate limit %d, must be greater than 0" n ()

  let check_depth = function
    | None -> ()
    | Some n when n >= 1 -> ()
    | Some n ->
      invalid_argf "Invalid depth limit %d, must be greater than 0" n ()

  let check_movetime = function
    | None -> None
    | (Some n) as movetime when n >= 1 -> movetime
    | Some n ->
      invalid_argf "Invalid movetime %d, must be greater than 0" n ()

  let manage_time
      ?(movestogo = None)
      ~wtime
      ~winc
      ~btime
      ~binc
      ~active
      () =
    (* Validate inputs. *)
    if wtime < 1 then
      invalid_argf "Invalid wtime %d, must be greater than 0" wtime ();
    if winc < 0 then
      invalid_argf "Invalid winc %d, must be positive" winc ();
    if btime < 1 then
      invalid_argf "Invalid btime %d, must be greater than 0" btime ();
    if binc < 0 then
      invalid_argf "Invalid binc %d, must be positive" binc ();
    Option.iter movestogo ~f:(function
        | n when n < 0 ->
          invalid_argf "Invalid movestogo %d, must be positive" n ()
        | _ -> ());
    (* Calculate the amount of time to search. *)
    let our_time, our_inc, their_time = match active with
      | Piece.White -> wtime, winc, btime
      | Piece.Black -> btime, binc, wtime in
    let time = match movestogo with
      | Some n -> our_time / (n + 3)
      | None ->
        let ratio =
          Float.(min (max (of_int our_time / of_int their_time) 1.0) 2.0) in
        our_time / int_of_float (20.0 *. ratio) in
    time + our_inc

  let default_depth = 8

  let create
      ?(nodes = None)
      ?(mate = None)
      ?(depth = Some default_depth)
      ?(movetime = None)
      ?(movestogo = None)
      ?(wtime = None)
      ?(winc = None)
      ?(btime = None)
      ?(binc = None)
      ?(infinite = false)
      ?(moves = [])
      ~active
      ~stop
      () =
    check_nodes nodes;
    check_mate mate;
    check_depth depth;
    let movetime = check_movetime movetime in
    let max_time = match wtime, btime with
      | None, None -> None
      | Some _, None -> invalid_arg "Missinc btime"
      | None, Some _ -> invalid_arg "Missing wtime"
      | Some wtime, Some btime ->
        let winc, binc = match winc, binc with
          | None, None -> 0, 0
          | Some winc, Some binc -> winc, binc
          | Some _, None -> invalid_arg "Missing binc"
          | None, Some _ -> invalid_arg "Missing winc" in
        Some (manage_time ~wtime ~winc ~btime ~binc ~movestogo ~active ()) in
    if not infinite
    && Option.is_none nodes
    && Option.is_none mate
    && Option.is_none depth
    && Option.is_none movetime
    && Option.is_none max_time
    && List.is_empty moves
    then invalid_arg "Limits were explicitly unspecified"
    else {infinite; nodes; mate; depth; movetime; max_time; stop; moves}
end

type limits = Limits.t

(* Constants. *)
let inf = 65535
let mate_score = inf / 2
let max_ply = 64

(* Mate scores should be relative to the distance from the root position.
   Shorter distances are to be preferred. *)
let is_mate score = score >= mate_score - max_ply && score <= mate_score
let is_mated score = score >= -mate_score && score <= (-mate_score) + max_ply

(* When we return a mate score during search, it's from the perspective
   of the player that is being mated. *)
let mating ply = mate_score - ply
let mated ply = -mate_score + ply

(* There are three main types of nodes, given a score `s`:

   1. `Pv` nodes are those where `alpha > s` and `s < beta` hold. Since this
      node's score is inside of our window, we will consider it as part of
      the principal variation.

   2. `Cut` nodes are those where `s >= beta` holds, meaning this node is
      expected to produce a beta cutoff.

   3. `All` nodes are those where `s <= alpha` holds, meaning we do not
      expect this node to improve alpha.
*)
type node = Pv | Cut | All [@@deriving equal]

let pp_node ppf = function
  | Pv -> Format.fprintf ppf "PV%!"
  | Cut -> Format.fprintf ppf "CUT%!"
  | All -> Format.fprintf ppf "ALL%!"

(* Transposition table for caching search results. *)
module Tt = struct
  module Entry = struct
    type t = {
      ply   : int;
      depth : int;
      score : int;
      best  : Legal.t;
      node  : node;
    } [@@deriving fields]

    let position {best; _} = Legal.parent best
    let same {best; _} m = Legal.same best m
  end

  type entry = Entry.t
  type t = (Zobrist.key, entry) Hashtbl.t

  let create () = Hashtbl.create (module Int64)
  let clear = Hashtbl.clear
  let find tt pos = Hashtbl.find tt @@ Position.hash pos

  let of_score score ply =
    if is_mate score then score + ply
    else if is_mated score then score - ply
    else score

  (* Store the evaluation results for the position.

     If the position has already been cached in the table, then we can
     replace it if the new entry's depth is greater or equal to that of
     the old entry.
  *)
  let store tt pos ~ply ~depth ~score ~best ~node =
    Position.hash pos |> Hashtbl.update tt ~f:(function
        | Some entry when Entry.depth entry > depth -> entry
        | None | Some _ ->
          let score = of_score score ply in
          Entry.Fields.create ~ply ~depth ~score ~best ~node)

  let to_score score ply =
    if is_mate score then score - ply
    else if is_mated score then score + ply
    else score

  (* Check for a previous evaluation of the position at a comparable depth.

     - Pv: the score is an exact evaluation for this position.

     - Cut: the score is a lower bound, so only return it if it causes a
            beta cutoff, which would prune the rest of the branch being
            searched.

     - All: the score is an upper bound, so if it doesn't improve alpha
            we can use that score to prune the rest of the branch being
            searched.
  *)
  let lookup tt ~pos ~depth ~ply ~alpha ~beta ~pv = match find tt pos with
    | None -> Second None
    | Some entry when pv || ply <= 0 || Entry.depth entry < depth ->
      Second (Some entry)
    | Some entry ->
      let score = to_score entry.score ply in
      match entry.node with
      | Pv -> First score
      | Cut when score >= beta -> First score
      | All when score <= alpha -> First score
      | _ -> Second (Some entry)

  (* Extract the principal variation from the table. *)
  let pv tt n m =
    let rec aux i acc pos = match find tt pos with
      | None -> List.rev acc
      | Some Entry.{best; _} when n > i ->
        aux (i + 1) (best :: acc) @@ Legal.child best
      | _ -> List.rev acc in
    aux 1 [m] @@ Legal.child m
end

type tt = Tt.t

module Result = struct
  type t = {
    pv       : Legal.t list;
    score    : Uci.Send.Info.score;
    nodes    : int;
    depth    : int;
    seldepth : int;
    time     : int;
  } [@@deriving fields]

  let best {pv; _} = List.hd pv
  let best_exn {pv; _} = List.hd_exn pv
end

type result = Result.t

(* The `check` parameter is for deciding when to generate quiet checks in
   quiescence search. *)
let is_noisy ?(check = true) m =
  Legal.is_capture m ||
  Move.is_promote @@ Legal.move m ||
  (check && Legal.gives_check m)

let is_quiet ?(check = true) = Fn.non @@ is_noisy ~check

(* Our state for the entirety of the search. *)
module State = struct
  module Oa = Option_array

  type t = {
    limits                     : limits;
    root                       : Position.t;
    history                    : (Zobrist.key, int) Hashtbl.t;
    tt                         : tt;
    start_time                 : Time.t;
    excluded                   : Legal.t Oa.t;
    killer1                    : Legal.t Oa.t;
    killer2                    : Legal.t Oa.t;
    move_history               : int array;
    mutable move_history_max_w : int;
    mutable move_history_max_b : int;
    mutable stopped            : bool;
    mutable seldepth           : int;
    mutable nodes              : int;
    mutable root_depth         : int;
    evals                      : int array;
    iter                       : Result.t -> unit;
    ponder                     : unit future option;
  }

  let killer_size = max_ply
  let move_history_size = Piece.Color.count * Square.(count * count)
  let eval_size = Piece.Color.count * max_ply

  let create ~limits ~root ~history ~tt ~iter ~ponder =
    (* Make sure that the root position is in our history. *)
    let history = Hashtbl.copy history in
    Position.hash root |> Hashtbl.update history ~f:(function
        | Some n -> n | None -> 1); {
      limits;
      root;
      history;
      tt;
      start_time = Time.now ();
      excluded = Oa.create ~len:max_ply;
      killer1 = Oa.create ~len:killer_size;
      killer2 = Oa.create ~len:killer_size;
      move_history = Array.create ~len:move_history_size 0;
      move_history_max_w = 1;
      move_history_max_b = 1;
      stopped = false;
      seldepth = 1;
      nodes = 0;
      root_depth = 1;
      evals = Array.create ~len:eval_size 0;
      iter;
      ponder;
    }

  let elapsed st =
    int_of_float @@ Time.(Span.to_ms @@ diff (now ()) st.start_time)

  (* Start a new iteration. *)
  let new_iter st =
    st.stopped <- false;
    st.seldepth <- 1;
    st.root_depth <- st.root_depth + 1

  (* Increment the number of nodes we've evaluated. *)
  let inc_nodes st = st.nodes <- st.nodes + 1

  (* Get the first killer move. *)
  let killer1 ply st = Oa.get st.killer1 ply

  (* Get the second killer move. *)
  let killer2 ply st = Oa.get st.killer2 ply

  (* Is `m` a killer move? *)
  let is_killer m ply st = match killer1 ply st with
    | Some m' when Legal.same m m' -> true
    | _ -> match killer2 ply st with
      | Some m' -> Legal.same m m'
      | None -> false

  (* Update the killer move for a particular ply. *)
  let update_killer ply m st =
    killer1 ply st |> Option.iter
      ~f:(Oa.set_some st.killer2 ply);
    Oa.set_some st.killer1 ply m

  let history_idx m =
    let c = Piece.Color.to_int @@ Position.active @@ Legal.parent m in
    let m = Legal.move m in
    let src = Square.to_int @@ Move.src m in
    let dst = Square.to_int @@ Move.dst m in
    ((src lsl 1) lor c) * Square.count + dst

  (* Update the move history heuristic. *)
  let update_move_history m depth st =
    if depth > 0 then begin
      let i = history_idx m in
      let d = Array.unsafe_get st.move_history i + (depth * depth) in
      Array.unsafe_set st.move_history i d;
      match Position.active @@ Legal.parent m with
      | White ->
        st.move_history_max_w <- max d st.move_history_max_w
      | Black ->
        st.move_history_max_b <- max d st.move_history_max_b
    end

  let move_history_max pos st = match Position.active pos with
    | White -> st.move_history_max_w
    | Black -> st.move_history_max_b

  let eval_idx pos ply =
    let c = Piece.Color.to_int @@ Position.active pos in
    ply * Piece.Color.count + c

  (* Update the evaluation history and return whether our position is
     improving or not *)
  let update_eval pos ply eval st =
    Array.unsafe_set st.evals (eval_idx pos ply) eval;
    ply < 2 || eval > Array.unsafe_get st.evals (eval_idx pos (ply - 2))

  let stop st = st.stopped <- true

  let push_history pos st =
    Position.hash pos |>
    Hashtbl.update st.history ~f:(function
        | Some n -> n + 1 | None -> 1)

  let pop_history pos st =
    Position.hash pos |>
    Hashtbl.change st.history ~f:(function
        | None | Some 1 -> None
        | Some n -> Some (n - 1))

  let pondering st = match st.ponder with
    | Some p -> not @@ Future.is_decided p
    | None -> false

  let excluded st ~ply = Oa.unsafe_get st.excluded ply
  let has_excluded st ~ply = Oa.unsafe_is_some st.excluded ply
  let set_excluded st m ~ply = Oa.unsafe_set_some st.excluded ply m
  let clear_excluded st ~ply = Oa.unsafe_set_none st.excluded ply

  let is_excluded st m ~ply =
    excluded st ~ply |> Option.exists ~f:(Legal.same m)
end

type state = State.t

(* We use an iterator object that incrementally applies selection sort to
   the array. In the context of alpha-beta pruning, we may not actually
   visit all the moves for a given position, so it makes no sense to waste
   time sorting the entire thing. *)
module Iterator = struct
  type t = {
    mutable i : int;
    length    : int;
    moves     : (Legal.t * int) array;
  }

  let empty = {
    i = 0;
    length = 0;
    moves = [||];
  }

  let create moves = {
    i = 0;
    length = Array.length moves;
    moves;
  }

  let next it =
    let c = it.i in
    let n = it.length in
    if c < n then
      let best_score = ref (-inf) in
      let best_index = ref c in
      let moves = it.moves in
      for i = c to n - 1 do
        let _, score = Array.unsafe_get moves i in
        if score > !best_score then begin
          best_score := score;
          best_index := i;
        end
      done;
      let i = !best_index in
      let result = Array.unsafe_get moves i in
      if i > c then
        Array.unsafe_get moves c |>
        Array.unsafe_set moves i;
      it.i <- c + 1;
      Uopt.some result
    else Uopt.none

  let fold_until =
    let open Continue_or_stop in
    let[@specialise] rec aux acc it ~f ~finish =
      let x = next it in
      if Uopt.is_some x then
        match f acc @@ Uopt.unsafe_value x with
        | Continue y -> aux y it ~f ~finish
        | Stop z -> z
      else finish acc in
    fun it ~init -> aux init it
end

(* Move ordering is critical for optimizing the performance of alpha-beta
   pruning. We use some heuristics to determine which moves are likely
   to be the best, and then search those first, hoping that the worse
   moves get pruned more effectively. *)
module Order = struct
  let good_capture_offset = 100
  let bad_capture_offset = -100
  let promote_offset = 96
  let killer1_offset = 95
  let killer2_offset = 94
  let castle_offset = 93
  let history_offset = -90
  let history_scale = 180

  (* Check if a particular move has been evaluated already. *)
  let is_hash ttentry = match ttentry with
    | Some entry -> Tt.Entry.same entry
    | None -> fun _ -> false

  let promote_by_value m =
    Legal.move m |> Move.promote |>
    Option.value_map ~default:0 ~f:(fun k ->
        Piece.Kind.value @@ Move.Promote.to_piece_kind k)

  let promote_by_offset m =
    let open Move.Promote in
    Legal.move m |> Move.promote |>
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

  (* Score each move according to `eval`. Note that `moves` is assumed to be
     non-empty. *)
  let score_aux moves ~eval =
    let best_score = ref (-inf) in
    let best_move = ref @@ List.hd_exn moves in
    let moves = array_of_list_map moves ~f:(fun m ->
        let score = eval m in
        if score > !best_score then begin
          best_score := score;
          best_move := m;
        end;
        m, score) in
    Iterator.create moves, !best_move

  (* Score the moves for normal search. Priority (from best to worst):

     1. Moves that were cached in the TT as PV or CUT nodes.
     2. Captures that produced a non-negative SEE score.
     3. Killer moves.
     4. Castling moves.
     5. Move history score (the "distance" heuristic).
     6. Captures that produced a negative SEE score.
  *)
  let score st moves ~ply ~pos ~ttentry =
    let killer1 = State.killer1 ply st in
    let killer2 = State.killer2 ply st in
    let move_history = st.move_history in
    let move_history_max = State.move_history_max pos st in
    let is_hash = is_hash ttentry in
    let killer m = match killer1, killer2 with
      | Some k, _ when Legal.same m k -> killer1_offset
      | _, Some k when Legal.same m k -> killer2_offset
      | _ -> 0 in
    let move_history m =
      let i = State.history_idx m in
      let h = Array.unsafe_get move_history i in
      ((h * history_scale) + move_history_max - 1) / move_history_max in
    score_aux moves ~eval:(fun m ->
        if is_hash m then inf
        else match See.go m with
          | Some see when see >= 0 -> good_capture_offset + see
          | Some see -> bad_capture_offset + see
          | None ->
            let promote = promote_by_offset m in
            if promote <> 0 then promote
            else
              let killer = killer m in
              if killer <> 0 then killer
              else match Legal.castle_side m with
                | Some _ -> castle_offset
                | None -> move_history m + history_offset)

  (* Score the moves for quiescence search. *)
  let qscore moves ~ttentry ~check =
    let is_hash = is_hash ttentry in
    let f m = is_noisy m ~check || is_hash m in
    List.filter moves ~f |> function
    | [] -> None
    | moves -> Some (score_aux moves ~eval:(fun m ->
        if is_hash m then inf
        else
          let p = promote_by_value m in
          if p <> 0 then p else match See.go m with
            | Some value -> value
            | None -> 0))
end

(* Helpers for the search. *)
module Search = struct
  (* The results for a single ply. *)
  type t = {
    mutable best  : Legal.t;
    mutable alpha : int;
    mutable score : int;
    mutable node  : node;
  }

  let create ?(alpha = -inf) ?score ~best () = {
    best;
    alpha;
    score = Option.value score ~default:(-inf);
    node = All
  }

  let update_history_and_killer st m ~ply ~depth =
    if is_quiet m then begin
      State.update_killer ply m st;
      State.update_move_history m depth st;
    end

  (* Beta cutoff. *)
  let cutoff st t m ~score ~beta ~ply ~depth =
    let result = score >= beta in
    if result then begin
      update_history_and_killer st m ~ply ~depth;
      t.best <- m;
      t.node <- Cut;
    end; result

  (* Alpha may have improved. *)
  let better t m ~score =
    if score > t.score then begin
      t.score <- score;
      if score > t.alpha then begin
        t.best <- m;
        t.alpha <- score;
        t.node <- Pv;
      end
    end

  (* Find a cached evaluation of the position. *)
  let lookup (st : state) pos ~depth ~ply ~alpha ~beta ~pv =
    Tt.lookup st.tt ~pos ~depth ~ply ~alpha ~beta ~pv

  (* Cache an evaluation. *)
  let store (st : state) t pos ~depth ~ply ~score =
    Tt.store st.tt pos ~ply ~depth ~score ~best:t.best ~node:t.node

  let has_reached_limits st =
    let elapsed = State.elapsed st in
    not (State.pondering st) && begin
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
    if result then State.stop st;
    result

  (* Will playing this position likely lead to a repetition draw? *)
  let check_repetition history pos =
    Position.hash pos |> Hashtbl.find history |>
    Option.value_map ~default:false ~f:(fun n -> n > 2)

  (* Will the position lead to a draw? *)
  let drawn (st : state) pos =
    check_repetition st.history pos ||
    Position.halfmove pos >= 100 ||
    Position.is_insufficient_material pos

  (* Mate distance pruning.

     If we've already found a shorter path to checkmate than our current
     distance from the root, then just cut off the search here.
  *)
  let mdp ~alpha ~beta ~ply =
    if ply > 0 then
      let alpha = max alpha @@ mated ply in
      let beta = min beta @@ mating ply in
      if alpha >= beta then First alpha else Second (alpha, beta)
    else Second (alpha, beta)

  (* Evaluations aren't useful for positions that are in check, so just
     assume that if we've made it this far from the root then it's a draw *)
  let end_of_line pos ~check = if check then 0 else Eval.go pos
end

let b2i = Bool.to_int
let b2in = Fn.compose b2i not

(* Quiescence search is used when we reach our maximum depth for the main
   search. The goal is then to keep searching only "noisy" positions, until
   we reach one that is "quiet", and then return our evaluation. *)
module Quiescence = struct
  (* Delta pruning.

     Skip searching this position if we're not in endgame and the static
     evaluation is significantly lower than alpha.
  *)
  let delta =
    let margin = Eval.Material.knight_mg in
    fun pos eval alpha -> eval + margin < alpha && not (Eval.is_endgame pos)

  (* Decide whether to use the TT evaluation or the result of the evaluation
     function (or neither, if we're in check). *)
  let eval st pos ~alpha ~beta ~ply ~check ~ttentry =
    if not check then
      let eval = Eval.go pos in
      let score = match (ttentry : Tt.entry option) with
        | Some {score; node = Cut; _}
          when Tt.to_score score ply > eval -> score
        | Some {score; node = All; _}
          when Tt.to_score score ply <= eval -> score
        | None | Some _ -> eval in
      if score >= beta then First score
      else if delta pos score alpha then First score
      else Second (max score alpha, Some score)
    else Second (alpha, None)

  (* TT entries for quiescence search have a depth of either 0 or -1,
     depending on whether this is the first ply or we're in check. *)
  let tt_depth ~check ~init = b2i (check || init) - 1

  (* To avoid state explosion, only generate quiet checks at the root. *)
  let order st moves pos ~ply ~check ~init ~ttentry =
    match Order.qscore moves ~check:init ~ttentry with
    | Some (it, best) -> Some (it, best, false)
    | None when not check -> None
    | None ->
      (* We're in check, but our only responses are quiet moves, so just
         generate all evasions. *)
      let it, best = Order.score st moves ~ply ~pos ~ttentry in
      Some (it, best, true)

  (* Search a position until it becomes "quiet". *)
  let rec go ?(init = true) st pos ~alpha ~beta ~ply ~pv =
    let check = Position.in_check pos in
    if Search.drawn st pos then 0
    else if ply >= max_ply || Search.check_limits st
    then Search.end_of_line pos ~check
    else Position.legal_moves pos |> function
      | [] -> if check then mated ply else 0
      | moves -> match Search.mdp ~alpha ~beta ~ply with
        | First alpha -> alpha
        | Second (alpha, beta) ->
          with_moves st pos moves ~alpha ~beta ~ply ~check ~pv ~init

  (* Search the available moves for a given position, but only if they are
     "noisy". *)
  and with_moves ?(init = true) st pos moves ~alpha ~beta ~ply ~check ~pv =
    let depth = tt_depth ~check ~init in
    match Search.lookup st pos ~depth ~ply ~alpha ~beta ~pv with
    | First score -> score
    | Second ttentry ->
      match eval st pos ~alpha ~beta ~ply ~check ~ttentry with
      | First score -> score
      | Second (alpha, score) ->
        match order st moves pos ~ply ~check ~init ~ttentry with
        | None -> Option.value_exn score
        | Some (it, best, evasion) ->
          let t = Search.create ~alpha ?score ~best () in
          let finish () = t.score in
          let f = child st t ~qe:(ref 0) ~beta ~eval ~ply ~pv ~evasion in
          let score = Iterator.fold_until it ~init:() ~finish ~f in
          Search.store st t pos ~depth ~ply ~score;
          score

  (* Search a child of the current node. *)
  and child st t ~qe ~beta ~eval ~ply ~pv ~evasion = fun () (m, order) ->
    let open Continue_or_stop in
    if should_skip t m ~qe ~order ~evasion
    then Continue ()
    else begin
      let pos = Legal.child m in
      State.push_history pos st;
      State.inc_nodes st;
      let score = Int.neg @@ go st pos ~pv
          ~init:false
          ~ply:(ply + 1)
          ~alpha:(-beta)
          ~beta:(-t.alpha) in
      State.pop_history pos st;
      Search.better t m ~score;
      if Search.cutoff st t m ~score ~beta ~ply ~depth:0
      || st.stopped then Stop t.score
      else Continue ()
    end

  and should_skip t m ~qe ~order ~evasion =
    t.score > mated max_ply &&
    if evasion then
      not (Legal.is_capture m) &&
      let result = !qe > 1 in
      incr qe;
      result
    else order < 0
end

(* The main search of the game tree. The core of it is the negamax algorithm
   with alpha-beta pruning (and other enhancements). *)
module Main = struct
  (* Decide whether to use the TT evaluation or the result of the evaluation
     function (or whether to skip altogether). *)
  let eval st pos ~ply ~check ~ttentry =
    if not check then
      let eval = Eval.go pos in
      let score = match (ttentry : Tt.entry option) with
        | Some {score; node = Cut; _}
          when Tt.to_score score ply > eval -> score
        | Some {score; node = All; _}
          when Tt.to_score score ply <= eval -> score
        | None | Some _ -> eval in
      let improving = State.update_eval pos ply score st in
      Some score, improving
    else None, false

  let update_seldepth (st : state) ply pv =
    if pv then st.seldepth <- max st.seldepth ply

  (* Search from a new position. *)
  let rec go ?(null = false) (st : state) pos ~alpha ~beta ~ply ~depth ~pv =
    let check = Position.in_check pos in
    update_seldepth st ply pv;
    if Search.drawn st pos then 0
    else if ply >= max_ply || Search.check_limits st
    then Search.end_of_line pos ~check
    else match Position.legal_moves pos with
      | [] -> if check then mated ply else 0
      | moves ->
        (* Check + single reply extension. *)
        let single = match moves with [_] -> true | _ -> false in
        let depth = depth + b2i (check || single) in
        match Search.mdp ~alpha ~beta ~ply with
        | First alpha -> alpha
        | Second (alpha, beta) when depth <= 0 ->
          (* Depth exhausted, drop down to quiescence search. *)
          Quiescence.with_moves st pos moves ~alpha ~beta ~ply ~check ~pv
        | Second (alpha, beta) ->
          (* Search the available moves. *)
          with_moves st pos moves ~alpha ~beta ~ply ~depth ~check ~pv ~null

  (* Search the available moves for the given position. *)
  and with_moves ?(null = false) st pos moves
      ~alpha ~beta ~ply ~depth ~check ~pv =
    (* Find a cached evaluation of the position. *)
    match Search.lookup st pos ~depth ~ply ~alpha ~beta ~pv with
    | First score -> score
    | Second ttentry ->
      let eval, improving = eval st pos ~ply ~check ~ttentry in
      match try_pruning_before_child st pos moves
              ~eval ~alpha ~beta ~ply ~depth ~check
              ~null ~pv ~improving ~ttentry with
      | Some score -> score
      | None ->
        let it, best = Order.score st moves ~ply ~pos ~ttentry in
        let t = Search.create ~alpha ~best () in
        let finish _ = t.score in
        let f = child st t pos ~eval ~beta ~depth
            ~ply ~check ~pv ~improving ~ttentry in
        let score = Iterator.fold_until it ~init:0 ~finish ~f in
        if not @@ State.has_excluded st ~ply then
          Search.store st t pos ~depth ~ply ~score;
        score

  (* There are a number of pruning heuristics we can use before we even try
     to search the child nodes of this position. This shouldn't be done if
     we're in check, performing a PV search, or searching a null move.

     Note that `eval` should be `None` if the position is in check.
  *)
  and try_pruning_before_child st pos moves
      ~eval ~alpha ~beta ~ply ~depth ~check
      ~null ~pv ~improving ~ttentry =
    match eval with
    | None -> None
    | Some _ when pv || null -> None
    | Some eval ->
      match razor st pos moves ~eval ~alpha ~ply ~depth ~pv with
      | Some _ as score -> score
      | None ->
        match rfp ~depth ~eval ~beta ~improving with
        | Some _ as score -> score
        | None -> match nmp st pos ~eval ~beta ~ply ~depth with
          | Some _ as score -> score
          | None -> probcut st pos moves ~depth ~ply ~beta ~ttentry ~improving

  (* Search a child of the current node. *)
  and child st t pos ~eval ~beta ~depth ~ply
      ~check ~pv ~improving ~ttentry = fun i (m, order) ->
    let open Continue_or_stop in
    if should_skip st t m ~eval ~beta ~depth ~ply ~check ~pv ~order
    then Continue (i + 1)
    else match semc st pos m ~depth ~ply ~beta ~check ~ttentry with
      | First score -> Stop score
      | Second _ when st.stopped -> Stop t.score
      | Second ext ->
        let r = lmr st m ~i ~beta ~ply ~depth ~check ~pv ~improving in
        (* Explore the child node *)
        let pos = Legal.child m in
        State.inc_nodes st;
        let score = pvs st t pos ~i ~r ~beta ~ply ~depth:(depth + ext) ~pv in
        Search.better t m ~score;
        if Search.cutoff st t m ~score ~beta ~ply ~depth
        || st.stopped then Stop t.score
        else Continue (i + 1)

  (* These pruning heuristics depend on conditions that change as we continue
     to search all children of the current node.

     The main idea is that if it looks like we're unlikely to improve the
     position at this point, then we should just skip searching the current
     move.
  *)
  and should_skip st t m ~eval ~beta ~depth ~ply ~check ~pv ~order =
    State.is_excluded st m ~ply || begin
      t.score > mated max_ply && begin
        futile m ~eval ~alpha:t.alpha ~beta ~depth ~check ~pv ||
        see m ~order ~depth
      end
    end

  (* Futility pruning.

     If our score is within a margin below alpha, then skip searching
     quiet moves (since they are likely to be "futile" in improving alpha).

     Note that `eval` should be `None` if we are in check.
  *)
  and futile m ~eval ~alpha ~beta ~depth ~check ~pv =
    not pv &&
    is_quiet m &&
    depth <= futile_max_depth &&
    Option.value_map eval ~default:false
      ~f:(fun eval -> eval + futile_margin depth <= alpha)

  and futile_margin depth =
    let m = Eval.Material.pawn_mg in
    m + m * depth

  and futile_max_depth = 6

  (* Bad captures have a particular offset in the move ordering, corresponding
     to a negative SEE value.

     If this move is unlikely to allow us compensation given the current depth
     of the search, then just skip the move.
  *)
  and see m ~order ~depth =
    Option.is_some @@ Legal.capture m &&
    order <= Order.bad_capture_offset &&
    order - Order.bad_capture_offset < depth * see_margin

  and see_margin = -(Piece.Kind.value Pawn * 2)

  (* Reverse futility pruning.

     If our score is within a margin above beta, then it is likely too
     good, and should cause a cutoff.
  *)
  and rfp ~depth ~eval ~beta ~improving =
    let e = eval - rfp_margin depth improving in
    Option.some_if (depth <= rfp_max_depth && e >= beta) eval

  and rfp_margin depth improving =
    let m = Eval.Material.pawn_mg in
    let r = b2i improving in
    m * (depth - r)

  and rfp_max_depth = 8

  (* Null move pruning.

     If we forfeit our right to play a move and our opponent's best
     response still produces a beta cutoff, then we know this position
     is unlikely.
  *)
  and nmp st pos ~eval ~beta ~ply ~depth =
    if eval >= beta
    && depth >= nmp_min_depth
    && not (Eval.is_endgame pos)
    && Threats.(count @@ get pos @@ Position.inactive pos) <= 0 then
      let r = if depth <= 6 then 2 else 3 in
      let score =
        Position.null_move_unsafe pos |> go st
          ~alpha:(-beta)
          ~beta:(-beta + 1)
          ~ply:(ply + 1)
          ~depth:(depth - r - 1)
          ~null:true
          ~pv:false |> Int.neg in
      Option.some_if (score >= beta) beta
    else None

  and nmp_min_depth = 3

  (* Razoring.

     When approaching the horizon, if our evaluation is significantly
     lower than alpha, then drop down to quiescence search to see if
     the position can be improved.
  *)
  and razor st pos moves ~eval ~alpha ~ply ~depth ~pv =
    if depth <= razor_max_depth && eval + razor_margin depth < alpha then
      let score = Quiescence.with_moves st pos moves ~ply ~pv
          ~check:false ~alpha:(alpha - 1) ~beta:alpha in
      Option.some_if (score < alpha) score
    else None

  and razor_max_depth = 7

  and razor_margin =
    let p = Eval.Material.pawn_mg in
    let n = Eval.Material.knight_mg in
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
    && probcut_tt ~depth ~beta_cut ~ttentry
    && Threats.(count @@ get pos @@ Position.active pos) > 0 then
      let finish () = None in
      let f = probcut_child st pos ~depth ~ply ~beta_cut in
      match Order.qscore moves ~ttentry ~check:true with
      | Some (it, _) -> Iterator.fold_until it ~init:() ~finish ~f
      | None -> None
    else None

  and probcut_child st pos ~depth ~ply ~beta_cut = fun () (m, _) ->
    let open Continue_or_stop in
    if not @@ State.is_excluded st m ~ply then
      let alpha = -beta_cut and beta = -beta_cut + 1 in
      let child = Legal.child m in
      State.inc_nodes st;
      State.push_history child st;
      (* Confirm with quiescence search first. *)
      let score = Int.neg @@ Quiescence.go st child
          ~alpha ~beta ~ply:(ply + 1) ~pv:false in
      let score =
        if score >= beta_cut then
          (* Do the full search. *)
          let depth = depth - (probcut_min_depth - 1) in
          Int.neg @@ go st child ~alpha ~beta ~depth
            ~ply:(ply + 1) ~pv:false
        else score in
      State.pop_history child st;
      if score >= beta_cut then
        (* Save this cutoff in the TT. *)
        let depth = depth - (probcut_min_depth - 2) in
        Tt.store st.tt pos ~ply ~depth ~score ~best:m ~node:Cut;
        Stop (Some score)
      else if st.stopped then Stop None
      else Continue ()
    else Continue ()

  (* If the evaluation was cached then make sure that it doesn't refute
     our hypothesis. *)
  and probcut_tt ~depth ~beta_cut ~ttentry = match ttentry with
    | Some (entry : Tt.entry) ->
      entry.depth < depth - (probcut_min_depth - 2) ||
      entry.score >= beta_cut
    | None -> true

  and probcut_margin improving =
    let p = Eval.Material.pawn_mg in
    let n = Eval.Material.knight_mg in
    (n / 2) - ((p / 2) * b2i improving)

  and probcut_min_depth = 5

  (* Singular extensions + multi-cut.

     If we have a TT entry with the best move, then try a reduced search of
     the position where we search every move except this one. If the result
     is within a margin below the score for this TT entry, then we say that
     this move is "singularly" good, and thus we should extend the search
     depth.

     On the other hand, if this entry has a score within a margin above beta,
     then it probably isn't singular and we can cut off the search early.
  *)
  and semc st pos m ~depth ~ply ~beta ~ttentry ~check = match ttentry with
    | None -> Second 0
    | Some entry ->
      let ttscore = Tt.(to_score (Entry.score entry) ply) in
      if not check
      && ply > 0
      && ply < st.root_depth * 2
      && not (equal_node All @@ Tt.Entry.node entry)
      && not (is_mate ttscore || is_mated ttscore)
      && depth >= sext_min_depth
      && not (State.has_excluded st ~ply)
      && Tt.Entry.same entry m
      && Tt.Entry.depth entry >= sext_min_ttdepth depth then
        let target = ttscore - depth * 3 in
        let depth = (depth - 1) / 2 in
        State.set_excluded st m ~ply;
        let score = go st pos ~depth ~ply
            ~alpha:(target - 1)
            ~beta:target
            ~pv:false in
        State.clear_excluded st ~ply;
        if score < target then Second 1
        else if target >= beta then First target
        else Second 0
      else Second 0

  and sext_min_depth = 4
  and sext_min_ttdepth depth = depth - (sext_min_depth - 1)

  (* Late move reduction.

     For moves that are likely to fail low, reduce the depth of the search.
     However, to avoid losing precision, we have to skip under the following
     conditions:

     - Any time we're in check
     - Any time we're searching PV node in a PVS search
     - Capture moves
     - Moves that give check
     - Killer moves
  *)
  and lmr st m ~i ~beta ~ply ~depth ~check ~pv ~improving =
    if not (check || pv)
    && i >= lmr_min_index
    && depth >= lmr_min_depth
    && is_quiet m
    && not @@ State.is_killer m ply st then
      let t = Bb.count @@ Legal.new_threats m in
      max 0 (1 + b2in improving - t)
    else 0

  and lmr_min_depth = 3
  and lmr_min_index = 1

  (* Principal variation search.

     Attempt to search with a zero window around alpha, and do a full search
     if the score is within our normal window This can allow us to skip lines
     that are unlikely to be part of the PV.
  *)
  and pvs st t pos ~i ~r ~beta ~ply ~depth ~pv =
    let[@specialise] go ?(r = 0) alpha ~pv =
      State.push_history pos st;
      let score = go st pos ~pv ~alpha
          ~beta:(-t.alpha)
          ~ply:(ply + 1)
          ~depth:(depth - r - 1) in
      State.pop_history pos st;
      -score in
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

  (* Search from the root position.

     Note that a TT lookup should never cut the search short in the root
     node. We always want to continue with a full search.
  *)
  let root (st : state) moves ~alpha ~beta ~depth =
    let ply = 0 and pos = st.root and pv = true in
    match Search.lookup st pos ~depth ~ply ~alpha ~beta ~pv with
    | First _ -> assert false
    | Second ttentry ->
      let check = Position.in_check pos in
      let eval, improving = eval st pos ~ply ~check ~ttentry in
      let it, best = Order.score st moves ~ply ~pos ~ttentry in
      let t = Search.create ~alpha ~best () in
      let finish _ = t.score in
      let f = child st t pos ~eval ~beta ~depth
          ~ply ~check ~pv ~improving ~ttentry in
      let score = Iterator.fold_until it ~init:0 ~finish ~f in
      Search.store st t pos ~depth ~ply ~score;
      score, t.best

  (* Aspiration window.

     The idea is that if we have the best score from a shallower search,
     then we can use it as the basis for a window to search around (the
     initial values of alpha and beta), and hopefully narrow the search
     space.
  *)
  let rec aspire ?(low = 250) ?(high = 250) st moves depth basis =
    let alpha, beta = window basis depth low high in
    let score, best = root st moves ~alpha ~beta ~depth in
    (* Search was stopped, or we landed inside the window. *)
    if st.stopped || (score > alpha && score < beta) then score, best
    else (* Result was outside the window, so we need to widen a bit. *)
      let low, high = widen score beta low high in
      aspire st moves depth basis ~low ~high

  (* Set up the initial window to search around. *)
  and window basis depth low high =
    if basis <= -inf || depth <= 1 then -inf, inf
    else max (-inf) (basis - low), min inf (basis + high)

  (* Widen the window based on the result of a search. *)
  and widen score beta low high =
    if score >= beta then low, high * 2 else low * 2, high
end

let ply_to_moves ply = (ply + (ply land 1)) / 2

(* We'll use the UCI datatype, which is either a numerical score in centipawns,
   or our distance from checkmate. *)
let convert_score score tt root ~pv ~mate ~mated =
  let open Uci.Send.Info in
  if mate then Mate (ply_to_moves (mate_score - score))
  else if mated then Mate (-(ply_to_moves (mate_score + score)))
  else match Tt.find tt root with
    | None | Some Tt.Entry.{node = Pv; _} -> Cp (score, None)
    | Some Tt.Entry.{node = Cut; _} -> Cp (score, Some `lower)
    | Some Tt.Entry.{node = All; _} -> Cp (score, Some `upper)

(* For debugging, make sure that the PV is a legal sequence of moves. *)
let assert_pv pv moves = List.fold pv ~init:moves ~f:(fun moves m ->
    if not @@ List.exists moves ~f:(Legal.same m) then
      failwithf "Found an invalid move %s in the PV"
        (Position.San.to_string m) ();
    Position.legal_moves @@ Legal.child m) |> ignore

let extract_pv (st : state) moves ~best =
  let pv = Tt.pv st.tt st.root_depth best in
  assert_pv pv moves;
  pv

let result (st : state) ~score ~time ~pv ~mate ~mated =
  let score = convert_score score st.tt st.root ~pv ~mate ~mated in
  let r = Result.Fields.create ~pv ~score ~time
      ~nodes:st.nodes
      ~depth:st.root_depth
      ~seldepth:st.seldepth in
  st.iter r;
  r

(* Use iterative deepening to optimize the search. This works by using TT
   entries from shallower searches in the move ordering for deeper searches,
   which makes pruning more effective. *)
let rec iterdeep ?(prev = None) (st : state) moves =
  let basis = Option.value_map prev ~default:(-inf) ~f:snd in
  let score, best = Main.aspire st moves st.root_depth basis in
  (* Get the elapsed time ASAP. *)
  let time = State.elapsed st in
  (* If the search was stopped, use the previous completed result, if
     available. *)
  let mate = is_mate score in
  let mated = is_mated score in
  if Limits.stopped st.limits then match prev with
    | Some (result, _) -> result
    | None -> result st ~score ~time ~pv:[best] ~mate ~mated
  else next st moves ~score ~best ~mate ~mated ~time

(* Decide whether to continue iterating. *)
and next st moves ~score ~best ~mate ~mated ~time =
  let pv = extract_pv st moves ~best in
  let result = result st ~score ~time ~pv ~mate ~mated in
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
  (* Stop searching once we've found a mate in X (if applicable). *)
  let mate_in_x =
    no_ponder && mate &&
    Limits.mate st.limits |>
    Option.value_map ~default:false ~f:(fun n ->
        ply_to_moves (mate_score - score) <= n) in
  (* Don't continue if there's a mating sequence within the
     current depth limit. *)
  let mate_inevitable = no_ponder && begin
      (mate && mate_score - score <= st.root_depth) ||
      (mated && mate_score + score <= st.root_depth)
    end in
  (* Continue iterating? *)
  if movetime_done
  || too_long
  || max_nodes
  || max_depth
  || mate_in_x
  || mate_inevitable then result else begin
    State.new_iter st;
    let prev = Some (result, score) in
    iterdeep st moves ~prev
  end

(* Either checkmate or stalemate. *)
let no_moves root iter =
  let score =
    let open Uci.Send.Info in
    if Position.in_check root then Mate 0 else Cp (0, None) in
  let r = Result.Fields.create
      ~pv:[] ~score ~nodes:0 ~depth:0 ~seldepth:0 ~time:0 in
  iter r;
  r

let moves pos limits =
  let moves = Position.legal_moves pos in
  match Limits.moves limits with
  | [] -> moves
  | searchmoves ->
    List.filter moves ~f:(fun m ->
        List.exists searchmoves ~f:(Legal.is_move m))

let go ?(iter = ignore) ?(ponder = None) ~root ~limits ~history ~tt () =
  match moves root limits with
  | [] -> no_moves root iter
  | moves ->
    let st = State.create ~root ~limits ~history ~tt ~iter ~ponder in
    iterdeep st moves
