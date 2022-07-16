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
    time     : int option;
    stop     : unit future;
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

  let gametime
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
      ~active
      ~stop
      () =
    check_nodes nodes;
    check_mate mate;
    check_depth depth;
    let movetime = check_movetime movetime in
    let gametime = match wtime, btime with
      | None, None -> None
      | Some _, None -> invalid_arg "Missinc btime"
      | None, Some _ -> invalid_arg "Missing wtime"
      | Some wtime, Some btime ->
        let winc, binc = match winc, binc with
          | None, None -> 0, 0
          | Some winc, Some binc -> winc, binc
          | Some _, None -> invalid_arg "Missing binc"
          | None, Some _ -> invalid_arg "Missing winc" in
        Some (gametime ~wtime ~winc ~btime ~binc ~movestogo ~active ()) in
    let time = Option.merge movetime gametime ~f:min in
    if not infinite
    && Option.is_none nodes
    && Option.is_none mate
    && Option.is_none depth
    && Option.is_none time
    then invalid_arg "Limits were explicitly unspecified"
    else {infinite; nodes; mate; depth; time; stop}
end

type limits = Limits.t

(* Constants. *)
let inf = 65535
let mate_score = inf / 2
let max_ply = 256

(* Mate scores should be relative to the distance from the root position.
   Shorter distances are to be preferred. *)
let is_mate score = score >= mate_score - max_ply
let is_mated score = score <= (-mate_score) + max_ply

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

(* Transposition table for caching search results. *)
module Tt = struct
  module Entry = struct
    type t = {
      ply   : int;
      depth : int;
      score : int;
      best  : Position.legal;
      node  : node;
    } [@@deriving fields]

    let position {best; _} = Position.Legal.parent best
  end

  type entry = Entry.t
  type t = (Zobrist.key, entry) Hashtbl.t

  let create () = Hashtbl.create (module Int64)
  let clear = Hashtbl.clear

  (* Store the evaluation results for the position.

     If the position has already been cached in the table, then we can
     replace it if the new entry's depth is greater or equal to that of
     the old entry.
  *)
  let store tt pos ~ply ~depth ~score ~best ~node =
    let key = Position.hash pos in
    Hashtbl.update tt key ~f:(function
        | Some entry when Entry.depth entry > depth -> entry
        | None | Some _ ->
          Entry.Fields.create ~ply ~depth ~score ~best ~node)

  (* If this was a mate score, then adjust it to be relative to the
     current distance from root. *)
  let adjust_mate score ply =
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
  let lookup tt ~pos ~depth ~ply ~alpha ~beta ~pv =
    match Hashtbl.find tt @@ Position.hash pos with
    | None -> Second (alpha, beta, None)
    | Some entry when ply <= 0 || Entry.depth entry < depth ->
      Second (alpha, beta, Some entry)
    | Some entry ->
      let score = adjust_mate entry.score ply in
      match entry.node with
      | Pv when not pv -> First (score, entry.best)
      | Pv -> Second (alpha, beta, Some entry)
      | Cut ->
        let alpha = max alpha score in
        if alpha >= beta then First (score, entry.best)
        else Second (alpha, beta, Some entry)
      | All ->
        let beta = min beta score in
        if alpha >= beta then First (score, entry.best)
        else Second (alpha, beta, Some entry)

  (* Extract the principal variation from the table. *)
  let pv ?(mate = false) tt n m =
    let rec aux i acc pos =
      match Hashtbl.find tt @@ Position.hash pos with
      | None -> List.rev acc
      | Some Entry.{best; _} when mate || n > i ->
        let i = i + 1 in
        let acc = best :: acc in
        aux i acc @@ Legal.child best
      | _ -> List.rev acc in
    aux 1 [m] @@ Legal.child m
end

type tt = Tt.t

module Result = struct
  type t = {
    pv       : Position.legal list;
    score    : Uci.Send.Info.score;
    nodes    : int;
    depth    : int;
    seldepth : int;
    time     : int;
  } [@@deriving fields]

  let best {pv; _} = List.hd_exn pv
end

type result = Result.t

let is_quiet m =
  Option.is_none @@ Legal.capture m &&
  Option.is_none @@ Move.promote @@ Legal.move m

let is_noisy = Fn.non is_quiet

let b2i = Bool.to_int
let b2in b = Bool.to_int (not b)

(* Our state for the entirety of the search. *)
module State = struct
  module Oa = Option_array

  type state = {
    limits             : limits;
    root               : Position.t;
    history            : int Int64.Map.t;
    tt                 : tt;
    start_time         : Time.t;
    nodes              : int;
    killer1            : Position.legal Oa.t;
    killer2            : Position.legal Oa.t;
    move_history       : int array;
    move_history_max_w : int;
    move_history_max_b : int;
    stopped            : bool;
    seldepth           : int;
    evals              : int array;
    iter_              : Result.t -> unit;
  } [@@deriving fields]

  type 'a m = {run : 'r. ('a -> state -> 'r) -> state -> 'r}

  module M = struct
    type 'a t = 'a m

    let return x = {run = fun g s -> g x s}
    let bind x f = {run = fun g s -> x.run (fun x s -> (f x).run g s) s}
    let map x ~f = {run = fun g s -> x.run (fun x s -> g (f x) s) s}
    let map = `Custom map
  end

  include Monad.Make(M)

  include struct
    let get () = {run = fun g s -> g s s}
    let gets f = {run = fun g s -> g (f s) s}
    let update f = {run = fun g s -> g () (f s)}
    let run x s = x.run (fun x s -> x, s) s
    let eval x s = fst @@ run x s
    let update_if cnd f = if cnd then update f else return ()
  end

  let killer_size = max_ply
  let move_history_size = Piece.Color.count * Square.(count * count)
  let eval_size = Piece.Color.count * max_ply

  let create ~limits ~root ~history ~tt ~iter =
    (* Make sure that the root position is in our history. *)
    let history =
      Position.hash root |> Map.update history ~f:(function
          | Some n -> n | None -> 1) in {
      limits;
      root;
      history;
      tt;
      start_time = Time.now ();
      nodes = 0;
      killer1 = Oa.create ~len:killer_size;
      killer2 = Oa.create ~len:killer_size;
      move_history = Array.create ~len:move_history_size 0;
      move_history_max_w = 1;
      move_history_max_b = 1;
      stopped = false;
      seldepth = 0;
      evals = Array.create ~len:eval_size 0;
      iter_ = iter;
    }

  let elapsed st =
    int_of_float @@ Time.(Span.to_ms @@ diff (now ()) st.start_time)

  (* Start a new iteration. *)
  let new_iter st = {st with stopped = false; seldepth = 0}

  (* Call the iteration thunk when we have a new search result. *)
  let iter r st = st.iter_ r

  (* Increment the number of nodes we've evaluated. *)
  let inc_nodes st = {st with nodes = st.nodes + 1}

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
    let i = history_idx m in
    let d = Array.unsafe_get st.move_history i + (depth * depth) in
    Array.unsafe_set st.move_history i d;
    match Position.active @@ Legal.parent m with
    | White -> {st with move_history_max_w = max d st.move_history_max_w}
    | Black -> {st with move_history_max_b = max d st.move_history_max_b}

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

  let stop st = {st with stopped = true}

  let push_history pos st =
    let history =
      Position.hash pos |> Map.update st.history ~f:(function
          | Some n -> n + 1
          | None -> 1) in
    {st with history}

  let pop_history pos st =
    let history =
      Position.hash pos |> Map.change st.history ~f:(function
          | None | Some 1 -> None
          | Some n -> Some (n - 1)) in
    {st with history}
end

open State.Syntax

type 'a state = 'a State.m

let return = State.return

(* We use an iterator object that incrementally applies insertion sort to
   the array. In the context of alpha-beta pruning, we may not actually
   visit all the moves for a given position, so it makes no sense to waste
   time sorting the entire thing. *)
module Iterator = struct
  type t = {
    mutable i : int;
    length    : int;
    moves     : (Position.legal * int) array;
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
      if Uopt.is_some x then f acc @@ Uopt.unsafe_value x >>= function
        | Continue y -> aux y it ~f ~finish
        | Stop z -> return z
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
  let is_hash pos ~ttentry = match ttentry with
    | Some Tt.Entry.{best; _} -> Legal.same best
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
  let score moves ~ply ~pos ~ttentry =
    State.(gets @@ killer1 ply) >>= fun killer1 ->
    State.(gets @@ killer2 ply) >>= fun killer2 ->
    State.(gets move_history) >>= fun move_history ->
    State.(gets @@ move_history_max pos) >>| fun move_history_max ->
    let is_hash = is_hash pos ~ttentry in
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
  let qscore = function
    | [] -> Iterator.empty
    | moves -> fst @@ score_aux moves ~eval:(fun m ->
        let p = promote_by_value m in
        if p <> 0 then p
        else match See.go m with
          | Some value -> value
          | None -> 0)
end

let negm = Fn.compose return Int.neg

let leaf score ~ply = begin State.update @@ fun st -> {
    st with seldepth = max st.seldepth ply;
  } end >>| fun () -> score

let has_reached_limits =
  State.(gets elapsed) >>= fun elapsed ->
  State.(gets limits) >>= fun limits ->
  State.(gets nodes) >>| fun nodes ->
  Limits.stopped limits || begin
    match Limits.nodes limits with
    | Some n when nodes >= n -> true
    | _ -> match Limits.time limits with
      | Some t -> elapsed >= t
      | None -> false
  end

let check_limits =
  has_reached_limits >>= fun result ->
  State.(update_if result stop) >>| fun () -> result

(* Will playing this position likely lead to a repetition draw? *)
let check_repetition history pos =
  Position.hash pos |> Map.find history |>
  Option.value_map ~default:false ~f:(fun n -> n > 2)

(* Will the position lead to a draw? *)
let drawn pos =
  State.(gets history) >>| fun history ->
  check_repetition history pos ||
  Position.halfmove pos >= 100 ||
  Position.is_insufficient_material pos

(* These are conditions for returning a score of 0. *)
let check_limits_or_draw pos =
  check_limits >>= function
  | false -> drawn pos
  | true -> return true

(* Quiescence search is used when we reach our maximum depth for the main
   search. The goal is then to keep searching only "noisy" positions, until
   we reach one that is "quiet", and then return our evaluation. *)
module Quiescence = struct
  (* Search a position until it becomes "quiet". *)
  let rec go pos ~alpha ~beta ~ply =
    check_limits_or_draw pos >>= function
    | true -> leaf 0 ~ply
    | false -> Position.legal_moves pos |> function
      | [] -> leaf (if Position.in_check pos then mated ply else 0) ~ply
      | moves -> with_moves pos moves ~alpha ~beta ~ply

  (* Search the available moves for a given position, but only if they are
     "noisy". *)
  and with_moves pos moves ~alpha ~beta ~ply =
    State.(update inc_nodes) >>= fun () ->
    let eval = Eval.go pos in
    if eval >= beta then leaf beta ~ply
    else if delta pos ~eval ~alpha then leaf alpha ~ply
    else List.filter moves ~f:is_noisy |>
         Order.qscore |> Iterator.fold_until
           ~init:(max eval alpha)
           ~finish:(leaf ~ply)
           ~f:(child ~beta ~eval ~ply)

  (* Delta pruning. *)
  and delta pos ~eval ~alpha =
    eval + delta_margin <= alpha && not (Eval.is_endgame pos)

  and delta_margin = Piece.Kind.value Queen * Eval.material_weight

  (* Search a branch of the current node. *)
  and child ~beta ~eval ~ply = fun alpha (m, _) ->
    let open Continue_or_stop in
    let pos = Legal.child m in
    State.(update @@ push_history pos) >>= fun () ->
    go pos ~alpha:(-beta) ~beta:(-alpha) ~ply:(ply + 1) >>=
    negm >>= fun score ->
    State.(update @@ pop_history pos) >>| fun () ->
    if score >= beta then Stop beta else Continue (max score alpha)
end

(* The main search of the game tree. The core of it is the negamax algorithm
   with alpha-beta pruning (and other enhancements). *)
module Main = struct
  (* The results for a single ply. *)
  type t = {
    mutable best  : Position.legal;
    mutable alpha : int;
    mutable node  : node;
  }

  let new_search ?(alpha = -inf) ~best () = {best; alpha; node = All}

  let update_history_and_killer m ~ply ~depth =
    if is_quiet m then
      State.(gets @@ update_killer ply m) >>= fun () ->
      State.(update @@ update_move_history m depth)
    else return ()

  (* Beta cutoff. *)
  let cutoff t m ~ply ~depth =
    update_history_and_killer m ~ply ~depth >>| fun () ->
    t.best <- m;
    t.node <- Cut

  (* Alpha may have improved. *)
  let better t m ~score =
    if score > t.alpha then begin
      t.best <- m;
      t.alpha <- score;
      t.node <- Pv;
    end

  (* Find a cached evaluation of the position. *)
  let lookup pos ~depth ~ply ~alpha ~beta ~node =
    State.(gets tt) >>| fun tt ->
    let pv = equal_node node Pv in
    Tt.lookup tt ~pos ~depth ~ply ~alpha ~beta ~pv

  (* Cache an evaluation. *)
  let store t pos ~depth ~ply ~score =
    State.(gets tt) >>| fun tt ->
    Tt.store tt pos ~ply ~depth ~score ~best:t.best ~node:t.node

  (* Search from a new position. *)
  let rec go ?(null = false) pos ~alpha ~beta ~ply ~depth ~node =
    check_limits_or_draw pos >>= function
    | true -> leaf 0 ~ply
    | false ->
      let moves = Position.legal_moves pos in
      (* Check + single reply extension. *)
      let check = Position.in_check pos in
      let single = match moves with [_] -> true | _ -> false in
      let depth = depth + b2i (check || single) in
      (* Checkmate or stalemate. *)
      match moves with
      | [] -> leaf ~ply @@ if check then mated ply else 0
      | _ -> match mdp ~alpha ~beta ~ply with
        | First alpha -> return alpha
        | Second (alpha, beta) when depth <= 0 ->
          (* Depth exhausted, drop down to quiescence search. *)
          Quiescence.with_moves pos moves ~alpha ~beta ~ply
        | Second (alpha, beta) ->
          (* Find a cached evaluation of the position. *)
          lookup pos ~depth ~ply ~alpha ~beta ~node >>= function
          | First (score, _) -> leaf score ~ply
          | Second (alpha, beta, ttentry) ->
            (* Search the available moves. *)
            with_moves pos moves
              ~alpha ~beta ~ply ~depth ~check ~node ~null ~ttentry

  (* Search the available moves for the given position. *)
  and with_moves ?(ttentry = None) ?(null = false)
      pos moves ~alpha ~beta ~ply ~depth ~check ~node =
    eval pos ~ply ~check ~ttentry >>= fun (eval, improving) ->
    try_pruning_before_child pos moves
      ~eval ~alpha ~beta ~ply ~depth ~check
      ~null ~node ~improving ~ttentry >>= function
    | Some score -> leaf score ~ply
    | None -> Order.score moves ~ply ~pos ~ttentry >>= fun (it, best) ->
      let t = new_search ~alpha ~best () in
      let finish _ = return t.alpha in
      let f = child t ~eval ~beta ~depth ~ply ~check ~node ~improving in
      Iterator.fold_until it ~init:0 ~finish ~f >>= fun score ->
      store t pos ~depth ~ply ~score >>| fun () -> score

  (* There are a number of pruning heuristics we can use before we even try
     to search the child nodes of this position. This shouldn't be done if
     we're in check, performing a PV search, or searching a null move.

     Note that `eval` should be `None` if the position is in check.
  *)
  and try_pruning_before_child pos moves
      ~eval ~alpha ~beta ~ply ~depth ~check
      ~null ~node ~improving ~ttentry =
    match eval with
    | None -> return None
    | Some _ when equal_node node Pv || null -> return None
    | Some eval -> razor pos moves ~eval ~alpha ~ply ~depth >>= function
      | Some _ as score -> return score
      | None ->
        let their_threats =
          Threats.(count @@ get pos @@ Position.inactive pos) in
        match rfp ~depth ~eval ~beta ~their_threats ~improving with
        | Some _ as score -> return score
        | None -> nmp pos ~eval ~beta ~ply ~depth ~their_threats >>= function
          | Some _ as score -> return score
          | None -> probcut pos moves ~depth ~ply ~beta ~ttentry ~improving

  (* Decide whether to use the TT evaluation or the result of the evaluation
     function (or whether to skip altogether). *)
  and eval pos ~ply ~check ~ttentry =
    if not check then
      let eval = Eval.go pos in
      let score = match (ttentry : Tt.entry option) with
        | Some {score; node = Cut; _} when score > eval -> score
        | Some {score; node = All; _} when score < eval -> score
        | None | Some _ -> eval in
      State.(gets @@ update_eval pos ply score) >>| fun improving ->
      Some score, improving
    else return (None, false)

  (* Search a child of the current node. *)
  and child t ~eval ~beta ~depth ~ply
      ~check ~node ~improving = fun i (m, order) ->
    let open Continue_or_stop in
    let[@inline] next () = return @@ Continue (i + 1) in
    if not @@ should_skip t m ~eval ~beta ~depth ~check ~node ~order then
      (* Explore the child node. *)
      let pos = Legal.child m in
      lmr m ~i ~beta ~ply ~depth ~check ~node ~order ~improving >>= fun r ->
      pvs t pos ~i ~r ~beta ~ply ~depth ~node >>= fun score ->
      (* Update alpha if needed. *)
      better t m ~score;
      if score >= beta then
        (* Move was too good. *)
        cutoff t m ~ply ~depth >>| fun () -> Stop beta
      else next ()
    else next ()

  (* These pruning heuristics depend on conditions that change as we continue
     to search all children of the current node.

     The main idea is that if it looks like we're unlikely to improve the
     position at this point, then we should just skip searching the current
     move.
  *)
  and should_skip t m ~eval ~beta ~depth ~check ~node ~order =
    futile m ~eval ~alpha:t.alpha ~beta ~depth ~check ~node ||
    see m ~order ~depth

  (* Futility pruning.

     If our score is within a margin below alpha, then skip searching
     quiet moves (since they are likely to be "futile" in improving alpha).

     Note that `eval` should be `None` if we are in check.
  *)
  and futile m ~eval ~alpha ~beta ~depth ~check ~node =
    not (equal_node node Pv) &&
    is_quiet m &&
    not (Legal.gives_check m) &&
    depth <= futile_max_depth &&
    Option.value_map eval ~default:false
      ~f:(fun eval -> eval + futile_margin depth <= alpha)

  and futile_margin depth =
    let m = Piece.Kind.value Pawn * Eval.material_weight in
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

  (* Mate distance pruning.

     If we've already found a shorter path to checkmate than our current
     distance from the root, then just cut off the search here.
  *)
  and mdp ~alpha ~beta ~ply =
    let alpha = max alpha (-mate_score + ply) in
    let beta = min beta (mate_score - ply) in
    if alpha >= beta then First alpha else Second (alpha, beta)

  (* Reverse futility pruning.

     If our score is within a margin above beta, then it is likely too
     good, and should cause a cutoff.
  *)
  and rfp ~depth ~eval ~beta ~their_threats ~improving =
    Option.some_if begin
      depth <= rfp_max_depth &&
      eval - rfp_margin depth their_threats improving >= beta
    end eval

  and rfp_margin depth their_threats improving =
    let r = b2i (improving && their_threats <= 0) in
    Piece.Kind.value Pawn * Eval.material_weight * (depth - r)

  and rfp_max_depth = 6

  (* Null move pruning.

     If we forfeit our right to play a move and our opponent's best
     response still produces a beta cutoff, then we know this position
     is unlikely.
  *)
  and nmp pos ~eval ~beta ~ply ~depth ~their_threats =
    if their_threats <= 0
    && eval >= beta
    && depth >= nmp_min_depth
    && not @@ Eval.is_endgame pos then
      let r = if depth <= 6 then 2 else 3 in
      Position.null_move_unsafe pos |> go
        ~alpha:(-beta)
        ~beta:(-beta + 1)
        ~ply:(ply + 1)
        ~depth:(depth - r - 1)
        ~null:true
        ~node:Cut >>= negm >>| fun score ->
      Option.some_if (score >= beta) beta
    else return None

  and nmp_min_depth = 3

  (* Razoring.

     When approaching the horizon, if our evaluation is significantly
     lower than alpha, then do a zero window quiescence search and see
     if it will improve.
  *)
  and razor pos moves ~eval ~alpha ~ply ~depth =
    if depth <= razor_max_depth && eval + razor_margin depth < alpha then
      Quiescence.with_moves pos moves
        ~alpha:(alpha - 1) ~beta:alpha ~ply >>| fun score ->
      Option.some_if (score < alpha) score
    else return None

  and razor_max_depth = 3

  and razor_margin depth =
    Piece.Kind.value Knight * Eval.material_weight * depth

  (* ProbCut

     If we have any threats and a reduced search gives us a score within
     a margin above beta, then we can safely prune this branch.
  *)
  and probcut pos moves ~depth ~ply ~beta ~ttentry ~improving =
    let beta_cut = beta + probcut_beta_margin improving in
    if depth >= probcut_min_depth
    && probcut_tt ~depth ~beta_cut ~ttentry
    && Threats.(count @@ get pos @@ Position.active pos) > 0 then
      let finish () = return None in
      let f = probcut_child pos ~depth ~ply ~beta_cut in
      List.filter moves ~f:is_noisy |> Order.qscore |>
      Iterator.fold_until ~init:() ~finish ~f
    else return None

  and probcut_child pos ~depth ~ply ~beta_cut = fun () (m, _) ->
    let open Continue_or_stop in
    let child = Legal.child m in
    let alpha = -beta_cut and beta = -beta_cut + 1 in
    State.(update @@ push_history child) >>= fun () ->
    (* Confirm with quiescence search first. *)
    Quiescence.go child ~alpha ~beta ~ply:(ply + 1) >>=
    negm >>= fun score -> begin
      if score >= beta_cut then
        (* Do the full search. *)
        let depth = depth - probcut_min_depth + 1 in
        go child ~alpha ~beta ~depth
          ~ply:(ply + 1) ~node:Cut >>= negm
      else return score
    end >>= fun score ->
    State.(update @@ pop_history child) >>= fun () ->
    if score >= beta_cut then
      (* Save this cutoff in the TT. *)
      State.(gets tt) >>| fun tt ->
      let depth = depth - probcut_min_depth + 2 in
      Tt.store tt pos ~ply ~depth ~score ~best:m ~node:Cut;
      Stop (Some score)
    else return @@ Continue ()

  and probcut_tt ~depth ~beta_cut ~ttentry = match ttentry with
    | Some (entry : Tt.entry) ->
      entry.depth < depth - probcut_min_depth + 2 &&
      entry.score >= beta_cut
    | None -> true

  and probcut_beta_margin improving =
    let n = Piece.Kind.value Knight * Eval.material_weight in
    let p = Piece.Kind.value Pawn * Eval.material_weight in
    (n / 2) - ((p / 2) * b2i improving)

  and probcut_min_depth = 5

  (* Late move reduction.

     For moves that are likely to fail low, reduce the depth of the search.
     However, to avoid losing precision, we have to exclude the following
     kinds of moves:

     - Any PV node in a PVS search
     - Any response to us being in check
     - Moves with a negative SEE value
     - Moves that give check
     - Moves that introduce new threats
     - Killer moves
  *)
  and lmr m ~i ~beta ~ply ~depth ~check ~node ~order ~improving =
    if not check
    && not (equal_node node Pv)
    && i >= lmr_min_index
    && depth >= lmr_min_depth
    && Order.(order > bad_capture_offset)
    && not (Legal.gives_check m)
    then State.(gets @@ is_killer m ply) >>| function
      | false ->
        let t = Bb.count @@ Legal.new_threats m in
        max 0 (1 + b2in improving - t)
      | true -> 0
    else return 0

  and lmr_min_depth = 3
  and lmr_min_index = 1

  (* Principal variation search.

     Attempt to search with a zero window around alpha, and do a full search
     if the score is within our normal window This can allow us to skip lines
     that are unlikely to be part of the PV.
  *)
  and pvs ?(node = Pv) t pos ~i ~r ~beta ~ply ~depth =
    let f ?(r = 0) alpha node =
      State.(update @@ push_history pos) >>= fun () ->
      go pos ~node ~alpha ~beta:(-t.alpha)
        ~ply:(ply + 1) ~depth:(depth - r - 1) >>=
      negm >>= fun score ->
      State.(update @@ pop_history pos) >>| fun () ->
      score in
    (* Zero and full window for alpha. *)
    let zw = -t.alpha - 1 and fw = -beta in
    let node = swap_node node in
    if equal_node node Pv then
      (* First move in a PV node should always search the full window. *)
      if i = 0 then f fw node
      else f zw All ~r >>= fun score ->
        (* Ignore beta if this is the root node. *)
        if score > t.alpha && (ply = 0 || score < beta)
        then f fw node else return score
    else f zw All ~r >>= fun score ->
      (* Ignore beta if we're reducing the depth. *)
      if score > t.alpha && (r > 0 || score < beta)
      then f fw node else return score

  (* Determine the child node type for PVS:

     1. PV nodes remain PV nodes.
     2. CUT nodes become ALL nodes.
     3. ALL nodes become CUT nodes.
  *)
  and swap_node = function
    | Pv -> Pv
    | Cut -> All
    | All -> Cut

  (* Search from the root position.

     Note that a TT lookup should never cut the search short in the root
     node. We always want to continue with a full search.
  *)
  let root moves ~alpha ~beta ~depth =
    let ply = 0 and node = Pv in
    State.(gets root) >>= fun pos ->
    let check = Position.in_check pos in
    lookup pos ~depth ~ply ~alpha ~beta ~node >>= function
    | First _ -> assert false
    | Second (alpha, beta, ttentry) ->
      eval pos ~ply ~check ~ttentry >>= fun (eval, improving) ->
      Order.score moves ~ply ~pos ~ttentry >>= fun (it, best) ->
      let t = new_search ~alpha ~best () in
      let finish _ = return t.alpha in
      let f = child t ~eval ~beta ~depth ~ply ~check ~node ~improving in
      Iterator.fold_until it ~init:0 ~finish ~f >>= fun score ->
      store t pos ~depth ~ply ~score >>| fun () -> score, t.best

  (* Aspiration window.

     The idea is that if we have the best score from a shallower search,
     then we can use it as the basis for a window to search around (the
     initial values of alpha and beta), and hopefully narrow the search
     space.
  *)
  let rec aspire ?(low = 250) ?(high = 250) moves depth basis =
    let alpha, beta = window basis depth low high in
    root moves ~alpha ~beta ~depth >>= fun ((score, best) as result) ->
    State.(gets stopped) >>= fun stopped ->
    (* Search was stopped, or we landed inside the window. *)
    if stopped || (score > alpha && score < beta) then return result
    else (* Result was outside the window, so we need to widen a bit. *)
      let low, high = widen score beta low high in
      aspire moves depth basis ~low ~high

  (* Set up the initial window to search around. *)
  and window basis depth low high =
    if basis <= -inf || depth <= 1 then -inf, inf
    else basis - low, basis + high

  (* Widen the window based on the result of a search. *)
  and widen score beta low high =
    if score >= beta then low, high * 2 else low * 2, high
end

(* We'll use the UCI datatype, which is either a numerical score in centipawns,
   or our distance from checkmate. *)
let convert_score score tt root ~pv ~mate ~mated =
  let open Uci.Send.Info in
  let len = List.length pv in
  let full = (len + (len land 1)) / 2 in
  if mate then Mate full
  else if mated then Mate (-full)
  else match Hashtbl.find tt @@ Position.hash root with
    | None | Some Tt.Entry.{node = Pv; _} -> Cp (score, None)
    | Some Tt.Entry.{node = Cut; _} -> Cp (score, Some `lower)
    | Some Tt.Entry.{node = All; _} -> Cp (score, Some `upper)

(* For debugging, make sure that the PV is a legal sequence of moves. *)
let assert_pv pv moves = List.fold pv ~init:moves ~f:(fun moves m ->
    if not @@ List.exists moves ~f:(Legal.same m) then
      failwithf "Found an invalid move %s in the PV"
        (Position.San.of_legal m) ();
    Position.legal_moves @@ Legal.child m) |> ignore

let extract_pv moves ~depth ~best ~mate ~mated =
  State.(gets tt) >>| fun tt ->
  let pv = Tt.pv tt depth best ~mate:(mate || mated) in
  assert_pv pv moves;
  pv

let result ~depth ~score ~time ~pv ~mate ~mated =
  State.get () >>= fun {tt; root; nodes; seldepth; _} ->
  let score = convert_score score tt root ~pv ~mate ~mated in
  let r = Result.Fields.create ~pv ~score ~nodes ~depth ~seldepth ~time in
  State.(gets @@ iter r) >>| fun () -> r

(* Use iterative deepening to optimize the search. This works by using TT
   entries from shallower searches in the move ordering for deeper searches,
   which makes pruning more effective. *)
let rec iterdeep ?(prev = None) ?(depth = 1) moves =
  let basis = Option.value_map prev ~default:(-inf) ~f:snd in
  Main.aspire moves depth basis >>= fun (score, best) ->
  (* Get the elapsed time ASAP. *)
  State.(gets elapsed) >>= fun time ->
  (* If the search was stopped, use the previous completed result, if
     available. *)
  State.(gets limits) >>= fun limits ->
  let mate = is_mate score in
  let mated = is_mated score in
  if Limits.stopped limits then match prev with
    | Some (result, _) -> return result
    | None -> result ~depth ~score ~time ~pv:[best] ~mate ~mated
  else next moves ~depth ~score ~best ~mate ~mated ~time

(* Decide whether to continue iterating. *)
and next moves ~depth ~score ~best ~mate ~mated ~time =
  State.(gets limits) >>= fun limits ->
  State.(gets nodes) >>= fun nodes ->
  extract_pv moves ~depth ~best ~mate ~mated >>= fun pv ->
  result ~depth ~score ~time ~pv ~mate ~mated >>= fun result ->
  (* Last iteration may have eaten up at least half the allocated time,
     so the next (deeper) iteration is likely to take longer without
     having completed. Thus, we should abort the search. *)
  let too_long =
    Limits.time limits |>
    Option.value_map ~default:false ~f:(fun n -> time * 2 >= n) in
  (* Stop searching once we've reached the depth limit. *)
  let max_depth =
    Limits.depth limits |>
    Option.value_map ~default:false ~f:(fun n -> depth >= n) in
  (* Stop searching once we've reached the node limit. *)
  let max_nodes =
    Limits.nodes limits |>
    Option.value_map ~default:false ~f:(fun n -> nodes >= n) in
  (* Stop searching once we've found a mate in X (if applicable). *)
  let mate =
    mate &&
    Limits.mate limits |>
    Option.value_map ~default:false ~f:(fun n -> List.length pv <= n * 2) in
  (* Continue iterating? *)
  if not (too_long || max_nodes || max_depth || mate) then
    State.(update new_iter) >>= fun () ->
    let prev = Some (result, score) in
    iterdeep moves ~depth:(depth + 1) ~prev
  else return result

exception No_moves

let go ?(iter = ignore) ~root ~limits ~history ~tt () =
  match Position.legal_moves root with
  | [] -> raise No_moves
  | moves ->
    State.eval (iterdeep moves) @@
    State.create ~root ~limits ~history ~tt ~iter
