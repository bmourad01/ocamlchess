open Core_kernel
open Monads.Std
open Bap_future.Std

module Bb = Bitboard
module Pre = Precalculated
module Child = Position.Child
module Threats = Position.Threats
module Oa = Option_array
module See = Position.See

let[@inline][@specialise] (>>?) x f = match x with
  | None -> f ()
  | Some _ -> x

let b2i = Bool.to_int
let b2in = Fn.compose b2i not

module Limits = struct
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

(* Transposition table for caching search results. *)
module Tt = struct
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
  end

  type entry = Entry.t

  (* We're going to use a fixed-size flat hash table. I've found that
     searches to a depth of >=12 will cache too many positions using
     the standard library's hash table.

     Thus, with a fixed size we need to decide which entries to evict,
     and when.
  *)
  type t = entry Oa.t

  let create ?(len = 0x100000) () =
    if len <= 0 then
      invalid_argf
        "Invalid transposition table length %d, must be greater than zero"
        len ()
    else Oa.create ~len

  let clear tt = Oa.clear tt

  external mul_hi64 :
    (int64[@unboxed]) ->
    (int64[@unboxed]) ->
    (int[@untagged]) =
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
    let ex = b2i @@ equal_bound bound Exact in
    depth + 2 * b2i pv <= e.depth - ex

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
end

type tt = Tt.t

module Result = struct
  type t = {
    pv       : Child.t list;
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
  Child.is_capture m ||
  Move.is_promote @@ Child.move m ||
  (check && Child.gives_check m)

let is_quiet ?(check = true) = Fn.non @@ is_noisy ~check

(* Our state for the entirety of the search. *)
module State = struct
  type t = {
    limits                     : limits;
    root                       : Position.t;
    check                      : bool;
    history                    : (Zobrist.key, int) Hashtbl.t;
    tt                         : tt;
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
    iter                       : Result.t -> unit;
    ponder                     : unit future option;
  }

  let pv_size = max_ply + 2
  let killer_size = max_ply
  let move_history_size = Piece.Color.count * Square.(count * count)
  let countermove_size = (1 lsl Piece.bits) * Square.count

  let create ~limits ~root ~history ~tt ~iter ~ponder =
    (* Make sure that the root position is in our history. *)
    let history = Hashtbl.copy history in
    Position.hash root |> Hashtbl.update history ~f:(function
        | Some n -> n | None -> 1); {
      limits;
      root;
      check = Position.in_check root;
      history;
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
  let killer1 st ply = Oa.get st.killer1 ply

  (* Get the second killer move. *)
  let killer2 st ply = Oa.get st.killer2 ply

  (* Is `m` a killer move? *)
  let is_killer st m ply = match killer1 st ply with
    | Some k when Child.same k m -> true
    | _ -> match killer2 st ply with
      | Some k -> Child.same k m
      | None -> false

  (* Update the killer move for a particular ply. *)
  let update_killer st ply m =
    match killer1 st ply with
    | Some k when Child.same k m -> ()
    | None -> Oa.set_some st.killer1 ply m
    | Some k ->
      Oa.set_some st.killer2 ply k;
      Oa.set_some st.killer1 ply m

  let history_idx m =
    let c = Piece.Color.to_int @@ Position.active @@ Child.parent m in
    let m = Child.move m in
    let src = Square.to_int @@ Move.src m in
    let dst = Square.to_int @@ Move.dst m in
    ((src lsl 1) lor c) * Square.count + dst

  (* Update the move history heuristic. *)
  let update_move_history st m depth =
    if depth > 0 then begin
      let i = history_idx m in
      let d = Array.unsafe_get st.move_history i + (depth * depth) in
      Array.unsafe_set st.move_history i d;
      match Position.active @@ Child.parent m with
      | White ->
        st.move_history_max_w <- max d st.move_history_max_w
      | Black ->
        st.move_history_max_b <- max d st.move_history_max_b
    end

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
    ply < 2 || match Oa.unsafe_get st.evals ply with
    | Some e -> eval > e
    | None when ply < 4 -> true
    | None -> match Oa.unsafe_get st.evals ply with
      | Some e -> eval > e
      | None -> true

  let lookup_eval st ply = Oa.unsafe_get st.evals ply
  let lookup_eval_unsafe st ply = Oa.unsafe_get_some_exn st.evals ply
  let clear_eval st ply = Oa.unsafe_set_none st.evals ply

  let stop st = st.stopped <- true

  let push_history st pos =
    Position.hash pos |>
    Hashtbl.update st.history ~f:(function
        | Some n -> n + 1 | None -> 1)

  let pop_history st pos =
    Position.hash pos |>
    Hashtbl.change st.history ~f:(function
        | None | Some 1 -> None
        | Some n -> Some (n - 1))

  let pondering st = match st.ponder with
    | Some p -> not @@ Future.is_decided p
    | None -> false

  let excluded st ply = Oa.unsafe_get st.excluded ply
  let has_excluded st ply = Oa.unsafe_is_some st.excluded ply
  let set_excluded st ply m = Oa.unsafe_set_some st.excluded ply m
  let clear_excluded st ply = Oa.unsafe_set_none st.excluded ply

  let is_excluded st ply m =
    excluded st ply |> Option.exists ~f:(Child.is_move m)

  let update_pv st ply m =
    let pv = Array.unsafe_get st.pv ply in
    let child_pv = Array.unsafe_get st.pv (ply + 1) in
    Oa.unsafe_set_some pv 0 m;
    let[@inline] rec append_child i =
      match Oa.unsafe_get child_pv (i - 1) with
      | None -> Oa.unsafe_set_none pv i
      | Some m ->
        Oa.unsafe_set_some pv i m;
        append_child (i + 1) in
    append_child 1

  let extract_pv st ~ply ~depth =
    let pv = Array.unsafe_get st.pv ply in
    let[@inline] rec extract acc i =
      if i >= depth then List.rev acc
      else match Oa.unsafe_get pv i with
        | None -> List.rev acc
        | Some m -> extract (m :: acc) (i + 1) in
    extract [] 0

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
    moves     : (Child.t * int) array;
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
        fun m -> Child.same best m

  let promote_by_value m =
    Child.move m |> Move.promote |>
    Option.value_map ~default:0 ~f:(fun k ->
        Piece.Kind.value @@ Move.Promote.to_piece_kind k)

  let promote_by_offset m =
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
        | Some k when Child.same m k -> killer1_offset
        | _ -> match k2 with
          | Some k when Child.same m k -> killer2_offset
          | _ -> match cm with
            | Some c when Child.same m c -> countermove_offset
            | _ -> 0 in
    let move_history =
      let max = State.move_history_max st pos in
      fun m ->
        let i = State.history_idx m in
        let h = Array.unsafe_get st.move_history i in
        (* We "squish" the history score so that it fits between bad
           captures and castling moves. *)
        (((h * history_scale) + max - 1) / max) + history_offset in
    score_aux moves ~f:(fun m ->
        if is_hash m then hash_offset
        else match See.go m with
          | Some see when see >= 0 -> good_capture_offset + see
          | Some see -> bad_capture_offset + see
          | None ->
            let promote = promote_by_offset m in
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
        else
          let p = promote_by_value m in
          if p <> 0 then p else match See.go m with
            | Some value -> value
            | None -> 0))

  (* Score the moves for quiescence search, when we generate
     quiet check evasions. *)
  let qescore (st : state) pos moves = score_aux moves ~f:(fun m ->
      Array.unsafe_get st.move_history @@ State.history_idx m)

  (* Score the moves for ProbCut. *)
  let pcscore moves ~ttentry =
    let is_hash = is_hash ttentry in
    let f m = Child.is_capture m || is_hash m in
    List.filter moves ~f |> function
    | [] -> None
    | moves -> Some (score_aux moves ~f:(fun m ->
        if is_hash m then hash_offset
        else Option.value_exn (See.go m)))
end

(* Helpers for the search. *)
module Search = struct
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

  let update_quiet st m ~ply ~depth =
    if is_quiet m then begin
      State.update_killer st ply m;
      State.update_move_history st m depth;
      State.update_countermove st ply m;
    end

  (* Return true if we fail high. *)
  let cutoff ?(q = false) st t m ~score ~beta ~ply ~depth ~pv =
    let result = ref false in
    if score > t.score then begin
      t.score <- score;
      if score > t.alpha then begin
        t.best <- Uopt.some m;
        if pv then State.update_pv st ply m;
        if pv && score < beta then begin
          t.alpha <- score;
          if not q then t.bound <- Exact;
        end else begin
          update_quiet st m ~ply ~depth;
          t.bound <- Lower;
          result := true
        end
      end
    end;
    !result

  let qcutoff = cutoff ~q:true ~depth:0

  (* Find a cached evaluation of the position. *)
  let lookup (st : state) pos ~depth ~ply ~alpha ~beta ~pv =
    Tt.lookup st.tt ~pos ~depth ~ply ~alpha ~beta ~pv

  (* Cache an evaluation. *)
  let store (st : state) t pos ~depth ~ply ~score ~pv =
    let eval = Uopt.of_option @@ State.lookup_eval st ply in
    Tt.store st.tt pos ~ply ~depth ~score ~eval
      ~best:t.best ~bound:t.bound ~pv

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
      let beta = min beta @@ mating (ply + 1) in
      if alpha >= beta then First alpha else Second (alpha, beta)
    else Second (alpha, beta)

  (* Evaluations aren't useful for positions that are in check, so just
     assume that if we've made it this far from the root then it's a draw *)
  let end_of_line pos ~check = if check then 0 else Eval.go pos
end

(* Quiescence search is used when we reach our maximum depth for the main
   search. The goal is then to keep searching only "noisy" positions, until
   we reach one that is "quiet", and then return our evaluation. *)
module Quiescence = struct
  (* Delta pruning.

     Skip searching this position if we're not in endgame and the static
     evaluation is significantly lower than alpha.
  *)
  let delta =
    let margin = Eval.Material.queen_mg in
    fun pos eval alpha ->
      eval + margin < alpha &&
      not (Eval.Phase.is_endgame pos)

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
    if Search.drawn st pos then 0
    else if ply >= max_ply || Search.check_limits st
    then Search.end_of_line pos ~check
    else Position.children pos |> function
      | [] -> if check then mated ply else 0
      | moves -> with_moves st pos moves ~alpha ~beta ~ply ~check ~pv ~init

  (* Search the available moves for a given position, but only if they are
     "noisy". *)
  and with_moves ?(init = true) st pos moves ~alpha ~beta ~ply ~check ~pv =
    let depth = tt_depth ~check ~init in
    match Search.lookup st pos ~depth ~ply ~alpha ~beta ~pv with
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
          let t = Search.create ~alpha ?score 0 in
          let finish () = t.score in
          let f = child st t ~beta ~ply ~pv ~check ~quiet_evasion in
          let score = Iterator.fold_until it ~init:() ~finish ~f in
          Search.store st t pos ~depth ~ply ~score ~pv;
          score

  (* Search a child of the current node. *)
  and child st t ~beta ~ply ~pv ~check ~quiet_evasion = fun () (m, order) ->
    let open Continue_or_stop in
    if should_skip t m ~order ~check ~quiet_evasion then Continue ()
    else
      let pos = Child.self m in
      State.set_move st ply m;
      State.push_history st pos;
      State.inc_nodes st;
      let score = Int.neg @@ go st pos ~pv
          ~init:false
          ~ply:(ply + 1)
          ~alpha:(-beta)
          ~beta:(-t.alpha) in
      State.pop_history st pos;
      if Search.qcutoff st t m ~score ~beta ~ply ~pv then Stop t.score
      else if st.stopped then Stop t.score
      else Continue ()

  and should_skip t m ~order ~check ~quiet_evasion =
    t.score > mated max_ply && begin
      (* If we didn't generate quiet check evasions, then ignore moves
         with negative SEE values. *)
      (not quiet_evasion && order < 0) ||
      (* If we're in check, then allow only one non-capturing evasion
         to be searched. *)
      (check && not (Child.is_capture m) && begin
          let result = t.state > 1 in
          t.state <- t.state + 1;
          result
        end)
    end
end

(* The main search of the game tree. The core of it is the negamax algorithm
   with alpha-beta pruning (and other enhancements). *)
module Main = struct
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
  let rec go (st : state) pos ~alpha ~beta ~ply ~depth ~pv =
    let check = Position.in_check pos in
    if pv then st.seldepth <- max st.seldepth ply;
    if Search.drawn st pos then 0
    else if ply >= max_ply || Search.check_limits st
    then Search.end_of_line pos ~check
    else match Position.children pos with
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
          with_moves st pos moves ~alpha ~beta ~ply ~depth ~check ~pv

  (* Search the available moves for the given position. *)
  and with_moves st pos moves ~alpha ~beta ~ply ~depth ~check ~pv =
    match Search.lookup st pos ~depth ~ply ~alpha ~beta ~pv with
    | First score -> score
    | Second ttentry ->
      let score, improving = evaluate st pos ~ply ~check ~ttentry in
      prune_non_pv_node st pos moves ~score ~alpha ~beta
        ~ply ~depth ~pv ~improving ~ttentry |> function
      | Some score -> score
      | None ->
        let it = Order.score st moves ~ply ~pos ~ttentry in
        let t = Search.create ~alpha () in
        let finish _ = t.score in
        let f = child st t pos ~beta ~depth ~ply
            ~check ~pv ~improving ~ttentry in
        let score = Iterator.fold_until it ~init:0 ~finish ~f in
        if not @@ State.has_excluded st ply then
          Search.store st t pos ~depth ~ply ~score ~pv;
        score

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
      nmp st pos ~score ~beta ~ply ~depth >>? fun () ->
      probcut st pos moves ~depth ~ply ~beta ~ttentry ~improving

  (* Search a child of the current node. *)
  and child st t pos ~beta ~depth ~ply ~check ~pv
      ~improving ~ttentry = fun i (m, order) ->
    let open Continue_or_stop in
    if should_skip st t m ~i ~beta ~depth ~ply ~order then Continue (i + 1)
    else match semc st pos m ~depth ~ply ~beta ~check ~ttentry with
      | First score -> Stop score
      | Second _ when st.stopped -> Stop t.score
      | Second ext ->
        let r = lmr st m ~i ~order ~beta ~ply ~depth ~check ~pv ~improving in
        let pos = Child.self m in
        State.set_move st ply m;
        State.inc_nodes st;
        let score = pvs st t pos ~i ~r ~beta ~ply ~depth:(depth + ext) ~pv in
        if Search.cutoff st t m ~score ~beta ~ply ~depth ~pv then Stop t.score
        else if st.stopped then Stop t.score
        else Continue (i + 1)

  and should_skip st t m ~i ~beta ~depth ~ply ~order =
    State.is_excluded st ply m || begin
      i > 0 && t.score > mated max_ply && begin
        see m ~order ~depth ||
        futile st t m ~beta ~ply ~depth
      end
    end

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
    let m = Eval.Material.pawn_mg in
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
    let m = Eval.Material.pawn_mg in
    let r = b2i improving in
    m * (depth - r)

  and rfp_max_depth = 8

  (* Null move pruning.

     If we forfeit our right to play a move and our opponent's best
     response still produces a beta cutoff, then we know this position
     is unlikely.

     This should not be called when the position is in check.
  *)
  and nmp st pos ~score ~beta ~ply ~depth =
    if not (State.is_null_move st (ply - 1))
    && score >= beta
    && score >= State.lookup_eval_unsafe st ply
    && depth >= nmp_min_depth
    && not (State.has_excluded st ply)
    && not (Eval.Phase.is_endgame pos)
    && Threats.(count @@ get pos @@ Position.inactive pos) <= 0 then
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
    && not (is_mate beta || is_mated beta)
    && probcut_tt ~ply ~depth ~beta_cut ~ttentry then
      Order.pcscore moves ~ttentry |> Option.bind ~f:(fun it ->
          let eval = State.lookup_eval_unsafe st ply in
          let f = probcut_child st pos ~eval ~depth ~ply ~beta_cut in
          Iterator.fold_until it ~init:None ~finish:Fn.id ~f)
    else None

  (* Confirm the move with quiescence search before dropping down to the
     full search. *)
  and probcut_child st pos ~eval ~depth ~ply ~beta_cut = fun k (m, order) ->
    let open Continue_or_stop in
    if probcut_skip st m ~eval ~beta_cut ~ply ~order then Continue None
    else
      let alpha = -beta_cut in
      let beta = -beta_cut + 1 in
      let child = Child.self m in
      State.set_move st ply m;
      State.push_history st child;
      State.inc_nodes st;
      let score = Int.neg @@ Quiescence.go st child
          ~alpha ~beta ~ply:(ply + 1) ~pv:false in
      let score =
        if score >= beta_cut then
          let depth = depth - (probcut_min_depth - 1) in
          Int.neg @@ go st child ~alpha ~beta ~depth
            ~ply:(ply + 1) ~pv:false
        else score in
      State.pop_history st child;
      if score >= beta_cut then
        let depth = depth - (probcut_min_depth - 2) in
        Tt.store st.tt pos ~ply ~depth ~score
          ~best:(Uopt.some m) ~eval:(Uopt.some eval)
          ~bound:Lower ~pv:false;
        Stop (Some score)
      else if st.stopped then Stop None
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
      && Child.same m (Uopt.unsafe_value entry.best)
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

     For moves that are ordered later in the list, try reducing the depth
     of the search. We should avoid doing this for the first couple of
     moves, or if the depth is too low.

     Note that singular extensions will only happen for TT moves, which
     are always ordered first, so we don't need to worry about it interacting
     with the results of LMR.

     Additionally, we want to avoid reducing on moves that are typically
     forcing (or forced), which includes:

     - Any time we're in check
     - Good captures
     - Promotions
     - Killer moves
     - Countermoves
     - Giving check
  *)
  and lmr st m ~i ~order ~beta ~ply ~depth ~check ~pv ~improving =
    if not check
    && i >= lmr_min_index
    && depth >= lmr_min_depth
    && order < Order.countermove_offset
    && not (Child.gives_check m) then
      let t = Bb.count @@ Child.new_threats m in
      1 + b2in improving - t
    else 0

  and lmr_min_depth = 3
  and lmr_min_index = 2

  (* Principal variation search.

     Attempt to search with a zero window around alpha, and do a full search
     if the score is within our normal window This can allow us to skip lines
     that are unlikely to be part of the PV.
  *)
  and pvs st t pos ~i ~r ~beta ~ply ~depth ~pv =
    let[@specialise] go ?(r = 0) alpha ~pv =
      State.push_history st pos;
      let score = go st pos ~pv ~alpha
          ~beta:(-t.alpha)
          ~ply:(ply + 1)
          ~depth:(depth - r - 1) in
      State.pop_history st pos;
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
end

(* Aspiration window.

   The idea is that if we have the best score from a shallower search,
   then we can use it as the basis for an initial window to search
   around (the initial values of alpha and beta).

   Then, we can recursively use the resulting scores of each search to
   adjust the window based on whether we failed low or high.
*)
module Aspiration = struct
  let min_depth = 6
  let initial_delta = 10

  let rec loop st moves depth ~alpha ~beta ~delta =
    let score =
      Main.with_moves st st.root moves ~alpha ~beta ~depth
        ~check:st.check ~ply:0 ~pv:true in
    if not st.stopped then
      let new_delta = delta * 2 in
      if score >= beta then
        let beta = min inf (score + delta) in
        loop st moves depth ~alpha ~beta ~delta:new_delta
      else if score <= alpha then
        let beta = (alpha + beta) / 2 in
        let alpha = max (-inf) (score - delta) in
        loop st moves depth ~alpha ~beta ~delta:new_delta
      else score
    else score

  let go st moves depth basis =
    if depth < min_depth then
      (* It shouldn't be necessary to enter the loop at lower depths.
         We will get a more accurate "best" score if we just keep
         searching a bit deeper before entering the loop. *)
      Main.with_moves st st.root moves ~depth
        ~alpha:(-inf) ~beta:inf ~check:st.check ~ply:0 ~pv:true
    else
      let delta = initial_delta in
      let alpha = max (-inf) (basis - delta) in
      let beta = min inf (basis + delta) in
      loop st moves depth ~alpha ~beta ~delta
end

let ply_to_moves ply = (ply + (ply land 1)) / 2

(* We'll use the UCI datatype, which is either a numerical score in centipawns,
   or our distance from checkmate.

   The centipawn score is always exact, since the aspiration loop will not
   terminate until we get a score that is within our search window.
*)
let convert_score score ~mate ~mated =
  let open Uci.Send.Info in
  if mate then Mate (ply_to_moves (mate_score - score))
  else if mated then Mate (-(ply_to_moves (mate_score + score)))
  else Cp (score, Exact)

let result (st : state) ~score ~time ~pv ~mate ~mated =
  let score = convert_score score ~mate ~mated in
  let r = Result.Fields.create ~pv ~score ~time
      ~nodes:st.nodes
      ~depth:st.root_depth
      ~seldepth:st.seldepth in
  st.iter r;
  r

(* If we've found a mate that's shorter than the root depth, then we will
   want to cut off the PV extraction at that point. *)
let pv_depth (st : state) score mate mated =
  if mate then min (mate_score - score) st.root_depth
  else if mated then min (mate_score + score) st.root_depth
  else st.root_depth

(* Use iterative deepening to optimize the search. This works by using TT
   entries from shallower searches in the move ordering for deeper searches,
   which makes pruning more effective. *)
let rec iterdeep ?(prev = None) (st : state) moves =
  let basis = Option.value_map prev ~default:(-inf) ~f:snd in
  let score = Aspiration.go st moves st.root_depth basis in
  (* Get the elapsed time ASAP. *)
  let time = State.elapsed st in
  (* If the search was stopped, use the previous completed result, if
     available. *)
  let mate = is_mate score in
  let mated = is_mated score in
  let pv = State.extract_pv st ~ply:0 ~depth:(pv_depth st score mate mated) in
  if Limits.stopped st.limits then match prev with
    | Some (result, _) -> result
    | None -> result st ~score ~time ~pv ~mate ~mated
  else next st moves ~score ~pv ~mate ~mated ~time

(* Decide whether to continue iterating. *)
and next st moves ~score ~pv ~mate ~mated ~time =
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
    let prev = Some (result, score) in
    iterdeep st moves ~prev
  end

(* Either checkmate or stalemate. *)
let no_moves ?(mzero = false) root iter =
  let score =
    let open Uci.Send.Info in
    if not mzero && Position.in_check root then Mate 0 else Cp (0, Exact) in
  let r = Result.Fields.create
      ~pv:[] ~score ~nodes:0 ~depth:0 ~seldepth:0 ~time:0 in
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

let go ?(iter = ignore) ?(ponder = None) ~root ~limits ~history ~tt () =
  match moves root limits with
  | [] -> no_moves root iter
  | _ when mate_in_zero limits -> no_moves root iter ~mzero:true
  | moves ->
    let st = State.create ~root ~limits ~history ~tt ~iter ~ponder in
    iterdeep st moves
